use super::{key_deserializer::KeyDeserializer, Deserializer, Error, ErrorKind, ReportAt};
use either::Either;
use serde::{de, forward_to_deserialize_any};
use std::{borrow::Cow, iter, ops::Range};
use taml::{
	diagnostics::{
		Diagnostic, DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType,
		Reporter as diagReporter,
	},
	parsing::{Key, Map, Taml, TamlValue},
};
use tap::Pipe;

const EXTRA_FIELDS: &str = "taml::extra_fields";

#[allow(clippy::type_complexity)]
pub struct StructOrMapAccess<'a, 'de, Position: Clone, Reporter: diagReporter<Position>> {
	reporter: &'a mut Reporter,
	span: Range<Position>,
	entries: Box<
		dyn 'a
			+ Iterator<
				Item = (
					Key<'de, Position>,
					Either<Cow<'a, Taml<'de, Position>>, &'static str>,
				),
			>,
	>,
	next_value: Option<Either<Cow<'a, Taml<'de, Position>>, &'static str>>,
	fail_from_extra_fields: bool,
}
impl<'a, 'de, Position: Clone, Reporter: diagReporter<Position>>
	StructOrMapAccess<'a, 'de, Position, Reporter>
{
	pub fn new(
		reporter: &'a mut Reporter,
		span: Range<Position>,
		map: &'a Map<'de, Position>,
		struct_fields: Option<&'static [&'static str]>,
	) -> Self {
		let is_struct = struct_fields.is_some();
		let struct_fields = struct_fields.unwrap_or_default();
		let fail_from_extra_fields;
		#[allow(clippy::eval_order_dependence)]
		Self {
			span: span.clone(),
			entries: {
				let present = map.iter().filter_map(move |(k, v)| {
					((!is_struct || k.name.as_ref() != EXTRA_FIELDS)
						&& struct_fields.contains(&k.name.as_ref()))
					.then(|| (k.clone(), Either::Left(v.pipe(Cow::Borrowed))))
				});
				let absent = struct_fields.iter().filter_map({
					let span = span.clone();
					move |k| {
						(*k != EXTRA_FIELDS && !map.contains_key(*k)).then(|| {
							(
								Key {
									name: cervine::Cow::Borrowed(*k),
									span: span.clone(),
								},
								Either::Right(*k),
							)
						})
					}
				});
				let extra = map.iter().filter_map(move |(k, v)| {
					((is_struct && k.name.as_ref() == EXTRA_FIELDS)
						|| !struct_fields.contains(&k.name.as_ref()))
					.then(|| (k.clone(), v))
				});
				if struct_fields.contains(&EXTRA_FIELDS) {
					fail_from_extra_fields = false;
					present
						.chain(absent)
						.chain(iter::once((
							Key {
								name: cervine::Cow::Borrowed(EXTRA_FIELDS),
								span: span.clone(),
							},
							Either::Left(
								Taml {
									value: TamlValue::Map(
										extra.map(|(k, v)| (k, v.clone())).collect(),
									),
									span,
								}
								.pipe(Cow::Owned),
							),
						)))
						.pipe(Box::new)
				} else if !is_struct {
					fail_from_extra_fields = false;
					present
						.chain(extra.map(|(k, v)| (k, v.pipe(Cow::Borrowed).pipe(Either::Left))))
						.pipe(Box::new)
				} else {
					let mut found_extra_fields = false;
					for (k, _) in extra {
						found_extra_fields = true;
						reporter.report_with(|| Diagnostic {
							r#type: DiagnosticType::UnknownField,
							labels: vec![
								DiagnosticLabel {
									caption: format!("Unknown field `{}`.", k.name)
										.pipe(Cow::Owned::<str>)
										.into(),
									span: k.span.clone().into(),
									priority: DiagnosticLabelPriority::Primary,
								},
								DiagnosticLabel {
									caption: if struct_fields.is_empty() {
										"Hint: This struct does not accept any fields."
											.pipe(Cow::Borrowed)
									} else {
										let mut message =
											"Hint: The following additional fields are accepted here:"
												.to_string();

										let mut listed_any = false;
										for field in struct_fields {
											if !map.contains_key(*field) {
												listed_any = true;
												message = message
													+ " `" + field
													.replace('`', "\\`")
													.as_str() + "`,"
											}
										}
										if listed_any {
											message.pop();
											message.push('.');
										} else {
											message += "(None)"
										}
										message.pipe(Cow::Owned)
									}
									.into(),
									span: span.clone().into(),
									priority: DiagnosticLabelPriority::Auxiliary,
								},
							],
						})
					}
					fail_from_extra_fields = found_extra_fields;
					if fail_from_extra_fields {
						// This avoids additionally reporting missing fields,
						// which in this case is likely to be a less specific duplicate error.
						present.pipe(Box::new)
					} else {
						present.chain(absent).pipe(Box::new)
					}
				}
			},
			next_value: None,
			fail_from_extra_fields,
			reporter,
		}
	}
}

impl<'a, 'de, Position: Clone, Reporter: diagReporter<Position>> de::MapAccess<'de>
	for StructOrMapAccess<'a, 'de, Position, Reporter>
{
	type Error = Error;

	fn next_key_seed<K>(&mut self, seed: K) -> Result<Option<K::Value>, Self::Error>
	where
		K: de::DeserializeSeed<'de>,
	{
		self.entries
			.next()
			.map(|(k, v)| {
				self.next_value = Some(v);
				seed.deserialize(KeyDeserializer {
					key: k,
					reporter: self.reporter,
				})
			})
			.or_else(|| {
				self.fail_from_extra_fields
					.then(|| Err(ErrorKind::Reported.into()))
			})
			.transpose()
			.report_at(self.reporter, self.span.clone())
	}

	fn next_value_seed<V>(&mut self, seed: V) -> Result<V::Value, Self::Error>
	where
		V: de::DeserializeSeed<'de>,
	{
		self.next_value
			.take()
			.expect("`next_value_seed` called before `next_key_seed`")
			.pipe(|value| match value {
				Either::Left(value) => seed
					.deserialize(&mut Deserializer(value.as_ref(), self.reporter))
					.report_at(self.reporter, value.span.clone()),
				Either::Right(key) => seed
					.deserialize(MissingFieldDeserializer(key, self.span.clone()))
					.map_err({
						let span = self.span.clone();
						|err| {
							match err.kind {
								ErrorKind::SerdeCustom { msg } => todo!(),
								ErrorKind::SerdeInvalidType { .. } => {
									self.reporter.report_with(|| Diagnostic {
										r#type: DiagnosticType::MissingField,
										labels: vec![DiagnosticLabel {
											caption: Some(
												format!(
													"Missing field `{}`.",
													key.replace('`', "\\`")
												)
												.pipe(Cow::Owned),
											),
											span: span.into(),
											priority: DiagnosticLabelPriority::Primary,
										}],
									});
									ErrorKind::Reported
								}
								ErrorKind::SerdeInvalidValue {
									unexpected,
									expected,
								} => todo!(),
								ErrorKind::SerdeInvalidLength { len, expected } => todo!(),
								ErrorKind::SerdeUnknownVariant { variant, expected } => todo!(),
								ErrorKind::SerdeUnknownField { field, expected } => todo!(),
								ErrorKind::SerdeMissingField { field } => todo!(),
								ErrorKind::SerdeDuplicateField { field } => todo!(),
								ErrorKind::InvalidValue { msg } => todo!(),
								ErrorKind::Reported => ErrorKind::Reported,
							}
							.into()
						}
					})
					.report_at(self.reporter, self.span.clone()),
			})
	}

	fn size_hint(&self) -> Option<usize> {
		let size = self.entries.size_hint();
		size.1.filter(|l| *l == size.0)
	}
}

struct MissingFieldDeserializer<'a, Position: Clone>(&'a str, Range<Position>);
impl<'a, 'de, Position: Clone> de::Deserializer<'de> for MissingFieldDeserializer<'a, Position> {
	type Error = Error;

	fn deserialize_any<V>(self, visitor: V) -> Result<V::Value, Self::Error>
	where
		V: de::Visitor<'de>,
	{
		visitor.visit_none()
	}

	forward_to_deserialize_any! {
		bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
		bytes byte_buf option unit unit_struct newtype_struct seq tuple
		tuple_struct map struct enum identifier ignored_any
	}
}
