use joinery::JoinableIterator;
use paste::paste;
use serde::de::{self, Visitor};
use std::{
	borrow::Cow,
	fmt::{self, Debug, Display, Formatter},
	iter,
	ops::Range,
};
use taml::{
	diagnostics::{
		Diagnostic, DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType,
		Reporter as diagReporter,
	},
	parsing::{parse, IntoToken, Key, Taml, TamlValue, VariantPayload},
	Decoded, Token,
};
use tap::{Conv, Pipe};

mod enum_access;
mod key_deserializer;
mod list_access;
mod struct_or_map_access;

use enum_access::EnumAndVariantAccess;
use list_access::ListAccess;
use struct_or_map_access::StructOrMapAccess;

/// Used to decode *Decoded* values (`<…:…>`).
pub type Decoder = dyn Fn(&str) -> core::result::Result<Cow<[u8]>, Vec<DecodingError>>;

pub struct DecodingError {
	pub decoded_span: Range<usize>,
	pub message: Cow<'static, str>,
}

/// A TAML [Serde](`serde`)-[`Deserializer`](`serde::Deserializer`) implementation.
pub struct Deserializer<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> {
	pub data: &'a Taml<'de, Position>,
	pub reporter: &'a mut Reporter,
	pub decoders: &'a [(&'a str, &'a Decoder)],
}
impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>>
	Deserializer<'a, 'de, Position, Reporter>
{
	fn by_ref(&mut self) -> Deserializer<'_, 'de, Position, Reporter> {
		Deserializer {
			reporter: self.reporter,
			..*self
		}
	}
}

/// An *unspecific* deserialization error.  
/// To get more precise information, refer to the optional (unversioned!) [human-readable diagnostics](`taml::diagnostics`).
#[derive(Debug)]
pub struct Error {
	kind: ErrorKind,
}
impl Error {
	fn invalid_value(msg: &'static str) -> Self {
		ErrorKind::InvalidValue { msg }.into()
	}
	fn is_reported(&self) -> bool {
		matches!(self.kind, ErrorKind::Reported)
	}
}
impl fmt::Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		match &self.kind {
			ErrorKind::SerdeCustom { msg } => todo!(),
			ErrorKind::SerdeInvalidType {
				unexpected,
				expected,
			} => write!(
				f,
				"Invalid type: Expected {} but found {}",
				expected, unexpected
			),
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
			ErrorKind::Reported => write!(f, "Reported"),
		}
	}
}

#[derive(Debug)]
enum ErrorKind {
	SerdeCustom {
		msg: String,
	},
	SerdeInvalidType {
		unexpected: String,
		expected: String,
	},
	SerdeInvalidValue {
		unexpected: String,
		expected: String,
	},
	SerdeInvalidLength {
		len: usize,
		expected: String,
	},
	SerdeUnknownVariant {
		variant: String,
		expected: &'static [&'static str],
	},
	SerdeUnknownField {
		field: String,
		expected: &'static [&'static str],
	},
	SerdeMissingField {
		field: &'static str,
	},
	SerdeDuplicateField {
		field: &'static str,
	},
	InvalidValue {
		msg: &'static str,
	},
	Reported,
}

impl From<ErrorKind> for Error {
	fn from(kind: ErrorKind) -> Self {
		Self { kind }
	}
}

impl std::error::Error for Error {}
impl de::Error for Error {
	// This error type is never constructed directly. // <- This is wrong.
	fn custom<T>(msg: T) -> Self
	where
		T: fmt::Display,
	{
		ErrorKind::SerdeCustom {
			msg: msg.to_string(),
		}
		.into()
	}
	fn invalid_type(unexp: de::Unexpected, exp: &dyn de::Expected) -> Self {
		ErrorKind::SerdeInvalidType {
			unexpected: unexp.to_string(),
			expected: exp.to_string(),
		}
		.into()
	}
	fn invalid_value(unexp: de::Unexpected, exp: &dyn de::Expected) -> Self {
		ErrorKind::SerdeInvalidValue {
			unexpected: unexp.to_string(),
			expected: exp.to_string(),
		}
		.into()
	}
	fn invalid_length(len: usize, exp: &dyn de::Expected) -> Self {
		ErrorKind::SerdeInvalidLength {
			len,
			expected: exp.to_string(),
		}
		.into()
	}
	fn unknown_variant(variant: &str, expected: &'static [&'static str]) -> Self {
		ErrorKind::SerdeUnknownVariant {
			variant: variant.to_string(),
			expected,
		}
		.into()
	}
	fn unknown_field(field: &str, expected: &'static [&'static str]) -> Self {
		ErrorKind::SerdeUnknownField {
			field: field.to_string(),
			expected,
		}
		.into()
	}
	fn missing_field(field: &'static str) -> Self {
		ErrorKind::SerdeMissingField { field }.into()
	}
	fn duplicate_field(field: &'static str) -> Self {
		ErrorKind::SerdeDuplicateField { field }.into()
	}
}

/// Shorthand for <code>[std]::[result](std::result)::[Result](std::result::Result)&lt;T, [Error]></code>.
pub type Result<T> = std::result::Result<T, Error>;

/// [Deserialize](`de::Deserialize`)s `T` from a TAML-formatted string slice.
///
/// Human-readable diagnostics are [optionally reported](`taml::diagnostics`) while [`Error`]s are unspecific.
///
/// # Errors
///
/// Iff `taml_str` is not a valid TAML document or does not structurally match `T`'s [`Deserialize`](`de::Deserialize`) implementation.
pub fn from_taml_str<'de, T: de::Deserialize<'de>, Reporter: diagReporter<usize>>(
	taml_str: &'de str,
	reporter: &mut Reporter,
	decoders: &[(&str, &Decoder)],
) -> Result<T> {
	use logos::Logos as _;
	let lexer = Token::lexer(taml_str).spanned();
	from_taml_tokens(lexer, reporter, decoders)
}

/// [Deserialize](`de::Deserialize`)s `T` from a previously-tokenised TAML document.
///
/// Human-readable diagnostics are [optionally reported](`taml::diagnostics`) while [`Error`]s are unspecific.
///
/// # Errors
///
/// Iff `tokens` can't be parsed into a valid TAML document or does not structurally match `T`'s [`Deserialize`](`de::Deserialize`) implementation.
pub fn from_taml_tokens<'de, T: de::Deserialize<'de>, Position: PositionImpl>(
	tokens: impl IntoIterator<Item = impl IntoToken<'de, Position>>,
	reporter: &mut impl diagReporter<Position>,
	decoders: &[(&str, &Decoder)],
) -> Result<T> {
	let root = parse(tokens, reporter).map_err(|()| ErrorKind::Reported.conv::<Error>())?;

	from_taml_tree(
		&Taml {
			value: TamlValue::Map(root),
			span: Position::default()..Position::default(),
		},
		reporter,
		decoders,
	)
}

/// [Deserialize](`de::Deserialize`)s `T` from a pre-parsed TAML document.
///
/// Human-readable diagnostics are [optionally reported](`taml::diagnostics`) while [`Error`]s are unspecific.
///
/// # Errors
///
/// Iff `taml` does not structurally match `T`'s [`Deserialize`](`de::Deserialize`) implementation.
pub fn from_taml_tree<'de, T: de::Deserialize<'de>, Position: PositionImpl>(
	taml: &Taml<'de, Position>,
	reporter: &mut impl diagReporter<Position>,
	decoders: &[(&str, &Decoder)],
) -> Result<T> {
	T::deserialize(&mut Deserializer {
		data: taml,
		reporter,
		decoders,
	})
}

trait ReportFor<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> {
	fn report_for(self, deserializer: &mut Deserializer<'a, 'de, Position, Reporter>) -> Self;
}
impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>, V>
	ReportFor<'a, 'de, Position, Reporter> for Result<V>
{
	fn report_for(self, deserializer: &mut Deserializer<'a, 'de, Position, Reporter>) -> Self {
		match self {
			Ok(ok) => Ok(ok),
			Err(e) => {
				match e.kind {
					ErrorKind::SerdeCustom { msg } => todo!(),
					ErrorKind::SerdeInvalidType {
						unexpected,
						expected,
					} => todo!(),
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
					ErrorKind::Reported => (),
				};
				Err(ErrorKind::Reported.into())
			}
		}
	}
}

trait ReportAt<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> {
	fn report_at(self, reporter: &mut Reporter, span: Range<Position>) -> Self;
}
impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>, V>
	ReportAt<'a, 'de, Position, Reporter> for Result<V>
{
	fn report_at(self, reporter: &mut Reporter, span: Range<Position>) -> Self {
		match self {
			Ok(ok) => Ok(ok),
			Err(err) => {
				if !err.is_reported() {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::CustomErrorFromVisitor,
						labels: vec![DiagnosticLabel {
							caption: err.to_string().pipe(Cow::Owned::<str>).into(),
							span: span.into(),
							priority: DiagnosticLabelPriority::Primary,
						}],
					});
				}
				Err(ErrorKind::Reported.into())
			}
		}
	}
}

trait ReportInvalid {
	fn report_invalid_value<V>(self, msg: &'static str) -> Result<V>;
	fn report_invalid_type<V>(self, msg: &'static str) -> Result<V>;
	fn report_invalid_type_owned<V>(self, msg: impl Display) -> Result<V>;
}
impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> ReportInvalid
	for &mut Deserializer<'a, 'de, Position, Reporter>
{
	fn report_invalid_value<V>(self, msg: &'static str) -> Result<V> {
		let span = self.data.span.clone().into();
		self.reporter.report_with(move || Diagnostic {
			r#type: DiagnosticType::InvalidValue,
			labels: vec![DiagnosticLabel {
				caption: msg.pipe(Cow::Borrowed).into(),
				span,
				priority: DiagnosticLabelPriority::Primary,
			}],
		});
		Err(ErrorKind::Reported.into())
	}

	fn report_invalid_type<V>(self, msg: &'static str) -> Result<V> {
		let span = self.data.span.clone().into();
		self.reporter.report_with(move || Diagnostic {
			r#type: DiagnosticType::InvalidType,
			labels: vec![DiagnosticLabel {
				caption: msg.pipe(Cow::Borrowed).into(),
				span,
				priority: DiagnosticLabelPriority::Primary,
			}],
		});
		Err(ErrorKind::Reported.into())
	}

	fn report_invalid_type_owned<V>(self, msg: impl Display) -> Result<V> {
		let span = self.data.span.clone().into();
		self.reporter.report_with(move || Diagnostic {
			r#type: DiagnosticType::InvalidType,
			labels: vec![DiagnosticLabel {
				caption: msg.to_string().pipe(Cow::Owned::<str>).into(),
				span,
				priority: DiagnosticLabelPriority::Primary,
			}],
		});
		Err(ErrorKind::Reported.into())
	}
}

macro_rules! parsed {
	($Variant:ident => $($Type:ident),*$(,)?) => {$(
		paste! {
			fn [<deserialize_ $Type>]<V>(self, visitor: V) -> Result<V::Value>
			where
				V: de::Visitor<'de>,
			{
				match &self.data.value {
					TamlValue::$Variant(v) => visitor
						.[<visit_ $Type>](
							v.parse()
								.map_err(|_| Error::invalid_value(concat!("Expected ", stringify!($Type), ".")))
								.report_for(self)?,
						)
						.report_for(self),
					_ => self.report_invalid_type(concat!("Expected ", stringify!($Type), ".")),
				}
			}
		}
	)*};
}

macro_rules! parsed_float {
	($Variant:ident => $($Type:ident),*$(,)?) => {$(
		paste! {
			fn [<deserialize_ $Type>]<V>(self, visitor: V) -> Result<V::Value>
			where
				V: de::Visitor<'de>,
			{
				match &self.data.value {
					TamlValue::Integer(i) => {
						let span = self.data.span.clone().pipe(Some);
						self.reporter.report_with(|| Diagnostic {
							r#type: DiagnosticType::InvalidType,
							labels: vec![
								DiagnosticLabel {
									caption: concat!("Expected ", stringify!($Type), ".").pipe(Cow::Borrowed).into(),
									span: span.clone(),
									priority: DiagnosticLabelPriority::Primary,
								},
								DiagnosticLabel {
									caption: format!("Hint: Try `{}.0`.", i).pipe(Cow::Owned::<str>).into(),
									span,
									priority: DiagnosticLabelPriority::Auxiliary,
								},
							],
						});
						Err(ErrorKind::Reported.into())
					}
					TamlValue::$Variant(v) => visitor
						.[<visit_ $Type>](
							v.parse()
								.map_err(|_| Error::invalid_value(concat!("Expected ", stringify!($Type), ".")))
								.report_for(self)?,
						)
						.report_for(self),
					_ => self.report_invalid_type(concat!("Expected ", stringify!($Type), ".")),
				}
			}
		}
	)*};
}

#[allow(clippy::non_ascii_literal)]
impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> de::Deserializer<'de>
	for &mut Deserializer<'a, 'de, Position, Reporter>
{
	type Error = Error;

	fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::String(s) => visitor.visit_str(s),
			TamlValue::Decoded(decoded) => {
				visitor.visit_decoded(decoded, self.decoders, self.reporter)
			}
			TamlValue::Integer(i) => todo!(),
			TamlValue::Float(f) => todo!(),
			TamlValue::List(l) => visitor.visit_seq(ListAccess::new(self.by_ref(), l)),
			TamlValue::Map(m) => visitor.visit_map(StructOrMapAccess::new(
				self.reporter,
				self.data.span.clone(),
				self.decoders,
				m,
				None,
			)),
			TamlValue::EnumVariant { .. } => {
				visitor.visit_enum(EnumAndVariantAccess(&mut self.by_ref()))
			}
		}
		.report_for(self)
	}

	fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "true" => visitor.visit_bool(true).report_for(self),
			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "false" => visitor.visit_bool(false).report_for(self),
			_ => self.report_invalid_type("Expected boolean unit variant `true` or `false`."),
		}
	}

	parsed!(Integer => i8, i16, i32, i64, i128);
	parsed!(Integer => u8, u16, u32, u64, u128);
	parsed_float!(Float => f32, f64);

	fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::String(s) => visitor
				.visit_char(
					s.parse()
						.map_err(|_| {
							Error::invalid_value("Expected single character string (`\"…\"`).")
						})
						.report_for(self)?,
				)
				.report_for(self),
			_ => self.report_invalid_type("Expected single character string (`\"…\"`)."),
		}
	}

	fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::String(s) => visitor.visit_str(s).report_for(self),
			_ => self.report_invalid_type("Expected string (`\"…\"`)."),
		}
	}

	fn deserialize_string<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		self.deserialize_str(visitor)
	}

	fn deserialize_bytes<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::Decoded(decoded) => visitor
				.visit_decoded(decoded, self.decoders, self.reporter)
				.report_for(self),
			_ => self.report_invalid_type("Expected decoded string (`<…:…>`)."),
		}
	}

	fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		self.deserialize_bytes(visitor)
	}

	fn deserialize_option<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		// Options are flattened; that there's this `Deserializer` instance at all already means there is a value here.
		visitor.visit_some(&mut self.by_ref()).report_for(self)
	}

	fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		self.deserialize_tuple(0, visitor)
	}

	fn deserialize_unit_struct<V>(self, name: &'static str, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		self.deserialize_struct(name, &[], visitor)
	}

	fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		visitor
			.visit_newtype_struct(&mut self.by_ref())
			.report_for(self)
	}

	fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::List(l) => visitor
				.visit_seq(ListAccess::new(self.by_ref(), l))
				.report_for(self),
			_ => self.report_invalid_type("Expected list (`(…)`)."),
		}
	}

	fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::List(l) if l.len() == len => visitor
				.visit_seq(list_access::ListAccess::new(self.by_ref(), l))
				.report_for(self),
			TamlValue::List(l) => self.report_invalid_type_owned(format_args!(
				"Expected {}-tuple, but found a list with {} elements.",
				len,
				l.len(),
			)),
			_ => self.report_invalid_type_owned(format_args!("Expected {}-tuple (`(…)`).", len)),
		}
	}

	fn deserialize_tuple_struct<V>(
		self,
		_name: &'static str,
		len: usize,
		visitor: V,
	) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		self.deserialize_tuple(len, visitor)
	}

	fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::Map(m) => visitor
				.visit_map(StructOrMapAccess::new(
					self.reporter,
					self.data.span.clone(),
					self.decoders,
					m,
					None,
				))
				.report_for(self),
			_ => self.report_invalid_type("Expected map."),
		}
	}

	fn deserialize_struct<V>(
		self,
		_name: &'static str,
		fields: &'static [&'static str],
		visitor: V,
	) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::Map(m) => visitor
				.visit_map(StructOrMapAccess::new(
					self.reporter,
					self.data.span.clone(),
					self.decoders,
					m,
					fields.into(),
				))
				.report_for(self),
			_ => self.report_invalid_type("Expected struct."),
		}
	}

	fn deserialize_enum<V>(
		self,
		_name: &'static str,
		_variants: &'static [&'static str],
		visitor: V,
	) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::EnumVariant { .. } => visitor
				.visit_enum(EnumAndVariantAccess(self))
				.report_for(self),
			_ => self.report_invalid_type("Expected enum."),
		}
	}

	fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.data.value {
			TamlValue::EnumVariant {
				key,
				payload: VariantPayload::Unit,
			} => visitor.visit_str(key.name.as_ref()).report_for(self),
			_ => self.report_invalid_type("Expected identifier."),
		}
	}

	fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		visitor.visit_unit().report_for(self)
	}

	fn is_human_readable(&self) -> bool {
		true
	}
}

trait VisitDecoded<'de> {
	type Value;
	fn visit_decoded<Position: PositionImpl>(
		self,
		decoded: &Decoded<Position>,
		decoders: &[(&str, &Decoder)],
		reporter: &mut impl diagReporter<Position>,
	) -> Result<Self::Value>;
}
impl<'de, V> VisitDecoded<'de> for V
where
	V: de::Visitor<'de>,
{
	type Value = V::Value;

	fn visit_decoded<Position: PositionImpl>(
		self,
		decoded: &Decoded<Position>,
		decoders: &[(&str, &Decoder)],
		reporter: &mut impl diagReporter<Position>,
	) -> Result<Self::Value> {
		#![allow(clippy::map_unwrap_or)] // Needed to borrow `reporter`.
		decoders
			.iter()
			.find_map(|(encoding, decoder)| {
				(*encoding == decoded.encoding.as_ref()).then(|| *decoder)
			})
			.map(|decoder| match decoder(decoded.decoded.as_ref()) {
				Ok(Cow::Borrowed(slice)) => self.visit_bytes(slice),
				Ok(Cow::Owned(vec)) => self.visit_byte_buf(vec),
				Err(errors) => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidValue,
						labels: errors
							.into_iter()
							.map(
								|DecodingError {
								     decoded_span,
								     message,
								 }| DiagnosticLabel {
									caption: message.into(),
									span: {
										let escape_shift = decoded.decoded[..decoded_span.start]
											.chars()
											.filter(|c| *c == '>')
											.count();
										let start = decoded_span.start + escape_shift;
										let end = decoded_span.end
											+ escape_shift + decoded.decoded[decoded_span]
											.chars()
											.filter(|c| *c == '>')
											.count();
										decoded.decoded_span.start.offset_range(start..end)
									},
									priority: DiagnosticLabelPriority::Primary,
								},
							)
							.chain(iter::once(DiagnosticLabel {
								caption: Cow::Borrowed("Encoding specified here.").into(),
								span: decoded.encoding_span.clone().into(),
								priority: DiagnosticLabelPriority::Auxiliary,
							}))
							.collect(),
					});
					Err(ErrorKind::Reported.into())
				}
			})
			.unwrap_or_else(|| {
				reporter.report_with(|| Diagnostic {
					r#type: DiagnosticType::CustomErrorFromVisitor,
					labels: vec![
						DiagnosticLabel {
							caption: Cow::Owned::<str>(format!(
								"Unrecognized encoding `{}`.",
								decoded.encoding.replace('`', "\\`")
							))
							.into(),
							span: decoded.encoding_span.clone().into(),
							priority: DiagnosticLabelPriority::Primary,
						},
						DiagnosticLabel {
							caption: Cow::Owned::<str>(format!(
								"Hint: Available encodings are: {}.",
								if decoders.is_empty() {
									"(None)".to_string()
								} else {
									format!(
										"`{}`",
										decoders
											.iter()
											.map(|(encoding, _)| encoding.replace('`', "\\`"))
											.join_with("`, `")
									)
								}
							))
							.into(),
							span: decoded.encoding_span.clone().into(),
							priority: DiagnosticLabelPriority::Auxiliary,
						},
					],
				});
				Err(ErrorKind::Reported.into())
			})
	}
}

pub trait PositionImpl: Debug + Clone + Default + PartialEq {
	fn offset_range(&self, local_range: Range<usize>) -> Option<Range<Self>>;
}

impl PositionImpl for usize {
	fn offset_range(&self, local_range: Range<usize>) -> Option<Range<Self>> {
		Some(self + local_range.start..self + local_range.end)
	}
}

impl PositionImpl for () {
	fn offset_range(&self, _local_range: Range<usize>) -> Option<Range<Self>> {
		None
	}
}
