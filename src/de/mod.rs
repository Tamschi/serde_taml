use paste::paste;
use serde::de;
use std::{
	borrow::Cow,
	fmt::{self, Debug, Formatter},
	ops::Range,
};
use taml::{
	diagnostics::{
		Diagnostic, DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType,
		Reporter as diagReporter,
	},
	parsing::{parse, IntoToken, Key, Taml, TamlValue, VariantPayload},
	Token,
};
use tap::{Conv, Pipe};

mod key_deserializer;
mod list_access;
mod struct_access;

pub struct Deserializer<'a, 'de, Position: Clone, Reporter: diagReporter<Position>>(
	pub &'a Taml<'de, Position>,
	pub &'a mut Reporter,
);
impl<'a, 'de, Position: Clone, Reporter: diagReporter<Position>>
	Deserializer<'a, 'de, Position, Reporter>
{
	fn by_ref(&mut self) -> Deserializer<'_, 'de, Position, Reporter> {
		Deserializer(self.0, self.1)
	}
}

#[derive(Debug)]
pub struct Error {
	kind: ErrorKind,
}
impl Error {
	fn invalid_value(msg: &'static str) -> Self {
		ErrorKind::InvalidValue { msg }.into()
	}
}
impl fmt::Display for Error {
	fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
		todo!()
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
impl From<()> for Error {
	fn from(_: ()) -> Self {
		ErrorKind::Reported.into()
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

pub type Result<T> = std::result::Result<T, Error>;

#[allow(clippy::missing_errors_doc)]
pub fn from_str<'de, T: de::Deserialize<'de>, Reporter: diagReporter<usize>>(
	str: &'de str,
	reporter: &mut Reporter,
) -> Result<T> {
	use logos::Logos as _;
	let lexer = Token::lexer(str).spanned();
	from_tokens(lexer, reporter)
}

#[allow(clippy::missing_errors_doc)]
pub fn from_tokens<'de, T: de::Deserialize<'de>, Position: Clone + Default + Ord>(
	tokens: impl IntoIterator<Item = impl IntoToken<'de, Position>>,
	reporter: &mut impl diagReporter<Position>,
) -> Result<T> {
	//TODO: This seems overly explicit.
	let root = parse(tokens, reporter)?;

	from_taml(
		&Taml {
			value: TamlValue::Map(root),
			span: Position::default()..Position::default(),
		},
		reporter,
	)
}

#[allow(clippy::missing_errors_doc)]
pub fn from_taml<'de, T: de::Deserialize<'de>, Position: Clone + Ord>(
	taml: &Taml<'de, Position>,
	reporter: &mut impl diagReporter<Position>,
) -> Result<T> {
	T::deserialize(&mut Deserializer(&taml, reporter))
}

trait ReportFor<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>> {
	fn report_for(self, deserializer: &mut Deserializer<'a, 'de, Position, Reporter>) -> Self;
}
impl<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>, V>
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

trait ReportAt<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>> {
	fn report_at(self, reporter: &mut Reporter, span: Range<Position>) -> Self;
}
impl<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>, V>
	ReportAt<'a, 'de, Position, Reporter> for Result<V>
{
	fn report_at(self, reporter: &mut Reporter, span: Range<Position>) -> Self {
		match self {
			Ok(ok) => Ok(ok),
			Err(_) => todo!(),
		}
	}
}

trait ReportInvalidValue {
	fn report_invalid_value<V>(self, msg: &'static str) -> Result<V>;
}
impl<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>> ReportInvalidValue
	for &mut Deserializer<'a, 'de, Position, Reporter>
{
	fn report_invalid_value<V>(self, msg: &'static str) -> Result<V> {
		let span = self.0.span.clone().into();
		self.1.report_with(move || Diagnostic {
			r#type: DiagnosticType::InvalidValue,
			labels: vec![DiagnosticLabel {
				caption: msg.pipe(Cow::Borrowed).into(),
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
				match &self.0.value {
					TamlValue::$Variant(v) => visitor
						.[<visit_ $Type>](
							v.parse()
								.map_err(|_| Error::invalid_value(concat!("Expected ", stringify!($Type), ".")))
								.report_for(self)?,
						)
						.report_for(self),
					_ => self.report_invalid_value(concat!("Expected ", stringify!($Type), ".")),
				}
			}
		}
	)*};
}

#[allow(clippy::non_ascii_literal)]
impl<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>> de::Deserializer<'de>
	for &mut Deserializer<'a, 'de, Position, Reporter>
{
	type Error = Error;

	fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.0.value {
			TamlValue::String(s) => visitor.visit_str(s).report_for(self),
			TamlValue::Integer(i) => todo!(),
			TamlValue::Float(f) => todo!(),
			TamlValue::List(l) => todo!(),
			TamlValue::Map(m) => todo!(),
			TamlValue::EnumVariant { key, payload } => todo!(),
		}
	}

	fn deserialize_bool<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.0.value {
			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "true" => visitor.visit_bool(true).report_for(self),
			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "false" => visitor.visit_bool(false).report_for(self),
			_ => self.report_invalid_value("Expected boolean unit variant `true` or `false`."),
		}
	}

	parsed!(Integer => i8, i16, i32, i64, i128);
	parsed!(Integer => u8, u16, u32, u64, u128);
	parsed!(Float => f32, f64);

	fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.0.value {
			TamlValue::String(s) => visitor
				.visit_char(
					s.parse()
						.map_err(|_| {
							Error::invalid_value("Expected single character string (`\"…\"`).")
						})
						.report_for(self)?,
				)
				.report_for(self),
			_ => self.report_invalid_value("Expected single character string (`\"…\"`)."),
		}
	}

	fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.0.value {
			TamlValue::String(s) => visitor.visit_str(s).report_for(self),
			_ => self.report_invalid_value("Expected string (`\"…\"`)."),
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
		todo!()
	}

	fn deserialize_byte_buf<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		todo!()
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
		match &self.0.value {
			TamlValue::List(l) if l.is_empty() => visitor.visit_unit().report_for(self),
			_ => self.report_invalid_value("Expected unit (`()`)."),
		}
	}

	fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		self.deserialize_unit(visitor)
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
		match &self.0.value {
			TamlValue::List(l) => visitor
				.visit_seq(list_access::ListAccess::new(self.1, self.0.span.clone(), l))
				.report_for(self),
			_ => self.report_invalid_value("Expected list (`(…)`)."),
		}
	}

	fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		todo!()
	}

	fn deserialize_tuple_struct<V>(
		self,
		name: &'static str,
		len: usize,
		visitor: V,
	) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		todo!()
	}

	fn deserialize_map<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.0.value {
			TamlValue::Map(m) => visitor
				.visit_map(struct_access::StructAccess::new(
					self.1,
					self.0.span.clone(),
					m,
					&[],
				))
				.report_for(self),
			_ => self.report_invalid_value("Expected map."),
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
		match &self.0.value {
			TamlValue::Map(m) => visitor
				.visit_map(struct_access::StructAccess::new(
					self.1,
					self.0.span.clone(),
					m,
					fields,
				))
				.report_for(self),
			_ => self.report_invalid_value("Expected struct."),
		}
	}

	fn deserialize_enum<V>(
		self,
		name: &'static str,
		variants: &'static [&'static str],
		visitor: V,
	) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match &self.0.value {
			TamlValue::EnumVariant { key, payload } => {
				todo!()
			}
			_ => self.report_invalid_value("Expected enum."),
		}
	}

	fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		todo!()
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
