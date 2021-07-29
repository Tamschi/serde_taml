use joinery::JoinableIterator;
use paste::paste;
use serde::de;
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
	DataLiteral, Token,
};
use tap::{Conv, Pipe};

mod enum_access;
mod key_deserializer;
mod list_access;
mod struct_or_map_access;

pub mod type_overrides;

use enum_access::EnumAndVariantAccess;
use list_access::ListAccess;
use struct_or_map_access::StructOrMapAccess;
use type_overrides::{AssertAcceptableAndUnwrapOrDefault, ForcedTamlValueType, Override, OVERRIDE};

/// Used to encode data literals (`<…:…>`) into binary data.
pub type Encoder = dyn Fn(&str) -> core::result::Result<Cow<[u8]>, Vec<EncodeError>>;

/// Simplified diagnostic that can be emitted as error by [`Encoder`] implementations.
///
/// These diagnostics are intended for human consumption and **their processing within `serde_taml` is not versioned**.
pub struct EncodeError {
	/// The half-open range of the unescaped input [`str`] to label, indexed by bytes.
	pub unescaped_input_span: Range<usize>,
	/// A text with which to label `input_span`. This should ideally be a detailed and self-contained error message.
	pub message: Cow<'static, str>,
}

/// A TAML [Serde](`serde`)-[`Deserializer`](`serde::Deserializer`) implementation.
pub struct Deserializer<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> {
	/// The parsed TAML tree to deserialize further.
	pub data: &'a Taml<'de, Position>,
	/// A reporter for human-readable [`diagnostics`](`taml::diagnostics`).
	///
	/// Diagnostic output is not versioned! You can skip its generation entirely by passing `&mut ()`.
	pub reporter: &'a mut Reporter,
	/// A list of recognised encodings for data literals, as pairs of (unescaped) encoding identifier and [`Encoder`] references.
	pub encoders: &'a [(&'a str, &'a Encoder)],
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
			ErrorKind::SerdeCustom { msg } => write!(f, "Serde-custom: {}", msg),
			ErrorKind::SerdeInvalidType {
				unexpected,
				expected,
			} => write!(
				f,
				"Invalid type: Expected {} but found {}.",
				expected, unexpected
			),
			ErrorKind::SerdeInvalidValue {
				unexpected,
				expected,
			} => write!(
				f,
				"Invalid value: Expected {} but found {}.",
				expected, unexpected
			),
			ErrorKind::SerdeInvalidLength { len, expected } => write!(
				f,
				"Invalid type: Expected {} but found a length of {}.",
				expected, len
			),
			ErrorKind::SerdeUnknownVariant { variant, expected } => write!(
				f,
				"Unknown variant `{}`, expected one of: {}",
				variant.replace('`', "\\`"),
				if expected.is_empty() {
					Cow::Borrowed("None")
				} else {
					Cow::Owned(format!("`{}`", expected.iter().join_with("`, `")))
				}
			),
			ErrorKind::SerdeUnknownField { field, expected } => write!(
				f,
				"Unknown field `{}`, expected one of: {}",
				field.replace('`', "\\`"),
				if expected.is_empty() {
					Cow::Borrowed("None")
				} else {
					Cow::Owned(format!("`{}`", expected.iter().join_with("`, `")))
				}
			),
			ErrorKind::SerdeMissingField { field } => write!(f, "Missing field: {}", field),
			ErrorKind::SerdeDuplicateField { field } => write!(f, "Duplicate field: {}", field),
			ErrorKind::InvalidValue { msg } => write!(f, "Invalid value: {}", msg),
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
	encoders: &[(&str, &Encoder)],
) -> Result<T> {
	use logos::Logos as _;
	let lexer = Token::lexer(taml_str).spanned();
	from_taml_tokens(lexer, reporter, encoders)
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
	encoders: &[(&str, &Encoder)],
) -> Result<T> {
	let root = parse(tokens, reporter).map_err(|()| ErrorKind::Reported.conv::<Error>())?;

	from_taml_tree(
		&Taml {
			value: TamlValue::Map(root),
			span: Position::default()..Position::default(),
		},
		reporter,
		encoders,
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
	encoders: &[(&str, &Encoder)],
) -> Result<T> {
	OVERRIDE.take();
	T::deserialize(&mut Deserializer {
		data: taml,
		reporter,
		encoders,
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
			Err(Error {
				kind: ErrorKind::Reported,
			}) => Err(ErrorKind::Reported.into()),
			Err(e) => {
				let span = deserializer.data.span.clone();
				deserializer.reporter.report_with(|| match e.kind {
					ErrorKind::SerdeCustom { msg } => Diagnostic {
						type_: DiagnosticType::CustomErrorFromVisitor,
						labels: vec![DiagnosticLabel::new(
							msg,
							span,
							DiagnosticLabelPriority::Primary,
						)],
					},
					ErrorKind::SerdeInvalidType {
						unexpected,
						expected,
					} => Diagnostic {
						type_: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							format!("Expected {} but found {}.", expected, unexpected),
							span,
							DiagnosticLabelPriority::Primary,
						)],
					},
					ErrorKind::SerdeInvalidValue {
						unexpected,
						expected,
					} => Diagnostic {
						type_: DiagnosticType::InvalidValue,
						labels: vec![DiagnosticLabel::new(
							format!("Expected {} but found {}.", expected, unexpected),
							span,
							DiagnosticLabelPriority::Primary,
						)],
					},
					ErrorKind::SerdeInvalidLength { len, expected } => Diagnostic {
						type_: DiagnosticType::InvalidLength,
						labels: vec![DiagnosticLabel::new(
							format!(
								"Expected {} but found something with length {}.",
								expected, len
							),
							span,
							DiagnosticLabelPriority::Primary,
						)],
					},
					ErrorKind::SerdeUnknownVariant { variant, expected } => Diagnostic {
						type_: DiagnosticType::UnknownVariant,
						labels: vec![
							DiagnosticLabel::new(
								format!("Unknown variant: `{}`.", variant.replace('`', "\\`")),
								span.clone(),
								DiagnosticLabelPriority::Primary,
							),
							DiagnosticLabel::new(
								if expected.is_empty() {
									"Hint: No expected variants available.".pipe(Cow::Borrowed)
								} else {
									let mut message =
										"Hint: The following variants are accepted here:"
											.to_string();

									let mut listed_any = false;
									for variant in expected {
										listed_any = true;
										message = message
											+ " `" + variant.replace('`', "\\`").as_str()
											+ "`,";
									}
									if listed_any {
										message.pop();
										message.push('.');
									} else {
										message += "(None)";
									}
									message.pipe(Cow::Owned)
								},
								span,
								DiagnosticLabelPriority::Auxiliary,
							),
						],
					},
					ErrorKind::SerdeUnknownField { field, expected } => Diagnostic {
						type_: DiagnosticType::UnknownField,
						labels: vec![
							DiagnosticLabel::new(
								format!("Unknown field: `{}`.", field.replace('`', "\\`")),
								span.clone(),
								DiagnosticLabelPriority::Primary,
							),
							DiagnosticLabel::new(
								if expected.is_empty() {
									"Hint: No expected fields available.".pipe(Cow::Borrowed)
								} else {
									let mut message =
										"Hint: The following fields are accepted here:".to_string();

									let mut listed_any = false;
									for field in expected {
										listed_any = true;
										message = message
											+ " `" + field.replace('`', "\\`").as_str()
											+ "`,";
									}
									if listed_any {
										message.pop();
										message.push('.');
									} else {
										message += "(None)";
									}
									message.pipe(Cow::Owned)
								},
								span,
								DiagnosticLabelPriority::Auxiliary,
							),
						],
					},
					ErrorKind::SerdeMissingField { field } => Diagnostic {
						type_: DiagnosticType::MissingField,
						labels: vec![DiagnosticLabel::new(
							format!("Missing field: `{}`.", field.replace('`', "\\`")),
							span,
							DiagnosticLabelPriority::Primary,
						)],
					},
					ErrorKind::SerdeDuplicateField { field } => Diagnostic {
						// TAML cannot actually contain duplicate fields after parsing
						// (since that raises an error and the data structure does not support it),
						// but a `serde::de::Deserialize`-implementation can still generate this error.
						type_: DiagnosticType::CustomErrorFromVisitor,
						labels: vec![DiagnosticLabel::new(
							format!("Duplicate field: `{}`.", field.replace('`', "\\`")),
							span,
							DiagnosticLabelPriority::Primary,
						)],
					},
					ErrorKind::InvalidValue { msg } => Diagnostic {
						type_: DiagnosticType::InvalidValue,
						labels: vec![DiagnosticLabel::new(
							msg,
							span,
							DiagnosticLabelPriority::Primary,
						)],
					},
					ErrorKind::Reported => unreachable!(),
				});
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
						type_: DiagnosticType::CustomErrorFromVisitor,
						labels: vec![DiagnosticLabel::new(
							err.to_string(),
							span,
							DiagnosticLabelPriority::Primary,
						)],
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
		let span = self.data.span.clone();
		self.reporter.report_with(move || Diagnostic {
			type_: DiagnosticType::InvalidValue,
			labels: vec![DiagnosticLabel::new(
				msg,
				span,
				DiagnosticLabelPriority::Primary,
			)],
		});
		Err(ErrorKind::Reported.into())
	}

	fn report_invalid_type<V>(self, msg: &'static str) -> Result<V> {
		let span = self.data.span.clone();
		self.reporter.report_with(move || Diagnostic {
			type_: DiagnosticType::InvalidType,
			labels: vec![DiagnosticLabel::new(
				msg,
				span,
				DiagnosticLabelPriority::Primary,
			)],
		});
		Err(ErrorKind::Reported.into())
	}

	fn report_invalid_type_owned<V>(self, msg: impl Display) -> Result<V> {
		let span = self.data.span.clone();
		self.reporter.report_with(move || Diagnostic {
			type_: DiagnosticType::InvalidType,
			labels: vec![DiagnosticLabel::new(
				msg.to_string(),
				span,
				DiagnosticLabelPriority::Primary,
			)],
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
								.map_err(|_| Error::invalid_value(concat!("Failed to parse ", stringify!($Type), ".")))
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
							type_: DiagnosticType::InvalidType,
							labels: vec![
								DiagnosticLabel::new(
									concat!("Expected ", stringify!($Type), "."),
									span.clone(),
									DiagnosticLabelPriority::Primary,
								),
								DiagnosticLabel::new(
									format!("Hint: Try `{}.0`.", i),
									span,
									DiagnosticLabelPriority::Auxiliary,
								),
							],
						});
						Err(ErrorKind::Reported.into())
					}
					TamlValue::$Variant(v) => visitor
						.[<visit_ $Type>](
							v.parse()
								.map_err(|_| Error::invalid_value(concat!("Failed to parse ", stringify!($Type), ".")))
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
		match if let Some(o) = OVERRIDE.take() {
			o.pick(&self.data.value, &self.data.span, self.reporter)?
		} else {
			&self.data.value
		} {
			TamlValue::String(s) => visitor.visit_str(s),
			TamlValue::DataLiteral(data_literal) => {
				visitor.visit_data_literal(data_literal, self.encoders, self.reporter)
			}
			TamlValue::Integer(i) => if let Ok(i) = i.parse() {
				visitor.visit_u8(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_i8(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_u16(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_i16(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_u32(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_i32(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_u64(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_i64(i)
			} else if let Ok(i) = i.parse() {
				visitor.visit_u128(i)
			} else {
				visitor.visit_i128(
					i.parse()
						.map_err(|_| Error::invalid_value(concat!("Failed to parse integer.")))
						.report_for(self)?,
				)
			}
			.report_for(self),
			TamlValue::Decimal(f) => visitor
				.visit_f64(
					f.parse()
						.map_err(|_| Error::invalid_value(concat!("Failed to parse float.")))
						.report_for(self)?,
				)
				.report_for(self),
			TamlValue::List(l) => visitor.visit_seq(ListAccess::new(self.by_ref(), l)),
			TamlValue::Map(m) => visitor.visit_map(StructOrMapAccess::new(
				self.reporter,
				self.data.span.clone(),
				self.encoders,
				m,
				None,
			)),
			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "true" => visitor.visit_bool(true),
			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "false" => visitor.visit_bool(false),
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
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(
				ForcedTamlValueType::EnumVariant,
				&[ForcedTamlValueType::String, ForcedTamlValueType::Integer],
			)
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::String(str) => match str.as_ref() {
				"true" => visitor.visit_bool(true),
				"false" => visitor.visit_bool(false),
				_ => self.report_invalid_value(r#"Expected boolean string `"true"` or `"false"`."#),
			},

			TamlValue::Integer(str) => match *str {
				"0" => visitor.visit_bool(false),
				"1" => visitor.visit_bool(true),
				_ => {
					self.report_invalid_value("Expected boolean integer `0` (false) or `1` (true).")
				}
			},

			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "true" => visitor.visit_bool(true).report_for(self),
			TamlValue::EnumVariant {
				key: Key { name, .. },
				payload: VariantPayload::Unit,
			} if name == "false" => visitor.visit_bool(false).report_for(self),
			TamlValue::EnumVariant { .. } => {
				self.report_invalid_type("Expected boolean unit variant `true` or `false`.")
			}

			TamlValue::DataLiteral(_)
			| TamlValue::List(_)
			| TamlValue::Map(_)
			| TamlValue::Decimal(_) => unreachable!(),
		}
		.report_for(self)
	}

	parsed!(Integer => i8, i16, i32, i64, i128);
	parsed!(Integer => u8, u16, u32, u64, u128);
	parsed_float!(Decimal => f32, f64);

	fn deserialize_char<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(
				ForcedTamlValueType::String,
				&[
					ForcedTamlValueType::EnumVariant,
					ForcedTamlValueType::Integer,
				],
			)
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::String(str)
			| TamlValue::EnumVariant {
				key: Key { name: str, .. },
				payload: VariantPayload::Unit,
			} if str.as_ref().chars().count() == 1 => visitor.visit_char(str.chars().next().unwrap()),
			TamlValue::Integer(str) if str.chars().count() == 1 => {
				visitor.visit_char(str.chars().next().unwrap())
			}

			TamlValue::String(_) => self.report_invalid_value("Expected single codepoint string."),
			TamlValue::EnumVariant { .. } => {
				self.report_invalid_value("Expected single codepoint identifier.")
			}
			TamlValue::Integer(_) => {
				self.report_invalid_value("Expected positive single digit integer.")
			}

			TamlValue::DataLiteral(_)
			| TamlValue::List(_)
			| TamlValue::Map(_)
			| TamlValue::Decimal(_) => unreachable!(),
		}
		.report_for(self)
	}

	fn deserialize_str<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(
				ForcedTamlValueType::String,
				&[
					ForcedTamlValueType::EnumVariant,
					ForcedTamlValueType::Integer,
					ForcedTamlValueType::Decimal,
				],
			)
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::String(str)
			| TamlValue::EnumVariant {
				key: Key { name: str, .. },
				payload: VariantPayload::Unit,
			} => match str {
				cervine::Cow::Owned(str) => visitor.visit_str(str.as_str()),
				cervine::Cow::Borrowed(str) => visitor.visit_borrowed_str(str),
			},

			TamlValue::EnumVariant { .. } => {
				self.report_invalid_value("Expected plain identifier.")
			}

			TamlValue::Integer(str) | TamlValue::Decimal(str) => visitor.visit_borrowed_str(str),

			TamlValue::DataLiteral(_) | TamlValue::List(_) | TamlValue::Map(_) => unreachable!(),
		}
		.report_for(self)
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
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(
				ForcedTamlValueType::DataLiteral,
				&[ForcedTamlValueType::String],
			)
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::DataLiteral(data_literal) => {
				visitor.visit_data_literal(data_literal, self.encoders, self.reporter)
			}

			TamlValue::String(str)
			| TamlValue::EnumVariant {
				key: Key { name: str, .. },
				payload: VariantPayload::Unit,
			} => match str {
				cervine::Cow::Owned(str) => visitor.visit_bytes(str.as_bytes()),
				cervine::Cow::Borrowed(str) => visitor.visit_borrowed_bytes(str.as_bytes()),
			},

			TamlValue::EnumVariant { .. }
			| TamlValue::Integer(_)
			| TamlValue::Decimal(_)
			| TamlValue::List(_)
			| TamlValue::Map(_) => unreachable!(),
		}
		.report_for(self)
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
		// Options are flattened; that this `Deserializer` instance exists at all already means there is a value here.
		// Similarly, overrides aren't reset here and will be used for the next layer of deserialisation instead.
		visitor.visit_some(&mut self.by_ref()).report_for(self)
	}

	fn deserialize_unit<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(ForcedTamlValueType::List, &[ForcedTamlValueType::Struct])
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::List(l) if l.is_empty() => visitor.visit_unit(),
			TamlValue::List(_) => self.report_invalid_type("Expected unit (`()`)."),

			TamlValue::Map(m) if m.is_empty() => visitor.visit_unit(),
			TamlValue::Map(_) => self.report_invalid_type("Expected unit struct."),

			TamlValue::DataLiteral(_)
			| TamlValue::String(_)
			| TamlValue::EnumVariant { .. }
			| TamlValue::Integer(_)
			| TamlValue::Decimal(_) => unreachable!(),
		}
		.report_for(self)
	}

	fn deserialize_unit_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		OVERRIDE.insert_if_none(ForcedTamlValueType::Struct);
		self.deserialize_unit(visitor)
	}

	fn deserialize_newtype_struct<V>(self, _name: &'static str, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		// Newtypes are flattened.
		// Similarly, overrides aren't reset here and will be used for the next layer of deserialisation instead.
		visitor
			.visit_newtype_struct(&mut self.by_ref())
			.report_for(self)
	}

	fn deserialize_seq<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(ForcedTamlValueType::List, &[])
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::List(l) => visitor.visit_seq(ListAccess::new(self.by_ref(), l)),
			TamlValue::DataLiteral(_)
			| TamlValue::String(_)
			| TamlValue::EnumVariant { .. }
			| TamlValue::Integer(_)
			| TamlValue::Decimal(_)
			| TamlValue::Map(_) => unreachable!(),
		}
		.report_for(self)
	}

	fn deserialize_tuple<V>(self, len: usize, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(ForcedTamlValueType::List, &[])
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::List(l) if l.len() == len => {
				visitor.visit_seq(list_access::ListAccess::new(self.by_ref(), l))
			}
			TamlValue::List(l) => self.report_invalid_type_owned(format_args!(
				"Expected list with {} element(s), but found one with {} element(s).",
				len,
				l.len(),
			)),

			TamlValue::DataLiteral(_)
			| TamlValue::String(_)
			| TamlValue::EnumVariant { .. }
			| TamlValue::Integer(_)
			| TamlValue::Decimal(_)
			| TamlValue::Map(_) => unreachable!(),
		}
		.report_for(self)
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
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(ForcedTamlValueType::Struct, &[])
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::Map(m) => visitor.visit_map(StructOrMapAccess::new(
				self.reporter,
				self.data.span.clone(),
				self.encoders,
				m,
				None,
			)),

			TamlValue::DataLiteral(_)
			| TamlValue::String(_)
			| TamlValue::EnumVariant { .. }
			| TamlValue::Integer(_)
			| TamlValue::Decimal(_)
			| TamlValue::List(_) => unreachable!(),
		}
		.report_for(self)
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
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(ForcedTamlValueType::Struct, &[])
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::Map(m) => visitor.visit_map(StructOrMapAccess::new(
				self.reporter,
				self.data.span.clone(),
				self.encoders,
				m,
				fields.into(),
			)),

			TamlValue::DataLiteral(_)
			| TamlValue::String(_)
			| TamlValue::EnumVariant { .. }
			| TamlValue::Integer(_)
			| TamlValue::Decimal(_)
			| TamlValue::List(_) => unreachable!(),
		}
		.report_for(self)
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
		match OVERRIDE
			.take()
			.assert_acceptable_and_unwrap(ForcedTamlValueType::EnumVariant, &[])
			.pick(&self.data.value, &self.data.span, self.reporter)?
		{
			TamlValue::EnumVariant { .. } => visitor.visit_enum(EnumAndVariantAccess(self)),
			TamlValue::DataLiteral(_)
			| TamlValue::String(_)
			| TamlValue::Map(_)
			| TamlValue::Integer(_)
			| TamlValue::Decimal(_)
			| TamlValue::List(_) => unreachable!(),
		}
		.report_for(self)
	}

	fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		OVERRIDE.insert_if_none(ForcedTamlValueType::EnumVariant);
		self.deserialize_str(visitor)
	}

	fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		if let Some(o) = OVERRIDE.take() {
			o.pick(&self.data.value, &self.data.span, self.reporter)?;
		}

		visitor.visit_unit().report_for(self)
	}

	fn is_human_readable(&self) -> bool {
		true
	}
}

trait VisitDataLiteral<'de> {
	type Value;
	fn visit_data_literal<Position: PositionImpl>(
		self,
		data_literal: &DataLiteral<Position>,
		encoders: &[(&str, &Encoder)],
		reporter: &mut impl diagReporter<Position>,
	) -> Result<Self::Value>;
}
impl<'de, V> VisitDataLiteral<'de> for V
where
	V: de::Visitor<'de>,
{
	type Value = V::Value;

	fn visit_data_literal<Position: PositionImpl>(
		self,
		data_literal: &DataLiteral<Position>,
		encoders: &[(&str, &Encoder)],
		reporter: &mut impl diagReporter<Position>,
	) -> Result<Self::Value> {
		#![allow(clippy::map_unwrap_or)] // Needed to borrow `reporter`.
		encoders
			.iter()
			.find_map(|(encoding, decoder)| {
				(*encoding == data_literal.encoding.as_ref()).then(|| *decoder)
			})
			.map(
				|decoder| match decoder(data_literal.unencoded_data.as_ref()) {
					Ok(Cow::Borrowed(slice)) => self.visit_bytes(slice),
					Ok(Cow::Owned(vec)) => self.visit_byte_buf(vec),
					Err(errors) => {
						reporter.report_with(|| Diagnostic {
							type_: DiagnosticType::EncodeFailed,
							labels: errors
								.into_iter()
								.map(
									|EncodeError {
									     unescaped_input_span,
									     message,
									 }| {
										DiagnosticLabel::new(
											message,
											{
												let escape_shift = data_literal.unencoded_data
													[..unescaped_input_span.start]
													.chars()
													.filter(|c| *c == '>')
													.count();
												let start =
													unescaped_input_span.start + escape_shift;
												let end = unescaped_input_span.end
													+ escape_shift + data_literal
													.unencoded_data[unescaped_input_span]
													.chars()
													.filter(|c| *c == '>')
													.count();
												data_literal
													.unencoded_data_span
													.start
													.offset_range(start..end)
											},
											DiagnosticLabelPriority::Primary,
										)
									},
								)
								.chain(iter::once(DiagnosticLabel::new(
									"Encoding specified here.",
									data_literal.encoding_span.clone(),
									DiagnosticLabelPriority::Auxiliary,
								)))
								.chain(iter::once(DiagnosticLabel::new(
									format!(
										"Hint: Available encodings are: {}.",
										if encoders.is_empty() {
											"(None)".to_string()
										} else {
											format!(
												"`{}`",
												encoders
													.iter()
													.map(|(encoding, _)| encoding
														.replace('`', "\\`"))
													.join_with("`, `")
											)
										}
									),
									None,
									DiagnosticLabelPriority::Auxiliary,
								)))
								.collect(),
						});
						Err(ErrorKind::Reported.into())
					}
				},
			)
			.unwrap_or_else(|| {
				reporter.report_with(|| Diagnostic {
					type_: DiagnosticType::UnknownEncoding,
					labels: vec![
						DiagnosticLabel::new(
							format!(
								"Unrecognized encoding `{}`.",
								data_literal.encoding.replace('`', "\\`")
							),
							data_literal.encoding_span.clone(),
							DiagnosticLabelPriority::Primary,
						),
						DiagnosticLabel::new(
							format!(
								"Hint: Available encodings are: {}.",
								if encoders.is_empty() {
									"(None)".to_string()
								} else {
									format!(
										"`{}`",
										encoders
											.iter()
											.map(|(encoding, _)| encoding.replace('`', "\\`"))
											.join_with("`, `")
									)
								}
							),
							None,
							DiagnosticLabelPriority::Auxiliary,
						),
					],
				});
				Err(ErrorKind::Reported.into())
			})
	}
}

/// Implemented by types usable as `Position` generic type parameter in this library.
pub trait PositionImpl: Debug + Clone + Default + PartialEq {
	/// Adds `self` to both limits of `local_range` and returns the result in [`Some`].  
	/// If this operation does not make sense, [`None`] is returned instead.
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
