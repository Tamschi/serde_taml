//! TAML type overrides affect only the expected TAML representation of a value,
//! but **not** how the [`super::Deserializer`] interacts with Serde
//! (beyond delayed panics on encountering incompatibilities).
//!
//! > **Warning:**
//! >
//! > I had to use a (thread-local) side channel here.
//! > It should be well-behaved with any [`de::Deserialize`] implementation that Serde generated,
//! > but there may be issues if you jump between two deserializers.
//! >
//! > A proper fix is planned, but can only be implemented on stable once trait specialisation lands.
//!
//! # Serde/TAML compatibility table:
//! 
//! Defaults are highlighted in <span style="background: green; color: white">green</span><span style="display: inline-block; width: 0; height: 0; overflow: hidden"> and prefixed with "(default)"</span>.
//!
//! <table>
//! <thead>
//! <tr>
//!     <th rowspan=0>deserialize_…</th>
//!     <th colspan=7>TAML</th>
//! </tr>
//! <tr>
//!     <th>data literal</th>
//!     <th>decimal</th>
//!     <th>enum variant</th>
//!     <th>integer</th>
//!     <th>list</th>
//!     <th>string</th>
//!     <th>struct</th>
//! </tr>
//! </thead>
//! <tbody>
//! <tr>
//!     <th>any</th>
//!     <td colspan=7>(restricting to one default cell below, in a row marked with "(any)")</td>
//! </tr>
//! <tr>
//!     <th>bool<br>(any, with priority)</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(default) </span><code>true</code>, <code>false</code></td>
//!     <td><code>1</code>, <code>0</code></td>
//!     <td style="border: none"></td>
//!     <td><code>"true"</code>, <code>"false"</code></td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>u8, i8, u16, i16, u32, i32, i64, u64, i128, u128<br>(any, in that order)</th>
//!     <td colspan=7>TODO</td>
//! </tr>
//! <tr>
//!     <th>f32, f64<br>(any, always as f64)</th>
//!     <td colspan=7>TODO</td>
//! </tr>
//! <tr>
//!     <th>char</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td>single-codepoint identifier of unit variant</td>
//!     <td>single-digit positive integer</td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>single-codepoint</td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>str, string<br>(any)</th>
//!     <td style="border: none"></td>
//!     <td>full literal</td>
//!     <td>identifier of unit variant</td>
//!     <td>full literal</td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>unquoted and unescaped</td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>bytes, bytes_buf<br>(any)</th>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>encoded</td>
//!     <td style="border: none"></td>
//!     <td>identifier of unit variant in UTF-8</td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td>unquoted and unescaped in UTF-8</td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>option</th>
//!     <td colspan=7>(transparent)</td>
//! </tr>
//! <tr>
//!     <th>unit</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(default) </span><code>()</code></td>
//!     <td style="border: none"></td>
//!     <td>no fields only</td>
//! </tr>
//! <tr>
//!     <th>unit_struct</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td><code>()</code></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>no fields only</td>
//! </tr>
//! <tr>
//!     <th>newtype_struct</th>
//!     <td colspan=7>(transparent)</td>
//! </tr>
//! <tr>
//!     <th>seq<br>(any)</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>any list</td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>tuple, tuple_struct</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>exact length only</td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>map<br>(any)</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>any struct</td>
//! </tr>
//! <tr>
//!     <th>struct</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>exact fields only</td>
//! </tr>
//! <tr>
//!     <th>enum<br>(any)</th>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>any enum variant¹</td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>identifier</th>
//!     <td style="border: none"></td>
//!     <td>full literal</td>
//!     <td style="background: green; color: white"><span style="display: inline-block; width: 0; height: 0; overflow: hidden">(d efault)</span>identifier of unit variant</td>
//!     <td>full literal</td>
//!     <td style="border: none"></td>
//!     <td>unquoted and unescaped</td>
//!     <td style="border: none"></td>
//! </tr>
//! <tr>
//!     <th>ignored_any</th>
//!     <td colspan=7>(restricting, but only by TAML value type)</td>
//! </tr>
//! </tbody>
//! </table>
//!
//! ¹ Use the `"serde-object-assist"` feature to predict enum variants, if necessary.


use crate::de::ErrorKind;
use serde::de;
use std::{cell::Cell, fmt::Display, ops::Range, thread::LocalKey};
use taml::{
	diagnostics::{
		Diagnostic, DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType,
		Reporter as diagReporter,
	},
	parsing::TamlValue,
};
use tap::Pipe;

use super::{Error, PositionImpl};

thread_local! {
	/// Sets a TAML data type override for the next value aside from options, which are transparent.
	///
	/// > This isn't a great solution, but should behave fine with well-behaved [`de::Deserialize`] implementations.
	/// >
	/// > A fixed version would use trait specialisation to wrap [`super::Deserializer`] instances.
	/// > (It's possible this might function even know due to `deserialize_with = "…"` duck-typing,
	/// > but implementing it as such would be outside Serde's documented API and as such too brittle.)
	pub(super) static OVERRIDE: Cell<Option<ForcedTamlValueType>> = Cell::default();
}

pub(super) trait Override {
	fn set(&'static self, force: ForcedTamlValueType);
	fn take(&'static self) -> Option<ForcedTamlValueType>;
	fn insert_if_none(&'static self, new_default: ForcedTamlValueType);
}
impl Override for LocalKey<Cell<Option<ForcedTamlValueType>>> {
	fn set(&'static self, force: ForcedTamlValueType) {
		self.with(|override_| override_.set(Some(force)));
	}

	fn take(&'static self) -> Option<ForcedTamlValueType> {
		self.with(Cell::take)
	}

	fn insert_if_none(&'static self, new_default: ForcedTamlValueType) {
		self.with(|this| {
			if this.get().is_none() {
				this.set(new_default.into())
			}
		})
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum ForcedTamlValueType {
	DataLiteral,
	Decimal,
	EnumVariant,
	Integer,
	List,
	String,
	Struct,
}
impl ForcedTamlValueType {
	pub fn pick<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>>(
		self,
		value: &'a TamlValue<'de, Position>,
		span: &Range<Position>,
		reporter: &mut Reporter,
	) -> Result<&'a TamlValue<'de, Position>, Error> {
		#[allow(
			clippy::match_same_arms,
			clippy::non_ascii_literal,
			clippy::single_match_else
		)]
		match self {
			ForcedTamlValueType::String => match value {
				v @ TamlValue::String(_) => Ok(v),
				_ => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							r#"Expected string (`"…"`)."#,
							span.clone(),
							DiagnosticLabelPriority::Primary,
						)],
					});
					Err(ErrorKind::Reported.into())
				}
			},

			ForcedTamlValueType::DataLiteral => match value {
				v @ TamlValue::Decoded(_) => Ok(v),
				_ => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							"Expected data literal (`<…;…>`).",
							span.clone(),
							DiagnosticLabelPriority::Primary,
						)],
					});
					Err(ErrorKind::Reported.into())
				}
			},

			ForcedTamlValueType::Integer => match value {
				v @ TamlValue::Integer(_) => Ok(v),
				_ => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							"Expected integer.",
							span.clone(),
							DiagnosticLabelPriority::Primary,
						)],
					});
					Err(ErrorKind::Reported.into())
				}
			},

			ForcedTamlValueType::Decimal => match value {
				TamlValue::Integer(i) => {
					let span = span.clone().pipe(Some);
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![
							DiagnosticLabel::new(
								"Expected decimal.",
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
				v @ TamlValue::Float(_) => Ok(v),
				_ => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							"Expected decimal.",
							span.clone(),
							DiagnosticLabelPriority::Primary,
						)],
					});
					Err(ErrorKind::Reported.into())
				}
			},

			ForcedTamlValueType::EnumVariant => match value {
				v @ TamlValue::EnumVariant { .. } => Ok(v),
				_ => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							r#"Expected enum variant (`key` or `key(…)`)."#,
							span.clone(),
							DiagnosticLabelPriority::Primary,
						)],
					});
					Err(ErrorKind::Reported.into())
				}
			},

			ForcedTamlValueType::List => match value {
				v @ TamlValue::List(_) => Ok(v),
				_ => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							"Expected list (`(…)`).",
							span.clone(),
							DiagnosticLabelPriority::Primary,
						)],
					});
					Err(ErrorKind::Reported.into())
				}
			},

			ForcedTamlValueType::Struct => match value {
				v @ TamlValue::Map(_) => Ok(v),
				_ => {
					reporter.report_with(|| Diagnostic {
						r#type: DiagnosticType::InvalidType,
						labels: vec![DiagnosticLabel::new(
							"Expected struct.",
							span.clone(),
							DiagnosticLabelPriority::Primary,
						)],
					});
					Err(ErrorKind::Reported.into())
				}
			},
		}
	}
}
impl Display for ForcedTamlValueType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match self {
			ForcedTamlValueType::DataLiteral => "data literal",
			ForcedTamlValueType::Decimal => "decimal",
			ForcedTamlValueType::EnumVariant => "enum variant",
			ForcedTamlValueType::Integer => "integer",
			ForcedTamlValueType::List => "list",
			ForcedTamlValueType::String => "string",
			ForcedTamlValueType::Struct => "struct",
		})
	}
}

pub(crate) trait AssertAcceptableAndUnwrapOrDefault<T> {
	fn assert_acceptable_and_unwrap(self, default: T, other_acceptable: &[T]) -> T;
}
impl AssertAcceptableAndUnwrapOrDefault<ForcedTamlValueType> for Option<ForcedTamlValueType> {
	fn assert_acceptable_and_unwrap(
		self,
		default: ForcedTamlValueType,
		other_acceptable: &[ForcedTamlValueType],
	) -> ForcedTamlValueType {
		match self {
			None => default,
			Some(forced) if forced == default || other_acceptable.contains(&forced) => forced,
			Some(forced) => panic!(
				"Unsupported TAML type override: Can't expect {} when parsing {}.",
				forced, default
			),
		}
	}
}

/// Overrides TAML value type restrictions to expect a data literal.
///
/// # Errors
///
/// Iff `T::deserialize(deserializer)` errors.
///
/// # Delayed Panics
///
/// Only the following can be overridden to deserialize from data literals:
///
/// - TODO
///
/// In all other cases, a panic will occur during deserialization.
pub fn from_data_literal<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
	D: de::Deserializer<'de>,
	T: de::Deserialize<'de>,
{
	OVERRIDE.set(ForcedTamlValueType::DataLiteral);
	T::deserialize(deserializer)
}

/// Overrides TAML value type restrictions to expect a decimal.
///
/// # Errors
///
/// Iff `T::deserialize(deserializer)` errors.
///
/// # Delayed Panics
///
/// Only the following can be overridden to deserialize from decimals:
///
/// - TODO
///
/// In all other cases, a panic will occur during deserialization.
pub fn from_decimal<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
	D: de::Deserializer<'de>,
	T: de::Deserialize<'de>,
{
	OVERRIDE.set(ForcedTamlValueType::Decimal);
	T::deserialize(deserializer)
}

/// Overrides TAML value type restrictions to expect an enum variant.
///
/// # Errors
///
/// Iff `T::deserialize(deserializer)` errors.
///
/// # Delayed Panics
///
/// Only the following can be overridden to deserialize from enum variants:
///
/// - TODO
///
/// In all other cases, a panic will occur during deserialization.
pub fn from_enum_variant<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
	D: de::Deserializer<'de>,
	T: de::Deserialize<'de>,
{
	OVERRIDE.set(ForcedTamlValueType::EnumVariant);
	T::deserialize(deserializer)
}

/// Overrides TAML value type restrictions to expect an integer.
///
/// # Errors
///
/// Iff `T::deserialize(deserializer)` errors.
///
/// # Delayed Panics
///
/// Only the following can be overridden to deserialize from integers:
///
/// - TODO
///
/// In all other cases, a panic will occur during deserialization.
pub fn from_integer<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
	D: de::Deserializer<'de>,
	T: de::Deserialize<'de>,
{
	OVERRIDE.set(ForcedTamlValueType::Integer);
	T::deserialize(deserializer)
}

/// Overrides TAML value type restrictions to expect a list.
///
/// # Errors
///
/// Iff `T::deserialize(deserializer)` errors.
///
/// # Delayed Panics
///
/// Only the following can be overridden to deserialize from lists:
///
/// - TODO
///
/// In all other cases, a panic will occur during deserialization.
pub fn from_list<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
	D: de::Deserializer<'de>,
	T: de::Deserialize<'de>,
{
	OVERRIDE.set(ForcedTamlValueType::List);
	T::deserialize(deserializer)
}

/// Overrides TAML value type restrictions to expect a string.
///
/// # Errors
///
/// Iff `T::deserialize(deserializer)` errors.
///
/// # Delayed Panics
///
/// Only the following can be overridden to deserialize from strings:
///
/// - TODO
///
/// In all other cases, a panic will occur during deserialization.
pub fn from_string<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
	D: de::Deserializer<'de>,
	T: de::Deserialize<'de>,
{
	OVERRIDE.set(ForcedTamlValueType::String);
	T::deserialize(deserializer)
}

/// Overrides TAML value type restrictions to expect a struct.
///
/// # Errors
///
/// Iff `T::deserialize(deserializer)` errors.
///
/// # Delayed Panics
///
/// Only the following can be overridden to deserialize from structs:
///
/// - TODO
///
/// In all other cases, a panic will occur during deserialization.
pub fn from_struct<'de, D, T>(deserializer: D) -> Result<T, D::Error>
where
	D: de::Deserializer<'de>,
	T: de::Deserialize<'de>,
{
	OVERRIDE.set(ForcedTamlValueType::Struct);
	T::deserialize(deserializer)
}
