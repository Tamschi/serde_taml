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

thread_local!(pub(super) static OVERRIDE: Cell<Option<ForcedTamlValueType>> = Cell::default());

pub(super) trait Override {
	fn set(&'static self, force: ForcedTamlValueType);
	fn take(&'static self) -> Option<ForcedTamlValueType>;
}
impl Override for LocalKey<Cell<Option<ForcedTamlValueType>>> {
	fn set(&'static self, force: ForcedTamlValueType) {
		self.with(|override_| override_.set(Some(force)));
	}

	fn take(&'static self) -> Option<ForcedTamlValueType> {
		self.with(Cell::take)
	}
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub(super) enum ForcedTamlValueType {
	String,
	DataLiteral,
	Integer,
	Decimal,
}
impl ForcedTamlValueType {
	pub fn pick<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>>(
		&self,
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
			ForcedTamlValueType::DataLiteral => todo!(),
			ForcedTamlValueType::Integer => todo!(),
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
		}
	}
}
impl Display for ForcedTamlValueType {
	fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
		f.write_str(match self {
			ForcedTamlValueType::String => "string",
			ForcedTamlValueType::DataLiteral => "data literal",
			ForcedTamlValueType::Integer => "integer",
			ForcedTamlValueType::Decimal => "decimal",
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
/// - Strings
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
