use super::{
	key_deserializer::KeyDeserializer, Deserializer, Error, PositionImpl, ReportFor, Result,
};
use crate::de::{list_access::ListAccess, struct_or_map_access::StructOrMapAccess, ErrorKind};
use debugless_unwrap::DebuglessUnwrap;
use serde::de;
use std::{borrow::Cow, ops::Range};
use taml::{
	diagnostics::{
		Diagnostic, DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType,
		Reporter as diagReporter,
	},
	parsing::{TamlValue, VariantPayload},
};
use tap::Pipe;
use try_match::try_match;

pub struct EnumAndVariantAccess<
	'a,
	'b,
	'de,
	Position: PositionImpl,
	Reporter: diagReporter<Position>,
>(pub &'a mut Deserializer<'b, 'de, Position, Reporter>);
impl<'a, 'b, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> de::EnumAccess<'de>
	for EnumAndVariantAccess<'a, 'b, 'de, Position, Reporter>
{
	type Error = Error;

	type Variant = Self;

	fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant)>
	where
		V: de::DeserializeSeed<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0.data.value)
			.debugless_unwrap();
		seed.deserialize(KeyDeserializer {
			key: variant.key.clone(),
			reporter: self.0.reporter,
		})
		.report_for(self.0)
		.map(|v| (v, self))
	}
}
impl<'a, 'b, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> de::VariantAccess<'de>
	for EnumAndVariantAccess<'a, 'b, 'de, Position, Reporter>
{
	type Error = Error;

	fn unit_variant(self) -> Result<()> {
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0.data.value)
			.debugless_unwrap();
		match variant.payload {
			VariantPayload::Unit => Ok(()),
			_ => self
				.0
				.report_unexpected_variant("Expected unit variant.", variant.key.span.clone()),
		}
	}

	fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value>
	where
		T: de::DeserializeSeed<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0.data.value)
			.debugless_unwrap();
		seed.deserialize(&mut Deserializer {
			data: match variant.payload {
				VariantPayload::Tuple(list) if list.len() == 1 => &list[0],
				_ => {
					return self.0.report_unexpected_variant(
						"Expected newtype variant.",
						variant.key.span.clone(),
					)
				}
			},
			..self.0.by_ref()
		})
		.report_for(self.0)
	}

	fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0.data.value)
			.debugless_unwrap();
		match variant.payload {
			VariantPayload::Tuple(list) => visitor
				.visit_seq(ListAccess::new(self.0.by_ref(), list))
				.report_for(self.0),
			_ => self
				.0
				.report_unexpected_variant("Expected tuple variant.", variant.key.span.clone()),
		}
	}

	fn struct_variant<V>(self, fields: &'static [&'static str], visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0.data.value)
			.debugless_unwrap();
		match variant.payload {
			VariantPayload::Structured(map) => visitor
				.visit_map(StructOrMapAccess::new(
					self.0.reporter,
					self.0.data.span.clone(),
					self.0.encoders,
					map,
					fields.into(),
				))
				.report_for(self.0),
			_ => self
				.0
				.report_unexpected_variant("Expected struct variant.", variant.key.span.clone()),
		}
	}
}

trait ReportUnexpectedVariant<Position: PositionImpl> {
	fn report_unexpected_variant<V>(
		self,
		msg: &'static str,
		key_span: Range<Position>,
	) -> Result<V>;
}
impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>>
	ReportUnexpectedVariant<Position> for &mut Deserializer<'a, 'de, Position, Reporter>
{
	fn report_unexpected_variant<V>(
		self,
		msg: &'static str,
		key_span: Range<Position>,
	) -> Result<V> {
		let enum_span = self.data.span.clone();
		self.reporter.report_with(|| Diagnostic {
			r#type: DiagnosticType::CustomErrorFromVisitor,
			labels: vec![
				DiagnosticLabel {
					caption: msg.pipe(Cow::Borrowed).into(),
					span: enum_span.into(),
					priority: DiagnosticLabelPriority::Primary,
				},
				DiagnosticLabel {
					caption: "Variant selected here.".pipe(Cow::Borrowed).into(),
					span: key_span.into(),
					priority: DiagnosticLabelPriority::Auxiliary,
				},
			],
		});
		Err(ErrorKind::Reported.into())
	}
}
