use std::{borrow::Cow, ops::Range};

use crate::de::{list_access::ListAccess, struct_access::StructAccess, ErrorKind};

use super::{key_deserializer::KeyDeserializer, Deserializer, Error, ReportFor};
use debugless_unwrap::DebuglessUnwrap;
use serde::de;
use taml::{
	diagnostics::{
		Diagnostic, DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType,
		Reporter as diagReporter,
	},
	parsing::{Taml, TamlValue, VariantPayload},
};
use tap::Pipe;
use try_match::try_match;

pub struct EnumAndVariantAccess<'a, 'b, 'de, Position: Clone, Reporter: diagReporter<Position>>(
	pub &'a mut Deserializer<'b, 'de, Position, Reporter>,
);
impl<'a, 'b, 'de, Position: Clone, Reporter: diagReporter<Position>> de::EnumAccess<'de>
	for EnumAndVariantAccess<'a, 'b, 'de, Position, Reporter>
{
	type Error = Error;

	type Variant = Self;

	fn variant_seed<V>(self, seed: V) -> Result<(V::Value, Self::Variant), Self::Error>
	where
		V: de::DeserializeSeed<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0 .0.value)
			.debugless_unwrap();
		seed.deserialize(KeyDeserializer {
			key: variant.key.clone(),
			reporter: self.0 .1,
		})
		.report_for(self.0)
		.map(|v| (v, self))
	}
}
impl<'a, 'b, 'de, Position: Clone, Reporter: diagReporter<Position>> de::VariantAccess<'de>
	for EnumAndVariantAccess<'a, 'b, 'de, Position, Reporter>
{
	type Error = Error;

	fn unit_variant(self) -> Result<(), Self::Error> {
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0 .0.value)
			.debugless_unwrap();
		match variant.payload {
			VariantPayload::Unit => Ok(()),
			_ => self
				.0
				.report_unexpected_variant("Expected unit variant.", variant.key.span.clone()),
		}
	}

	fn newtype_variant_seed<T>(self, seed: T) -> Result<T::Value, Self::Error>
	where
		T: de::DeserializeSeed<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0 .0.value)
			.debugless_unwrap();
		seed.deserialize(&mut Deserializer(
			match variant.payload {
				VariantPayload::Tuple(list) if list.len() == 1 => &list[0],
				_ => {
					return self.0.report_unexpected_variant(
						"Expected newtype variant.",
						variant.key.span.clone(),
					)
				}
			},
			self.0 .1,
		))
		.report_for(self.0)
	}

	fn tuple_variant<V>(self, _len: usize, visitor: V) -> Result<V::Value, Self::Error>
	where
		V: de::Visitor<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0 .0.value)
			.debugless_unwrap();
		match variant.payload {
			VariantPayload::Tuple(list) => visitor
				.visit_seq(ListAccess::new(self.0 .1, self.0 .0.span.clone(), list))
				.report_for(self.0),
			_ => self
				.0
				.report_unexpected_variant("Expected tuple variant.", variant.key.span.clone()),
		}
	}

	fn struct_variant<V>(
		self,
		fields: &'static [&'static str],
		visitor: V,
	) -> Result<V::Value, Self::Error>
	where
		V: de::Visitor<'de>,
	{
		let variant = try_match!(TamlValue::EnumVariant { key, payload } = &self.0 .0.value)
			.debugless_unwrap();
		match variant.payload {
			VariantPayload::Structured(map) => visitor
				.visit_map(StructAccess::new(
					self.0 .1,
					self.0 .0.span.clone(),
					map,
					fields,
				))
				.report_for(self.0),
			_ => self
				.0
				.report_unexpected_variant("Expected struct variant.", variant.key.span.clone()),
		}
	}
}

trait ReportUnexpectedVariant<Position: Clone> {
	fn report_unexpected_variant<V>(
		self,
		msg: &'static str,
		key_span: Range<Position>,
	) -> Result<V, Error>;
}
impl<'a, 'de, Position: Clone, Reporter: diagReporter<Position>> ReportUnexpectedVariant<Position>
	for &mut Deserializer<'a, 'de, Position, Reporter>
{
	fn report_unexpected_variant<V>(
		self,
		msg: &'static str,
		key_span: Range<Position>,
	) -> Result<V, Error> {
		let enum_span = self.0.span.clone();
		self.1.report_with(|| Diagnostic {
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
