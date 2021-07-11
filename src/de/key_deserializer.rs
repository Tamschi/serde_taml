use super::{Error, ReportAt, Result};
use serde::{de, forward_to_deserialize_any};
use taml::{diagnostics::Reporter as diagReporter, parsing::Key};

pub struct KeyDeserializer<'a, 'de, Position: Clone, Reporter: diagReporter<Position>> {
	pub key: Key<'de, Position>,
	pub reporter: &'a mut Reporter,
}
impl<'a, 'de, Position: Clone, Reporter: diagReporter<Position>> de::Deserializer<'de>
	for KeyDeserializer<'a, 'de, Position, Reporter>
{
	type Error = Error;

	fn deserialize_identifier<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		visitor
			.visit_str(self.key.name.as_ref())
			.report_at(self.reporter, self.key.span)
	}

	fn deserialize_ignored_any<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		visitor.visit_unit().report_at(self.reporter, self.key.span)
	}

	fn deserialize_any<V>(self, visitor: V) -> Result<V::Value>
	where
		V: de::Visitor<'de>,
	{
		self.deserialize_identifier(visitor)
	}

	forward_to_deserialize_any! {
		bool i8 i16 i32 i64 i128 u8 u16 u32 u64 u128 f32 f64 char str string
		bytes byte_buf option unit unit_struct newtype_struct seq tuple
		tuple_struct map struct enum
	}
}
