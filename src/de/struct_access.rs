use std::ops::Range;

use super::{key_deserializer::KeyDeserializer, Deserializer, Error, ReportAt};
use cervine::Cow;
use either::Either;
use indexmap::IndexMap;
use serde::de;
use taml::{
	diagnostics::Reporter as diagReporter,
	parsing::{Key, Map, Taml},
};
use tap::Pipe;

#[allow(clippy::type_complexity)]
pub struct StructAccess<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>> {
	reporter: &'a mut Reporter,
	span: Range<Position>,
	entries: Box<
		dyn 'a
			+ Iterator<
				Item = (
					Key<'de, Position>,
					Either<&'a Taml<'de, Position>, &'static str>,
				),
			>,
	>,
	next_value: Option<Either<&'a Taml<'de, Position>, &'static str>>,
}
impl<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>>
	StructAccess<'a, 'de, Position, Reporter>
{
	pub fn new(
		reporter: &'a mut Reporter,
		span: Range<Position>,
		map: &'a Map<'de, Position>,
		fields: &'static [&'static str],
	) -> Self {
		Self {
			reporter,
			span: span.clone(),
			entries: {
				let present = map.iter().map(|(k, v)| (k.clone(), Either::Left(v)));
				let absent = fields
					.iter()
					.filter(move |k| !map.contains_key(**k))
					.map(move |k| {
						(
							Key {
								name: Cow::Borrowed(*k),
								span: span.clone(),
							},
							Either::Right(*k),
						)
					});
				present.chain(absent).pipe(Box::new)
			},
			next_value: None,
		}
	}
}

impl<'a, 'de, Position: Clone + Ord, Reporter: diagReporter<Position>> de::MapAccess<'de>
	for StructAccess<'a, 'de, Position, Reporter>
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
					.deserialize(&mut Deserializer(value, self.reporter))
					.report_at(self.reporter, value.span.clone()),
				Either::Right(key) => todo!(),
			})
	}

	fn size_hint(&self) -> Option<usize> {
		let size = self.entries.size_hint();
		size.1.filter(|l| *l == size.0)
	}
}
