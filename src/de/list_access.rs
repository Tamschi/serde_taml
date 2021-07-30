use super::{Deserializer, Error, PositionImpl, ReportFor, Result};
use serde::de;
use taml::{diagnostics::Reporter as diagReporter, parsing::Taml};
use tap::Pipe;

#[allow(clippy::type_complexity)]
pub struct ListAccess<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> {
	deserializer: Deserializer<'a, 'de, Position, Reporter>,
	entries: Box<dyn 'a + Iterator<Item = &'a Taml<'de, Position>>>,
}
impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>>
	ListAccess<'a, 'de, Position, Reporter>
{
	pub fn new(
		deserializer: Deserializer<'a, 'de, Position, Reporter>,
		list: &'a [Taml<'de, Position>],
	) -> Self {
		Self {
			deserializer,
			entries: list.iter().pipe(Box::new),
		}
	}
}

impl<'a, 'de, Position: PositionImpl, Reporter: diagReporter<Position>> de::SeqAccess<'de>
	for ListAccess<'a, 'de, Position, Reporter>
{
	type Error = Error;

	fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>>
	where
		T: de::DeserializeSeed<'de>,
	{
		self.entries
			.next()
			.map(|e| {
				seed.deserialize(&mut Deserializer {
					data: e,
					..self.deserializer.by_ref()
				})
			})
			.transpose()
			.report_for(&mut self.deserializer)
	}

	fn size_hint(&self) -> Option<usize> {
		let size = self.entries.size_hint();
		size.1.filter(|l| *l == size.0)
	}
}
