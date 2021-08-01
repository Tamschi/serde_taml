use super::{Deserializer, Error, ReportFor, Result};
use serde::de;
use taml::{Position, diagnostics::Reporter as diagReporter, parsing::Taml};
use tap::Pipe;

#[allow(clippy::type_complexity)]
pub struct ListAccess<'a, 'de, P: Position, Reporter: diagReporter<P>> {
	deserializer: Deserializer<'a, 'de, P, Reporter>,
	entries: Box<dyn 'a + Iterator<Item = &'a Taml<'de, P>>>,
}
impl<'a, 'de, P: Position, Reporter: diagReporter<P>> ListAccess<'a, 'de, P, Reporter> {
	pub fn new(deserializer: Deserializer<'a, 'de, P, Reporter>, list: &'a [Taml<'de, P>]) -> Self {
		Self {
			deserializer,
			entries: list.iter().pipe(Box::new),
		}
	}
}

impl<'a, 'de, P: Position, Reporter: diagReporter<P>> de::SeqAccess<'de>
	for ListAccess<'a, 'de, P, Reporter>
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
