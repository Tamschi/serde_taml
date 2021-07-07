use super::{Deserializer, Error, ReportAt};
use serde::de;
use std::ops::Range;
use taml::{
	diagnostics::Reporter as diagReporter,
	parsing::{List, Taml},
};
use tap::Pipe;

#[allow(clippy::type_complexity)]
pub struct ListAccess<'a, 'de, Position: Clone, Reporter: diagReporter<Position>> {
	reporter: &'a mut Reporter,
	span: Range<Position>,
	entries: Box<dyn 'a + Iterator<Item = &'a Taml<'de, Position>>>,
}
impl<'a, 'de, Position: Clone, Reporter: diagReporter<Position>>
	ListAccess<'a, 'de, Position, Reporter>
{
	#[allow(clippy::ptr_arg)]
	pub fn new(
		reporter: &'a mut Reporter,
		span: Range<Position>,
		list: &'a List<'de, Position>,
	) -> Self {
		Self {
			reporter,
			span,
			entries: list.iter().pipe(Box::new),
		}
	}
}

impl<'a, 'de, Position: Clone, Reporter: diagReporter<Position>> de::SeqAccess<'de>
	for ListAccess<'a, 'de, Position, Reporter>
{
	type Error = Error;

	fn next_element_seed<T>(&mut self, seed: T) -> Result<Option<T::Value>, Self::Error>
	where
		T: de::DeserializeSeed<'de>,
	{
		self.entries
			.next()
			.map(|e| seed.deserialize(&mut Deserializer(e, self.reporter)))
			.transpose()
			.report_at(self.reporter, self.span.clone())
	}

	fn size_hint(&self) -> Option<usize> {
		let size = self.entries.size_hint();
		size.1.filter(|l| *l == size.0)
	}
}
