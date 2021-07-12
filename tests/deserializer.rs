use cast::u64;
use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use serde::Deserialize;
use serde_taml::de::from_taml_str;
use std::borrow::Cow;
use taml::diagnostics::{DiagnosticLabelPriority, DiagnosticLevel};
use tap::TapFallible;

//TODO: Split up this test.
#[test]
fn deserializer() {
	#[derive(Debug, Deserialize, PartialEq)]
	struct Deserializable {
		#[serde(default)]
		none: Option<bool>,
		#[serde(default)]
		some: Option<bool>,

		seq: Vec<u8>,

		zero_u8: u8,
		one_u8: u8,

		zero_i8: i8,
		one_i8: i8,
		minus_one_i8: i8,

		empty_table: Vec<()>,

		tabular: Vec<Tabular>,

		variants: Vec<Enum>,

		unit_variant: Enum,
		weird_variant: Enum,
		newtype_variant: Enum,
		tuple_variant: Enum,

		r#false: bool,
		r#true: bool,
	}

	#[derive(Debug, Deserialize, PartialEq)]
	enum Enum {
		Structured { i32: i32, f64: f64 },
		Tuple(u8, u8),
		Newtype(u8),
		Unit,
		Weird(),
	}

	#[derive(Debug, Deserialize, PartialEq)]
	struct Tabular {
		first: u8,
		second: u8,
	}

	let mut reporter = vec![];

	let input = "
		some: true

		#

		seq: (0, 1, 2)

		zero_u8: 0
		one_u8: 1

		zero_i8: 0
		one_i8: 1
		minus_one_i8: -1

		# [[empty_table]]

		# [[tabular].{first, second}]
		0, 1
		2, 3

		# [[tabular].{{first, second}}]
		4, 5

		# [variants]:Structured
		i32: 12345
		f64: 6789.0

		# [[variants]:Tuple]
		(0, 1)

		# [[variants]:Newtype]
		(3)

		# [[variants]]
		Unit

		# [[variants]:Weird]
		()

		#

		unit_variant: Unit
		weird_variant: Weird()
		newtype_variant: Newtype(4)
		tuple_variant: Tuple(5, 6)

		false: false
		true: true
	";

	assert_eq!(
		dbg!(from_taml_str::<Deserializable, _>(
			input,
			&mut reporter,
			&[]
		))
		.tap_err(|_| {
			let mut codemap = CodeMap::new();
			let input_span = codemap.add_file("".to_string(), input.to_string()).span;

			Emitter::stderr(ColorConfig::Auto, Some(&codemap)).emit(
				reporter
					.into_iter()
					.map(|diagnostic| Diagnostic {
						level: match diagnostic.level() {
							DiagnosticLevel::Warning => Level::Warning,
							DiagnosticLevel::Error => Level::Error,
						},
						message: "This shouldn't happen.".to_string(),
						code: Some(diagnostic.code()),
						spans: diagnostic
							.labels
							.into_iter()
							.map(|label| SpanLabel {
								span: match label.span {
									Some(span) => {
										input_span.subspan(u64(span.start), u64(span.end))
									}
									None => input_span.subspan(input_span.len(), input_span.len()),
								},
								label: label.caption.map(Cow::into_owned),
								style: match label.priority {
									DiagnosticLabelPriority::Primary => SpanStyle::Primary,
									DiagnosticLabelPriority::Auxiliary => SpanStyle::Secondary,
								},
							})
							.collect(),
					})
					.collect::<Vec<_>>()
					.as_slice(),
			);
		})
		.unwrap(),
		Deserializable {
			none: None,
			some: Some(true),

			seq: vec![0, 1, 2],

			zero_u8: 0,
			one_u8: 1,

			zero_i8: 0,
			one_i8: 1,
			minus_one_i8: -1,

			empty_table: vec![],

			tabular: vec![
				Tabular {
					first: 0,
					second: 1,
				},
				Tabular {
					first: 2,
					second: 3,
				},
				Tabular {
					first: 4,
					second: 5,
				},
			],

			variants: vec![
				Enum::Structured {
					i32: 12345,
					f64: 6789.0
				},
				Enum::Tuple(0, 1),
				Enum::Newtype(3),
				Enum::Unit,
				Enum::Weird(),
			],

			unit_variant: Enum::Unit,
			weird_variant: Enum::Weird(),
			newtype_variant: Enum::Newtype(4),
			tuple_variant: Enum::Tuple(5, 6),

			r#false: false,
			r#true: true,
		}
	);
}
