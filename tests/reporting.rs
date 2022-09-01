use cast::u64;
use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use educe::*;
use indexmap::{indexmap, IndexMap};
use serde::{de::IgnoredAny, Deserialize};
use serde_taml::de::from_taml_str;
use std::io::stdout;
use taml::diagnostics::{DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType};

#[allow(non_camel_case_types)]
type tamlDiagnostic = taml::diagnostics::Diagnostic<usize>;

#[derive(Debug, PartialEq, Deserialize)]
struct NoFields {}

#[test]
fn unknown_field_none_accepted() {
	let text = "key: \"value\"\n";
	let mut diagnostics = vec![];
	from_taml_str::<NoFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::UnknownField,
			labels: vec![
				DiagnosticLabel::new(
					"Unknown field `key`.",
					0..3,
					DiagnosticLabelPriority::Primary
				),
				DiagnosticLabel::new(
					"Hint: This struct does not accept any fields.",
					0..0,
					DiagnosticLabelPriority::Auxiliary,
				)
			]
		}]
	);
	report(text, diagnostics)
}

#[test]
fn unknown_fields_none_accepted() {
	let text = "key: \"value\"\n\
    another: \"value\"\n";
	let mut diagnostics = vec![];
	from_taml_str::<NoFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[
			tamlDiagnostic {
				type_: DiagnosticType::UnknownField,
				labels: vec![
					DiagnosticLabel::new(
						"Unknown field `key`.",
						0..3,
						DiagnosticLabelPriority::Primary
					),
					DiagnosticLabel::new(
						"Hint: This struct does not accept any fields.",
						0..0,
						DiagnosticLabelPriority::Auxiliary,
					),
				]
			},
			tamlDiagnostic {
				type_: DiagnosticType::UnknownField,
				labels: vec![
					DiagnosticLabel::new(
						"Unknown field `another`.",
						13..20,
						DiagnosticLabelPriority::Primary
					),
					DiagnosticLabel::new(
						"Hint: This struct does not accept any fields.",
						0..0,
						DiagnosticLabelPriority::Auxiliary,
					),
				]
			}
		]
	);
	report(text, diagnostics)
}

#[derive(Debug, PartialEq, Deserialize)]
struct ThreeFields {
	#[serde(default)]
	field_1: i8,

	#[serde(default)]
	field_2: String,

	#[serde(default)]
	field_3: f32,
}

#[test]
fn expected_other_fields() {
	let text = "key: \"value\"\n";
	let mut diagnostics = vec![];
	from_taml_str::<ThreeFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::UnknownField,
			labels: vec![
				DiagnosticLabel::new(
					"Unknown field `key`.",
					0..3,
					DiagnosticLabelPriority::Primary
				),
				DiagnosticLabel::new(
					"Hint: The following additional fields are accepted here: `field_1`, `field_2`, `field_3`.",
					0..0,
					DiagnosticLabelPriority::Auxiliary,
				)
			]
		}]
	);
	report(text, diagnostics)
}

#[test]
fn expected_additional_fields() {
	let text = r#"field_2: ""
key: "value"
"#;
	let mut diagnostics = vec![];
	from_taml_str::<ThreeFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::UnknownField,
			labels: vec![
				DiagnosticLabel::new(
					"Unknown field `key`.",
					12..15,
					DiagnosticLabelPriority::Primary
				),
				DiagnosticLabel::new(
					"Hint: The following additional fields are accepted here: `field_1`, `field_3`.",
					0..0,
					DiagnosticLabelPriority::Auxiliary,
				)
			]
		}]
	);
	report(text, diagnostics)
}

#[derive(Debug, PartialEq, Deserialize)]
struct TypedFields {
	i8: Option<i8>,
	string: Option<String>,
	f32: Option<f32>,
	f64: Option<f64>,
}

#[test]
fn expect_i8() {
	let text = "i8: \"value\"\n";
	let mut diagnostics = vec![];
	from_taml_str::<TypedFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::InvalidType,
			labels: vec![DiagnosticLabel::new(
				"Expected integer.",
				4..11,
				DiagnosticLabelPriority::Primary,
			)]
		}]
	);
	report(text, diagnostics)
}

#[test]
fn expect_string() {
	let text = "string: 0\n";
	let mut diagnostics = vec![];
	from_taml_str::<TypedFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::InvalidType,
			labels: vec![DiagnosticLabel::new(
				r#"Expected string (`"…"`)."#,
				8..9,
				DiagnosticLabelPriority::Primary,
			)]
		}]
	);
	report(text, diagnostics)
}

#[test]
fn expect_f32() {
	let text = "f32: (1, 2, 3, 4, 5)\n";
	let mut diagnostics = vec![];
	from_taml_str::<TypedFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::InvalidType,
			labels: vec![DiagnosticLabel::new(
				"Expected decimal.",
				5..20,
				DiagnosticLabelPriority::Primary,
			)]
		}]
	);
	report(text, diagnostics)
}

#[test]
fn decimal_hint_1() {
	let text = "f32: 1\n";
	let mut diagnostics = vec![];
	from_taml_str::<TypedFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::InvalidType,
			labels: vec![
				DiagnosticLabel::new("Expected decimal.", 5..6, DiagnosticLabelPriority::Primary),
				DiagnosticLabel::new("Hint: Try `1.0`.", 5..6, DiagnosticLabelPriority::Auxiliary),
			]
		}]
	);
	report(text, diagnostics)
}

#[test]
fn decimal_hint_2() {
	let text = "f64: 2\n";
	let mut diagnostics = vec![];
	from_taml_str::<TypedFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::InvalidType,
			labels: vec![
				DiagnosticLabel::new("Expected decimal.", 5..6, DiagnosticLabelPriority::Primary),
				DiagnosticLabel::new("Hint: Try `2.0`.", 5..6, DiagnosticLabelPriority::Auxiliary),
			]
		}]
	);
	report(text, diagnostics)
}

#[derive(Debug, Deserialize, PartialEq)]
struct ExtraFields {
	known: String,

	#[serde(rename = "taml::extra_fields")]
	extra_fields: IndexMap<String, String>,
}

#[test]
fn extra_fields() {
	let text = "known: \"It is known.\"\n\
    unknown: \"It is unknowable.\"\n";
	let mut diagnostics = vec![];
	assert_eq!(
		from_taml_str::<ExtraFields, _>(text, &mut diagnostics, &[])
			.map_err(|err| eprintln!("{}; diagnostics: {:#?}", err, diagnostics))
			.unwrap(),
		ExtraFields {
			known: "It is known.".to_string(),
			extra_fields: indexmap! {
				"unknown".to_string() => "It is unknowable.".to_string()
			},
		},
	);
	assert_eq!(diagnostics.as_slice(), &[]);
	report(text, diagnostics);
}

#[derive(Debug, Deserialize, Educe)]
#[educe(PartialEq)]
struct IgnoredExtraFields {
	known: String,

	#[allow(dead_code)]
	#[educe(PartialEq(ignore))]
	#[serde(rename = "taml::extra_fields")]
	extra_fields: IgnoredAny,
}

#[test]
fn ignored_extra_fields() {
	let text = "known: \"It is known.\"\n\
    unknown: \"It is unknowable.\"\n";
	let mut diagnostics = vec![];
	assert_eq!(
		from_taml_str::<IgnoredExtraFields, _>(text, &mut diagnostics, &[]).unwrap(),
		IgnoredExtraFields {
			known: "It is known.".to_string(),
			extra_fields: IgnoredAny,
		},
		"diagnostics: {:?}",
		diagnostics
	);
	assert_eq!(diagnostics.as_slice(), &[]);
	report(text, diagnostics);
}

#[derive(Debug, Deserialize, PartialEq)]
struct MissingFields {
	missing_field: i8,
}

#[test]
#[allow(clippy::reversed_empty_ranges)]
fn missing_fields() {
	let text = "\n";
	let mut diagnostics = vec![];
	from_taml_str::<MissingFields, _>(text, &mut diagnostics, &[]).unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			type_: DiagnosticType::MissingField,
			labels: vec![
				DiagnosticLabel::new(
					"Missing field `missing_field`.",
					0..0,
					DiagnosticLabelPriority::Primary,
				),
				DiagnosticLabel::new(
					"Invalid type: Unexpected Option value, expected i8.",
					None,
					DiagnosticLabelPriority::Auxiliary,
				)
			]
		}]
	);
	report(text, diagnostics);
}

fn report(text: &str, diagnostics: Vec<tamlDiagnostic>) {
	let mut codemap = CodeMap::new();
	let file_span = codemap.add_file("TAML".to_string(), text.to_string()).span;

	let diagnostics: Vec<_> = diagnostics
		.into_iter()
		.map(|diagnostic| Diagnostic {
			code: Some(diagnostic.code()),
			level: match diagnostic.level() {
				taml::diagnostics::DiagnosticLevel::Warning => Level::Warning,
				taml::diagnostics::DiagnosticLevel::Error => Level::Error,
			},
			message: diagnostic.message().to_string(),
			spans: diagnostic
				.labels
				.into_iter()
				.map(|label| SpanLabel {
					label: label.caption.map(|c| c.to_string()),
					style: match label.priority {
						taml::diagnostics::DiagnosticLabelPriority::Primary => SpanStyle::Primary,
						taml::diagnostics::DiagnosticLabelPriority::Auxiliary => {
							SpanStyle::Secondary
						}
					},
					span: match label.span {
						Some(span) => file_span.subspan(u64(span.start), u64(span.end)),
						None => file_span.subspan(file_span.len(), file_span.len()),
					},
				})
				.collect(),
		})
		.collect();

	if !diagnostics.is_empty() {
		// Not great, but seems to help a bit with output weirdness.
		let stdout = stdout();
		let _stdout_lock = stdout.lock();
		Emitter::stderr(ColorConfig::Auto, Some(&codemap)).emit(&diagnostics)
	}
}
