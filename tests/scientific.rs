use cast::u64;
use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use debugless_unwrap::DebuglessUnwrap;
use scientific::Scientific;
use serde::Deserialize;
use serde_taml::de::{from_taml_str, type_overrides::from_decimal};
use std::io::stdout;

#[allow(non_camel_case_types)]
type tamlDiagnostic = taml::diagnostics::Diagnostic<usize>;

#[derive(Debug, Deserialize)]
struct Sci {
	#[serde(deserialize_with = "from_decimal")]
	pi: Scientific,
}

#[test]
fn unsupported_characters() {
	let text = "pi: 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";
	let mut diagnostics = vec![];
	let parsed = from_taml_str::<Sci, _>(text, &mut diagnostics, &[]);
	assert_eq!(diagnostics.as_slice(), &[]);
	report(text, diagnostics);
	assert_eq!(parsed.debugless_unwrap().pi.to_string(), "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679")
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
