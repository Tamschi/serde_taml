use cast::{u64, u8};
use codemap::CodeMap;
use codemap_diagnostic::{ColorConfig, Diagnostic, Emitter, Level, SpanLabel, SpanStyle};
use serde::Deserialize;
use serde_taml::de::{from_taml_str, EncodingError};
use std::{borrow::Cow, io::stdout, iter};
use taml::diagnostics::{DiagnosticLabel, DiagnosticLabelPriority, DiagnosticType};
use tap::{Conv, Pipe};

#[allow(non_camel_case_types)]
type tamlDiagnostic = taml::diagnostics::Diagnostic<usize>;

#[derive(Debug, Deserialize)]
struct Decoded {
	#[serde(with = "serde_bytes")]
	data: Vec<u8>,
}

#[test]
fn unsupported_character() {
	let text = "data: <Windows-1252:Bonjour ! До свидания!>\n";
	let mut diagnostics = vec![];
	from_taml_str::<Decoded, _>(
		text,
		&mut diagnostics,
		&[("Windows-1252", &|decoded| {
			// See <ftp://ftp.unicode.org/Public/MAPPINGS/VENDORS/MICSFT/WINDOWS/CP1252.TXT>.
			// `#` (0x23) is used as spacer.
			const INSERT_0X80: &str = "€#‚ƒ„…†‡ˆ‰Š‹Œ#Ž##‘’“”•–—˜™š›œ#žŸ";

			let mut decoded_chars = decoded.char_indices();
			#[allow(clippy::never_loop)]
			let mut error_start = 'success: loop {
				let mut encoded = vec![];
				for (i, d) in decoded_chars.by_ref() {
					encoded.push(match d {
						'\0'..='\u{7F}' | '\u{A0}'..='\u{FF}' => u8(d.conv::<u32>()).unwrap(),
						_ => {
							match INSERT_0X80
								.chars()
								.enumerate()
								.find_map(|(o, c)| (d == c).then(|| o))
							{
								Some(o) => u8(0x80 + o).unwrap(),
								None => break 'success i,
							}
						}
					})
				}
				return Ok(Cow::Owned(encoded));
			}
			.pipe(Some);

			let mut errors = vec![];
			for (i, d) in decoded_chars.chain(iter::once((decoded.len(), 'a'))) {
				if matches!(d, '\0'..='\u{7F}' | '\u{A0}'..='\u{FF}') || INSERT_0X80.contains(d) {
					error_start.take().into_iter().for_each(|error_start| {
						errors.push(EncodingError {
							decoded_span: error_start..i,
							message: Cow::Owned(format!(
								"Unsupported character(s): `{}`.",
								&decoded[error_start..i]
							)),
						})
					})
				} else {
					error_start.get_or_insert(i);
				}
			}
			Err(errors)
		})],
	)
	.unwrap_err();
	assert_eq!(
		diagnostics.as_slice(),
		&[tamlDiagnostic {
			r#type: DiagnosticType::InvalidValue,
			labels: vec![
				DiagnosticLabel {
					caption: "Unsupported character(s): `До`.".pipe(Cow::Borrowed).into(),
					span: Some(30..34),
					priority: DiagnosticLabelPriority::Primary,
				},
				DiagnosticLabel {
					caption: "Unsupported character(s): `свидания`."
						.pipe(Cow::Borrowed)
						.into(),
					span: Some(35..51),
					priority: DiagnosticLabelPriority::Primary,
				},
				DiagnosticLabel {
					caption: "Encoding specified here.".pipe(Cow::Borrowed).into(),
					span: Some(7..19),
					priority: DiagnosticLabelPriority::Auxiliary,
				}
			]
		}]
	);
	report(text, diagnostics)
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
