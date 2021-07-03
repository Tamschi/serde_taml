#![doc(html_root_url = "https://docs.rs/serde_taml/0.0.1")]
#![warn(clippy::pedantic)]
#![allow(clippy::if_not_else, clippy::too_many_lines)]
#![allow(clippy::missing_errors_doc, clippy::missing_panics_doc)] //TODO

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

pub mod de;
