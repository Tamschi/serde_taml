#![doc(html_root_url = "https://docs.rs/serde_taml/0.0.1")]
#![warn(clippy::pedantic)]
#![allow(clippy::if_not_else, clippy::too_many_lines)]

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

pub mod de;
