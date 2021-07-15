//! [Serde](`serde`)-adapter for the [`taml`] crate.
//!
//! For TAML syntax help, see the latter's documentation.
//!
//! *Data literals* are bytes towards Serde.
//! Use for example the [`serde_bytes`] crate as follows:
//!
//! ```taml
//! binary_data: <UTF-8:Arbitrary string input!>
//! ```
//! ```rust
//! use serde::Deserialize;
//! use serde_taml::de::from_taml_str;
//! use std::borrow::Cow;
//! #
//! # let input = "binary_data: <UTF-8:Arbitrary string input!>";
//!
//! #[derive(Deserialize)]
//! struct Configuration {
//!     #[serde(with = "serde_bytes")]
//!     binary_data: Vec<u8>,
//! }
//!
//! let mut diagnostics = vec![]; // Alternatively `()` to skip reporting!
//! let configuration: Result<Configuration, _> = from_taml_str(input, &mut diagnostics, &[
//!     ("UTF-8", &|str| Ok(Cow::Borrowed(str.as_bytes())))
//! ]);
//! ```

#![doc(html_root_url = "https://docs.rs/serde_taml/0.0.1")]
#![warn(clippy::pedantic)]
#![allow(clippy::if_not_else, clippy::too_many_lines)]
#![warn(missing_docs)]

#[cfg(doctest)]
pub mod readme {
	doc_comment::doctest!("../README.md");
}

/// [Serde](`serde`)-compatible TAML deserialisation facilities.
pub mod de;

pub use taml;
