//! [Serde](`serde`)-adapter for the [`taml`] crate.
//!
//! For TAML syntax help, see the latter's documentation and <https://taml.schichler.dev>.
//!
//! # Features
//!
//! ## `"serde-object-assist"`
//!
//! Iff this features is enabled, `serde_taml` supports variant spying compatible with `serde-object@0.0.0-alpha.0`.
//!
//! > It's a bit of a hack, but you can deserialize unknown enums through the Serde API this way.
//!
//! # Expecting data literals
//!
//! Data literals are bytes towards Serde.
//! Use for example the [`serde_bytes`](http://docs.rs/serde_bytes/0.11) crate as follows:
//!
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
//! let mut diagnostics = vec![]; // Alternatively `()` to skip reporting, though ideally always print diagnostics.
//! let configuration: Result<Configuration, _> = from_taml_str(
//!     "binary_data: <UTF-8:Arbitrary string input!>",
//!     &mut diagnostics,
//!     &[("UTF-8", &|str| Ok(Cow::Borrowed(str.as_bytes())))], // Encodings are consumer-defined.
//! );
//!
//! assert_eq!(
//!     configuration.unwrap().binary_data,
//!     "Arbitrary string input!".as_bytes(),
//! );
//! ```
//!
//! # Type overrides
//!
//! `serde_taml` (usually) infers a single expected TAML type based on the requested Serde type,
//! but it's possible to replace this expectation with a different one in many cases!
//!
//! This can for example be used to parse arbitrary precision decimals:
//!
//! ```rust
//! use scientific::Scientific;
//! use serde::Deserialize;
//! use serde_taml::de::{
//!     from_taml_str,
//!     type_overrides::from_decimal,
//! };
//! use std::borrow::Cow;
//! #
//! # let input = "pi: 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679";
//!
//! #[derive(Deserialize)]
//! struct Precise {
//!     #[serde(deserialize_with = "from_decimal")]
//!     pi: Scientific, // Requests a string.
//! }
//!
//! let mut diagnostics = vec![];
//! let configuration: Result<Precise, _> = from_taml_str(
//!     "pi: 3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679",
//!     &mut diagnostics,
//!     &[],
//! );
//!
//! assert_eq!(
//!     configuration.unwrap().pi.to_string(),
//!     "3.1415926535897932384626433832795028841971693993751058209749445923078164062862089986280348253421170679",
//! );
//! ```
//!
//! See [`de::type_overrides`] for the compatibility table and standard mappings.

#![doc(html_root_url = "https://docs.rs/serde_taml/0.0.3")]
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

pub use taml::validate;
