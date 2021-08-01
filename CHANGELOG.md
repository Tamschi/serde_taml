# serde_taml Changelog

<!-- markdownlint-disable no-trailing-punctuation -->

## 0.0.3

2021-08-01

- **Breaking**:
  - Updated `taml` dependency to 0.0.11.
    > This mainly affects carriage returns.
    > See the changelog at <https://github.com/Tamschi/taml/releases/tag/v0.0.11>.
  - Moved `PositionImpl` to `taml::Position`.

## 0.0.2

2021-07-30

This is an overall rework of the library, which means just about all of the API changed.

The `"serde-object-assist"` feature is still available unchanged.

- **Breaking**:
  - Changed license to `MIT OR Apache-2.0`.
  - Updated `taml` dependency to 0.0.10.
    > This comes with some format changes!
    > Read up on them on <https://taml.schichler.dev>.
  - Set minimum Rust version to 1.53
    > which is required by the newer `taml` dependency.
  - Reworked the entire API with additional features.
  - Likely fixed all panics.

- Features:
  - `taml::validate` is now re-exported as `validate`.
    > You should call this to ensure the memory layout is correct!

## 0.0.1

2020-08-26

Initial unstable release
