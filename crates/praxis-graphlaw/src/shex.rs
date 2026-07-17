//! ShEx validation entry point. Delegates to the native validator
//! (`crate::shex_native`), which validates directly against `TripleIndex`
//! with no external ShEx crate dependency -- mirroring `crate::shacl`'s
//! own fully-native architecture. An earlier version of this module
//! delegated to the `shex_ast`/`shex_validation`/`rudof_rdf` crates; that
//! approach was replaced after fuzzing found a real, confirmed spec
//! violation in the delegated crates' `OneOf` handling that couldn't be
//! fixed without owning the matching logic (see `shex_native.rs`'s module
//! doc comment and `docs/jira/26.7.4/012-shacl-fmea-matrix.md`-adjacent
//! notes for the investigation). The native validator was cross-checked
//! against the full existing ShEx test surface (60+ vendored W3C
//! shexTest-derived cases, stress/impossible/vocabulary-fuzz tests) before
//! this switch, with zero regressions.

pub use crate::shex_native::{
    validate_shex_native as validate_shex, ShexValidationFailure, ShexValidationReport,
};
