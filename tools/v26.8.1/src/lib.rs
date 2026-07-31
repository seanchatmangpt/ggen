//! Shared library for the v26.8.1 tooling binaries
//! (`ggen-v26-8-1-verifier` / crown, `subsystem_verifier`,
//! `project_coverage`). Extracted so the coverage-matrix projection logic
//! has exactly one implementation, consumed identically by the manufacturing
//! binary (which writes `docs/v26.8.1/coverage-matrix.csv`) and the crown
//! (which only ever recomputes the same projection in memory and
//! byte-compares it against what is already on disk -- see
//! `coverage_projection` module docs for the observer/verifier split this
//! exists to enforce).

pub mod coverage_projection;
