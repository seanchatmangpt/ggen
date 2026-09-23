//! `ggen-graph-wasm`: wasm32-buildable QueryEngine boundary for ggen-graph: OxigraphEngine (native) + RemoteEngine (wasm-safe, delegates SPARQL execution over HTTP).
//!
//! Manufactured by ggen-graph-wasm-pack from an admitted `gwp:WasmQueryEngineCrateSpec` --
//! do not hand-edit the trait/module shell; regenerate from the spec instead. Real
//! request-building/response-parsing bodies inside `remote_engine` are hand-written glue
//! this pack's construct_only boundary deliberately leaves as an honest `todo!()`, not a
//! faked implementation -- see ggen-graph-wasm-pack's pack.toml for the disclosed scope.

#[cfg(all(feature = "oxigraph-engine", not(target_arch = "wasm32")))]
pub mod oxigraph_engine;

#[cfg(feature = "remote-engine")]
pub mod remote_engine;

/// One SPARQL SELECT result row, reshaped to plain owned strings -- the same row shape
/// both `OxigraphEngine` and `RemoteEngine` return, so callers never need to know which
/// engine produced a given `Vec<Row>`.
pub type Row = std::collections::BTreeMap<String, String>;

/// A pluggable SPARQL query-execution boundary. `OxigraphEngine` (native-only) embeds a
/// real oxigraph store; `RemoteEngine` (wasm32-safe) delegates execution over HTTP to an
/// already-running external SPARQL endpoint -- mirroring `ggen_igniter`'s `--engine
/// qlever` precedent of "hold the parsed graph in-process, delegate query *execution*
/// externally" so a wasm32 build never needs to link oxigraph at all.
pub trait QueryEngine {
    type Error: std::fmt::Debug;

    /// Runs `sparql` (a SPARQL SELECT query string) and returns its result rows.
    fn query(&self, sparql: &str) -> Result<Vec<Row>, Self::Error>;
}
