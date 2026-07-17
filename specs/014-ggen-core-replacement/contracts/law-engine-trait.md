# Contract: `LawEngine` trait (internal interface, engine → ggen-graph/ggen-marketplace)

This is the one new internal interface this migration introduces — the seam between the
replacement engine's law/SHACL/N3 evaluation (backed by `praxis-graphlaw`, a different RDF
library family than the rest of this codebase) and the existing oxigraph-based crates that
need to consume its output without taking on that dependency themselves. Full design
rationale in
[03-RDF-ENGINE-BRIDGE-DESIGN](../../../docs/jira/v26.7.16/03-RDF-ENGINE-BRIDGE-DESIGN.md).

## Interface

```rust
/// Exposed by the engine crate. No oxrdf, no spargebra, no oxigraph model
/// type appears in this signature -- only plain strings.
pub trait LawEngine: Send + Sync {
    fn materialize(&self, facts_ntriples: &str, rules_n3: &str) -> Result<MaterializeOutcome>;
    fn validate_shacl(&self, facts_ntriples: &str, shapes_ttl: &str) -> Result<ShaclOutcome>;
    fn check_denials(&self, facts_ntriples: &str, rules_n3: &str) -> Result<Vec<String>>;
}
```

## Contract rules

1. **No model types cross the boundary.** Every parameter and return value is a plain
   `&str`/`String`/`Vec<String>` (N-Triples-encoded facts, N3-encoded rules, Turtle-encoded
   shapes). No `oxrdf::*`, `spargebra::*`, or `oxigraph::*` type may appear in this trait's
   signature, ever — this is the entire point of the seam.
2. **Callers own re-ingestion.** A caller in `ggen-graph`/`ggen-marketplace` receives
   `MaterializeOutcome.derived: Vec<String>` (N-Triples lines) and is responsible for loading
   them into its own oxigraph store — the engine does not reach into the caller's store.
3. **`ggen-graph`'s own `validate_shacl` remains authoritative for its existing callers.**
   `LawEngine::validate_shacl` is only for the μ-pipeline's law gate inside the engine;
   `ggen-graph`'s pre-existing SHACL module (`/Users/sac/ggen/crates/ggen-graph/src/lib.rs:24,39`)
   is unaffected and continues to serve its current, unrelated callers. The two must never be
   silently conflated as "the SHACL validator."
4. **No new `oxrdf`/`spargebra` dependency in consumers.** Adding this trait to
   `ggen-graph`'s or `ggen-marketplace`'s `Cargo.toml` dependency list is a contract
   violation — they consume the trait's `&str`-only output, not the library that produced
   it.

## Verification

- A unit test constructing a fact set requiring N3 materialization to satisfy a SHACL shape
  (mirroring the source crate's own `graphlaw_e2e.rs` proof: the same fixture fails under
  plain oxigraph and succeeds once N3-derived facts are present) demonstrates the trait is
  load-bearing, not decorative.
- `cargo tree -p ggen-graph` and `cargo tree -p ggen-marketplace` must show no
  `oxrdf`/`spargebra` dependency after this migration.
