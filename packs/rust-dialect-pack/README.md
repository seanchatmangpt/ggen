# rust-dialect-pack (experimental, not wired into `ggen sync`/CI)

An RDF/SHACL/SPARQL encoding of a decidable subset of the Rust this repository's own
packs actually emit ("GEC" — ggen-emitted-core), built to test whether a graph-checker
could pre-screen generated Rust before a real `cargo build`. This is a research artifact
from a bounded scoping pass, not a production capability. Read this file before trusting
anything in `ontology.ttl`/`shapes.ttl`/`rules/*.rq` at face value.

## What is real here

- `ontology.ttl` (55 classes, 81 properties) and `shapes.ttl` (30 SHACL node shapes, 12
  `sh:sparql` boundary rules) were built against a real corpus sweep: 109 real
  Rust-emitting template sources across 21 packs, with 10 named exceptions (real `unsafe`
  in 2 FFI/WASM files, `dyn Trait`, `impl Trait` in both quantifier positions, associated
  types, generic traits) each cited to an exact `file:line`.
- `rules/010`–`090` are 9 SPARQL CONSTRUCT/ASK files, each citing the real Rust
  typing/borrowing rule it encodes (Rust Book, RFCs, RustBelt, Oxide, Stacked Borrows) and
  the exact ggen template site it was written against.
- All of it validates under `pyshacl` and `rdflib` today: `Conforms: True` on the grounded
  fixtures, `Conforms: False` against a deliberately-planted adversarial fixture, 7/7 ASK
  rules correctly flip False→True on that fixture, both CONSTRUCT rules fire non-trivially.
- Prior art was surveyed for real before any of this was written: RustBelt/Iris's semantic
  typing, lifetime logic, and adequacy theorem are categorically out of reach for any RDF
  rule dialect (they require higher-order guarded and linear-resource reasoning; RDF
  entailment is monotonic first-order). The one piece that *does* transcribe cleanly —
  lifetime-inclusion (`'a: 'b`) as a preorder — already exists as real, production Datalog
  in Polonius, rustc's own next-generation borrow checker.

## What is not yet real, stated as precisely as the above

- **There is no Rust-to-RDF extractor.** Every fact this pack checks was hand-transcribed
  from a specific cited source line, not derived automatically from real Rust source. "Run
  the checker against every file this repo has generated" is not currently a mechanical
  operation — it does not scale past the ~13 hand-modeled sites in
  `fixtures/gec-facts.ttl`.
- **The only real agreement number is 8/8 (100%) on 8 hand-picked sites** where a real,
  currently-synced consumer crate exists to check against (`affidavit-verify`,
  `cargo-cicd-verify`, `clap-noun-verb-cli`, `tcps-generated`). This is not a claim about
  the other ~101 template sites.
- **3 of the 9 rules' primary citations have zero real generated Rust anywhere in this
  repo to check against** (`osx-clnr-pack`, `mcpp-pack`, `anti-llm-cheat-lsp-pack` — none
  have a committed `examples/*-verify` consumer). Those rules are grounded against
  template *text*, never against a compiled artifact.
- **One real fact-base defect was found**: `fixtures/gec-facts.ttl`'s
  `gec:Variant_AffidavitChain_IoError` individual asserts `gec:hasFromConversion true`,
  but the real generated file (`examples/affidavit-verify/src/affidavit_chain.rs`) has no
  `#[from]` on that variant and uses a manual `.map_err(...)` instead. This causes no wrong
  verdict today (no active rule consults that predicate), but rule `080`'s own header
  proposes building exactly this kind of check on top of it — a latent false-negative
  waiting to be built on an unverified fact.
- **Not wired into anything.** No `pack.toml`, no `ggen.toml` entry, no `just` recipe, no
  CI workflow. It only runs when invoked by hand.
- Real compilation was required once to calibrate any of the above (10 real
  `cargo build`/`cargo test` runs across every synced consumer in this repo, ~369 tests,
  0 failures) — that is the cost this pack's own design accepts, not something it works
  around.

## If this becomes real infrastructure

The next real step is not more rules — it's the extractor: a `syn`-based (or `rustc`
`-Z parse-crate-metadata`-based) tool that projects real `.rs` source into `gec:` facts
automatically, so the 8/8 number above can be recomputed against the full generated
corpus instead of 8 hand-picked sites, and the 3 untestable rules can either get a real
consumer to check against or be retired.
