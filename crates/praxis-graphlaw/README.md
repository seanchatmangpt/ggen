# praxis-graphlaw

Fork of the roxi reasoning engine (upstream: https://github.com/pbonte/roxi,
via the local fork at /Users/sac/roxi), adopted into the praxis workspace as
the GraphLaw law-state engine, v26.7.5. Library name: `praxis_graphlaw`
(upstream lib name was `minimal`).

## What it provides

Central type: `TripleStore` (`src/lib.rs`).

- Native N3 rules with builtins (pest grammars: `src/parser/n3.pest`,
  `src/shexc.pest`)
- Stratified Datalog: `materialize()`, `prove()`, `solve()`
- SPARQL 1.1: `query()` (spargebra/sparesults, sparql-12 features)
- Native SHACL: `validate_shacl()`
- ShEx / ShExC: `validate_shex()`, `validate_shex_c()`
- Denial checking: `check_denials()`
- Incremental maintenance: DRed, IMaRS; RSP (RDF stream processing)

## GraphLaw role

In praxis, this crate is the law-state engine: RDF graphs are the law state,
N3/Datalog rules are the law, SHACL/ShEx are admission gates, and SPARQL is
the query surface. It backs the GraphEngine seam consumed by `ggen`.

## Adoption notes

- Package renamed `roxi` (v0.1.0) -> `praxis-graphlaw` (v26.7.5); lib
  `minimal` -> `praxis_graphlaw`. Test/bench/doctest paths rewritten
  accordingly. No engine logic was changed.
- Dev-dependency `chicago-tdd-tools = 26.7.1` was dropped: no test imports it
  (only two doc comments mention it), and the workspace pin
  `chicago-tdd-tools = "=26.6.30"` in `praxis-retrofit` made the two
  requirements unresolvable in one graph. `proptest` is depended on directly.
- Conformance manifest-writer tests originally wrote to
  `../docs/jira/26.7.4/manifests` (a path in the upstream repo layout); they
  now write inside this crate at `docs/jira/26.7.4/manifests`, which is
  gitignored as generated test output.
- The crate inherits no workspace lints (the praxis root defines none), so no
  lint opt-out was needed; upstream code compiles with warnings only.

## License

MIT, matching upstream: "The MIT License (MIT), Copyright (c) 2022-now
Pieter Bonte, Ghent University - imec, Belgium" (see /Users/sac/roxi/LICENSE.txt).
This fork retains the MIT license (`license = "MIT"` in Cargo.toml).

## Test evidence

`cargo test -p praxis-graphlaw` (2026-07-06): 380 passed, 0 failed across the
unit, integration (datalog/n3/shacl/shex/sparql11 conformance, stress,
fuzz/proptest) and doctest suites.
