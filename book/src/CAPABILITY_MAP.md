# Repository Capability and Pack Map

This map binds the pack-writer pattern language to repository evidence. It is an observation ledger, not a roadmap and not a claim that every pattern already has a complete pack witness.

## Standing vocabulary

- **IMPLEMENTED** — a live crate or command owns the capability.
- **PACK_WITNESS** — at least one committed pack and consumer exercise the capability.
- **PARTIAL** — machinery exists, but the complete chapter claim is not demonstrated at the same scale.
- **TARGET** — the chapter is normative design guidance without a current repository witness.
- **ARCHIVE_ONLY** — evidence exists only in historical material and cannot establish current standing.

A chapter may cite multiple states. For example, deterministic RDF deltas can be **IMPLEMENTED**, while a claimed Level Five product based on them remains **PARTIAL**.

## Capability ownership

| Surface | Current responsibility | Primary evidence |
|---|---|---|
| `ggen-config` | Typed `ggen.toml` parsing, validation and deterministic manifest collections | `crates/ggen-config/src/manifest/types.rs` |
| `ggen-engine` | Schema dispatch, pack resolution, graph/law loading, projection, templates, bounded writes and receipts | `crates/ggen-engine/src/lib.rs`, `crates/ggen-engine/src/pack.rs`, `crates/ggen-engine/src/sync.rs` |
| `ggen-graph` | Deterministic RDF state, deltas, SHACL, SPARQL, hooks and transition receipts | `crates/ggen-graph/src/lib.rs` |
| `praxis-graphlaw` | Rule materialization and graph-law hooks | `crates/praxis-graphlaw/src/lib.rs` |
| `ggen-marketplace` | Discovery, composition, lifecycle, passports, isolation and substitution | `crates/ggen-marketplace/src/lib.rs`, `crates/ggen-marketplace/src/packs_registry/` |
| `ggen-lsp` | Live diagnostics, admissibility-pack emission, repair routes and OCEL evidence | `crates/ggen-lsp/src/lib.rs`, `crates/ggen-lsp/src/pack/mod.rs` |
| `ggen-cli-lib` | User-facing command routing into engine capabilities | `crates/ggen-cli/src/` |
| `chicago-tdd-tools` | Independent state-based test support | `crates/chicago-tdd-tools/` |
| `ggen-cheat-scanner` | Detection of vacuous or suspicious generated proof surfaces | `crates/ggen-cheat-scanner/` |
| `cargo-cicd` / `cargo-cicd-pack` | Release-law execution and generated delivery surfaces | `packs/cargo-cicd-pack/` |

## Pack witnesses

| Pack | Demonstrated capability | Evidence boundary | Standing |
|---|---|---|---|
| `packs/ggen-verify-pack` | Admits external build, test, clippy and byte-identity evidence as graph facts; refuses missing, red or stale evidence | Pack manifest, gates, bootstrap emitter and reasoner-independence test | **PACK_WITNESS** |
| `packs/star-toml-pack` | Generates typed TOML admission code, validation behavior, docs and two independent proof styles | `examples/star-toml-verify`, generated tests, drift mutation and idempotency record in `pack.toml` | **PACK_WITNESS** |
| `packs/lsp-max-pack` | Generates ontology-sourced lint-rule TOML, documentation and independent proof tests | `examples/lsp-max-verify`, SHACL validation, mutation and idempotency record in `pack.toml` | **PACK_WITNESS** |
| `packs/praxis-core-pack` | Projects Praxis refusal and core-law surfaces with generated verification | Pack templates, ontology and verification consumer | **PACK_WITNESS** |
| `packs/cargo-cicd-pack` | Projects CI/CD and release assets from ontology | Pack ontology/templates; broader release portability remains bounded by available SDKs | **PARTIAL** |
| `packs/wasm4pm-facts-pack` | Projects WASM4PM fact coverage and proof artifacts | Pack templates and proof surfaces | **PARTIAL** |
| `packs/affidavit-pack` | Generates affidavit/evidence-oriented artifacts with a consumer | `examples/affidavit-verify` | **PARTIAL** |
| `packs/chicago-tdd-tools-pack` | Makes Chicago-style test support consumable as a pack | Pack manifest and templates | **PARTIAL** |
| `packs/repo-as-found-pack` | Models admitted observations of an existing repository | `packs/repo-as-found-pack/pack.toml` | **PARTIAL** |
| `packs/repo-load-path-pack` | Models repository load-path decisions | `packs/repo-load-path-pack/pack.toml` | **PARTIAL** |
| `packs/repo-intervention-pack` | Models bounded repository interventions | `packs/repo-intervention-pack/pack.toml` | **PARTIAL** |
| `packs/repo-reconciliation-pack` | Models reconciliation of repository states | `packs/repo-reconciliation-pack/pack.toml` | **PARTIAL** |
| `packs/temporary-works-pack` | Models temporary construction required during change | `packs/temporary-works-pack/pack.toml` | **PARTIAL** |
| `packs/claude-code-pack` | Projects coding-agent configuration | Pack manifest and templates | **PARTIAL** |
| `packs/gh-terraform-pack` | Projects GitHub/Terraform infrastructure surfaces | Pack manifest and templates | **PARTIAL** |
| `packs/mfw-pcp-level5-pack` | Carries Multifractal Workflow / proof-carrying-plan Level Five material | Pack manifest and ontology | **PARTIAL** |
| `packs/ma-case-study-pack` | Case-study corpus pack | Pack ontology and design record | **PARTIAL** |

Marketplace descriptors under `marketplace/packs/` are discoverable package records. They are not automatically equivalent to executable local packs and must not be cited as a pack witness without a resolved package, generated output and consumer receipt.

## Field-to-capability alignment

| Book field and chapters | Live capability binding | Concrete pack witnesses | Current crown standing |
|---|---|---|---|
| **Using the Pattern Language** | Documentation law, standing vocabulary and executable acceptance | This map; `book/scripts/check_book.py` | **IMPLEMENTED** structurally; runtime claims remain bounded |
| **I. Whole Manufacturing System** — 1–12 | `ggen-engine`, `ggen-graph`, `praxis-graphlaw`, receipts and bounded writes | `ggen-verify-pack`, `star-toml-pack` | **PARTIAL** at whole-system scale |
| **II. Pack as a Living Part** — 13–24 | Pack parsing/resolution, manifests, hashes, policies, routes and provenance | `ggen-lsp` emitted admissibility pack; local pack manifests | **PARTIAL**; identity/passport work is implemented in marketplace |
| **III. Choose the Right Kind of Part** — 25–36 | Marketplace package types, codegen packs, hook packs and corpus packs | `star-toml-pack`, `ggen-verify-pack`, `ma-case-study-pack` | **PACK_WITNESS** for the taxonomy, not every subtype at L5 |
| **IV. Standing before Scale** — 37–52 | Verification ladders, cheat scanning, consumer tests, drift and receipts | `ggen-verify-pack`, `star-toml-pack`, `lsp-max-pack` | **PACK_WITNESS** for several maturity cells; universal L5 is **TARGET** |
| **V. Independent Reality** — 53–64 | Independent fixtures/tests and reference-tree comparison | `star-toml-pack`, `lsp-max-pack`, TCPS project record | **PARTIAL** |
| **VI. Ontology Neighborhood** — 65–80 | RDF parsing, stable IRIs, public vocabularies and graph-law materialization | Most ontology-bearing packs | **IMPLEMENTED** primitives; modeling quality is pack-specific |
| **VII. Admission Gates** — 81–94 | SHACL/SPARQL gate execution and typed refusal before writes | `ggen-verify-pack`, `star-toml-pack`, `lsp-max-pack` | **PACK_WITNESS** |
| **VIII. Deterministic Projection** — 95–120 | SPARQL extraction/CONSTRUCT, Tera rendering and write ownership | `star-toml-pack`, `lsp-max-pack`, `praxis-core-pack` | **PACK_WITNESS** |
| **IX. Complete Product Surfaces** — 121–134 | Generated Rust modules, manifests, C/WASM boundaries | TCPS book laboratory, `wasm4pm-facts-pack` | **PARTIAL**; full multi-target substitution is not universally verified |
| **X. Proof beside Product** — 135–148 | Generated tests, Chicago TDD, mutation sensitivity and cheat scanning | `star-toml-pack`, `lsp-max-pack`, `ggen-verify-pack` | **PACK_WITNESS** |
| **XI. Consumer Place** — 149–160 | Real `ggen.toml`, sync, generated mounts, build/test and replay | `examples/star-toml-verify`, `examples/lsp-max-verify`, `examples/affidavit-verify` | **PACK_WITNESS** |
| **XII. Pack Neighborhood** — 161–172 | Union graph, composition, non-interference, output ownership and passports | Marketplace composition code; multi-pack tests; repository lifecycle packs | **PARTIAL** |
| **XIII. Time, Change and Repair** — 173–186 | Locking, checksum freeze, idempotency, drift refusal, LSP repair routes | `star-toml-pack`, `lsp-max-pack`, emitted LSP admissibility packs | **PACK_WITNESS** for checksum/idempotency; longitudinal repair is **PARTIAL** |
| **XIV. Receipts and Standing** — 187–200 | Input/output/transition hashes, signatures, verification facts and provenance | `ggen-verify-pack`, graph transition receipts, marketplace passports | **PARTIAL** across the full supply chain |
| **XV. Change the Engine Only at the Boundary** — 201–212 | Shared schema dispatch, pack gate execution, write/error semantics | `ggen-engine` and reasoner-independence tests | **IMPLEMENTED** machinery; necessity claims remain case-specific |
| **XVI. Level Five Sequence** — 213–224 | Product/ontology/acceptance matrices and certification planning | `book/code/packs/canonical-level-five-pack`, TCPS design records | **PARTIAL** |
| **XVII. TCPS Complete Pattern** — 225–302, 337 | TCPS ontology, generated core/product/release artifacts and defect ledger | Book TCPS packs and project-record evidence | **PARTIAL** until current-head full toolchain replay |
| **XVIII. Make a New Language** — 303–320 | Practicum applying all pack-facing crates | No single repository pack can witness an arbitrary new domain | **TARGET** |
| **XIX. Certification Laboratories** — 321–336 | Adversarial mutation, consumer verification, composition, receipts and release checks | Canonical Level Five laboratory assets | **PARTIAL** until all laboratories execute on current head |
| **Pattern Reference** | Current manifests, layouts, examples and failure codes | Live pack manifests and crate schemas take precedence | **IMPLEMENTED** only where checked against current code |

## Chapter citation rule

Each chapter must identify:

1. the owning crate or command;
2. at least one exact repository path that can falsify the claim;
3. a real pack witness when one exists;
4. the current standing from this map;
5. an explicit gap when no witness exists.

A chapter may not cite a marketplace descriptor, archived document, generated listing or case-study statement as proof of a live capability unless a current consumer and verifier bind it to a reproducible run.

## Verification

```bash
python3 book/scripts/check_book.py
python3 book/scripts/check_level_five.py

# Representative live witnesses
cargo test -p ggen-engine --test reasoner_independence_e2e
cargo test -p ggen-graph
cargo test -p ggen-marketplace
```

The commands above are falsifiers, not claims that they passed in the environment reading this page. Record their exact outcome before promoting standing.