# GraphLaw Semantic Kernel — DoD#9 + Metamodel Hygiene Audit Receipt

Date: 2026-10-08
Lane: alpha-gamma
Base: ggen main @ fcfd6349d (v26.10.8, workspace-integration WIP for v26.10.9 present, uncommitted, preserved)
Subject: `crates/praxis-graphlaw/src/ggen_law.rs`, `crates/praxis-graphlaw/tests/ggen_law_pack_audit.rs`

## Laws enforced

- **DoD#9 disjointness** (`scan_dod9`, `check_dod9_disjoint`): Pack != ABB != SBB,
  pairwise; an IRI typed as two law classes is `E_DOD9_COLLISION`.
- **Metamodel hygiene** (`check_hygiene`): packs are strict consumers; an
  `owl:equivalentClass`/`rdfs:subClassOf` axiom between the pinned root and an
  external namespace is `E_METAMODEL_HYGIENE_VIOLATION`; both-outside and
  both-under axioms are allowed.
- **Authority root pin (fail-closed)**: family IRIs (`ea:`, `togaf:`,
  opengroup togaf) outside `https://spec.chatmangpt.com/ea/v1#` are
  `E_AUTHORITY_ROOT_UNPINNED`.
- **LawState namespace isolation** (`PackIngest`): each pack ingests under
  `urn:ggen:pack:<name>#`; projection into `urn:domain#` refuses pack-internal
  predicates/objects, foreign subjects, and invalid names
  (`E_NAMESPACE_LEAK`).

## Corpus result

n_packs=91, n_triples=26,924 — every `packs/*/ontology.ttl` parsed with the
real praxis-graphlaw parser and passed all three gates clean. Violations
found: none. One pre-existing defect repaired to make the corpus parseable:
`packs/autofde-semantic-registry-pack/ontology.ttl` line 28 carried an invalid
relative IRI `<../source-inventory.md>` (`prov:wasDerivedFrom`), refused by
the rio Turtle parser; rewritten to the literal `"source-inventory.md"`.

## Non-vacuity falsifiers (all refused, all pass)

- `falsifier_dod9_collision_is_refused` -> E_DOD9_COLLISION
- `falsifier_hygiene_violation_is_refused` -> E_METAMODEL_HYGIENE_VIOLATION
- `falsifier_unpinned_authority_iri_is_refused` -> E_AUTHORITY_ROOT_UNPINNED
- `falsifier_namespace_leak_is_refused` -> E_NAMESPACE_LEAK

## Verification (real gates, this session)

- `cargo test -p praxis-graphlaw --lib ggen_law` -> 11 passed, 0 failed
- `cargo test -p praxis-graphlaw --test ggen_law_pack_audit` -> 5 passed, 0 failed
- `cargo test -p ggen-abb-sbb` -> 59 passed, 0 failed (11 new depgraph tests,
  40 lib + 4 integration + 3 doc + 1 marketplace fixture; prior 48 unchanged)

## Cross-pack Datalog resolver (same receipt, part 2)

`crates/ggen-abb-sbb/src/depgraph.rs`: facts extracted from real
`pack.toml`/`ggen.toml` text (`[pack] name` + optional `[graph]` table);
forward-chained `transitive_dep` fixpoint; typed refusals
`Refusal::CyclicPackDependency` (with reconstructed cycle path),
`Refusal::UnboundPort`, plus existing `DuplicateArtifactPath`,
`DanglingReference`, `DuplicateElement`; Kahn topological sync order with the
consumer last. Gates: cycle-with-path, dangling dep, duplicate artifact path,
unbound port, bound-through-transitive-dep pass, consumer edges, duplicate
pack name.

## Per-pack audit detail

```
n_packs=91 n_triples=26924
packs/affidavit-pack/ontology.ttl: 239 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/anti-llm-cheat-lsp-pack/ontology.ttl: 54 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/autofde-execution-profile-pack/ontology.ttl: 0 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/autofde-semantic-registry-pack/ontology.ttl: 5345 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/automatic-autonomic-operations-pack/ontology.ttl: 218 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/cargo-cicd-pack/ontology.ttl: 324 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/certification-assist-evidence-control-pack/ontology.ttl: 234 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/certification-assist-pack/ontology.ttl: 527 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/chicago-tdd-tools-pack/ontology.ttl: 100 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-behavior-pack/ontology.ttl: 0 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-boundary-pack/ontology.ttl: 0 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-crate-pack/ontology.ttl: 0 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-pack/ontology.ttl: 80 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-routing-pack/ontology.ttl: 0 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-schema-pack/ontology.ttl: 216 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-specimen-pack/ontology.ttl: 19 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/clap-noun-verb-verification-pack/ontology.ttl: 0 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/claude-code-pack/ontology.ttl: 51 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/crown-conjecture-pack/ontology.ttl: 104 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/dogfood-lifecycle-pack/ontology.ttl: 164 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/domain-capability-pack/ontology.ttl: 214 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/dspy-pack/ontology.ttl: 526 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/fastmcp-pack/ontology.ttl: 13 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/fortune5-architecture-pack/ontology.ttl: 281 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/fortune5-deployment-blocks-pack/ontology.ttl: 385 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/fortune5-required-capabilities-pack/ontology.ttl: 272 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/fortune5-testing-bblock-pack/ontology.ttl: 119 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/gall-core-pack/ontology.ttl: 184 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/gall-semantic-work-pack/ontology.ttl: 50 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-combinatorial-maximalism-pack/ontology.ttl: 131 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-constitution-pack/ontology.ttl: 118 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-ecosystem-ocel-pack/ontology.ttl: 71 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-lean4-rust-pipeline-pack/ontology.ttl: 160 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-release-pack/ontology.ttl: 28 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-self-host-pack/ontology.ttl: 109 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-self-pack/ontology.ttl: 33 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ggen-verify-pack/ontology.ttl: 62 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/gh-actions-errc-pack/ontology.ttl: 72 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/gh-enterprise-architecture-pack/ontology.ttl: 116 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/gh-terraform-pack/ontology.ttl: 1581 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/github-actions-pack/ontology.ttl: 236 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/goat-capabilities-pack/ontology.ttl: 106 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/k8s-pack-DESIGN/ontology.ttl: 38 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/level-five-book-pack/ontology.ttl: 4037 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/lsp-max-pack/ontology.ttl: 46 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ma-case-study-pack/ontology.ttl: 169 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/mcpp-pack/ontology.ttl: 48 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/mermaid-pack/ontology.ttl: 423 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/mfact-pack/ontology.ttl: 84 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/mfw-pack/ontology.ttl: 125 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/mfw-pcp-level5-pack/ontology.ttl: 247 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/mmdio-pack/ontology.ttl: 68 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/multi-projection-pack/ontology.ttl: 57 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/ontostar-mustar-powlv2-agent-pack/ontology.ttl: 129 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/osx-clnr-pack/ontology.ttl: 224 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/pack-authoring-pack/ontology.ttl: 27 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/pcq-marketplace-pack/ontology.ttl: 45 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/pddl-embedded-workflow-pack/ontology.ttl: 68 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/praxis-core-pack/ontology.ttl: 171 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/process-intelligence-rag-pack/ontology.ttl: 143 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/process-mining-proof-pack/ontology.ttl: 28 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/repo-as-found-pack/ontology.ttl: 81 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/repo-intervention-pack/ontology.ttl: 108 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/repo-load-path-pack/ontology.ttl: 91 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/repo-reconciliation-pack/ontology.ttl: 111 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/rmcp-pack/ontology.ttl: 552 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/rust-dialect-pack/ontology.ttl: 510 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/rwr-level5-foundation-pack/ontology.ttl: 321 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/safe-ea-strategy-self-play-pack/ontology.ttl: 174 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/self-monitoring-pack/ontology.ttl: 209 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/shacl-projection-pack/ontology.ttl: 10 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/shacl-to-pydantic-pack/ontology.ttl: 8 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/speedrun-talent-network-pack/ontology.ttl: 181 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/standing-ladder-pack/ontology.ttl: 105 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/star-toml-pack/ontology.ttl: 97 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/tai-enterprise-rebuild-pack/ontology.ttl: 182 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/tcps-cli-pack/ontology.ttl: 39 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/tcps-core-pack/ontology.ttl: 246 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/tcps-ffi-pack/ontology.ttl: 17 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/tcps-release-pack/ontology.ttl: 383 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/tcps-std-pack/ontology.ttl: 20 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/tcps-wasm-pack/ontology.ttl: 6 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/temporary-works-pack/ontology.ttl: 44 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/typer-pack/ontology.ttl: 15 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/wasm4pm-algorithms-pack/ontology.ttl: 749 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/wasm4pm-breed-provenance-pack/ontology.ttl: 434 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/wasm4pm-cognition-pack/ontology.ttl: 352 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/wasm4pm-compat-pack/ontology.ttl: 47 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/wasm4pm-facts-pack/ontology.ttl: 1034 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/wasm4pm-interview-assist-pack/ontology.ttl: 2036 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
packs/wasm4pm-pack/ontology.ttl: 43 triples, E_DOD9_COLLISION=none, E_METAMODEL_HYGIENE_VIOLATION=none, E_AUTHORITY_ROOT_UNPINNED=none
```
