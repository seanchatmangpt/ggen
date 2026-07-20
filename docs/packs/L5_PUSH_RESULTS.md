# L5 Push — Implementation Results

Generated 2026-07-19. A 20-agent workflow (one per pack, real parallel implementation) pushed every pack against its own L5_VALIDATION_REPORT gaps: extending ontologies so templates project facts instead of hardcoding them, adding missing test coverage, wiring persisted verification, and honestly re-scoring afterward. This document is the compiled, INDEPENDENTLY VERIFIED result -- not a pass-through of the agents' own self-reports.

## What independent verification actually found

Every pack's changes were re-verified for real after the workflow completed (not accepted
as self-reported): all 6 packs wired in `examples/receiptctl` were rebuilt and re-tested together
(129 tests, 0 failures after fixes); the other 11 codegen packs were combined into one fresh
scratch consumer wiring all 11 simultaneously (187 tests, 0 failures after fixes); the 3
non-codegen packs (self-monitoring, dogfood-lifecycle, ma-case-study) were verified via their real
`crates/praxis-graphlaw` test suites (13 tests, 0 failures after fixes) plus real script execution.

**This verification pass found and fixed 6 real bugs the 20 parallel agents' own isolated
verification missed, because isolated single-pack scratch tests cannot see cross-pack
interactions:**

1. **Systemic FM-WRITE-008 collision across 11 packs.** Working independently, 11 different pack
   agents each invented the identical idea -- inject a `mod`/`pub use` wiring block into the
   shared consumer's `src/lib.rs` to close the "consumer effort" gap. Each verified fine in
   isolation (one pack, one scratch consumer). The moment two such packs are combined in one real
   consumer (confirmed live: `wasm4pm-cognition-pack` and `wasm4pm-facts-pack`, both wired in
   `examples/receiptctl`), this engine's own `FM-WRITE-008` duplicate-output guard correctly
   refuses the sync. Fixed by retargeting all 11 templates to their own uniquely-named file
   (`src/<pack>_lib_wiring.rs`); the consumer now needs one hand-written `include!(...)` line per
   pack -- a real, disclosed, smaller-than-before but nonzero consumer-effort gap, not the "consumer
   wires ggen.toml, done" several of these packs' own agents claimed.
2. **Stale mirrored fact, `wasm4pm-facts-pack`.** Its "verbatim copy" of `wasm4pm-algorithms-pack`'s
   `Algo_optimized_dfg`/`Algo_streaming_log` individuals still carried the pre-fix `"discover_dfg"`
   `wasmExport` value this session corrected in `wasm4pm-algorithms-pack` earlier -- the two mirrors
   had drifted. Once both packs are wired together (as `examples/receiptctl` does), the union
   graph's multi-valued property produced duplicate enum variants, a hard compile error. Fixed by
   re-applying the correction to the mirror.
3. **Pre-existing mirror drift, `Algo_playout.inputFormat`** ("xes" in `wasm4pm-algorithms-pack` vs.
   the semantically-correct "petri_net_handle" in `wasm4pm-facts-pack") -- not introduced this
   round, but only surfaced once real union-graph compilation was attempted. Fixed to the correct
   value.
4. **Consumer break from a widened generated signature.** `wasm4pm-compat-pack`'s
   `emit_receipt_chained` gained a new required `chain_hash: String` parameter (a real, deliberate
   ontology-expressiveness improvement). `examples/receiptctl`'s hand-written `handlers.rs` (a
   consumer file, correctly untouched by the pack agent) broke as a result. Fixed by computing a
   real (if non-cryptographic) hash of the receipt id in the handler.
5. **A hook removal broke a second test the removing agent didn't check.** `self-monitoring-pack`'s
   agent removed the original `smon:derive_escalation_obligation` hook when adding three
   severity-parameterized variants, reasoning correctly that keeping both would double-count in
   `self_monitoring_hook_actuation.rs`'s assertion -- but this broke a *different*, second
   pre-existing test (`self_monitoring_real_session_actuation.rs`) that depends on the original
   hook's exact name. Fixed by restoring the original hook and retargeting the three new variants
   to their own class (`smon:PrioritizedEscalationObligation`) instead of overloading
   `smon:EscalationObligation`.
6. **A self-referential comment, twice.** Two different templates (`mfw-pack`'s catalog proof, and
   this verification pass's own restoration note in `self-monitoring-pack/hook.ttl`) contained the
   literal text of a marker string their own file's naive substring-matching logic searches for --
   the exact same class of bug found and fixed three times already this session. Fixed both by
   rewording.

**One self-reported L5 claim was checked and rejected:** `mfw-pack`'s agent claimed `reached_l5:
true` on Consumer effort while its own `after_level` field said `L2` (an internally inconsistent
self-report), citing the exact `mod_wiring`-into-`lib.rs` mechanism that bug #1 above required
retargeting away from `lib.rs` specifically because it collides with 8 other packs doing the same
thing. Rejected -- **0 dimensions across all 140 cells are accepted as reaching L5 this pass**,
consistent with the L5_VALIDATION_REPORT's own finding.

## Real, verified test counts after fixes

| Consumer | Packs | Tests | Result |
|---|---|---|---|
| `examples/receiptctl` (real, committed) | clap-noun-verb, chicago-tdd-tools, wasm4pm-{compat,algorithms,cognition,facts} (6) | 129 | 0 failures |
| Combined scratch consumer (all 11 at once) | praxis-core, star-toml, lsp-max, cargo-cicd, mcpp, osx-clnr, affidavit, anti-llm-cheat-lsp, wasm4pm-pack, mfact, mfw (11) | 187 | 0 failures |
| `crates/praxis-graphlaw` real test suites | self-monitoring, ma-case-study (dogfood-lifecycle verified via shapes + shellcheck, no dedicated Rust suite) | 13 | 0 failures |

All three consumers re-synced a second time and confirmed byte-identical output (idempotent).

## Per-pack: what changed, honestly re-scored

For each pack: the real files changed (agent-reported, spot-checked above) and the honest before->after level per dimension, after independent verification. `after_level` reflects post-fix reality where this verification pass found and corrected an issue; otherwise it is the agent's own re-score, accepted because the underlying build/test evidence checked out.

### Codegen packs

#### clap-noun-verb-pack

Changes made:
- packs/clap-noun-verb-pack/ontology.ttl: added cnv:hasCommand (formal Noun->Command object property, replacing the implicit string-match-only relation) and cnv:staticResponse (a literal Rust expression a Command can carry to fully specify its behavior in the ontology); added new cnv:SessionPing individual using cnv:staticResponse and wired cnv:hasCommand triples for both existing nouns.
- packs/clap-noun-verb-pack/shapes.ttl (new file): SHACL NodeShapes for cnv:Command (non-empty noun/verb/handler/doc/returnType/args, optional single-valued staticResponse) and cnv:Noun (label/comment required, >=1 cnv:hasCommand) -- a real, checked structural constraint that did not exist before.
- packs/clap-noun-verb-pack/templates/clap_noun_verb_routes.rs.tmpl: (1) SPARQL commands query now OPTIONAL-binds ?static_response; (2) function body renders `Ok(<static_response>)` directly (no handler call) when cmd.static_response is set, closing the handler gap to genuinely zero for that command class; (3) added a real content-assertion proof test for cnv:SessionPing (asserts actual json field values, not just .is_ok()); (4) replaced the Tier-2 drift-canary's hand-transcribed EXPECTED_* literals/counts with `const EXPECTED_COMMAND_COUNT = {{ commands | length }}` etc., rendered by the same Tera loop/SPARQL projection that produces the routes -- removing the human-transcription step the audit named as gap (d).
- packs/clap-noun-verb-pack/pack.toml: updated description to document the zero-handler static-response mechanism, cnv:hasCommand, shapes.ttl, and template-derived (not hand-transcribed) proof literals.
- Scratch consumer built at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-clap-noun-verb-pack/ (Cargo.toml, ggen.toml, schema/domain.ttl, src/{lib,main,verbs/{mod,handlers}}.rs) mirroring examples/receiptctl's wiring pattern, used only to validate the changes above without touching the shared examples/receiptctl consumer (which is wired to 5 other packs other agents may be editing concurrently).

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | engine-level: generating the mod-wiring/crate-scaffolding into a consumer's own lib.rs/Cargo.toml is not a capability this pack's templates can safely add without either editing a shared example (exam... |
| Handler-gap size | L2 | L2 | The mechanism only covers commands with fixed, deterministic output. SessionLogin/SessionVerify/UserCreate still require hand-written handlers because their behavior depends on runtime input or extern... |
| Ontology expressiveness | L2 | L3 | Still no behavioral/effect semantics (pre/post-conditions, error taxonomy) for the 3 non-static commands, so a different template set could regenerate identical signatures but not an equivalent workin... |
| Consumer effort | L2 | L2 | engine-level: as in Generation depth -- auto-generating mod/lib wiring into a consumer requires either writing into a shared example (off-limits) or engine-level support this pack alone cannot add. |
| Test generation | L3 | L3 | Gap (b) (a real failure/error path per command) and gap (c) (exercising the actual #[verb]-macro CLI dispatch rather than direct fn calls) are both still open -- all 10 tests call generated wrapper fn... |
| Regeneration lifecycle | L2 | L2 | engine-level: wiring shapes.ttl invocation into the sync pipeline itself (so drift is refused automatically, not just detectable via a separate manual command) requires a change to ggen-engine's sync ... |
| Target-API fidelity | L2 | L2 | upstream-external: closing this gap requires either CI access to compile against the real, externally-versioned clap-noun-verb crate (a target project this repo does not own or control) or a machine-r... |

#### wasm4pm-compat-pack

Changes made:
- packs/wasm4pm-compat-pack/ontology.ttl: added a real per-EventType attribute schema (w4pm:Attribute class, w4pm:hasAttribute/w4pm:attrName/w4pm:attrType blank-node individuals: graph_hash/pack_hash/chain_hash, all xsd:string); replaced the string-literal w4pm:objectType property with a real IRI object property w4pm:hasObjectType pointing at the w4pm:ObjectType individuals (OntologyGraph/Pack/SyncReceipt); added w4pm:precedes sequencing facts stating the real emission order (GraphUnionHashed -> PackLockVerified -> ReceiptChained)
- packs/wasm4pm-compat-pack/templates/events.rs.tmpl: added an attribute_rows SPARQL query grouped via Tera's group_by filter to render each emit_* fn with real ontology-declared String attribute params and a non-empty `attributes: vec![OCELEventAttribute{...}]` (previously always Vec::new()); added an independent event_count SPARQL COUNT(DISTINCT ?et) query emitted as a new pub const EVENT_TYPE_COUNT; switched object-type derivation from a hardcoded string field to `row.object_type | split(pat="#") | last` against the new IRI-typed hasObjectType binding
- packs/wasm4pm-compat-pack/templates/emission_boundary.md.tmpl: added a 'Declared attributes' table (from the new attribute_rows query) and an 'Emission order' section (from a new sequence_rows query over w4pm:precedes), and updated object-type rendering to use the split filter against the new IRI binding
- packs/wasm4pm-compat-pack/templates/wasm4pm_compat_events_proof.rs.tmpl: updated all three emit_* call sites to pass the new ontology-declared attribute params and assert on real non-empty OCELEventAttribute values (name+value) instead of asserting attributes.is_empty(); added a new event_type_count_matches_independent_sparql_oracle test plus an assert_eq!(all.len(), EVENT_TYPE_COUNT) tying the exhaustive-match cardinality proof to the independent COUNT query instead of a purely hand-written '3'; extended the JSON round-trip assertions to check the new attribute key/value substrings
- packs/wasm4pm-compat-pack/pack.toml: updated the pack description to reflect the ontology-declared attribute schema and the independent-SPARQL-oracle cardinality test
- Built a scratch consumer at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-wasm4pm-compat-pack/ (own Cargo.toml with empty [workspace], own ggen.toml wiring only wasm4pm-compat-pack by absolute path, rust-toolchain.toml copied in) after confirming examples/receiptctl's full sync is currently broken by a concurrent agent's edit to clap-noun-verb-pack (reproduced via git stash on my own pack's changes: identical FM-TPL-017 failure with or without my edits, so it is not caused by this pack)

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | Generate the crate-level mod wiring and at least one real call site per emit_* fn (e.g. wired into an actual sync-pipeline hook), and move id/timestamp derivation into generated code -- none of which ... |
| Handler-gap size | L2 | L2 | Needs either ontology-encoded firing conditions (which pipeline stage triggers which event) wired into a real call site inside ggen-engine's/ggen-graph's sync pipeline, or a composed-pack mechanism ge... |
| Ontology expressiveness | L2 | L3 | L5 requires the ontology to be a complete behavioral specification (pre/post-conditions, real typed attribute values beyond string, error/failure-case individuals) such that a wholly different templat... |
| Consumer effort | L2 | L2 | Needs the pack (or a composed pack) to generate the mod declaration and a self-registering call site itself -- an auto-discovery mechanism analogous to clap-noun-verb's distributed-slice registry. Bui... |
| Test generation | L3 | L3 | L4 requires error-path coverage plus wiring into just pre-commit/CI; L5 requires the whole suite to be sufficient evidence with zero human review, which needs the literal event-name/objectType values ... |
| Regeneration lifecycle | L2 | L2 | Needs a freeze-slot or inject mechanism for any file a consumer might hand-edit, and an actual refuse-on-drift check (non-zero exit when on-disk content hash != what current ontology+templates would p... |
| Target-API fidelity | L2 | L2 | Needs either (a) this proof wired into CI so a target version bump is automatically re-verified (closing L2->L3 first, per the audit's own gap note), or (b) an actual ontology-to-ontology tracking rel... |

#### wasm4pm-algorithms-pack

Changes made:
- packs/wasm4pm-algorithms-pack/ontology.ttl: added pi:verifiedAgainst property definition, applied it to pi:Algo_ocel_dfg recording a dated (2026-07-18) hand-verification that its pi:wasmExport literal 'discover_ocel_dfg' resolves to a real pub fn in the pinned wasm4pm-compat@26.6.29 crate; added a header note honestly scoping this to 1-of-60 rows (the only wasmExport reachable from this workspace's real dependency graph)
- packs/wasm4pm-algorithms-pack/shapes.ttl (new file): SHACL NodeShape for pi:ProcessIntelligenceAlgorithm asserting cardinality (exactly-one for 9 required properties, optional-but-non-empty for pi:cliAlias), closed-enumeration sh:in constraints for pi:category (9 values), pi:outputType (7 values), pi:inputFormat (6 values), and numeric ranges for pi:speedTier [1,80]/pi:qualityTier [0,100] -- turns previously-implicit template assumptions into machine-checkable ontology facts
- packs/wasm4pm-algorithms-pack/templates/algorithms_catalog_proof.rs.tmpl: rewritten from a fully-static template (0 SPARQL bindings, 8 hand-transcribed spot-check rows, 2 hand-counted magic numbers) into a SPARQL-driven template with 2 bindings (`algorithms`, and a new `category_counts` GROUP BY/COUNT aggregate query) that generates full-field assert_eq! assertions for ALL 60 rows (not 8), derives the total count via sparql_count(rows=algorithms) instead of a literal '60', and derives all 9 per-category counts from a live GROUP BY aggregate instead of hand-counting ontology.ttl's section headers -- directly closes the 'full-field assertions... or an independently-derived second query path replacing the hand-transcribed literals' gap named in L5_VALIDATION_REPORT.md for this pack
- packs/wasm4pm-algorithms-pack/templates/algorithms_target_fidelity_proof.rs.tmpl (new file): SPARQL-driven template (queries pi:verifiedAgainst facts) generating tests/algorithms_target_fidelity_proof.rs, which actually calls the real, pinned wasm4pm_compat::dfg::discover_ocel_dfg on a constructed OCEL value (2 events sharing one object relationship) and asserts the resulting DFG has exactly the expected node/edge/frequency shape -- a real compiled+executed call against the target crate, not a signature-only or string-only check; includes an honest cardinality tripwire asserting only 1 row is currently verified this way

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | L5 requires 'the entire crate surface -- types, logic, tests, docs' to jointly precipitate. This pack still generates a data catalog + lookup, never real discovery/conformance logic (which correctly b... |
| Handler-gap size | L2 | L2 | Needs the catalog itself (not merely a test) to compose with another pack's generated invocation logic (e.g. a generated caller that dispatches AlgorithmId::OcelDfg to wasm4pm_compat::discover_ocel_df... |
| Ontology expressiveness | L2 | L2 | L5 requires the ontology to be 'a complete specification; a different template set could regenerate an equivalent system.' No pre/post-conditions, composability constraints, or behavioral contracts ex... |
| Consumer effort | L2 | L2 | blocked_reason: engine-level/shared-infra. lib.rs is a hand-maintained file shared across all 6 packs receiptctl wires together, actively being regenerated/edited by other agents working other packs' ... |
| Test generation | L3 | L3 | L5 requires passing the suite to 'certify the subsystem' with zero human review. This proof is still not wired into any CI/pre-commit gate (it lives in a scratch consumer this session, and in receiptc... |
| Regeneration lifecycle | L2 | L2 | blocked_reason: engine-level. No lock/receipt mechanism in this pack (or the shared ggen-engine sync path) actively refuses a sync when an output's on-disk hash diverges from its last receipted hash (... |
| Target-API fidelity | L1 | L2 | blocked_reason: upstream-external. Verifying the other 59 wasmExport literals requires either (a) a dependency on the `wasm4pm` crate itself, which this repo's own architecture rule forbids as a nativ... |

#### wasm4pm-cognition-pack

Changes made:
- packs/wasm4pm-cognition-pack/ontology.ttl: added compat:breedFamily property (+ its rdfs:domain/range declaration) and asserted it on all 55 compat:CognitionBreed individuals, turning the previously comment-only '# ---- X family ----' section groupings into a real, queryable RDF fact per breed
- packs/wasm4pm-cognition-pack/templates/cognition_catalog.rs.tmpl: SPARQL query and generated CognitionBreedInfo struct/BREED_CATALOG now carry a 'family' field sourced from compat:breedFamily; added a generated breed_families() helper; frontmatter set to force:true (was implicit force-less, causing a silent-clobber refusal on content change)
- packs/wasm4pm-cognition-pack/templates/cognition_catalog_docs.md.tmpl (new): generates docs/generated/W4PM_COGNITION_BREED_CATALOG.md, a full 55-row markdown table (family/id/label/doc) plus a citations section -- closes the 'no docs output whatsoever' generation-depth gap
- packs/wasm4pm-cognition-pack/templates/cognition_dispatch_handler.rs.tmpl (new): generates src/w4pm_cognition_dispatch_handler.rs with unless_exists:true -- a freeze slot for the single hand-completable dispatch_cognition_run body that a later ggen sync never overwrites
- packs/wasm4pm-cognition-pack/templates/cognition_dispatch.rs.tmpl: split so it now only emits the always-regenerated (force:true) per-breed run_X wrapper functions, re-exporting dispatch_cognition_run from the new handler module instead of defining it inline -- adding breed #56 now regenerates this file safely without touching the hand-completed handler
- packs/wasm4pm-cognition-pack/templates/cognition_mod_wiring.rs.tmpl (new): inject:true template that adds 'pub mod w4pm_cognition_dispatch_handler;' into the consumer's src/lib.rs after the '// GENERATED catalogs/emission surfaces' marker, skip_if-guarded against duplicate injection -- reduces (does not eliminate) hand-wiring consumer effort
- packs/wasm4pm-cognition-pack/templates/cognition_proof_full.rs.tmpl (new): generates tests/w4pm_cognition_proof_full.rs, a SPARQL-driven round-trip proof asserting all 55 breeds (not the 8-row hand-transcribed spot sample in the pre-existing proof file) against label/citation/as_str/from_breed_id
- packs/wasm4pm-cognition-pack/templates/w4pm_cognition_proof.rs.tmpl: updated #[path] mod includes to also compile the new dispatch_handler.rs module (needed after the dispatch/handler split)
- packs/wasm4pm-cognition-pack/pack.toml: description updated to list all 7 generated artifacts and the freeze-slot/full-proof/mod-wiring additions

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L3 | upstream-external: real cognition-breed algorithms are wasm4pm's owned domain (see CLAUDE.md Process Intelligence Boundary table) -- this repo does not own or vendor that logic, and fabricating 55 alg... |
| Handler-gap size | L2 | L2 | upstream-external: the only real behavior here is calling into wasm4pm's own cognition_run ABI, a crate this workspace does not vendor or own the source of within this pack's authority. |
| Ontology expressiveness | L2 | L2 | upstream-external: encoding each algorithm's actual behavioral contract in RDF would require formalizing 55 distinct AI algorithms (STRIPS, CDCL SAT, POMDP, etc.) from first principles, which is wasm4... |
| Consumer effort | L2 | L2 | engine-level: fully eliminating consumer mod-wiring for all three files, plus auto-generating the Cargo.toml dependency block, would need either extending every existing template's inject scope (risk ... |
| Test generation | L3 | L4 | Documented honestly in the new test file's own header: because both templates share the same `breeds` SPARQL query and ORDER BY clause, a bug shared by both queries (e.g. a wrong ORDER BY copy-pasted ... |
| Regeneration lifecycle | L2 | L3 | engine-level: semantic-diff-on-regen output is a `ggen-engine` sync-pipeline feature (crates/ggen-engine/src/sync.rs's write-decision reporting), not something a single pack's templates can add -- cha... |
| Target-API fidelity | L1 | L1 | upstream-external: the wasm4pm-cognition crate (the actual target being tracked) is not present as a buildable/importable Rust crate in this environment -- only its parent project's paper/doc artifact... |

#### wasm4pm-facts-pack

Changes made:
- packs/wasm4pm-facts-pack/ontology.ttl: fixed 2 real upstream-drift facts found by a new diff script (pi:Algo_optimized_dfg and pi:Algo_streaming_log's pi:wasmExport corrected from diverged values to match upstream wasm4pm exactly)
- packs/wasm4pm-facts-pack/shapes.ttl (new): SHACL NodeShapes for compat:CognitionBreed and pi:ProcessIntelligenceAlgorithm with real minCount/maxCount/datatype/sh:in cardinality and range constraints, verified both to conform against real ontology.ttl and to reject a sabotaged individual (non-vacuous)
- packs/wasm4pm-facts-pack/ontology/rules/breed_standing.n3 (new): bundled copy of the previously-unshipped law rule this pack's own docs referenced but never included
- packs/wasm4pm-facts-pack/scripts/check_upstream_drift.sh (new): runnable per-individual diff against a live wasm4pm checkout; found and the pass fixed 2/60 real algorithm drifts, now reports 0 drift
- packs/wasm4pm-facts-pack/DRIFT_LOG.md (new): dated record of the drift check run and the 2 corrections made, with the upstream commit hash pinned
- packs/wasm4pm-facts-pack/WIRING.md (new): exact consumer [law] table wiring instructions, plus the discovered engine constraint that [law].rules paths must be relative with no '..' traversal (so a consumer must locally copy the rule file, not reference the pack's copy in place)
- packs/wasm4pm-facts-pack/templates/registry_catalog.rs.tmpl (new): generates src/wasm4pm_facts_registry.rs -- real compilable typed Rust (Breed/Algorithm structs, BREEDS/ALGORITHMS const tables, lookup_breed/lookup_algorithm/algorithms_by_category functions) via two independently-authored SPARQL SELECTs distinct from the markdown template's own query
- packs/wasm4pm-facts-pack/templates/wasm4pm_facts_mod_wiring.rs.tmpl (new): inject-mode template that appends 'mod wasm4pm_facts_registry;' into the consumer's existing src/lib.rs (skip_if idempotent)
- packs/wasm4pm-facts-pack/templates/full_coverage_proof.rs.tmpl (new): mechanical proof covering all 115 individuals (not 8 hand-picked) cross-checked between the registry catalog module and the rendered markdown, plus real behavioral tests of lookup_breed/lookup_algorithm/algorithms_by_category, a no-duplicate-ids invariant check, and a law-derivation internal-consistency test (all-ADMITTED xor all-EvidenceBound)
- packs/wasm4pm-facts-pack/templates/registry_artifact_proof.rs.tmpl: the 4 hand-picked breed spot-checks (eliza/mycin/strips/soar) now accept either the ADMITTED placeholder or the EvidenceBound law-derived value via a shared helper, instead of hardcoding ADMITTED -- fixes a regression the law-wiring activation would otherwise have caused, and closes part of the 'silently substituting a placeholder' gap
- packs/wasm4pm-facts-pack/pack.toml: version bump + description rewritten to document all 6 new/changed artifacts and the [law] wiring requirement
- scratch consumer at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-wasm4pm-facts-pack/ (own Cargo.toml with empty [workspace], own ggen.toml, copied rust-toolchain.toml, local copy of the law rule) used to verify sync/build/test in both law-wired and law-unwired states -- no files outside packs/wasm4pm-facts-pack and this scratch dir were modified

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L3 | L5 requires the ENTIRE crate surface -- not just this one added module -- to precipitate from RDF. The consumer's Cargo.toml, its own domain logic, its binary/main.rs, and most of its documentation ar... |
| Handler-gap size | L1 | L2 | The lookup/category functions are pure data accessors, not behavior specified as ontology-level contracts (there is no RDF fact saying 'lookup must return the individual with this id' -- that's still ... |
| Ontology expressiveness | L2 | L3 | L5 requires the ontology to be a complete specification such that a DIFFERENT template set could regenerate an EQUIVALENT WORKING SYSTEM -- this pack's ontology still only carries flat per-individual ... |
| Consumer effort | L2 | L2 | engine-level |
| Test generation | L3 | L4 | L5 requires the proof to be 'sufficient evidence on its own -- passing it certifies the subsystem' with no human in the loop at all. This pack's own regenerated test file (full_coverage_proof.rs.tmpl)... |
| Regeneration lifecycle | L2 | L2 | engine-level |
| Target-API fidelity | L2 | L3 | L5 requires fidelity to be DEFINITIONAL (structurally impossible to drift, because the pack tracks the target's own ontology rather than its API/data snapshot) -- here fidelity is still CHECKED, by a ... |

#### chicago-tdd-tools-pack

Changes made:
- packs/chicago-tdd-tools-pack/templates/cli_boundary_proof.rs.tmpl: added `sparql: tests` frontmatter (the SAME query cli_boundary_tests.rs.tmpl runs) so `EXPECTED`/`EXPECTED_TEST_COUNT` are now Tera-rendered from the live ontology query instead of a hand-transcribed Rust array with a hardcoded `3` -- this was the audit's named Test-generation/Regeneration-lifecycle gap (a 4th ontology individual used to require a second hand-edit inside the .tmpl; it no longer does).
- packs/chicago-tdd-tools-pack/ontology.ttl: added `ctt:composesCommand` property (rdfs-documented) and a 4th individual `ctt:receiptctl-algorithm-list` that composes its argv from a cross-pack `cnv:Command` (noun+verb) instead of a hand-typed `ctt:args` literal -- partial closure of the Handler-gap/Ontology-expressiveness axes (invocation shape now derived from another pack's generated command surface; exit code/needle remain this pack's own observed axiom).
- packs/chicago-tdd-tools-pack/templates/cli_boundary_tests.rs.tmpl: SPARQL query changed to `UNION` between literal `ctt:args` and a `?cmd cnv:noun/cnv:verb` join with `BIND(CONCAT(?noun," ",?verb) AS ?args)`, so both literal-args and command-composed tests render through one query.
- packs/chicago-tdd-tools-pack/templates/cli_boundary_doc.md.tmpl: same UNION query applied so the generated doc table also reflects composed-argv rows, not just literal ones.
- Verified consumer wiring intact: examples/receiptctl/ggen.toml already wires this pack; did not modify examples/receiptctl or any other pack's files (clap-noun-verb-pack was mid-edit by a concurrent agent -- confirmed via `git stash`/`git diff stash@{0}` round-trip that its working tree was restored byte-for-byte, then the stash was dropped).

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | L5 requires a template that generates the actual subsystem under test (e.g. a reusable in-process handler/library this pack's own tests call), not just externally-observed CLI-boundary assertions abou... |
| Handler-gap size | L2 | L3 | Only the invocation SHAPE (noun+verb) is composed; the expected exit code and stdout/stderr needles are still this pack's own hand-observed axiom, because clap-noun-verb-pack's ontology carries no suc... |
| Ontology expressiveness | L2 | L3 | Still no modeling of clap-noun-verb's error taxonomy, exit-code semantics, or pre/post-conditions as structured relations -- 3 of 4 individuals are still flat literal fixture rows. L5 ('a different te... |
| Consumer effort | L2 | L2 | Closing this requires publishing the `cli-proof` feature of the `chicago-tdd-tools` crate to crates.io -- an external, upstream publishing action this repo/pack cannot perform from inside a file edit.... |
| Test generation | L3 | L4 | Coverage is still only 4 narrow axioms, not the full receiptctl command surface, and the proof still isn't wired into any CI/pre-commit gate (that gate lives in the shared justfile/`just pre-commit` r... |
| Regeneration lifecycle | L2 | L4 | L5 ('drift is impossible by construction, lock + receipt refuse it') requires an active lock/receipt check that would FAIL a build if the two templates' queries ever diverged from each other -- today ... |
| Target-API fidelity | L2 | L2 | L5 requires axioms derivable from the target's own ontology/schema rather than observed against one running binary. That requires clap-noun-verb-pack (or receiptctl's own domain.ttl) to expose a forma... |

#### praxis-core-pack

Changes made:
- packs/praxis-core-pack/ontology.ttl: rewrote from 4/13 RefusalScenario coverage to full 13/13 (all variants in crates/praxis-core/src/refusal.rs, verified 2026-07-18): added pxc:RefusalCategory as a closed 8-individual enumeration (pxc:CategoryIdentity...CategoryReserved) with pxc:categoryLabel; replaced the string-valued pxc:categoryName property with an object property pxc:hasCategory pointing at those individuals; added pxc:variantKind ('unit'/'struct') and pxc:hasField/pxc:fieldType for the 6 struct (data-carrying) variants, encoding structural/error-case shape the templates now only project.
- packs/praxis-core-pack/templates/refusal_taxonomy_rs.tmpl: SPARQL query rewritten to join through pxc:hasCategory -> pxc:categoryLabel (thin projection, no hardcoded category strings); added variant_kind field to the generated struct; added two new generated functions with real control flow, category_for_scenario() and denial_lane_for_scenario(), both driven purely by the generated table.
- packs/praxis-core-pack/templates/refusal_taxonomy_md.tmpl: same query rewrite; added a Kind column to the generated markdown table.
- packs/praxis-core-pack/templates/refusal_taxonomy_mod_wiring.rs.tmpl: NEW template using the engine's real inject/after/skip_if frontmatter to auto-append 'mod praxis_core_refusal_table;' + 'pub use ...' into the consumer's src/lib.rs after a documented marker comment ('// ggen:praxis-core-pack:mod-wiring'), removing the hand-written mod-declaration step named as the consumer-effort gap (consumer still adds the one-line marker once, so not zero-touch).
- packs/praxis-core-pack/templates/refusal_taxonomy_proof.rs.tmpl: expanded from 4 hand-transcribed rows / 5 tests to all 13 rows / 8 tests: row-order check now covers all 13, a new test exercises the two generated lookup functions structurally, a new error-path test (unknown scenario name), and a NEW live-drift test that include_str!'s the real crates/praxis-core/src/refusal.rs (absolute path, this checkout) and counts its actual RefusalScenario::category() match arms, asserting the count equals this pack's generated row count -- an automated check against the real upstream source, not a second hand-transcription.

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | Still a reference-table-plus-lookup artifact, not the actual RefusalScenario enum type or its compose_denials/From<&Obligation> logic -- those live in crates/praxis-core/src/refusal.rs, a shared crate... |
| Handler-gap size | L1 | L2 | The actual behavior a consumer would want closed (RefusalScenario-to-category matching inside praxis_core::refusal itself, composition, Obligation conversion) is still 100% hand-written in crates/prax... |
| Ontology expressiveness | L2 | L3 | No behavior contracts (pre/post-conditions, state transitions) are encoded -- e.g. nothing in the ontology states 'compose_denials folds via OR, identity ADMITTED' or 'a RefusalScenario is produced ex... |
| Consumer effort | L2 | L2 | The consumer must still hand-write one marker comment ('// ggen:praxis-core-pack:mod-wiring') in src/lib.rs before the first sync, because inject mode hard-errors if the target file or marker is missi... |
| Test generation | L3 | L3 | L5 requires the passing suite alone to certify the subsystem. Since Generation depth is still L2 (no real subsystem generated, only a reference table + trivial lookup fns), a passing suite here certif... |
| Regeneration lifecycle | L2 | L3 | L4 requires the consumer to see a semantic diff + receipt with zero manual repair on upstream change; today an ontology/refusal.rs divergence is caught only by running `cargo test` (a human-invoked st... |
| Target-API fidelity | L1 | L2 | L3 requires generated output compiled against the pinned target crate in the pack's own CI proof -- the live-drift test reads refusal.rs as literal text (include_str!) and counts match-arm lines; it d... |

#### star-toml-pack

Changes made:
- packs/star-toml-pack/ontology.ttl: added stp:closedWorld, stp:required, stp:loaderFn, stp:cargoDependency properties and a stp:StarTomlModule/stp:TheModule individual; annotated both existing sections with closedWorld=true/loaderFn=star_toml::load_file, all 4 existing fields with required=true, and added a new stp:RetryCountField (retry_count, required=false) to prove optionality actually changes generated shape
- packs/star-toml-pack/templates/star_toml_config.rs.tmpl: deny_unknown_fields, the field-required/Option<T> wrapping, and the load() delegation target are now projected from stp:closedWorld/stp:required/stp:loaderFn instead of hardcoded; added freeze_policy: checksum + freeze_slots_dir (replacing implicit force:false-by-omission with an explicit, verified checksum-based drift guard)
- packs/star-toml-pack/templates/star_toml_config_proof.rs.tmpl: replaced force: true with freeze_policy: checksum; added retry_count to the structural literal and load-round-trip fixture; added a new optional_field_omitted_deserializes_as_none test proving stp:required=false reshapes the struct, not just documents an assumption
- packs/star-toml-pack/templates/star_toml_section_doc.md.tmpl: doc tables now render closedWorld/loaderFn/required from the ontology instead of only fieldName/rustType/doc
- packs/star-toml-pack/templates/star_toml_cargo_deps_doc.md.tmpl: new template generating docs/star_toml/CARGO_DEPENDENCIES.md from stp:cargoDependency facts (serde/star-toml/tempfile entries), so the exact Cargo.toml snippet a consumer needs is ontology-derived
- packs/star-toml-pack/templates/star_toml_mod_wiring.rs.tmpl: new inject-mode template that adds `pub mod star_toml_config;` to an existing src/lib.rs at a `// GGEN-INJECT: mod-root` marker, idempotent via skip_if
- packs/star-toml-pack/TARGET_API_FIDELITY.md: new dated (2026-07-18) hand-verification note comparing the generated load() delegation against the real star_toml::load_file signature at /Users/sac/star-toml/src/loader.rs:389 (crate v26.7.3), with an explicit statement of what it does/doesn't prove (L2, not L5)
- packs/star-toml-pack/pack.toml: version bumped 0.1.0 -> 0.2.0, description rewritten to describe the new ontology-projected behavior, mod-wiring, Cargo-deps doc, and freeze_policy
- Found and fixed 2 real template bugs discovered only by actually running sync in the scratch consumer: (1) SPARQL-bound xsd:boolean values compared as `f.required == "true"` always evaluated false (Tera binds them as native bools, not strings) -- fixed to `{% if f.required %}`; (2) the Cargo-deps template looped over SPARQL result rows printing `{{ d }}` (the whole row object, rendering literally as `[object]`) instead of `{{ d.dep }}` -- fixed

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L3 | L3 | L5 requires the entire crate surface (types, logic, tests, docs) to precipitate from RDF. Reaching further would need generating a full consumer crate scaffold (a real Cargo.toml, not just a doc descr... |
| Handler-gap size | L4 | L4 | L5 requires zero handler gap specifically because behavior is ontology-specified or composed from another pack's generated logic. The admission *policy itself* (that closed-world + a named loader func... |
| Ontology expressiveness | L2 | L3 | L5 requires the ontology to be a complete specification such that 'a different template set could regenerate an equivalent system.' No second, independently-written template set was built or attempted... |
| Consumer effort | L2 | L2 | L5 is 'consumer wires ggen.toml, done' -- the consumer must still hand-copy CARGO_DEPENDENCIES.md's lines into their own Cargo.toml; nothing merges them automatically, and the mod-wiring template requ... |
| Test generation | L3 | L3 | L5 requires the proof suite to be sufficient evidence on its own with no human in the loop, checkable from repo state. This run only happened in a throwaway scratch consumer outside the repo -- there ... |
| Regeneration lifecycle | L2 | L3 | L5 requires drift to be impossible by construction with regen as the only maintenance verb. Freeze-checksum only refuses on the specific file it's attached to and requires the pack author to remember ... |
| Target-API fidelity | L1 | L2 | upstream-external: closing this dimension past a hand-verified, dated check requires the star-toml crate (owned at /Users/sac/star-toml, outside this repo) to itself expose a machine-readable API/onto... |

#### lsp-max-pack

Changes made:
- packs/lsp-max-pack/ontology.ttl: added lm:pathGlobs, lm:excludeGlobs, lm:evalBudget as real RDF properties (rdf:Property with rdfs:domain lm:LintRule) on all 3 lm:LintRule individuals, populated with values verified against the real target struct ~/lsp-max/src/rule_pack_server.rs::Rule and its EvalBudget enum (#[serde(rename_all="snake_case")] -> "sync"/"background"); these fields previously existed only as hardcoded template literals
- packs/lsp-max-pack/ontology.ttl: corrected a factual error -- header comment claimed the target uses an 'AhoCorasick rule engine'; verified by reading ~/lsp-max/src/rule_pack_server.rs directly that evaluate() uses regex::Regex::find_iter with zero AhoCorasick references; comment rewritten with the correction and citation
- packs/lsp-max-pack/templates/rule_pack.toml.tmpl: SPARQL query extended to select ?pathGlobs ?excludeGlobs ?evalBudget; template body now renders path_globs/exclude_globs via Tera `split(pat=" ")` loops instead of hardcoded ["src/**", "crates/**"] / ["target/**", ".git/**"] literals, and emits eval_budget = "{{ evalBudget }}"; header comment corrected (AhoCorasick -> real regex::Regex::find_iter, cited)
- packs/lsp-max-pack/templates/rule_pack_proof.rs.tmpl: ParsedRule/Expected structs gained an eval_budget field; all 3 EXPECTED_RULES entries populated with eval_budget; comparison test now asserts eval_budget equality; added a fidelity-note doc comment citing the verified real Rule struct and explaining the honest boundary (no path/git dependency on ~/lsp-max, so this is a structural-echo proof, not an import-backed one)
- packs/lsp-max-pack/pack.toml: description updated to state the full ontology-sourced field list and the 2026-07-18 fidelity verification against rule_pack_server.rs::Rule
- Built a scratch consumer at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-lsp-max-pack (Cargo.toml with empty [workspace], ggen.toml wiring lsp-max-pack via absolute path, rust-toolchain.toml copied) since this pack is not wired in examples/receiptctl

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | upstream-external: the behavioral engine this pack's output feeds lives in ~/lsp-max (a separate, externally-maintained repo this workspace does not own or vendor); generating a real substitute for it... |
| Handler-gap size | L2 | L2 | upstream-external: same external-crate boundary as Generation depth -- the RulePackServer engine is out-of-repo and not something this pack can safely replace or extend without owning that codebase. |
| Ontology expressiveness | L2 | L3 | The ontology still only encodes 3 concrete rule individuals and one pack; it doesn't express pack composition/versioning semantics (ComposedPacks/PackConflict from the real engine), nor does it model ... |
| Consumer effort | L2 | L2 | upstream-external: generating a real, compiling RulePackServer bootstrap requires a live dependency on ~/lsp-max, a large external crate (tower-lsp-max, wasm4pm-adjacent deps) this repo does not vendo... |
| Test generation | L3 | L3 | upstream-external: a genuinely non-tautological, engine-exercising proof requires a live build dependency on ~/lsp-max; verifying that dependency compiles and behaves as expected is a nontrivial integ... |
| Regeneration lifecycle | L2 | L2 | engine-level: a genuinely independent second derivation path (e.g. a SHACL shape or a separate SPARQL-driven code path that cross-validates against the primary query without re-deriving the same tauto... |
| Target-API fidelity | L1 | L2 | upstream-external: making fidelity structurally guaranteed rather than hand-checked requires either vendoring ~/lsp-max's type definitions (schema import) or a live dependency enabling compile-time st... |

#### cargo-cicd-pack

Changes made:
- packs/cargo-cicd-pack/ontology.ttl: rewrote with a dated (2026-07-18/19) source audit against the real ~/cargo-cicd v26.7.6 checkout (src/nouns/*.rs), replacing the prior README-only transcription. Corrected a real bug found by the audit: `doctor` is NOT a bare noun with an empty verb -- src/nouns/doctor.rs defines three real verbs (repo/evidence/diff) via #[verb(...)] -- added cc:doctor-repo/evidence/diff and kept cc:doctor-bare only as an explicitly-marked deprecated compatibility row. Added 13 previously-missing nouns found only in source (trybuild, pipeline, sbom, hooks, verify, certification, ocel, receipt, trace, standing, gate, release_gate, claude_context; 23 new verb rows), each carrying a new cc:sourceFile triple citing the exact src/nouns/*.rs file it was verified against. Cross-checked the target project's OWN authoritative machine ontology (~/cargo-cicd/ontology/public/cargo-cicd-capabilities.ttl, a real SKOS/PROV-O ontology it ships) and added cc:targetOntologyConcept triples linking 9 rows to that ontology's own IRIs (StatusShow, TargetShow, TargetPrune, TestChanged, TrybuildChanged, GitStatus, GitClose, PublishRun, WorkspaceDoctor). Total commands: 52 (up from 26).
- packs/cargo-cicd-pack/templates/cargo_cicd_catalog.rs.tmpl: added a source_file field (SPARQL-derived from the new cc:sourceFile triples) to CargoCicdCommand, and four new real callable functions over the generated table -- find_command, commands_for_noun, is_valid_command, distinct_nouns -- replacing the pure-data-only const table with genuine compiled lookup/validation logic.
- packs/cargo-cicd-pack/templates/cargo_cicd_reference.md.tmpl: added a Source column (from cc:sourceFile) to the generated reference table so every row cites the exact upstream file it was audited against.
- packs/cargo-cicd-pack/templates/cargo_cicd_proof.rs.tmpl: rewrote the proof so per-row expected values (doc, source_file, row count) are rendered directly from the SAME SPARQL query that generates the catalog, instead of hand-transcribed literals -- this is the fix the L5 audit named verbatim for both Test generation and Regeneration lifecycle (drift is no longer possible via a stale hand-copied literal). Kept genuinely independent structural checks (no-duplicate-pairs, sortedness, distinct_nouns invariants, the doctor-migration shape) and added two new negative-path tests (is_valid_command/find_command on unknown noun/verb pairs).
- packs/cargo-cicd-pack/templates/cargo_cicd_mod_wiring.rs.tmpl: new template, inject:true + after:/skip_if: against the consumer's src/lib.rs (same pattern as praxis-core-pack), auto-declaring `mod cargo_cicd_catalog;` and re-exporting its public functions -- closes the exact Consumer-effort gap named in the audit (no more hand-added mod declaration after sync).
- Fixed a real E0700 lifetime-capture compile error in commands_for_noun (impl Iterator<...> needed an explicit `+ '_`) that a real `cargo test` run in the scratch consumer surfaced -- not caught by ggen graph validate or ggen sync run alone.

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | No generated logic shells out to or reimplements any real cargo-cicd behavior (status computation, git state, publish gating, etc.) -- L3 requires a compilable module that performs the target's actual... |
| Handler-gap size | L1 | L1 | upstream-external: closing this gap requires either shelling out to the real cargo-cicd binary (not installed on this machine, cargo install cargo-cicd was not run since that would require network/pub... |
| Ontology expressiveness | L1 | L1 | No argument types, cardinality, error/exit-code semantics, or pre/post-conditions are modeled for any command -- L2's 'facts + call metadata (names, args, types)' bar is not yet cleared, let alone L3'... |
| Consumer effort | L2 | L3 | The consumer still had to place a one-time `// ggen:cargo-cicd-pack:mod-wiring` anchor comment in their own lib.rs before the first sync -- L5's bar ('Consumer wires ggen.toml. Done.') requires even t... |
| Test generation | L3 | L4 | The suite only certifies the generated catalog/lookup surface against itself -- it says nothing about whether ontology.ttl correctly describes real cargo-cicd behavior, because no generated behavior w... |
| Regeneration lifecycle | L2 | L3 | L4 requires the consumer to see a semantic diff + receipt with zero manual repair on ontology change; today drift is caught (not silently allowed) but repair is a manual `rm ggen.lock` + re-sync, and ... |
| Target-API fidelity | L1 | L2 | L3 requires generated output to compile against the pinned target crate in this pack's own CI -- cargo-cicd is not installed on this machine and not wired as a pinned dependency anywhere in this pack,... |

#### mcpp-pack

Changes made:
- packs/mcpp-pack/ontology.ttl — full rewrite: added a dated (2026-07-18), file:line-cited hand-verification pass against ~/mcpp's real #[verb] source at pinned commit ae1106887a824f107dc8ac76daff6ef1b8dbf6c4; fixed a real fidelity bug (mcp:relay-verify, verb "verify", renamed to mcp:relay-verify-chain, verb "verify-chain" — the real fn is `verify_chain`, confirmed at crates/mcpp-server/src/cmds/relay.rs:331; the README-grepped original named a command that does not exist in real mcpp); added cnv:args structural facts (real argument name|type|required|about, reused verbatim from clap-noun-verb-pack's encoding) to all 9 commands, each individually verified against its real function signature
- packs/mcpp-pack/templates/mcpp_catalog.rs.tmpl — added `args: &'static str` field + SPARQL ?args projection; switched force:true to freeze_policy: checksum / freeze_slots_dir: .ggen/freeze/mcpp-pack
- packs/mcpp-pack/templates/mcpp_reference.md.tmpl — added an Args column driven by the same cnv:args facts; switched to freeze_policy: checksum
- packs/mcpp-pack/templates/mcpp_catalog_proof.rs.tmpl — updated spot-checks for the new args field and the relay-verify -> relay-verify-chain rename; added a new invariant test that every args string parses into well-formed field|type|required|about groups; switched to freeze_policy: checksum
- packs/mcpp-pack/templates/mcpp_dispatch.rs.tmpl — new template generating src/mcpp_dispatch.rs: a real, compilable, ontology-driven std::process::Command subprocess-dispatch shim (build_command/dispatch generic fns plus one generated per-command wrapper fn per catalog row, snake_case-derived names) — closes the 'no compilable logic module at all' gap named in the audit
- packs/mcpp-pack/templates/mcpp_dispatch_proof.rs.tmpl — new template generating tests/mcpp_dispatch_proof.rs: a real behavior proof introspecting the actual std::process::Command built by build_command/each generated wrapper via Command::get_program()/get_args() (hermetic, no mcpp binary required to run), not a hand-copied data mirror
- packs/mcpp-pack/templates/mcpp_mod_wiring.rs.tmpl — new inject:true template that appends `pub mod mcpp_catalog;`/`pub mod mcpp_dispatch;` (with #[path] attrs) into the consumer's src/lib.rs after a `// GENERATED catalogs/emission surfaces` marker line, with skip_if guarding re-injection — closes the 'consumer must hand-edit mod declarations' gap
- packs/mcpp-pack/pack.toml — rewrote description to document the verification pass, the new dispatch/proof/inject templates, and the freeze-policy change
- scratch consumer built at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-mcpp-pack/ (Cargo.toml with empty [workspace], ggen.toml wiring only mcpp-pack via absolute path, rust-toolchain.toml copied from repo root, minimal schema/empty.ttl, src/lib.rs with the inject marker comment) — used only to run and verify sync+build+test; not part of the repo

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L3 | The behavior each dispatch wrapper performs is still 'shell out to the real mcpp binary' — none of mcpp's own command semantics (what aat run/verify actually compute) is expressed in RDF or reproduced... |
| Handler-gap size | L1 | L2 | L5 requires the entry points' behavior itself to be specified in the ontology or composed from another pack's generated logic. Here the behavior still lives entirely in the external mcpp binary (invok... |
| Ontology expressiveness | L1 | L2 | The L3 bar named by the audit requires BOTH argument facts AND error-case facts; only argument facts were added, error/failure-mode facts are still absent. L4 (behavioral contracts: pre/post-condition... |
| Consumer effort | L2 | L3 | L5 ('Consumer wires ggen.toml. Done.') is not reached: the consumer still had to hand-author Cargo.toml, main.rs, rust-toolchain.toml, and a src/lib.rs stub containing the exact `// GENERATED catalogs... |
| Test generation | L3 | L3 | Held at L3, not escalated to L4/L5: mcpp_catalog_proof.rs is still hand-transcribed literals (unchanged ceiling from the original audit finding); mcpp_dispatch_proof.rs covers no error paths (e.g. mcp... |
| Regeneration lifecycle | L2 | L3 | The freeze skip does not fail the sync (exit code stayed 0) and there is no semantic diff shown to the consumer (L4), nor does it force reconciliation when the ontology itself changes underneath a fro... |
| Target-API fidelity | L1 | L2 | upstream-external: mcpp (~/mcpp) is an external project this repo does not own and does not itself publish or expose any formal ontology; L5 fidelity requires such a target-side ontology to exist to t... |

#### osx-clnr-pack

Changes made:
- packs/osx-clnr-pack/ontology.ttl: added oclnr:nextStage state-transition edges across all 5 Stage individuals (Audit->Plan->Review->Delete->Receipt); added oclnr:Argument class + oclnr:hasArgument relation with argName/argType/argRequired/argDefault/argPosition for all 5 catalogued commands (19 Argument individuals total), source-verified 2026-07-18 directly against ~/osx-clnr/src/nouns/{snapshot,receipt,emergency}.rs's real clap Subcommand enums; added oclnr:ByteUnit class (5 individuals: b/kb/mb/gb/tb with real SI multipliers 1/1e3/1e6/1e9/1e12) and oclnr:SnapshotSelectionPolicy (dateSuffixLength=17), transcribed line-by-line from ~/osx-clnr/src/domain/time.rs; corrected snapshot-delete and receipt-verify rdfs:comment text to match real source (previously paraphrased, now verbatim-accurate to the real Delete/Verify variant docs)
- packs/osx-clnr-pack/templates/osx_clnr_catalog_proof.rs.tmpl: updated 2 hand-transcribed literal assertions (command_snapshot_delete_deletes_oldest_or_all, command_receipt_verify_checks_measured_reclaim) to match the corrected, source-verified ontology doc strings
- packs/osx-clnr-pack/templates/osx_clnr_plan.rs.tmpl (new): generates src/osx_clnr_plan.rs, a real compilable Rust module reimplementing ~/osx-clnr's parse_size_in_bytes/parse_snapshot_date/select_oldest_snapshots algorithm; the BYTE_UNITS table and SNAPSHOT_DATE_SUFFIX_LEN constant are SPARQL-derived from the new ontology facts (not hardcoded in the template)
- packs/osx-clnr-pack/templates/osx_clnr_plan_proof.rs.tmpl (new): generates tests/osx_clnr_plan_proof.rs, 14 Chicago-TDD tests against the real generated logic in osx_clnr_plan.rs -- 1 SPARQL-derived cardinality assertion (byte-unit count) plus 13 behavioral tests including 6 adversarial/error-path cases (empty size string, unknown unit suffix, non-numeric value, unparsable snapshot names, n=0, n larger than available)
- packs/osx-clnr-pack/templates/osx_clnr_mod_wiring.rs.tmpl (new): inject:true/skip_if template that auto-wires 'pub mod osx_clnr_catalog;'/'pub mod osx_clnr_plan;' into a consumer's src/lib.rs at a one-time '// GGEN-OSX-CLNR-MOD-ANCHOR' marker, idempotently (verified: second sync run correctly reports 'skipped: skip_if')
- packs/osx-clnr-pack/pack.toml: description rewritten to document the above -- the dated 2026-07-18 source-verification pass, the real generated logic module, the SPARQL-driven unit table, and the mod-wiring template
- Scratch consumer built at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-osx-clnr-pack/ (Cargo.toml with empty [workspace], ggen.toml wiring osx-clnr-pack by absolute path, schema/domain.ttl, src/lib.rs with the anchor marker, copied rust-toolchain.toml) -- this pack was not wired in examples/receiptctl, so this is the primary real verification substrate used

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L3 | Only one concern (selection/parsing) is generated as real logic; the L4 bar requires 'the full module family for one concern (types + logic + docs)' and L5 requires the entire crate surface (types+log... |
| Handler-gap size | L1 | L2 | No generated dispatch/entry-point layer exists mapping the 5 catalogued cnv:Command individuals to callable functions (e.g. a generated `handle_snapshot_delete(...)` per command) with a defined hand-w... |
| Ontology expressiveness | L1 | L3 | No formal pre/post-condition or state-transition-semantics facts exist (L4 bar: 'behavior contracts (pre/post-conditions, state transitions)') -- nextStage states order but not what changes in system ... |
| Consumer effort | L2 | L2 | L3 requires 'wires ggen.toml + writes only domain logic' with zero mod-wiring effort of any kind; this pack's inject mechanism still requires the consumer to pre-create lib.rs with a one-time '// GGEN... |
| Test generation | L3 | L3 | L4 requires the proof to cover the FULL generated behavior surface with zero human re-transcription step; the catalog proof half still hand-transcribes literals by design (an explicit non-goal, docume... |
| Regeneration lifecycle | L2 | L3 | The two primary generated files (osx_clnr_catalog.rs, osx_clnr_plan.rs) still use `force: true` -- a hand-edit to either would be silently clobbered with no diff/warning/refusal, not the L4 bar ('cons... |
| Target-API fidelity | L1 | L2 | upstream-external for L3->L5: fidelity beyond a dated hand-verification pass requires either CI infrastructure wiring (a workspace-level concern, not owned by this pack) or the target (~/osx-clnr, an ... |

#### affidavit-pack

Changes made:
- packs/affidavit-pack/ontology.ttl: added dated hand-verification note (2026-07-18, checked against real ~/affidavit v26.6.22 README.md, byte-for-byte, not just 'the README' in the abstract) plus afd:minCardinality/afd:maxCardinality on hasEvent/hasCommitment, afd:monotonic/afd:unique on sequenceNumber, and a new afd:rejectCondition literal per afd:CertifyStage individual (structural predicate, e.g. Continuity's 'strictly increasing with no duplicates'); added afd:Verdict/afd:Accept/afd:Reject classes for verdict semantics
- packs/affidavit-pack/templates/affidavit_certify.rs.tmpl (new): generates src/affidavit_certify.rs -- a real ontology-order-derived Stage dispatcher (next_stage/is_terminal, regenerates correctly if an 8th stage is added) and check_continuity(), a working implementation of afd:Continuity's rejectCondition, plus freeze_policy=checksum/freeze_slots_dir so hand-edits are detected and regeneration refuses to clobber them
- packs/affidavit-pack/templates/affidavit_certify_proof.rs.tmpl (new): generates tests/affidavit_certify_proof.rs -- adversarial proof tests (duplicate sequence numbers, out-of-order sequence numbers, fail-fast-at-first-violation, empty/single-event edge cases) plus a SPARQL sparql_count()-derived EXPECTED_STAGE_COUNT constant (not hand-transcribed, unlike the pre-existing affidavit_proof.rs)
- packs/affidavit-pack/templates/affidavit_mod_wiring.rs.tmpl (new): inject-mode template that appends 'pub mod affidavit_catalog;'/'pub mod affidavit_certify;' after a '// ggen:mod-anchor' marker in the consumer's src/lib.rs, with skip_if guarding against double-injection
- packs/affidavit-pack/templates/affidavit_catalog.rs.tmpl: added freeze_policy=checksum + freeze_slots_dir=.ggen/freeze/affidavit-pack
- packs/affidavit-pack/pack.toml: rewrote description to record the version-pinned fidelity check and document the 4 generated artifacts and the freeze mechanism
- scratch consumer built at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-affidavit-pack/ (Cargo.toml, ggen.toml wiring affidavit-pack via absolute path, schema/domain.ttl, src/lib.rs with the mod-anchor, rust-toolchain.toml copied) to prove the above end to end

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L3 | Only 1 of 7 CertifyStage behaviors (Continuity) is actually executable; the other 6 (Decode, FormatCheck, ChainIntegrity, CommitmentVerify, ProfileEvaluation, FinalVerdict) require modeling ~/affidavi... |
| Handler-gap size | L1 | L2 | The actual comparison algorithm inside check_continuity is still hand-written directly in the .tmpl body, not derivable purely from the current rejectCondition (a free-text string, not a machine-check... |
| Ontology expressiveness | L1 | L3 | afd:rejectCondition values are prose strings, not formal/computable predicates (no pre/post-condition calculus), so a differently-designed template set targeting another language could not mechanicall... |
| Consumer effort | L2 | L3 | Consumer still must pre-create src/lib.rs containing the `// ggen:mod-anchor` marker before first sync (inject requires the target file to exist, per ggen-engine's FM-WRITE-003) -- 'wires ggen.toml. D... |
| Test generation | L3 | L4 | The proof suite only certifies the one implemented behavior (Continuity); it does not certify the full certify-pipeline subsystem (6 of 7 stages have no generated behavior to test), so passing it is n... |
| Regeneration lifecycle | L2 | L3 | This is a skip/refuse mechanism, not a diff: L4 requires the consumer to see a semantic diff + receipt with no manual repair, and L5 requires drift to be impossible by construction via lock+receipt re... |
| Target-API fidelity | L1 | L2 | upstream-external: closing the remaining gap to L4/L5 requires the ~/affidavit crate itself (an external project this repo does not own) to publish a machine-readable ontology of its own certify-pipel... |

#### anti-llm-cheat-lsp-pack

Changes made:
- packs/anti-llm-cheat-lsp-pack/ontology.ttl: added alc:pattern/alc:patternKind properties carrying real detection data (dead-alt suffix list, hedge regex) transcribed verbatim from ~/anti-llm-cheat-lsp/src/rules/{dead_alt,hedge}.rs, plus a dated target-fidelity verification note at the top of the file citing exact source line ranges
- packs/anti-llm-cheat-lsp-pack/templates/anti_llm_cheat_catalog.rs.tmpl: SPARQL query extended to select ?pattern/?pattern_kind; DetectionRule struct gained pattern/pattern_kind fields; added two new real, callable functions -- scan_dead_alt() (detects uncalled fn definitions with a configured suffix) and scan_hedge() (detects hedge-phrase comments) -- both driven by ontology-sourced pattern data looked up from DETECTION_RULES at runtime, not hardcoded literals
- packs/anti-llm-cheat-lsp-pack/templates/anti_llm_cheat_catalog_proof.rs.tmpl: rewrote to be SPARQL-driven -- per-row assertions now emitted by a Tera loop over the same layers/rules bindings the catalog template consumes, instead of hand-transcribed literals; added 6 new behavioral tests exercising scan_dead_alt/scan_hedge against concrete adversarial inputs (uncalled _v2 fn, called _alt fn, non-suffixed fn, hedge comment variants, non-comment line containing hedge words)
- created scratch consumer at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-anti-llm-cheat-lsp-pack/ (ggen.toml wiring the pack via absolute path, empty-workspace Cargo.toml, copied rust-toolchain.toml, lib.rs mounting the generated module) to run a real sync+build+test cycle

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L3 | L5 requires 'the entire crate surface -- types, logic, tests, docs' to precipitate from RDF. Still missing: no generated Cargo.toml/mod-wiring/crate scaffolding (a hand-written lib.rs mod declaration ... |
| Handler-gap size | L1 | L3 | L5 requires 'behavior itself is specified in the ontology... or composed from other packs' generated logic.' The ontology stores the pattern as an opaque string (a regex/suffix-list literal), not a st... |
| Ontology expressiveness | L1 | L3 | L5 requires the ontology to be 'a complete specification' such that 'a different template set could regenerate an equivalent system.' The DetectorLayer individuals still carry no behavioral content at... |
| Consumer effort | L2 | L2 | engine-level: a self-registering module-injection template pattern (append-to-marker in a consumer's lib.rs) is the kind of cross-pack convention this repo's injection templates use, and introducing a... |
| Test generation | L3 | L3 | L5 requires the generated suite ALONE to be 'sufficient evidence... that passing it certifies the subsystem.' The adversarial cases I added are still a small, hand-picked, template-author-chosen set (... |
| Regeneration lifecycle | L2 | L3 | Gap (b) from the audit -- 'a lock/receipt mechanism that detects and refuses drift' -- is untouched: both templates still declare `force: true`, and there is no pack-local lock file comparing a prior ... |
| Target-API fidelity | L1 | L2 | L5 requires fidelity to be 'definitional, not checked' -- this pack tracks a copied snapshot of the target's regex/suffix-list literals, which can silently drift the moment ~/anti-llm-cheat-lsp's sour... |

#### wasm4pm-pack

Changes made:
- packs/wasm4pm-pack/ontology.ttl: added w4c:dependsOn property + 7 real intra-workspace dependency edges (wasm4pm -> wasm4pm-cognition, miniml-core; wasm4pm-cli -> wasm4pm; wasm4pm-cognition -> prolog8; wasm4pm-planner -> prolog8, wasm4pm, wasm4pm-cognition), hand-verified against ~/wasm4pm@b6fedcbef8d5fbdab4dbb9827226e802fe961a71 (2026-07-18) by grepping each member crate's own Cargo.toml [dependencies] table
- packs/wasm4pm-pack/templates/wasm4pm_crate_deps.rs.tmpl (new): SPARQL-derived CRATE_DEPS edge list plus two real behavioral functions -- depends_on() (lookup) and transitive_dependencies() (DFS graph traversal with an explicit visited-set cycle guard) -- genuine control flow driven by ontology data, not a projected const array
- packs/wasm4pm-pack/templates/wasm4pm_crate_deps_proof.rs.tmpl (new): 8 hand-transcribed proof tests covering direct lookup, multi-hop transitive closure, leaf-crate (empty not error), unknown-crate-name (empty not panic), and no-self-loop invariants; independent oracle sharing no SPARQL query with the code under test
- packs/wasm4pm-pack/templates/wasm4pm_crate_mod_wiring.rs.tmpl (new): inject-mode template that auto-wires both generated modules into the consumer's src/lib.rs at the `// GENERATED catalogs/emission surfaces` marker (same convention examples/receiptctl already uses), removing the hand-written mod step
- packs/wasm4pm-pack/templates/wasm4pm_crate_catalog.rs.tmpl, wasm4pm_crate_catalog_proof.rs.tmpl, wasm4pm_crate_reference.md.tmpl: replaced `force: true` with `freeze_policy: checksum` + `freeze_slots_dir: .ggen/freeze/wasm4pm-pack` -- a hand-edit to a generated output between syncs now causes the next `ggen sync run` to refuse (Skipped, checksum mismatch) instead of silently clobbering it (verified live)
- packs/wasm4pm-pack/pack.toml: description rewritten to document the deps module, mod-wiring, freeze-policy drift refusal, and the dated hand-verification note/commit citation used for target-API fidelity

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L3 | Still only one behavioral concern (dependency traversal); L4 needs a full type+logic+tests+docs family, and L5 requires the entire crate surface (types, logic, tests, docs) to precipitate from RDF -- ... |
| Handler-gap size | L1 | L2 | L5 requires 'zero handler gap: behavior specified in the ontology or composed from other packs' generated logic' for the whole subsystem. This pack's only behavior is graph traversal; there is no broa... |
| Ontology expressiveness | L1 | L2 | Still no behavior/pre-post-condition contracts (L4 bar) and no claim that 'a different template set could regenerate an equivalent system' (L5) is coherent -- the ontology models structure (dependency... |
| Consumer effort | L2 | L3 | L5 bar is 'Consumer wires ggen.toml. Done.' The consumer must still author the initial marker comment and an empty lib.rs shell before first sync, and there is still no working subsystem beyond a refe... |
| Test generation | L3 | L3 | engine-level |
| Regeneration lifecycle | L2 | L3 | L4 requires semantic-diff-plus-receipt-on-upstream-change (distinguishing an intentional ontology-driven regen from accidental drift, and minting a receipt on the former); today freeze_policy:checksum... |
| Target-API fidelity | L1 | L2 | upstream-external |

#### mfact-pack

Changes made:
- packs/mfact-pack/ontology.ttl: added mfa:handsOffTo/mfa:readsFrom/mfa:writesTo relations chaining the 3 PipelineStages and linking each to the AuthorityDirs it actually reads/writes, grounded directly in the existing rdfs:comment prose (no new claims beyond the README)
- packs/mfact-pack/shapes.ttl (new file): SHACL NodeShapes for mfa:PipelineStage and mfa:AuthorityDir enforcing cardinality (exactly-one order/actor/label/comment/path) and relation typing (handsOffTo/readsFrom/writesTo must target the correct class)
- packs/mfact-pack/templates/mfact_catalog.rs.tmpl: added a handoffs SPARQL query + MFACT_HANDOFFS const; added real control-flow functions stage_for_actor, dir_for_path, next_stage, validate_pipeline_order, and an MfactCatalogError enum with Display/Error impls -- the pack's first generated behavior surface instead of only const data
- packs/mfact-pack/templates/mfact_catalog_proof.rs.tmpl: rewrote so per-row exact-value assertions are now rendered from the same sparql: stages/dirs/handoffs queries (no longer hand-transcribed literals), added 22 tests total including error-path coverage for the new functions (unknown actor/path, terminal-stage handoff, synthetically-broken pipeline order)
- packs/mfact-pack/templates/mfact_lib_wiring.rs.tmpl (new file): inject:true template that adds `mod mfact_catalog;`/`mod mfact_catalog_ext;`/`pub use ...` to the consumer's existing src/lib.rs, guarded by skip_if for idempotency -- closes the manual mod-wiring step
- packs/mfact-pack/templates/mfact_catalog_ext.rs.tmpl (new file): unless_exists + freeze_policy:checksum hand-editable extension point demonstrating real freeze/inject coexistence across regens
- packs/mfact-pack/TARGET_VERIFICATION.md (new file): dated (2026-07-18) hand-verification note checking ontology.ttl's claims against the real ~/mfact checkout at commit 801abf7933dabf5c95f9fb18ff21a7a8a1f6a564

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | To reach L4 the pack would need a second, related generated module (not just one catalog file) forming a family; L5 requires the entire crate surface -- including Cargo.toml, doc crate root, CI config... |
| Handler-gap size | L1 | L2 | L3 requires 'common behaviors generated; only domain-unique logic is hand-written, behind a stable seam' -- the pack would need an actual consumer-side domain-unique behavior (something mfact_catalog_... |
| Ontology expressiveness | L2 | L3 | L4 requires behavior contracts (pre/post-conditions, state transitions) encoded IN the ontology itself, not just structural relations -- e.g. what makes an Admit 'succeed' as an RDF-expressed precondi... |
| Consumer effort | L2 | L3 | L5 ('Consumer wires ggen.toml. Done.') requires the pack to work against a consumer that has genuinely done nothing but write ggen.toml -- here the consumer still must have a pre-existing src/lib.rs (... |
| Test generation | L3 | L3 | L4 requires the proof to be wired into CI/pre-commit and derived via a path structurally independent of the template under test (e.g., a second differently-shaped query or a checked-in fixture snapsho... |
| Regeneration lifecycle | L2 | L3 | L4 requires 'upstream ontology change -> regen -> consumer sees a semantic diff + receipt; no manual repair' -- today an ontology.ttl edit still silently regenerates the SPARQL-derived catalog/proof/d... |
| Target-API fidelity | L1 | L2 | upstream-external. L3 requires compiling generated output against the pinned target crate in the pack's own CI, but mfact is a Lean 4 project with no Rust crate surface to compile against, and L5 requ... |

#### mfw-pack

Changes made:
- packs/mfw-pack/ontology.ttl: added mfw:nextStage as a first-class RDF property with 10 explicit edge triples (Observe->Admit->...->Replay->Observe), so cycle transitions are graph structure, not order+1 arithmetic a template must reconstruct
- packs/mfw-pack/ontology.ttl: added mfw:SourceSnapshot class + mfw:Agents2026_07_16 individual recording the exact ~/mfw commit hash (20e9925bc9e63d3537b91e8c725de85fc6ed2084) and date (2026-07-16T15:57:22-07:00) AGENTS.md was transcribed from, as machine-readable RDF instead of only prose
- packs/mfw-pack/templates/mfw_catalog.rs.tmpl: added 5 new SPARQL bindings (transitions, stage_count, surface_count, crate_count, transition_count) -- the counts use an independently-shaped COUNT(...) aggregate query, distinct from the enumeration queries
- packs/mfw-pack/templates/mfw_catalog.rs.tmpl: generates NEXT_STAGE table + real pub fn next_stage()/is_valid_transition() dispatch logic driven by mfw:nextStage ontology data (not hardcoded control flow)
- packs/mfw-pack/templates/mfw_catalog.rs.tmpl: generates EXPECTED_*_LEN consts from the independent COUNT queries plus an embedded #[cfg(test)] mod self_check with 7 tests (row-count cross-checks + a full 10-hop cycle walk + transition-validity negative tests) that ships inside the catalog file itself, so drift is caught by cargo test with zero human re-transcription for these specific counts
- packs/mfw-pack/templates/mfw_catalog_proof.rs.tmpl: added 2 new behavioral tests (next_stage_matches_every_hand_transcribed_ontology_edge, is_valid_transition_refuses_a_skip_ahead_and_an_unknown_stage) exercising the new real dispatch logic, not just data tables
- packs/mfw-pack/templates/mfw_catalog_mod_wiring.rs.tmpl: new file -- inject:true template that adds `pub mod mfw_catalog;` to the consumer's src/lib.rs after a `// GGEN-INJECT: mod-root` marker, skip_if-guarded against re-insertion, following the same pattern already used by 9 other packs in this repo
- packs/mfw-pack/templates/mfw_reference.md.tmpl: added transitions and source_snapshot SPARQL bindings and rendered 'Cycle transitions' + 'Source snapshot' sections to the generated doc
- packs/mfw-pack/pack.toml: rewrote description to accurately reflect the new behavior, self-check module, mod-wiring template, and the source-snapshot mechanism plus its ~/mfw-is-external caveat

| Dimension | Before | After | Note |
|---|---|---|---|
| Generation depth | L2 | L2 | tests/mfw_catalog_proof.rs's row-content spot checks are still hand-transcribed literals (by its own explicit doc comment), and no Cargo.toml/build-manifest surface is generated by this pack at all --... |
| Handler-gap size | L1 | L2 | Only cycle-transition lookup/validation is ontology-specified; no receipt-shape, authority-surface-enforcement, or cross-pack composed behavior is generated. Reaching L5 requires the ontology to speci... |
| Ontology expressiveness | L1 | L2 | Only the 10-stage cycle's adjacency is now behavioral RDF; authority-surface and crate facts remain purely descriptive (path/label/comment), and no receipt/obligation/validation semantics from mfw's a... |
| Consumer effort | L2 | L2 | none -- reached for real |
| Test generation | L3 | L3 | Row-content values (labels, docs, transition endpoints as literal strings) in tests/mfw_catalog_proof.rs are still hand-transcribed, only counts are now independently derived; no CI wiring exists to m... |
| Regeneration lifecycle | L2 | L2 | engine-level: a generic content-drift-refusal mechanism (beyond aggregate counts) needs hooks into crates/ggen-engine's receipt/hash comparison path, shared by every other pack and pack-consumer in th... |
| Target-API fidelity | L1 | L2 | upstream-external: ~/mfw is a separate repository this ggen workspace does not own; it has no single canonical ontology/ directory to import (confirmed by directory listing), and this pack cannot safe... |

### Knowledge-hook packs

#### self-monitoring-pack

Changes made:
- packs/self-monitoring-pack/ontology.ttl: added smon:Severity/smon:SeverityScheme (Low/Medium/High) + smon:severity, smon:DeadlineHint/Scheme (ActNow/NextSession) + smon:deadlineHint, smon:ObligationStatus/Scheme (Open/Discharged/Refused) + smon:status/smon:dischargedAt/smon:refusalReason, smon:UngovernedTransition + smon:hasUngovernedQuestion/hasUngovernedResponse -- all disclosed with rationale, no public-ontology-first discipline broken
- packs/self-monitoring-pack/hook.ttl: removed the old unparameterized smon:derive_escalation_obligation hook (would have caused a duplicate-node regression once severity hooks existed) and replaced it with three mutually-exclusive smon:prioritize_escalation_obligation_{low,medium,high} hooks that compute severity from a real COUNT/GROUP BY aggregate; added smon:flag_ungoverned_transition, a catch-all detector for GroundingQuestion turns followed by an Other-classified response
- packs/self-monitoring-pack/shapes.ttl: added property shapes for severity/deadlineHint/status (closed sh:in vocabularies) plus a pure-SHACL-Core (sh:or + sh:not + sh:hasValue) conditional constraint enforcing dischargedAt/refusalReason when status is Discharged/Refused -- after discovering sh:sparql is a silent no-op in this engine (SHACL_SPARQL_BOUNDARY=CORE_ONLY in crates/praxis-graphlaw/src/shacl/model.rs); added UngovernedTransitionShape
- packs/self-monitoring-pack/fixtures/pattern-ungoverned.ttl: new positive fixture proving the ungoverned-transition catch-all fires correctly and only there
- packs/self-monitoring-pack/fixtures/pattern-fires-discharged.ttl: new fixture demonstrating the Discharged lifecycle state and exercising the shapes.ttl conditional constraint (real tamper test: removing dischargedAt fails closed)
- packs/self-monitoring-pack/queries/open_obligations.rq: new shipped SPARQL query for querying open (status=Open) obligations; discloses a confirmed engine limitation (ORDER BY silently returns zero rows on this engine's query() path) rather than shipping it broken
- packs/self-monitoring-pack/scripts/actuate_escalation.py: new shipped actuation script -- extracts every kh:Action's CONSTRUCT text verbatim from hook.ttl, executes via rdflib (independent SPARQL 1.1 engine), writes a JSON receipt (SHA-256 hash, timestamp, per-hook fire/no-fire) including receipted refusal paths on parse/execution failure
- packs/self-monitoring-pack/scripts/capture_and_validate.sh: new shipped pipeline chaining transcript_to_turtle.py (capture) directly into `ggen graph validate --shapes` (admission) as one command
- packs/self-monitoring-pack/scripts/measure_fire_precision_multi_session.py + fixtures/precision_report_multi_session.json: new longitudinal fire-precision measurement across 4 additional real, independently-dated session transcripts (313 real turns total), extending the prior single-snapshot analysis
- packs/self-monitoring-pack/README.md: added an 'L5-push additions (this pass) and honest re-score' section documenting every change above with exact verification evidence and named remaining gaps

| Dimension | Before | After | Note |
|---|---|---|---|
| Derivation power | L2 | L3 | L4 requires 'Derives plans: ordered obligation chains with discharge conditions' -- nothing here chains multiple obligations into an ordered plan. L5 requires re-planning as new facts arrive live (a s... |
| Actuation closure | L1 | L2 | L3's bar is 'A shipped actuation script fires on derivation and its effect is receipted' -- this is met for the fire/receipt half, but the script still requires a human or CI job to invoke it; there i... |
| Input acquisition | L3 | L3 | L4 requires 'Capture is continuous and validated (SHACL-gated) on ingest' -- this is one invocation per transcript file, not a running/watching capture daemon. L5 requires the pack to own capture+admi... |
| Fire precision | L4 | L4 | L5 requires precision 'measured on real captured data over time, not only fixtures' -- this pass adds real multi-session data but does NOT compute an actual precision/recall metric against independent... |
| Obligation lifecycle | L1 | L3 | L4 requires overdue/undischarged obligations to escalate via the same hook mechanism -- no such escalation hook exists. L5 requires a full replayable ledger from derivation through discharge/refusal, ... |
| Composability | L2 | L2 | The concrete next step the audit itself named (a real co-fire test against a second kh:Hook pack, specifically citing dogfood-lifecycle-pack as 'the natural candidate') is unbuildable without editing ... |
| Governance coverage | L1 | L3 | L4 requires happy path PLUS failure/exception paths of the whole process to be governed -- this pack still governs only two shapes of one narrow process (grounding-question/response), not e.g. Blocker... |

#### dogfood-lifecycle-pack

Changes made:
- packs/dogfood-lifecycle-pack/ontology.ttl — added a real dfl:Obligation vocabulary (derivedFrom, obligationTarget, severity {High/Medium/Low}, deadline xsd:duration, obligationStatus {Open/Discharged/Refused}, dischargedBy) plus a pack-level dfl:firesAfter composability disclosure triple
- packs/dogfood-lifecycle-pack/hook.ttl (new) — two real kh: Knowledge Hooks (derive_review_obligation, discharge_review_obligation) run through praxis-graphlaw's actual TripleStore::load_hook_pack + .materialize() engine, the same production mechanism packs/self-monitoring-pack/hook.ttl uses
- packs/dogfood-lifecycle-pack/hooks/dogfood-self-monitoring-precedence.ttl (new) — one additive kh:after triple declaring cross-pack hook ordering in RDF, kept separate from hook.ttl so this pack's own derivation still loads standalone
- packs/dogfood-lifecycle-pack/shapes.ttl — added dfl:ObligationShape (derivedFrom/obligationTarget/severity/deadline required; obligationStatus and dischargedBy optional, matching the append-only engine's actual semantics)
- packs/dogfood-lifecycle-pack/fixtures/session-discharged.ttl, session-adversarial-discharge.ttl, session-error-not-blocked.ttl (new) — positive discharge, adversarial different-agent-must-not-discharge, and adjacent-Error-must-not-fire fixtures, all ggen graph validate --shapes conformant
- packs/dogfood-lifecycle-pack/hooks/dogfood-lib.sh (new) — shared helpers: dogfood_repo_root (parameterizes the previously hardcoded /Users/sac/praxis path via $DOGFOOD_REPO_ROOT), dogfood_append_event, dogfood_bump_invocation_counter, dogfood_ingest_validate
- packs/dogfood-lifecycle-pack/hooks/dogfood-lifecycle-capture.sh — rewritten to source dogfood-lib.sh: parameterized path, per-invocation coverage counter bump before the tool-name filter, on-ingest ggen graph validate call after every append
- packs/dogfood-lifecycle-pack/hooks/dogfood-lifecycle-session-end.sh — rewritten to source dogfood-lib.sh: parameterized path, governance-gap detection (invocations_seen vs tool_events, receipted), log rotation past $DOGFOOD_MAX_EVENTS with a receipted rotation record
- packs/dogfood-lifecycle-pack/hooks/dogfood-lifecycle-receipt-spotcheck.sh — parameterized path via dogfood-lib.sh; recompute logic updated to handle the new payload shape (and the old one, for backward compatibility) plus rotation/refusal record types
- packs/dogfood-lifecycle-pack/hooks/cng-plan-admission-guard.sh — parameterized path; block path (exit 2) now appends a real dfl:Blocked ToolEvent (the only way one can ever exist, since PostToolUse never fires for a blocked call) and a hash-chained refusal receipt to the same receipts.jsonl chain
- packs/dogfood-lifecycle-pack/README.md — added a 'v26.7.18 gap-closing pass' section documenting each change, its verification command/output, and what remains open, including the disclosed self-monitoring-pack hook-IRI volatility found live

| Dimension | Before | After | Note |
|---|---|---|---|
| Derivation power | L1 | L3 | L4 needs ordered obligation CHAINS (multi-step discharge plans), not a single flat obligation per event. L5 needs standing re-planning as new facts arrive continuously -- a governor, not a one-shot ru... |
| Actuation closure | L2 | L3 | Actuation outcomes do not yet flow back into the graph as NEW downstream observed facts beyond the Blocked event + refusal receipt themselves (e.g. no linked follow-up fact when the operator subsequen... |
| Input acquisition | L3 | L3 | L4 requires capture to be continuous AND validated on ingest as the pack's OWN closed loop -- this pass adds on-ingest validation, a real improvement, but capture itself still depends on a consumer ma... |
| Fire precision | L2 | L3 | L4 needs malformed/colliding/gamed inputs beyond the one adversarial case built here (e.g. IRI-collision fixtures, deliberately malformed Turtle that is still shape-valid). L5 requires precision MEASU... |
| Obligation lifecycle | L1 | L3 | L4 needs overdue/undischarged obligations to ESCALATE via the same hook mechanism (a third hook chaining off an open+overdue query) -- not built. L5 needs a full replayable ledger from derivation thro... |
| Composability | L2 | L3 | L4 needs obligations to reference EACH OTHER across packs (e.g. this pack's dfl:Obligation linking to self-monitoring's smon:EscalationObligation by IRI, obligation chaining across packs) -- not built... |
| Governance coverage | L1 | L3 | This detects a COUNT gap, not which specific transition was ungoverned or why (no per-invocation reason/tool-name is retained in the counter, only a running total) -- L4's 'happy path + failure/except... |

### Case-study corpus pack

#### ma-case-study-pack

Changes made:
- packs/ma-case-study-pack/shapes.ttl: fixed ma:LEIRegisteredEntityShape, which previously checked the wrong property (fibo-be-le-lei:isQuantifiedBy) with a vacuous sh:minCount 0, closing the exact disclosed adversarial-resistance gap named in the audit. Now requires cmns-id:isIdentifiedBy minCount 1, verified live: adversarial-negative.ttl's missing-LEI counterparty now trips a real 3rd SHACL violation (previously only 2 fired).
- packs/ma-case-study-pack/fixtures/case-2.ttl (new): a second, wholly independent full-coverage case instance (different acquirer/target IRIs, Terminated status, Failed vote, and the first fixture in this pack to exercise Concept 8 -- ma:DealTermLineItem/ma:CounterOffer -- which case.ttl never touches). Validated shapes_conform:true against the unchanged ontology.ttl/shapes.ttl.
- packs/ma-case-study-pack/fixtures/mega-deal-second-request.ttl (new): a third minimal fixture (USD 1.25B deal) built to exercise the new second hook; validated shapes_conform:true.
- packs/ma-case-study-pack/fixtures/hook.ttl: added a second, independent obligation-deriving Knowledge Hook (derive_ma_second_request_review_obligation, HSR Second Request pattern at a USD 1B threshold) alongside the pre-existing single hook, broadening Verdict authority from 1 to 2 derivation rules. Threshold deliberately set above every existing fixture's amount so the pre-existing shared/off-limits crates/praxis-graphlaw/tests/ma_case_hook_actuation.rs (which asserts an exact '1 derived triple' count) is not broken -- confirmed by first observing it DID break at a lower threshold, then fixing by raising the threshold rather than touching the shared test.
- packs/ma-case-study-pack/queries/*.rq (new, 3 files): first committed SPARQL queries in this pack (CQ3.1 open due-diligence items, CQ7.1 vote outcome+quorum, CQ10.1 deal status), each executed for real via a scratch consumer against 3 independent case instances with zero query changes, producing correct distinct answers each time.
- packs/ma-case-study-pack/pddl-domain.ttl: corrected the file's own header, which cited a nonexistent crates/multifractal-workflow/tests/ma_case_pddl.rs as live proof of a real PDDL8 grounder/solver round-trip. Confirmed crates/multifractal-workflow does not exist anywhere in this workspace; header rewritten to disclose the false citation and state plainly what is/isn't real for this pack's planning story.
- packs/ma-case-study-pack/STANDING.md: updated the M&A-C4 row to record the pddl-domain.ttl citation correction and its evidence.
- Scratch verification harness at /private/tmp/claude-501/-Users-sac-ggen/70ac08c7-8655-49c4-baa2-018bc441bb4c/scratchpad/l5-push-ma-case-study-pack/ (Cargo.toml with empty [workspace], path-dependency on crates/praxis-graphlaw as a read-only library, rust-toolchain.toml copied in) -- used only to execute real TripleStore::load_hook_pack/.materialize()/.query() calls against this pack's fixtures; no shared file was edited, only read as a library.

| Dimension | Before | After | Note |
|---|---|---|---|
| Question coverage | L1 | L2 | L3 needs every one of the ~24 CQs in COMPETENCY_QUESTIONS.md backed by a committed query AND an automated (not hand-run) test asserting the expected answer -- only 3 of ~24 have queries today, and non... |
| Verdict authority | L2 | L2 | engine-level -- receipt/rationale emission alongside a derived triple is a hook-engine capability (praxis-graphlaw), not something this pack's data files can add on their own; a generic case-ingestion... |
| Domain fidelity | L2 | L2 | longitudinal/upstream-external -- domain-expert acceptance and real-world deal-data validation are external processes this session cannot manufacture; no pack-level edit substitutes for them. |
| Adversarial resistance | L2 | L2 | engine-level -- a real production-miss-to-fixture regression-feed mechanism does not exist anywhere in this repo's shared tooling for any case-study pack; building one from scratch is infrastructure w... |
| Planning integration | L2 | L2 | engine-level -- wiring pddl-domain.ttl to a real grounder/solver requires building a SPARQL-extraction + PDDL8 pipeline crate (the bribery-case precedent lives in a crate that does not exist in this w... |
| Generalization | L2 | L3 | L4 requires case-instance authoring to be GENERATED (forms/templates), not hand-written Turtle -- both case.ttl and case-2.ttl (and the new mega-deal fixture) are hand-authored, same as before. L5 req... |
| Reasoner independence | L2 | L2 | Not fundamentally blocked (oxigraph is already a workspace dependency, so a second-engine harness is buildable at the pack level in a future pass) -- simply not completed within this pass's time budge... |

## Honest overall level distribution after this pass

| Level | Cells |
|---|---|
| L1 | 3 |
| L2 | 68 |
| L3 | 61 |
| L4 | 8 |
| L5 | 0 |

(140 cells total across 20 packs, 7 dimensions each. L5 count is 0 -- the mfw-pack self-report was rejected per above.)
