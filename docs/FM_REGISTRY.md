# FM Registry

Every statically observable failure-mode code in the ggen workspace, one row
per code: trigger condition and remediation, derived from each code's actual
emission-site context (not invented). Closes XG-2's GAP-2 (codes with zero docs
text) and carries GAP-3's honest ledger in the Tested-by column.

## Census method

Sources: `crates/ggen-engine/src/**` and `crates/ggen-cli/src/**` (this census
found zero FM-code emission sites in `ggen-cli/src`; all sites live in
`ggen-engine/src`). A code counts as observable if it appears as (a) a static
literal `FM-<FAM>-NNN` (word-boundary, 3-digit), or (b) a parametric numeric
call site `AppError::fm_<fam>(N, ...)` minted via the constructors in
`crates/ggen-engine/src/error.rs:206-256`. Two minting mechanisms, one census:
static literals = 107 unique codes; parametric call sites = 33; union =
**118 unique codes**. Word-boundary matters: a bare `[0-9]+` grep also matches
the doc-comment wildcard `FM-CHAIN-0NN` (`verbs/handlers.rs:427`) as a phantom
`FM-CHAIN-0` — excluded here.

Counts as of 2026-11-03, re-derived on this census (XG-2's session prose said
87/37; this broader two-mechanism method re-derives 118/52 — the divergence is
census-method scope, not a contradiction; the parity court below re-checks this
table against the tree on every run, so these numbers self-heal on drift).

`Tested by` = files under `crates/*/tests/` whose text contains the code
(`none` = zero test references — XG-2's GAP-3, carried honestly, not closed:
52 of 118 codes carry `none` here; writing those tests is out of this lane's
scope by contract). Occurrences inside `src` `#[cfg(test)]` modules are not
counted as test coverage.

## Parity court

`crates/ggen-engine/tests/fm_registry_parity_test.rs` re-derives the census
(both mechanisms) and asserts set equality with this file's rows in both
directions, on every run. A new emission site without a row, or a row without
an emission site, fails the test with the exact codes named.

## Registry

### FM-CHAIN-* — receipt chain, portable envelopes, replay courts

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-CHAIN-001` | `ValidationChain::finish` observed failed checks | fix the first failing check listed in the error body | `none` |
| `FM-CHAIN-002` | receipt-chain hash computation failed during a sync receipt write | read the wrapped I/O error; verify `.ggen-v2/receipt-log.jsonl` is writable | `none` |
| `FM-CHAIN-007` | dynamic family member; no static production emission site (constructor round-trip witness only) | none - do not emit in production | `receipt_chain_e2e.rs` +1 more |
| `FM-CHAIN-009` | previous receipt chain recompute failed (legacy log unreadable / `ChainStanding::LegacyV2Unbound`) | inspect `.ggen-v2/receipt-log.jsonl`; repair or archive, never hand-edit receipts | `receipt_chain_differential_test.rs` +2 more |
| `FM-CHAIN-010` | previous receipt epoch record unreadable | repair the epoch state under `.ggen-v2/`; do not hand-edit receipt files | `none` |
| `FM-CHAIN-011` | receipt epoch construction failed | fix the wrapped I/O cause (disk space, permissions under `.ggen-v2/`) | `none` |
| `FM-CHAIN-014` | receipt chain append failure surfaced through `exec_err` in verb handlers | read the wrapped chain error; verify the receipt log is writable | `receipt_chain_e2e.rs` |
| `FM-CHAIN-015` | portable envelope: a consequence recorded as landed cannot be read for digest binding | fix the file's permissions (per the in-band remediation text) | `none` |
| `FM-CHAIN-016` | GALL-001 consequence is missing a string target | repair the GALL-001 receipt so every consequence carries a string target | `gall_checkpoint_001_replay_e2e.rs` |
| `FM-CHAIN-017` | GALL-001 receipt has no subject object | repair the GALL-001 receipt's subject object | `gall_checkpoint_001_replay_e2e.rs` |
| `FM-CHAIN-019` | replay-court envelope cannot be read/parsed as JSON | re-run the producing stage; if persistent, rebuild the envelope artifact | `replay_court_test.rs` |
| `FM-CHAIN-020` | replay-court execution error propagated from `execute_replay_court` | read the wrapped court error | `none` |

### FM-CLI-* — CLI argument validation (ValidationChain)

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-CLI-001` | `--parallel` given without `--jobs > 0` | pass `--jobs` greater than 0 alongside `--parallel` | `none` |
| `FM-CLI-002` | required CLI argument missing (ValidationChain CLI check) | supply the named argument; see the message body | `none` |
| `FM-CLI-042` | dynamic family placeholder (unit-test probe; no static production emission site) | none - constructor round-trip witness only | `none` |
| `FM-CLI-099` | dynamic family placeholder (`ValidationChain::require` unit-test probe) | none - unit-test probe only, never emitted in production | `none` |

### FM-CONFIG-* — manifest load, schema dispatch, gate globs

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-CONFIG-001` | `ggen.toml` not found in the current directory | run from the directory that owns `ggen.toml`; no `--manifest` flag exists | `doctor_surface_test.rs` +4 more |
| `FM-CONFIG-002` | `ggen.toml` TOML syntax error or unknown keys | fix the TOML syntax / remove unknown keys named in the error | `config_schema_dispatch_e2e.rs` +1 more |
| `FM-CONFIG-003` | `ggen.toml` parses but a semantic invariant fails (nested parse failure) | read the nested error - always specific, not generic | `doctor_surface_test.rs` +4 more |
| `FM-CONFIG-014` | a `[validation] gates` glob matched zero files (`GLOB_NO_MATCHES`) | fix the glob pattern or add the gate `.rq` files it should match | `none` |

### FM-GEN-* — generation rules: source/template resolution, body validation

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-GEN-005` | generation-rule query file unreadable | fix the query file's path/permissions relative to the manifest | `generation_rules_e2e.rs` |
| `FM-GEN-006` | generation-rule template file unreadable | fix the template file's path/permissions relative to the manifest | `generation_rules_e2e.rs` +1 more |
| `FM-GEN-007` | not-yet-implemented `QuerySource`/`TemplateSource` variant | use the implemented file-based sources; do not construct the named variant | `generation_rules_e2e.rs` +1 more |
| `FM-GEN-008` | template render failed (cause tag: `TEMPLATE_PARSE_FAILED`, `TEMPLATE_VARIABLE_MISSING`, ...) | read the bracketed cause tag; e.g. compute filtered values in a separate `{% set %}` before `~` concatenation | `generation_rules_typed_causes_e2e.rs` |
| `FM-GEN-009` | existing output target cannot be read as UTF-8 | re-encode or delete the existing target so the merge path can read it | `none` |
| `FM-GEN-010` | GENERATED/MANUAL merge markers out of order (`=======` before `<<<<<<< GENERATED`) | restore marker order: `<<<<<<< GENERATED` / `=======` / `>>>>>>> MANUAL` | `none` |
| `FM-GEN-011` | rendered output body is empty | check the query returned rows (or static body isn't blank); set `skip_empty` if empty is expected | `generation_rules_e2e.rs` |
| `FM-GEN-012` | rendered body exceeds `MAX_OUTPUT_BYTES` | check for an unbounded loop over query results, or split into multiple outputs | `none` |
| `FM-GEN-015` | `validation.no_unsafe = true` and the rendered body contains `unsafe` | remove the `unsafe` block from the template or disable the flag knowingly | `generation_rules_e2e.rs` |
| `FM-GEN-016` | `validation.validate_syntax = true` with a `.rs` target (no Rust parser wired; honest refusal) | unset the flag or accept the loud refusal; the check is not implemented yet | `generation_rules_e2e.rs` |

### FM-GRAPH-* — RDF store, SPARQL execution, canonicalization, deltas

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-GRAPH-001` | in-memory RDF store creation failed | fix the wrapped storage error; retry | `none` |
| `FM-GRAPH-002` | Turtle parse or store read error | fix the Turtle syntax named in the error | `dialect_dispatch_test.rs` +2 more |
| `FM-GRAPH-003` | SPARQL query failed to parse | fix the query syntax; the message names the position | `fortune5_architecture_pack_e2e.rs` +1 more |
| `FM-GRAPH-004` | store iteration failed | fix the wrapped storage fault; retry | `none` |
| `FM-GRAPH-005` | canonical blank-node label rejected (RDFC-1.0 canonicalization) | regenerate the label from a canonicalization run; never hand-mint blank-node labels | `none` |
| `FM-GRAPH-006` | quad deletion did not match any quad in the store | refresh the delta against current graph state; the store changed under the delta | `none` |
| `FM-GRAPH-008` | query contains a `GRAPH` clause (single default graph; silent zero-match otherwise) | remove the GRAPH clause; no named-graph ingestion path exists | `none` |
| `FM-GRAPH-009` | query text looks like a SPARQL UPDATE attempt | ggen accepts only SELECT/CONSTRUCT/DESCRIBE/ASK; move updates out of sync queries | `none` |
| `FM-GRAPH-010` | unsupported file extension refused before any store mutation | use a supported RDF dialect extension (`.ttl` etc.), not non-RDF files | `rdf_source_admission_e2e.rs` |
| `FM-GRAPH-013` | `BIND` re-binds an already-bound `AS` variable (SPARQL 1.1 freshness rule) | rename the `AS` variable so it is fresh in the group | `sparql_refusals_e2e.rs` |

### FM-KEY-* — signing/verifying keypair lifecycle

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-KEY-002` | `signing.key` empty/invalid; poisoned file deleted (self-heal) | re-run the command; the next call regenerates a fresh keypair | `none` |
| `FM-KEY-006` | keys directory cannot be created | fix permissions on the parent directory named in the error | `none` |
| `FM-KEY-010` | pre-existing `verifying.key` cannot be read/decoded | delete the stale `verifying.key` and regenerate the keypair | `none` |
| `FM-KEY-011` | `verifying.key` does not match the fresh signing key (stale public half) | rotate: delete `verifying.key` (signing.key rolled back automatically) and regenerate | `none` |
| `FM-KEY-012` | `.gitignore` write failed (non-AlreadyExists) - refusing to persist an unprotected key | fix permissions on the keys directory / `.gitignore` | `none` |

### FM-LAW-* — law/SHACL engine, rule materialization, gates

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-LAW-001` | law ops (`load_rules`/`materialize`) on a backend that does not implement them | use the `graphlaw` backend for law operations | `graphlaw_e2e.rs` |
| `FM-LAW-002` | SHACL validation on a backend that does not implement it | use the `graphlaw` backend for SHACL validation | `reasoner_independence_e2e.rs` |
| `FM-LAW-006` | GraphLaw rule load refused | read the refusal message; fix the rule document it names | `none` |
| `FM-LAW-008` | SHACL shapes graph refused (`graph.rs` path) | fix the shapes graph; read the wrapped engine error | `none` |
| `FM-LAW-009` | reasoner materialization failed | read the materializer message; fix the rule that failed | `none` |
| `FM-LAW-012` | legacy config shape needs fail-closed migration | apply the documented migration; update `ggen.toml` | `graphlaw_e2e.rs` +1 more |
| `FM-LAW-013` | SHACL shapes graph refused (`law_engine.rs` path) | fix the shapes graph; read the wrapped engine error | `graphlaw_e2e.rs` +4 more |
| `FM-LAW-015` | EA admission kernel-refusal family code (rendered into refusals) | read the wrapped kernel refusal body | `abb_sbb_datalog_admission_e2e.rs` +28 more |
| `FM-LAW-016` | GraphLaw hook pack load refused | fix the hook pack path/content named in the error | `generation_rules_e2e.rs` +2 more |
| `FM-LAW-017` | legacy non-empty `manifest.validation.shacl` (loud migration refusal) | migrate to the law-shapes surface; remove the legacy key | `generation_rules_e2e.rs` |
| `FM-LAW-019` | `[rules]` path unreadable or a rule document invalid at sync time | fix the rules path/files named in the error | `README.md` +2 more |
| `FM-LAW-020` | validation gate violation at `Error` severity (rule does not hold) | fix the graph to satisfy the rule, or re-grade the rule's severity | `generation_rules_e2e.rs` |

### FM-PACK-* — pack resolution, locking, dependency graph, scopes

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-PACK-001` | pack directory missing | restore the pack directory or fix the pack path in `ggen.toml`/lockfile | `dogfood_regression.rs` +2 more |
| `FM-PACK-002` | `pack.toml` missing or unreadable | restore `pack.toml` in the pack directory | `sabotage_tests.rs` |
| `FM-PACK-003` | `pack.toml` is syntactically invalid TOML | fix the TOML syntax; extra fields are allowed, invalid syntax is not | `pack_capabilities_parser_diff_test.rs` +3 more |
| `FM-PACK-004` | `ontology.ttl` missing from the pack | add `ontology.ttl` to the pack directory | `pack_e2e.rs` +1 more |
| `FM-PACK-005` | pack has no templates directory/template files | add a `templates/` directory with `.tmpl` files to the pack | `fortune5_testing_bblock_pack_e2e.rs` +5 more |
| `FM-PACK-006` | a pack file (or directory) became unreadable during composition | fix permissions on the named pack file | `none` |
| `FM-PACK-008` | pack content hash differs from the locked hash | re-lock the pack after verifying the content change is intended | `cross_pack_matrix.rs` +3 more |
| `FM-PACK-009` | `ggen.lock` unreadable or malformed | repair or regenerate `ggen.lock`; do not hand-edit | `sabotage_tests.rs` |
| `FM-PACK-010` | `git` not on `$PATH`, or `git clone` failed | install git or fix the network/source; read the wrapped clone error | `pack_e2e.rs` |
| `FM-PACK-011` | git pack checkout of the pinned version failed | verify the pin/version exists in the source repo; fix the pin marker | `none` |
| `FM-PACK-012` | read-only/dry-run resolve of an uncached git pack (mutation forbidden) | run once with network allowed to warm the cache, or pre-pin the pack | `framework_packs_e2e.rs` |
| `FM-PACK-013` | SHACL violation on the pack capability graph | fix the pack graph to satisfy the shape named in the refusal body | `composed_packs_e2e.rs` +5 more |
| `FM-PACK-014` | declared dependency absent from the resolved packs closure | declare/resolve the missing dependency pack in the consumer manifest | `annotated_pack_sync_smoke.rs` +3 more |
| `FM-PACK-015` | cross-pack depgraph kernel-refusal family code | read the wrapped kernel refusal body | `cli_boundary.rs` +2 more |
| `FM-PACK-016` | declared dependency graph contains a cycle | break the cycle in the packs' dependency declarations | `pack_capabilities_property_test.rs` +1 more |
| `FM-PACK-017` | subject of a dependency/capability query is not a resolved pack | resolve the pack first; check the name in `ggen.toml` `[packs]` | `pack_dependency_scope_e2e.rs` |
| `FM-PACK-018` | required capability has no provider in the pack closure | add a provider pack or drop the capability requirement | `annotated_pack_sync_smoke.rs` +5 more |
| `FM-PACK-019` | empty pack scope / resolved set | declare at least one pack in the manifest scope | `none` |
| `FM-PACK-020` | changed pack is not in the resolved pack set | re-resolve the scope; the changed pack's name does not match the manifest | `pack_dfcm_requirements_e2e.rs` |
| `FM-PACK-021` | pack declares `dialects` invalidly (empty set / invalid `*.ext -> dialect` mapping) | declare at least one valid `*.ext -> dialect` mapping | `pack_dialects_test.rs` |
| `FM-PACK-022` | plan content-hash mismatch, or plan missing a required key | re-run `ggen sync run` to regenerate the plan; do not hand-edit plans | `composition_plan_feed_test.rs` |
| `FM-PACK-023` | stale plan: the plan's pack set no longer matches the manifest's `[packs]` | re-run `ggen sync run` to regenerate the plan from the current manifest | `composition_plan_feed_test.rs` |

### FM-SHELL-* — shell-safety and external command launch

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-SHELL-001` | generated surface command refused by shell-safety analysis | fix the generated command; read which safety rule refused it | `frontmatter_fields_e2e.rs` +1 more |
| `FM-SHELL-002` | external command failed to launch | verify the binary named in the error is installed and on `$PATH` | `none` |

### FM-TPL-* — template parse, lint, frontmatter, shape conformance

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-TPL-001` | template has no frontmatter block | add the `---`-delimited frontmatter; see the 26-key allowlist | `none` |
| `FM-TPL-002` | unknown frontmatter key (outside the closed 26-key allowlist) | remove the key or use a listed key; the refusal names the allowlist | `combinatorial_matrix.rs` |
| `FM-TPL-003` | template body consumes a variable the SPARQL does not project | add the variable to the SELECT projection or fix the `{{ var }}` typo | `cli_boundary.rs` +1 more |
| `FM-TPL-004` | `to:` output path consumes an unprojected variable | project the variable in the query or fix the path template | `lint_validate_e2e.rs` +1 more |
| `FM-TPL-005` | identity `CONSTRUCT` detected (no-op mapping) | make the CONSTRUCT actually derive something | `lint_validate_e2e.rs` +1 more |
| `FM-TPL-006` | template file read/parse refused at sync time (path named) | fix the named template file's syntax/permissions | `none` |
| `FM-TPL-008` | `from:` escapes the template's own directory | keep `from:` paths inside the template's directory | `frontmatter_fields_e2e.rs` |
| `FM-TPL-009` | determinism guard asymmetry: primary `when:` false but recheck guard true | investigate graph nondeterminism between the two evaluations; the file was not written | `determinism_independent_reload_e2e.rs` +1 more |
| `FM-TPL-010` | SELECT without `ORDER BY` (strict mode) | add `ORDER BY` to the query, even a trivial one | `none` |
| `FM-TPL-011` | an `rdf:` frontmatter path is unreadable | fix the named RDF file's path/permissions | `none` |
| `FM-TPL-012` | `---` frontmatter delimiter not on its own line | put the opening `---` on its own line | `none` |
| `FM-TPL-013` | frontmatter block unterminated (no closing `---`) | add the closing `---` delimiter | `none` |
| `FM-TPL-014` | a `shape:`-referenced file is missing | add the named shape file or fix its path | `frontmatter_fields_e2e.rs` +2 more |
| `FM-TPL-015` | structural template-load failure (lenient glob loader) | read the per-file load errors; the glob itself resolved but files failed to load | `none` |
| `FM-TPL-017` | Tera render (string) failed | fix the Tera syntax; filter pipelines cannot chain inside `{% set %} ~` concatenation | `sync_e2e.rs` |
| `FM-TPL-023` | SHACL engine failure during frontmatter-rdf conformance | read the wrapped SHACL engine error; fix the shapes/data | `none` |
| `FM-TPL-025` | shape conformance non-conformance, fail-closed | fix the data to conform to the shape named in the refusal | `shape_shacl_enforcement_e2e.rs` +1 more |
| `FM-TPL-026` | engine error during shape conformance check | read the wrapped engine error | `none` |
| `FM-TPL-027` | doc-registered only this census: no live emission site found (mentioned at `template.rs:94` as a write.rs coercion refusal) | unresolved surface - reserved code with no live emitter; file a finding if you hit it | `none` |
| `FM-TPL-028` | unresolvable URI in template RDF handling (URI named) | fix the named URI / prefix declaration | `cross_pack_macros_e2e.rs` |

### FM-WATCH-* — watch mode

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-WATCH-001` | watch mode's initial sync failed | fix the wrapped sync error, then restart watch mode | `none` |
| `FM-WATCH-002` | filesystem watcher could not be created | fix the wrapped notify error (inotify/fsevents limits, permissions) | `none` |

### FM-WRITE-* — write planning, traversal, freeze, leases, tx journal

| Code | Trigger | Remediation | Tested by |
|------|---------|-------------|-----------|
| `FM-WRITE-001` | output root missing or cannot be canonicalized | create the root or fix `output_dir` in `ggen.toml` | `none` |
| `FM-WRITE-002` | path traversal: output escapes the manifest directory (cannot canonicalize probe) | keep output paths inside the tree the manifest directory owns | `frontmatter_fortune5_hardening_e2e.rs` +1 more |
| `FM-WRITE-003` | `inject: true` target file missing | create the target file before injecting into it | `combinatorial_matrix.rs` +1 more |
| `FM-WRITE-004` | inject `after:` marker not found in the target | add the marker or fix the marker spec | `combinatorial_matrix.rs` +1 more |
| `FM-WRITE-005` | existing file differs from render without `force` | add `force` to the rule or reconcile the file manually | `combinatorial_matrix.rs` +2 more |
| `FM-WRITE-006` | `freeze_policy: checksum` set without `freeze_slots_dir` | add `freeze_slots_dir` next to `freeze_policy: checksum` | `frontmatter_fields_e2e.rs` |
| `FM-WRITE-007` | rendered body exceeds `MAX_OUTPUT_BYTES` | split the template or cap its loops; same cap as FM-GEN-012 | `none` |
| `FM-WRITE-008` | `occurrence: Unique` selector matched more than one location | tighten the before/after marker so it matches exactly once | `framework_packs_e2e.rs` +5 more |
| `FM-WRITE-012` | stale or unreadable `FreshEpochLease` slot (exit code 5, Andon) | treat as Andon, not retry; regenerate the lease per the message | `cli_boundary.rs` +2 more |
| `FM-WRITE-020` | transactional journal band marker (`FM-WRITE-020..` disjoint band; doc-level census mention) | see the specific journal code emitted below it | `none` |
| `FM-WRITE-023` | journal line corrupt (not parseable JSON) during recovery | repair or truncate the journal at the named line; do not hand-forge records | `none` |
| `FM-WRITE-024` | journal chain break: record links a head that is not the current head | the journal was tampered/truncated; rebuild from the last consistent prefix | `none` |
| `FM-WRITE-025` | journal hash mismatch: record claims a hash the chain does not compute | the record was modified after writing; rebuild from the last consistent prefix | `none` |
| `FM-WRITE-026` | append refused: record links a head that is not the journal's current head | re-append against the current head; do not fork the chain | `none` |
| `FM-WRITE-027` | crash recovery refused: a file matches neither the before nor after state | manually reconcile the named file, then clear the journal | `none` |

## Known non-emission members (honest ledger)

These codes have no live static production emission site this census; they are
kept as rows because they appear in `src` (doc mentions, dynamic-family
witnesses) and the parity court requires every observable code to have a row:

- `FM-CHAIN-007`, `FM-CLI-042`, `FM-CLI-099` — dynamic-family witnesses in
  `src` unit tests (constructor round-trips), never emitted in production.
- `FM-TPL-027` — doc-comment mention only (`template.rs:94`); `write.rs` has
  no matching emission this census. Reserved/unresolved surface.
- `FM-WRITE-020` — band marker in the `transactional.rs` module doc, not a
  code an operator can receive.

## See Also

- `crates/ggen-engine/tests/fm_registry_parity_test.rs` (the court)
- `crates/ggen-engine/src/error.rs` (the `fm_*` minting constructors)
- `docs/jira/v26.11.3/receipts/sa-16.md` (lane receipt)
