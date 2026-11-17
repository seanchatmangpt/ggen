# v26.11.15 — gap-closing wave receipt

Scope: 19 parallel implementer lanes (L01–L19) + 1 independent verifier (L20), then
coordinator verification. Gaps were derived from the v26.10.20 follow-ups
(`docs/jira/v26.10.20/FINAL_STATUS.md` §4) and the known-broken items in CLAUDE.md; no
v26.11.15 spec existed. Every number below is a result observed in this session. Lane
reports were treated as claims, not proof.

Environment caveats (affect what could be run): `just` is not installed (recipe bodies run
via `cargo` directly); `.cargo/config.toml` hard-wires `sccache`, absent here, so every cargo
call used `RUSTC_WRAPPER=`; the disk allowance filled several times (lane runs lost to ENOSPC
were re-run, not counted as passes).

## Verified (real runs, exit 0 unless noted)

| Item | Result |
|---|---|
| `cargo build -p ggen-cli-lib --bin ggen` | exit 0 (compiles all lane library edits + the 3-crate deletion) |
| `cargo clippy -p ggen-config -p ggen-lsp -p ggen-marketplace -p ggen-mcp -p ggen-engine --all-targets -- -D warnings` | exit 0, 0 warnings |
| `ggen-cheat-scanner` scan + its own tests | ALIVE, 0 findings / 1209 files; scanner tests 20+19+3 pass |
| Dialect court `scripts/ci/dialect_declaration_court.py packs/*` | 102 PASS; negative path (dialect decl removed in a scratch copy) exits 1 |
| `ggen sync run --dry-run` | exit 0 (no fallout from empty-`RuleSet` refusal or crate deletions) |
| ggen-config (`--no-fail-fast`) | 201 pass; 4 fail = `pack_corpus_schema_test` reading external `/root/ggen-marketplace` (environmental) |
| ggen-config `schema_selector_corpus_test` (L10) | 3/3 over all tracked `ggen.toml` |
| ggen-lsp `--lib` | 242 pass, 1 fail = `chmod 000` test, meaningless as root (environmental) |
| ggen-lsp `sparql_lint_differential_test` (L13) | 2/2: author-time and sync-time agree on all 10 snippets |
| ggen-lsp E0014 analyzer + `check.rs` gate (L12) | pass |
| ggen-engine: law_engine (L01) 5, parse_errors 4, refusal_crosswalk 10 | pass |
| ggen-engine `receipt_head_log_chain_test` (L03) | 7/7; **RED-proven**: with the fix disabled 4/7 fail (empty, missing, dropped-middle, reordered) |
| ggen-engine `receipt_head_vs_log_test` | 5/5 |
| ggen-engine `pipeline_six_span_otel_test` (L15) | 2/2 (six spans in order, real durations; refused admission → ERROR-marked span) |
| ggen-engine `dialects_schema_registry_test` (L02) / `dialect_dispatch_test` | 2/2 incl. negative / 20/20 |
| ggen-engine `version_consistency_e2e` (L18) | 2/2 after fixing a real stale pin the guard found (`ggen-lsp` → `ggen-engine` 26.7.13) |
| ggen-engine `fresh_epoch_blocker_receipt_test` (L09) | 3/3 |
| ggen-marketplace `--lib` | 440 pass (incl. L19 cache-hit trust-tier tests, `load_pack_metadata` traversal test) |
| ggen-mcp lib 31, `fail_open_probe_test` 6, `write_apply_confirm_test` 7 | pass |
| ggen-graph lib 31, `law_engine_bridge_e2e` 4 | pass |

## Real defects found and fixed in this wave

1. `Installer::install_pack` cache-hit path skipped `verify_trust_tier` (L19) — now re-verifies; fails closed.
2. `ggen_pack_query` / `load_pack_metadata`: `pack_id` joined into a path unvalidated (traversal) — both now refuse non-plain names.
3. `ggen receipt verify`: a missing/empty receipt log was a loud SKIP, letting a deleted log launder a tampered head (L03) — now `[FM-CHAIN-019]`; full-chain walk added.
4. Stale `ggen-engine` self-pin in `ggen-lsp/Cargo.toml` (L18 miss, caught by its own guard).
5. Lane-authored tests that did not compile or would have failed: truncated `schema_selector_corpus_test`, missing trait import, wrong line/column expectations, missing `[generation]` in a test manifest, two `CHEAT-T03` no-assert tests, lint-header and doc-backtick issues.

## Open — NOT closed by this wave

- **Receipt chain rule — RESOLVED (rebuilt with graphlaw 26.10.13), signatures still unverified:** root cause was a hard cutover upstream: `graphlaw` 26.10.13 spells the rule `graphlaw-chain/v2-fold` and does not parse the legacy `praxis-chain/v2-fold` that this repo's committed head + 13 log records declared (the retirement plan called the string frozen and required dual-read; graphlaw did not provide it). Fix: byte-exact swap of the `chain_rule` field value only (1 head + 13 log records). Proven hash-neutral: all 211 chain hashes and the head hash (`aa02e6e7…`) are identical before/after. `ggen receipt history` now `valid: true` (211 records); before, it failed `FM-CHAIN-007` at index 198. Guard: `crates/ggen-engine/tests/receipt_chain_rule_names_test.rs` (4/4 green; 4/4 red on the pre-migration files). Audit record: `.ggen-v2/chain-rule-migration.json` (sha256 before/after). **Not verified:** the 211 Ed25519 signatures. They sign `chain_hash_hex`, which did not change, so they should still verify, but the signing key is not in this container, so `ggen receipt verify` stops at `FM-KEY-004`. Run it with the original key to confirm. No new receipt was appended (a sync here would mint a throwaway key and sign the head with it). A real `ggen sync run` was not re-run.
- **Second chain with the same legacy string, NOT migrated:** `.specify/specs/lsp-max/.ggen-v2/` (head + log). Same fix applies if wanted.
- **Version scheme:** crates.io already has ggen-graph/config/marketplace at 26.15.2 (> 26.11.15). Publish-ladder dry-runs: ggen-abb-sbb and ggen-config exit 0; ggen-graph killed by timeout; ggen-engine blocked on publish order. Nothing was published.
- `Step::DeriveN3` is in the external `graphlaw` crate; only the in-repo raw-string seam was typed (`RuleSet`).
- Dialects schema is a standalone `.specify/dialects-schema.ttl`; `ggen-pack-spec-pack` does not exist in this tree. `ggen graph validate` on it not run.
- `court-fresh-epoch`/`court-typed-refusal` were already in `pre-commit`; neither court was re-witnessed here (see `FRESH-EPOCH-BLOCKER.md`).
- Not run: `just pre-commit` and its gates (no `just`), full ggen-engine suite, ggen-lsp/ggen-cli integration suites, `ggen graph validate`, L14's `--workspace` lint widening (`just lint` stays root-only).
- L17 deleted `powl2-decompose`, `bcinr-pddl`, `bcinr-mfw-ir` (zero consumers by grep); comment-only references remain in `justfile` and `scripts/ci/guard-process-intelligence-boundary.sh` (live exclusion path, harmless).
- Docs regenerated from `.specify/repo-facts.ttl` (11 crates; sync recipes no longer called broken; cheat-scan "~464" replaced by the measured 0). `cli_proof_tests.rs` also regenerated (dropped invalid `--dry-run true` args).
