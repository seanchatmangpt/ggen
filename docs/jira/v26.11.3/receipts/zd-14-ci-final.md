# ZD-14: CI Final Fixes (fmt, census, repair idempotency, EA graph, book drift)

- Repo: `/Users/sac/ggen`, branch `v26-11-3-wave`, base HEAD `ed3f1dfb1` (working tree; NOT committed per lane law — coordinator owns git transitions).
- Scope files: `crates/ggen-abb-sbb/src/generated/refusals.rs`, `examples/affidavit-verify/ea.graph.json` (new), `.github/workflows/clap-noun-verb-zero-code.yml`, `.github/workflows/examples-repair-constructor.yml`, `.github/workflows/fortune5-bblock-normalize.yml` (verified only, no edit), `examples/zero-code-inventoryctl/docs/command-reference.md` + `SCHEMA_ADMITTED.md`, `book/src/CAPABILITY_MAP.md`, `book/gap-closure.toml`, this receipt.
- Note: `examples/affidavit-verify/ggen.lock` is a new untracked sync artifact (see Fix 3).

## Fix 1 — refusals.rs fmt (CI fmt gate)

- **Why the ×83 sweep missed it**: it did not miss it. XD-13's receipt names `crates/ggen-abb-sbb/src/generated/refusals.rs` in its 83-file list. The file was re-clobbered by post-sweep regeneration: commit `12b460c2e` (Nov 3 17:27, AFTER sweep commit `ed3f1dfb1` Nov 4 01:22) regenerated it unfmt'd from `templates/typed-boundary-refusal-rust.tera`. Landed-byte conflict, not a fmt-skip: no `#[rustfmt::skip]` attribute, no rustfmt.toml `ignore` entry (verified: `crates/ggen-abb-sbb/rustfmt.toml` carries `# ignore = []  # Requires nightly`, commented out).
- **Live flap risk (disclosed)**: during this session a concurrent lane's root `ggen sync run` (witness: file mtime 01:50 PST) regenerated the file UNFORMATTED again after I formatted it (~01:47) — the template emits blank lines between enum variants and doubled sentence-ending periods, which rustfmt strips/fixes. Every root sync re-arms the fmt gate. Permanent fix is making the template emit rustfmt-clean bytes; the template is outside this lane's scope (coordinator). Per brief, the fmt-gate-vs-generator conflict resolves in favor of the CI gate: formatted.
- Gates: `rustfmt --check crates/ggen-abb-sbb/src/generated/refusals.rs` → exit 0; `cargo fmt --all -- --check` → exit 0 (02:00 PST 2026-11-04; concurrent root syncs can re-clobber).

## Fix 2 — census allowlist (clap-noun-verb-zero-code.yml)

- Found: `.github/workflows/clap-noun-verb-zero-code.yml:48` "Assert the consumer has only three authored source files".
- The ea.graph.json widening itself was already landed by XD-14 (assert names exactly {ea.graph.json, ggen.toml, ontology.ttl}). The step's ACTUAL defect: it counts raw top-level files with `find`, but commit `ed3f1dfb1` committed the generated artifacts (Cargo.toml, ggen.lock, src/, tests/, docs/, .ggen-v2/) into `examples/zero-code-inventoryctl/` — so CI would see 5 files and `test ! -e Cargo.toml` would fail.
- **After**: census computes the authored set via `git ls-files` excluding the committed generated projections (`:!​.ggen-v2 :!​src :!​tests :!​docs :!​Cargo.toml :!​Cargo.lock :!​ggen.lock`), asserts exactly {ea.graph.json, ggen.toml, ontology.ttl}, then asserts each excluded path is git-tracked (`git ls-files --error-unmatch`) so the zero-code posture stays honest: the generated artifacts are tracked projections of `ggen sync run`, never authored files.
- Gates: census command executed under bash in `examples/zero-code-inventoryctl` → prints exactly the 3 authored files, count 3, all 5 generated paths tracked; `bash -n` on the workflow's extracted `run:` blocks → exit 0; `yaml.safe_load` → exit 0.

## Fix 3 — interview-assist repair idempotency (examples-repair-constructor.yml)

- **Before**: line ~80 `assert old in s` against `examples/interview-assist/ggen.toml`'s absolute pack path — if the repair already landed in-tree, the assert fails.
- **After**: `assert old in s or new in s` — mirrors XD-18's Fix-3 pattern (tai-enterprise-rebuild block, lines 83-87). Write is a no-op replace when `new` already present.
- Gates: `yaml.safe_load` → exit 0; `bash -n` on extracted `run:` blocks → exit 0. Not executed live (workflow targets CI's fresh checkout).

## Fix 4 — fortune5-bblock-normalize.yml commit guard

- **No edit needed — already fixed at HEAD**: the "Commit bounded normalized bytes" step already carries the empty-diff guard (`if [ -z "$changed" ]; then echo '...already normalized'; exit 0; fi` before the allowlist check and `git commit`), so the empty-commit `set -e` kill cannot fire. Verified: YAML parses; guard present at HEAD `ed3f1dfb1`; coordinator brief on this item was stale.
- The allowlist regex and scoped rustfmt (XD-18 Fix 4) verified in place, unmodified.

## Fix 5 — ea.graph.json for examples/affidavit-verify (new)

- Modeled on the 3 XD-14 graphs (inventoryctl / part-passport / self-host): strategy → capability → ABB → contract (CONSTRUCT ceiling, 3 required ports) → SBB (`pack:affidavit-pack`, 5 artifacts: src/affidavit_{types,certify,catalog,lib_wiring}.rs + docs/affidavit_reference.md — the pack's actually-generated outputs per the sync receipt) → qualification pinned to kernel-computed digests.
- Kernel-pinned digests (from `validate_ea_graph` first run with placeholders):
  - sbb_digest = `sha256:e0b4fecd5d5f2b457f6646b5624f80fd83ad4d1051c94589c2ef667b8d61000e`
  - contract_digest = `sha256:3557f51812f66bc4cd805e650cd5aee7f987011c9aa2a79ded98b0855600fc67`
  - graph_digest = `sha256:12d1c9d1f63d99b14daae5d38fe5b609f3dbd14845a9101d801b049cdc014a04`
- Gates: `cargo run -q -p ggen-abb-sbb --example validate_ea_graph -- examples/affidavit-verify/ea.graph.json` → `ADMITTED sbb:affidavit-manufacture (qualification qual:affidavit-manufacture)`, exit 0. `ggen sync run` in `examples/affidavit-verify` (built `target/debug/ggen`, v26.11.3) → exit 0; all 9 outputs "unchanged: content identical"; produced new untracked `ggen.lock` (coordinator: commit or gitignore per inventoryctl precedent — inventoryctl tracks it).

## Fix 6 — EOF blank lines (new docs)

- `examples/zero-code-inventoryctl/docs/command-reference.md` and `SCHEMA_ADMITTED.md`: stripped trailing blank line at EOF. Disclosed: command-reference.md is a sync-generated artifact (re-derivable trailing blank may come from the pack template — root cause not traced to the template this session; flagged for the pack lane).
- Sweep found the trailing-blank-line class is repo-wide (~100 files: most v26.11.3 receipts, README.md, CHANGELOG.md, book appendix/sparql pages, tcps docs). Out of lane scope — flagged for coordinator; not edited.

## Fix 7 — "337" book-map drift + praxis-core-pack witness binding

- Investigation: `book/tests/test_gap_closure_chicago.py::test_no_stale_pattern_337_claim_remains` failed on `book/src/CAPABILITY_MAP.md:75` (`XVII. TCPS Complete Pattern — 225–302, 337`); `test_every_pack_row_has_real_manifest_and_witness_state` failed on the `packs/praxis-core-pack` PACK_WITNESS row — praxis-core crate retired SJIRA-15, and `grep praxis-core-pack examples/*/ggen.toml` → zero live consumers (only pack-local and test-file references).
- **After**: XVII row `225–302, 337` → `225–302` (stale 337-jikoken-kensa page citation removed); praxis-core-pack row downgraded `**PACK_WITNESS**` → `**PARTIAL**` with evidence boundary reworded ("Pack templates and ontology; praxis-core crate retired (SJIRA-15), no live examples/* consumer"); the VIII alignment row's witness list drops `praxis-core-pack`.
- Follow-through: the downgrade creates a declared gap, so `book/gap-closure.toml` gained `[[obligation]] id="pack-praxis-core"` + `[evidence."praxis-composition"]` (path `crates/ggen-engine/tests/pack_compose_capabilities_e2e.rs`, marker `corpus_pack("praxis-core-pack")` — real composition test exercising the pack by name, witnessed at lines 72-98).
- Gates: `python3 book/scripts/validate_gap_closure.py` → `obligations=26 declared_gaps=26 evidence_records=18 used_evidence=18 errors=0`, exit 0; `test_no_stale_pattern_337_claim_remains` and `test_every_pack_row_has_real_manifest_and_witness_state` now pass; suite `python3 book/tests/test_gap_closure_chicago.py` → 1 remaining failure `test_active_turtle_is_valid_and_archive_quarantine_is_exact` — **pre-existing, out of lane scope**: `examples/industry/manufacturing-dflss/generated/shacl-manufacturing-shapes.ttl` line 8, `Bad syntax (Prefix "https:" not bound)`. Disclosed for coordinator (industry lane or book lane).

## Verification ladder summary (all run this session)

| Gate | Command | Exit |
|---|---|---|
| fmt workspace | `cargo fmt --all -- --check` | 0 |
| fmt refusals.rs | `rustfmt --check crates/ggen-abb-sbb/src/generated/refusals.rs` | 0 |
| YAML parse ×3 | `python3 -c yaml.safe_load` (zero-code, repair-constructor, fortune5) | 0 ×3 |
| bash -n ×2 | extracted `run:` blocks (zero-code, repair-constructor) | 0 ×2 |
| Census command | git ls-files census in examples/zero-code-inventoryctl | exactly 3 authored files |
| EA graph kernel | `cargo run -p ggen-abb-sbb --example validate_ea_graph -- examples/affidavit-verify/ea.graph.json` | 0, ADMITTED |
| Consumer sync | `ggen sync run` in examples/affidavit-verify | 0 |
| Gap validator | `python3 book/scripts/validate_gap_closure.py` | 0, errors=0 |
| Book Chicago suite | `python3 book/tests/test_gap_closure_chicago.py` | 2 fixed; 1 pre-existing failure (industry TTL, out of scope) |

## Standing

- Fixes 1-3, 5, 7: ALIVE (gates ran this session, exits cited).
- Fix 4: ALIVE-as-verified (no edit; already fixed at HEAD, brief stale).
- Fix 6: PARTIAL (owned docs fixed; repo-wide class flagged not fixed).
- Not committed — coordinator owns the integration commit.
