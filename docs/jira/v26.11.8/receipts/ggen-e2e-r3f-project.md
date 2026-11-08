# Lane V8-R3-f — ggen e2e repair R3 (project suite re-pin)

Standing: PASS

- **Subject**: /Users/sac/ggen @ 360bc169b (HEAD at lane start; two parallel lanes landed mid-flight under the coordinator — template/marketplace/graph suites landed in b0f5681f8/727573f1c/360bc169b, not this lane's diff)
- **Owned file**: `crates/ggen-cli/tests/integration_project_e2e.rs` (single file, disjoint from the in-flight `integration_ontology_e2e.rs` + `src/cmds/init.rs` edits visible in the tree — not touched)
- **Survey actions executed** (docs/jira/v26.11.8/receipts/ggen-e2e-repair-survey.md, project row): re-pin init→root `ggen init`; gen/plan→`sync run` smoke over the scaffold; delete `--type`/`--preset`/`--dry-run` flag tests; name-validation tests deleted on witnessed premise-rot.

## Deltas

12 tests → 7 tests, 12 ignored → 0 ignored.

Re-pins (all witnessed against `target/debug/ggen` version 26.11.5, 2026-11-08):
- new/init → root `ggen init` (asserts ggen.toml, Makefile, schema/, templates/)
- plan/gen → init scaffold + `[validation] strict_mode = false` + `sync run --skip-ea-admission` → asserts `ontology-summary.txt` on disk (same recipe as R2's ontology re-pin)
- help → `init --help` ("Usage: ggen init")
- invalid-verb → `ggen init invalid-verb` rc=1 (witnessed)
- new: `test_project_sync_rerun_stable` — double `sync run`, asserts byte-identical rendered summary (replaces the deleted preset tests with a real-collaborator determinism probe)

Deletions (cited):
- `--type`/`--output`/`--preset`/`--dry-run` tests: flags gone with the `project` noun
- `test_project_new_invalid_type`: flag gone
- `test_project_init_empty_name` + `test_project_init_whitespace_name`: **premise-rot deletion** — the pinned "validation rejects empty/whitespace name" capability does not exist on the moved surface. Witnessed: fresh-dir `ggen init --name ""` and `--name "test project"` both rc=0 with the scaffold written and `name = ""` verbatim in ggen.toml; the earlier rc=1 observations (also witnessed) were the already-initialized refusal, not name validation.

## Gates (verbatim, 2026-11-08)

- `cargo test -p ggen-cli-lib --test integration_project_e2e -- --test-threads=2` → **7 passed / 0 failed / 0 ignored**
- Ignored tails (0 reds behind ignore flags): template_e2e 0p/0f/3 filtered; marketplace_e2e 0p/0f/17 filtered; graph_e2e 0p/0f/6 filtered
- Unfiltered: template 3/0, marketplace 17/0, graph 6/0 (parallel lanes' files, re-verified this lane post-landing)
- `cargo test -p ggen-engine --lib -- --test-threads=2` → **244 passed / 0 failed / 2 ignored**

## See Also

- Survey: docs/jira/v26.11.8/receipts/ggen-e2e-repair-survey.md
- Prior lanes: acc295ec6 (F1), 2f9ba5bc5 (R2), 6afbbe8b3 (cli_ux), b0f5681f8 (R1-f template deletions)
