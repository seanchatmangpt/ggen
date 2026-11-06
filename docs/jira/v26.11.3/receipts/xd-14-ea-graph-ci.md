# XD-14 EA Graph CI Remediation Receipt

Standing: REFUSED

Lane: XD-14. Date: 2026-11-04. Repo: `/Users/sac/ggen`, branch `v26-11-3-wave`.
Scope files: `ea.graph.json` (root), `examples/zero-code-inventoryctl/ea.graph.json`,
`examples/part-passport/ea.graph.json`, `self-host/ea.graph.json`,
`docs/jira/v26.11.3/receipts/xd-14-ea-graph-ci.md`; one disclosed workflow edit in
`.github/workflows/clap-noun-verb-zero-code.yml`.

## Root cause (re-derived, not assumed)

FM-LAW-015 (`crates/ggen-engine/src/sync.rs`, `run_ea_graph_admission`, fail-closed
since WO-01 2026-11-03) refuses any sync whose project root lacks a valid
`ea.graph.json` in which every `candidate_sbbs` entry admits with CONSTRUCT authority
against a `QUALIFIED` qualification pinned to kernel-computed digests
(`sbb_content_digest`/`contract_digest`, `crates/ggen-abb-sbb/src/lib.rs::admit_sbb`).

Local-vs-CI asymmetry answer: there is **no** environment asymmetry. No local
`ea.graph.json` existed for any of the three consumer roots (`examples/zero-code-inventoryctl`,
`examples/part-passport`, `self-host`), none is gitignored (nothing in `.gitignore`),
none is generated-at-sync. Local runs fail identically when actually executed — CI
simply executes the gate. Confirmed by reproduction before the fix (all three dirs,
`ggen sync run` → `[FM-LAW-015] ... ea.graph.json is absent`).

Four failing checks map to:

| Check | Root |
|---|---|
| `clap-noun-verb-zero-code` | `examples/zero-code-inventoryctl/ea.graph.json` absent |
| `part-passport` | `examples/part-passport/ea.graph.json` absent |
| `ggen-self-host` (3 sync steps) | `self-host/ea.graph.json` absent |
| `ggen-root-dogfood` / manufacture-own-repository | root `ea.graph.json` tracked but 4 of 5 SBBs unqualified (`SBB_UNQUALIFIED`: docs-diataxis-pack, gdmcp-pack, github-actions-pack, admission-kernel-witness) |

Note: self-host's sync refuses on `FM-PACK-013`
(`GGEN_SELF_HOST_OBSERVATION_INCOMPLETE`) **before** the EA gate is reached; that
refusal is pre-existing and outside this lane's scope (self-host observation data /
`010_observation_complete.rq`). The `self-host/ea.graph.json` artifact is authored,
kernel-validated, and admits all of its SBBs, so FM-LAW-015 is satisfied for that
root once the observation lane lands its fix.

## Fix

1. **Authored three real EA graphs** (not `--skip-ea-admission`, not empty-SBB
   bypass fixtures): each declares strategy/capability/ABB/contract/SBB modeling
   what the root actually manufactures, with SBB digests and contract digests pinned
   to kernel-computed values via
   `cargo run -p ggen-abb-sbb --example validate_ea_graph -- <path>` and
   `QUALIFIED` qualifications binding `sbb_digest`/`contract_digest`.
2. **Qualified the 4 unqualified root SBBs**: added
   `qual:docs-diataxis-pack`, `qual:gdmcp-pack`, `qual:github-actions-pack`,
   `qual:admission-kernel-witness` to the root `ea.graph.json`, each pinning the
   SBB's existing declared digest (already kernel-exact) and the kernel-computed
   contract digest; precedent is the existing `qual:mdbook-pattern-language-pack`
   row in the same file.
3. **Disclosed workflow edit** (not an opt-out): the zero-code workflow's
   "Assert the consumer has only two authored source files" step hard-counts
   top-level files; committing `ea.graph.json` makes it 3. Bumped 2→3 and named
   `ea.graph.json` in the exact-set assertion
   (`.github/workflows/clap-noun-verb-zero-code.yml`). The second assertion at
   the former line 164 only counts `ggen.toml`/`ontology.ttl` and needed no edit.
   The later replay step (`test ! -e Cargo.toml` class checks) runs after sync and
   is unaffected. `ggen-self-host.yml` and `part-passport.yml` have no file-count
   assertions; no edits.

## Verification (all run this session)

- `cargo run -q -p ggen-abb-sbb --example validate_ea_graph -- <each of 4 graphs>`:
  all `validate: ok`; all 8 SBBs ADMITTED (5 root + 3 consumer), zero REFUSED.
- Root dogfood sequence: `./target/debug/ggen sync run` → exit 0;
  `./target/debug/ggen receipt verify` → exit 0;
  `python3 self-host/scripts/verify_root_regeneration.py` → exit 1 with
  `missing: []` — the exit 1 is the pre-existing whole-branch uncommitted-diff
  noise (every lane's in-flight edits are listed as "unauthorized", including this
  lane's three uncommitted artifacts), not a sync-output failure; in CI the
  candidate is committed and clean. Re-check after the coordinator commits.
- `examples/zero-code-inventoryctl`: `ggen sync run` → exit 0; all 8 declared
  outputs present (Cargo.toml, src/main.rs, src/lib.rs, generated_{cli,behavior,
  boundaries}.rs, tests/zero_code_cli.rs, docs/command-reference.md). Previously
  `[FM-LAW-015] ... is absent`.
- `examples/part-passport`: `ggen sync run` → exit 0; generated
  `crown-kernel.json` + `crown-kernel.nameplate.txt` byte-identical pre/post
  (`cmp` clean, `git diff` empty) — the EA graph admits manufacture without
  perturbing the drift-court artifacts.
- `self-host`: `ggen sync run` → exit 1 on `FM-PACK-013
  GGEN_SELF_HOST_OBSERVATION_INCOMPLETE` (pre-existing, other lane). FM-LAW-015 no
  longer reachable-blocks this root (artifact valid + admits).
- Generated zero-code outputs left **untracked** (matching CI's
  generate-fresh-per-run contract; part-passport keeps its committed drift-court
  files untouched).

## Standing

ALIVE for three of the four checks (zero-code, part-passport, root dogfood sync +
receipt verify); PARTIAL for `ggen-self-host` (FM-LAW-015 satisfied, blocked
downstream on the observation gate owned by another lane). Fail-closed law held:
no `--skip-ea-admission` anywhere in `.github/workflows/`; the SBB qualification
was fixed, not the gate.
