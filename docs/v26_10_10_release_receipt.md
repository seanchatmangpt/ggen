# v26.10.10 Release Receipt

Manufacturing receipt for the 2026-10-10 closeout. Status vocabulary per
`~/.claude/rules/operating-doctrine.md`: ALIVE items below were witnessed on the exact
subjects named; anything not re-verified at write time is marked as-of-date.

## 1. Identity

| Subject | Branch | HEAD | State |
|---|---|---|---|
| ggen main | `main` | `a303d16f6` (a303d16f60f7f3593f0c2800449cbb8cce8b0a45) | 17 modified files uncommitted in engine/marketplace (lane residue, not asserted clean) |
| strata | `main` | `a20440e` ("docs: standing table post-waves — protocol + valve ALIVE, zero known-red") | `stratus/mix.exs` modified; `docs/RELEASE.md`, `temprun/tests/clock_module_test.rs` untracked |
| ggen-marketplace | `hdit-v2-structs` | `aeca8a5a2` ("chore: drop committed erl_crash.dump") | clean-ish at spot-check |
| graphlaw | `docs/doc-hdit-scaffold-gl` | `e1059f1` ("docs: regen scaffolded skeletons with backticked identifiers") | DIRTY: Cargo.toml/Cargo.lock, src/hooks.rs, src/lib.rs, README.md + others |

30 commits landed on ggen main on 2026-10-10 (session wave merge `8e92df31f`/`7ff3b0337`
through `a303d16f6`).

## 2. Consequence (changed vs session start)

- **Praxis retirement**: vendored `praxis-core`/`praxis-graphlaw` workspace members retired;
  replaced by sibling `graphlaw` crate (SJIRA-15; commit `37958ed93` retired praxis rows,
  aligned root dep pins to 26.10.10).
- **star-toml migration**: in-code exceptions reduced 9 → 3 (commit `b40476da4`,
  "toml exceptions 9→3"). `star_toml::Validate`/`Validator` now the primary validation path
  in `crates/ggen-config/src/manifest/validation.rs`.
- **FM-PACK-018 two-tier + capability edges**: two-tier fix landed (`b97f52dcc`), unit court
  `crates/ggen-engine/tests/law_gates_closure_test.rs` (commit `b0f84930b`); capability
  edges are order-only (advisory, not gating). Capability surface tested in
  `crates/ggen-cli/tests/pack_capabilities_test.rs`.
- **Corpus annotation**: all 314 marketplace packs (`/Users/sac/ggen-marketplace/packs/`)
  carry a `[capabilities]` table — measured
  `grep -L '\[capabilities\]' packs/*/pack.toml | wc -l` = 0 (marketplace README, 2026-10-10,
  as-of-date). Strata family corrected 5 → 6 packs after ontology drift fix (`03ebdaee4`;
  marketplace `62f547b7e` fixed signer-algo/valve-refusal drift vs real sources).
- **G4 root cause + golden locks**: shape-closure keys made project-relative (`5c9da7a3e`);
  golden lock courts exist at `crates/ggen-engine/tests/sync_closure_golden_test.rs` and
  `receipt_chain_differential_test.rs`.
- **Version surface**: workspace at `26.10.10` (root `Cargo.toml:2`, internal dep pins
  aligned).
- **abb-sbb publish-ready**: `publish = false` lifted (`dcbf33333`) for the crates.io chain.
- **~40 new courts**: courts landed across commits including `b177c4b24` (property/e2e
  courts across 8 crates), `51199b102` (cheat-scanner frozen precision corpus, 19 fixtures /
  20 courts), `2364663b8` + `d9dee86b1` (five + seven courts: MCP 12-tool surface, tera
  codes, init/mine, doctor, transition receipts, SPARQL refusals), `b0f84930b`,
  `39e35b406` (graph determinism properties), `681c761`/`5651f59` (strata epoch-nonce +
  PDU-receipt interop). Exact ~40 tally is session-reported; the named commits are the
  auditable remainder.

## 3. Verification Ladder

- **Verifier pass 4: GO, zero red, 0 warnings** — session-reported; the named court commits
  above are the auditable trace. Not re-executed for this receipt.
- **Witness adjudication**: churn-era failures re-adjudicated green at HEAD — auditable via
  `crates/ggen-graph/audit/transcripts/*` (e.g. `13_adjudicate_gall_promotion.stdout`:
  `test result: ok. 31 passed; 0 failed`) and `v26_10_10_phase1_receipt.md` (verifier V18
  closing pass, all gates PASS, 4 discrepancies found and reconciled incl. the
  `ggen_bin()` invocation fix, suite 11 passed / 0 failed).
- **Feature matrix 6/6** — session-reported, not re-executed for this receipt.
- **graphlaw re-baseline 412/0 all-features** — session-reported against graphlaw
  `e1059f1`+dirty; no on-disk transcript located at write time. Replay command below is the
  falsifier.

## 4. Replay (cold checkout)

```sh
# ggen main suites (subsets named in the courts above)
cargo test -p ggen-engine --test sync_closure_golden_test
cargo test -p ggen-engine --test receipt_chain_differential_test
cargo test -p ggen-engine --test law_gates_closure_test
cargo test -p ggen-cli --test pack_capabilities_test
cargo test -p ggen-cheat-scanner          # frozen precision corpus, 20 courts
just test                                  # full ladder (30s hot -> 600s cold)
just pre-commit                            # gate list lives in justfile recipe

# marketplace corpus invariant
grep -L '\[capabilities\]' /Users/sac/ggen-marketplace/packs/*/pack.toml | wc -l  # expect 0

# graphlaw re-baseline (412/0 falsifier)
cd /Users/sac/graphlaw && cargo test --all-features

# regeneration
ggen sync run                              # receipt at .ggen-v2/receipt.json
ggen receipt verify

# publish dry-runs
cargo publish -p ggen-abb-sbb --dry-run
cargo publish -p ggen-engine --dry-run    # after abb-sbb lands on crates.io
```

## 5. Standing + Remaining Sequence (all USER GATED)

| Step | Standing | Gate | Prep doc |
|---|---|---|---|
| 1. Publish `ggen-abb-sbb` to crates.io | ALIVE (court-tested, `publish` lifted) | USER GATED | `docs/v26_10_10_repo_state_and_library_usage_report.md` |
| 2. Publish `ggen-engine` (depends on 1) | BLOCKED on step 1 | USER GATED | same |
| 3. graphlaw 3-commit split + updated gate | PARTIAL_ALIVE (e1059f1+dirty) | USER GATED | graphlaw `docs/branch-disposition.md` |
| 4. Tag v26.10.10 + push ggen main | BLOCKED on 1-2 | USER GATED | `docs/v26_10_10_phase1_receipt.md` |
| 5. Ambient ggen reinstall | BLOCKED on 4 | USER GATED | `aa20c07f3` workspace-binary preference guard is the regression court |

## Falsifiers

- Any replay command above failing at the named SHAs invalidates this receipt.
- A `[capabilities]`-less pack appearing after 2026-10-10 invalidates the corpus claim
  (as-of-date).
- graphlaw test count other than 412 passed / 0 failed at `e1059f1`+dirty invalidates the
  re-baseline claim.
