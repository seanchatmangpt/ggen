# Examples Health Receipt — v26.10.10

Lane: examples-health. Subject: /Users/sac/ggen @ main, HEAD acaf8714f+ (uncommitted lane edits elsewhere in tree; examples/ and packs/ clean per `git status --porcelain`). Date: 2026-10-10.

## 1. guard-pack-proofs (pre-commit proof gate) — FAIL

Command: `./scripts/ci/guard-pack-proofs.sh` (justfile:577 recipe run standalone).
Exit: **1**.

Tally:
- Built receiptctl example release binary: OK (one dead_code warning, `src/w4pm_algorithms_catalog.rs:702` `by_wasm_export`).
- `ggen sync run` on examples/receiptctl: **FAILED** — `[FM-PACK-008]` pack `chicago-tdd-tools-pack` content-hash mismatch: ggen.lock pins `blake3:b7222389...`, pack on disk hashes `blake3:4dbdd842...`.
- Later proof steps (ggen-cli-verify etc.): not reached.

Provenance: the drift is **committed, not session-introduced** — `git status --porcelain packs/chicago-tdd-tools-pack examples/receiptctl` is empty; last touch was commit 8e92df31f (star-toml migration wave), which changed the pack without re-locking receiptctl's ggen.lock.

## 2. examples/7-agent-validation — DOES NOT EXIST

`ls: No such file or directory`. Root `Cargo.toml:123` still carries `exclude = ["examples/7-agent-validation", ...]` and comments at :101/:117 reference it. Dead exclude entry / stale doc — the crate was removed and the exclusion not cleaned up. Not rot in code; rot in repo metadata.

## 3. Smoke of other examples (cargo check, tail line)

| Example | Result | Exit |
|---|---|---|
| examples/star-toml-verify | Finished dev profile, 10.98s | 0 |
| examples/ggen-cli-verify | Finished (cached), 0.12s | 0 |
| examples/rmcp-verify | Finished dev profile, 10.87s | 0 |
| examples/wasm4pm-verify | Finished (cached), 0.13s | 0 |

(53 dirs total under examples/, incl. `_archive/`, `archive*/` — not swept.)

## 4. Verdict on the pre-commit proof gate

The gate is exercising **real state** — it caught a genuine, committed pack/lock drift that a static check would miss. But it is currently **red on main HEAD**: any `just pre-commit` run today fails at guard-pack-proofs before reaching downstream gates. Remediation (out of this lane's scope): delete `examples/receiptctl/ggen.lock` to intentionally re-lock, or restore the pack to its pinned hash. Secondary finding: prune the dead `7-agent-validation` exclude from Cargo.toml.

## See Also

- `justfile:522` (pre-commit gate chain), `scripts/ci/guard-pack-proofs.sh`
