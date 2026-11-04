# ZD-38: Advisory Ignores — quality.yml `cargo audit` Hard Gate

Lane: ZD-38 (create/update agent). Branch: `v26-11-3-wave`. Date: 2026-11-04.
Owner files: `deny.toml` ([advisories] only), `.audit.toml` (new),
`.cargo/audit.toml` (new — **deviation from assignment**, see below),
`Cargo.lock` (tokio bump — disclosed minimal fix, see below), this receipt.

## Finding (ZB-8)

`quality.yml:39` (the "Replay retained quality evidence" step, `set -euo
pipefail`) hard-gates bare `cargo audit`. Pre-lane state: `cargo audit` exit 1
(`error: 2 vulnerabilities found!` — RUSTSEC-2026-0194/0195, quick-xml 0.37.5,
upstream-blocked by process_mining ^0.37.4 per
`docs/jira/v26.11.3/receipts/xc-8-cve-bump.md`).

## What landed

1. **`deny.toml` [advisories]** — replaced the commented-out template ignore
   list with:
   - `{ id = "RUSTSEC-2026-0194", reason = "..." }` and
     `{ id = "RUSTSEC-2026-0195", reason = "..." }` (cargo-deny 0.20.2 parses
     `{id, reason}` tables; verified by run), each reason citing
     `docs/jira/v26.11.3/receipts/xc-8-cve-bump.md`.
   - `unmaintained = "workspace"` / `unsound = "workspace"` — cargo-deny 0.20
     removed the lint-level downgrade for these classes (fields now take
     scope values: all/workspace/transitive/none); `workspace` errors only on
     direct deps, so transitive-only unmaintained/unsound findings surface as
     warnings. **No ID-level ignores for RUSTSEC-2024-0436 (paste),
     RUSTSEC-2024-0449 (unsafe-libyaml), or RUSTSEC-2026-0386 (tokio)** —
     paste and unsafe-libyaml are transitive-only (scope covers them), and
     tokio was fixed, not ignored.
   - NOT landed: `version = 2` (docs: field no longer used; class lint-level
     fields `vulnerability`/`notice`/`severity-threshold` removed and now
     emit config errors), and `vulnerability`/`yanked` overrides (removed in
     0.20 / default warn).

2. **`.cargo/audit.toml` (new)** — **the operative cargo-audit config**.
   Measured finding: cargo-audit 0.22.2 does **not** read `.audit.toml` (tested
   empirically: `.audit.toml` and root `audit.toml` both had zero effect on
   exit code). Its documented config locations are `~/.cargo/audit.toml` or
   `./.cargo/audit.toml`. Schema: **bare advisory ID strings only** —
   `{id, reason}` table form is a hard parse error
   (`invalid type: map, expected a string`), witnessed. So the ignore list
   there is bare strings with the registered-divergence reason carried in the
   adjacent comment block + mirrored reason-stringed entries in `deny.toml`.

3. **`.audit.toml` (new)** — kept per the assignment, but with a header note
   that cargo-audit 0.22.2 does not load it; the operative file is
   `.cargo/audit.toml`. Kept in sync (bare strings + reason comment).

4. **`Cargo.lock`: tokio 1.53.1 → 1.54.1** — **disclosed out-of-owner-file
   fix** (owner-file discipline preserved for everything else).
   RUSTSEC-2026-0386 (tokio AsyncFd unsound) affected a direct dep with an
   in-range fix (>=1.53.3), so it was fixed by `cargo update -p tokio` rather
   than ignored or scope-suppressed. `cargo check --workspace` exit 0 after.

## Gates (run this session, actual exits)

| Gate | Before | After |
|---|---|---|
| `cargo audit` (the quality.yml gate, bare, no flags) | exit 1 — `error: 2 vulnerabilities found!` (RUSTSEC-2026-0194/0195) | **exit 0** |
| `cargo deny check advisories` | exit 1 — 5 errors (2 vulns + unmaintained paste/unsafe-libyaml + unsound tokio) | **exit 0** — `advisories ok` |
| `cargo check --workspace` | exit 0 (pre-existing, xc-8) | exit 0 (post tokio bump) |
| `cargo audit --ignore RUSTSEC-2026-0194 --ignore RUSTSEC-2026-0195` | — | exit 0 (flag path, no config file) |

cargo audit still prints the 7 warning-class findings (unmaintained ×3,
unsound salsa, yanked ×2) as allowed warnings — `warning: 7 allowed warnings
found` — the gate surface is unchanged, only the 2 hard vulnerabilities are
registered-divergence ignored.

## Registered-divergence law compliance

Reason strings: `deny.toml` carries `{id, reason}` entries citing
xc-8-cve-bump.md. `.cargo/audit.toml` cannot carry machine-parsed reasons
(schema takes bare strings on 0.22.2 — parse error otherwise, witnessed), so
the reason lives in the file's comment block and is mirrored in deny.toml.
Close condition (unchanged from xc-8): a process_mining release on
quick-xml >= 0.38 enables `cargo update -p quick-xml --precise >= 0.41`, at
which point both ignore entries are deleted, not left rotting.

## Not committed

Diff left uncommitted per assignment.
