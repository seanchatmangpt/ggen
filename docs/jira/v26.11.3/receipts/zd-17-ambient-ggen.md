# zd-17: Ambient ggen reinstall 26.10.11 -> 26.11.3

Date: 2026-11-04
Lane: environment action (no repo file ownership except this receipt)

## Problem

Ambient `~/.local/bin/ggen` was 26.10.11 (built 2025-10-11) and refused
v26.11.3 receipt-log entries (FM-CHAIN-003 unknown field `generation`).
Correct per its own contract, but every skill/gate shelling out to ambient
ggen failed.

## Action

- Backup: `cp ~/.local/bin/ggen ~/.local/bin/ggen.bak-26.10.11` — exit 0
  (36,914,848 bytes, confirmed on disk).
- Source: `/Users/sac/ggen` @ `ed3f1dfb1` ("v26.11.3 CI unblock wave..."),
  workspace version 26.11.3.
- Note: `crates/ggen` does not exist in this tree; the binary crate is
  `crates/ggen-cli` (`name = "ggen"`, `src/main.rs`).
- Install: `cd /Users/sac/ggen && cargo install --path crates/ggen-cli --root ~/.local`
  — exit 0, release build 4m01s, "Replaced package `ggen-cli-lib v26.6.11 ...
  with ... v26.11.3` (executable `ggen`)".

## Verification

| Command | Exit | Result |
|---|---|---|
| `ggen --version` | 0 | `ggen 26.11.3` |
| `ggen sync run --help` | 0 | flags listed incl. `--dry-run`, `--watch`, `--skip-ea-admission`, `--format`, `--select`, `--introspect` |
| `ggen receipt verify` (in /Users/sac/ggen) | 0 | `"valid": true`, `"signature_valid": true`, `"signed": true` |
| `cd /Users/sac/ggen/self-host && ggen sync run` | 0 | previously-refusing sync now succeeds; `generated/REPOSITORY_CENSUS.md` unchanged (content identical) |

## Standing

ALIVE — exact subject `/Users/sac/.local/bin/ggen` observed executing at
26.11.3 with all four verification commands exiting 0 on 2026-11-04.

Rollback: `cp ~/.local/bin/ggen.bak-26.10.11 ~/.local/bin/ggen`.
