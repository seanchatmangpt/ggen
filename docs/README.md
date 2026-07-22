# Documentation

Start with the four pages written for exactly this purpose — each grounded in a real,
verified-this-pass run against the codebase, not reconstructed from what this kind of doc usually
says:

| Page | What it's for |
|------|----------------|
| [GETTING_STARTED.md](GETTING_STARTED.md) | Install, build, run a real sync, verify a receipt chain. Every command was actually run; output is transcribed. |
| [FAQ.md](FAQ.md) | Real questions about this codebase — crate/pack counts, `ggen-core`'s fate, the two `ggen.toml` schemas, whether `just pre-commit` passes — each tied to a specific file or command. |
| [PERFORMANCE_QUICK_START.md](PERFORMANCE_QUICK_START.md) | The one performance command (`just slo-check`) that's actually automated, and a real run of it against this codebase — including a currently-reproducing Phase 1 bug, even though the command as a whole exits `0`. |
| [../CONTRIBUTING.md](../CONTRIBUTING.md) | Development workflow, Chicago TDD testing policy, PR process. |

For the repository root's project overview, quickstart, and known-limitations summary, see
[../README.md](../README.md).

## Everything else

`docs/` holds a large, long-lived documentation tree — well over 100 files and subdirectories
covering architecture, the pack marketplace, the Level-5 pack-promotion program, research notes,
ADRs, planning history, and more. **[INDEX.md](INDEX.md)** is the actively-maintained directory
map for all of it — a full tree with a currency note per directory (what's live, what's archived,
what's flagged for a maintainer decision). Start there for anything not covered by the four pages
above.

A few entry points worth naming directly, because they're the ones a new reader most often wants:

- **Architecture / crate map** — `../CLAUDE.md` and `../.claude/rules/architecture.md` at the
  repo root are the actively-maintained sources; `reference/workspace/crates.md` under this
  directory is a lighter-weight sibling.
- **`sync` command reference** — [reference/ggen_sync_manual.md](reference/ggen_sync_manual.md)
- **Pack / marketplace architecture** — [marketplace/ARCHITECTURE.md](marketplace/ARCHITECTURE.md)
- **Pack maturity tracking** —
  [l5-promotion/L5_PROMOTION_PROGRAM.md](l5-promotion/L5_PROMOTION_PROGRAM.md) (the
  12-capability Level-5 bar, per pack) and
  [packs/PACK_MATURITY_MODEL.md](packs/PACK_MATURITY_MODEL.md) (a narrower, stricter,
  not-yet-reconciled sibling calibration — the two currently disagree in places; treat the L5
  program doc as more current, and re-check both before trusting either as a snapshot of "how
  mature is pack X right now").
- **Claims / release-standing ledger** — [aps/README.md](aps/README.md) and its `claims.toml`:
  a machine-readable claim → falsifier → evidence → standing record for release-relevant
  assertions about ggen. This is the authoritative source for "does X actually work," ahead of
  any prose (including this page) — a falsifier is a command you can re-run yourself.
- **Agent implementation guides** — [agent/README.md](agent/README.md)
- **Performance documentation hub** (broader than the Quick Start above; includes some
  aspirational/not-yet-automated targets, clearly marked as such) —
  [performance/README.md](performance/README.md)

## A caution about this directory

Several subdirectories are explicitly flagged in `INDEX.md` as unreconciled duplicates or
archive-adjacent content that was never moved into `archive/` (`explanations/` vs.
`explanation/`, `how-to-guides/` vs. `how-to/`, `preserved/`). That flagging is itself accurate
as of `INDEX.md`'s own last-verified date — check that date before assuming today's directory
listing still matches its description one-for-one, and prefer a fresh `ls docs/` over trusting
any cached directory count (including any specific number quoted in this file or `INDEX.md`) if
the exact figure matters to what you're doing.
