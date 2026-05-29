# v26.5.28 Release Boundary (Reference)

> Reference + release artifact. The named boundary of the v26.5.28 Genesis Run: what is
> inside (must build, work, and be proven before rest), what is gated, and what is
> archived out. Nothing rests until everything inside this boundary is green.

## The boundary

The boundary is **the whole ggen product** — not a subset. The rest gate
([Sabbath-grade done](../../explanation/sabbath-grade-done.md)) applies to the whole
workspace: `cargo make check && lint && test && slo-check && audit`, all green.

## Command surface decisions

| Class | Nouns | Status |
|-------|-------|--------|
| **KEEP / PROVE** (default surface) | `init`, `sync`, `graph`, `pack`, `policy`, `doctor`, `utils` | Each must reach PROVEN in the [command-proof matrix](../cli/command-proof-matrix.md) |
| **GATED** (opt-in delivery plane) | `lsp` (+ its 10 verbs, incl. `serve --protocol mcp`) | Behind `--features lsp`; proven by the `ggen-lsp` suite |
| **ARCHIVE** (out of default surface) | `a2a`, `framework`, `mcp`, `sigma`, `wizard` | Behind default-off `experimental` feature; code preserved (non-deletion doctrine) |

## Why these five are archived, not deleted

Per the [non-deletion doctrine](../../explanation/oracle-gaps.md), code is fossil
evidence — nothing is deleted. The five ambiguous nouns are **gated behind a default-off
`experimental` Cargo feature**, which removes them from the default CLI surface (because
`clap-noun-verb` discovers nouns from `pub mod` declarations) while keeping every line in
the tree. This ends the false advertisement (the Oracle Gap) without destroying work.

> **Archive-gate mechanism (closure in progress):** add `experimental = []` to
> `crates/ggen-cli/Cargo.toml` and `#[cfg(feature = "experimental")]` to the
> `pub mod a2a; framework; mcp; sigma; wizard;` lines in `cmds/mod.rs`. This page records
> the *decision*; [feature-flags reference](../workspace/feature-flags.md) records the
> *flags that exist*. When the gate lands, `experimental` is documented there.

## Rest-gate conditions (the Sabbath signature)

The v26.5.28 release receipt (`.ggen/receipts/<ts>.json`, Ed25519-signed) is written
**only** when all hold:

- [ ] `cargo make check && lint && test && slo-check && audit` — all green, whole workspace
- [ ] `git status` clean — no WIP; every file committed, reverted, or classified out
- [ ] Command-proof matrix — every KEEP/PROVE noun at **PROVEN**; the 5 archived nouns gated
- [ ] No Oracle Gap — no `todo!`/`unimplemented!`/theory-only command
- [ ] Docs match the build — DOCS-REST-1 complete; one canonical Diátaxis surface
- [ ] Version = `26.5.28` across `Cargo.toml`, the version-declaring crates, and docs

## Known open items inside the boundary (honest backlog)

These are tracked, not hidden — the boundary cannot rest until each is closed:

1. **Archive-gate the 5 nouns** — add `experimental` feature + cfg the `pub mod` lines.
2. **`pack add` lockfile fix** — wire `install_pack` to `PackInstaller::update_lockfile`
   so `.ggen/packs.lock` is actually written (PROOF-PENDING → PROVEN).
3. **Verifying build** — run the closure-lane edits (ggen.construct fail-loud; version
   strings 26.5.21→26.5.28 in `ggen-lsp-a2a`/`ggen-lsp-mcp`) through one green build.
4. **DOCS-REST-1** — collapse the two legacy Diátaxis trees into this one canonical
   surface; runnable examples + playground (gated on proven commands).

## See also

- [Sabbath-grade done](../../explanation/sabbath-grade-done.md) — the rest gate in full
- [Command-proof matrix](../cli/command-proof-matrix.md) — per-noun proof status
- [Crates](../workspace/crates.md) · [Feature flags](../workspace/feature-flags.md)
