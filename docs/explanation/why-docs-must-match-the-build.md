<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Why Docs Must Match the Build](#why-docs-must-match-the-build)
  - [Docs fiction is legacy-path contamination](#docs-fiction-is-legacy-path-contamination)
  - [The standard: Diátaxis × combinatorial maximalism](#the-standard-di%C3%A1taxis-%C3%97-combinatorial-maximalism)
  - [What "matches the build" means operationally](#what-matches-the-build-means-operationally)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Why Docs Must Match the Build

> Explanation. The documentation corollary of the Genesis Run, and the rule governing
> this rewrite (DOCS-REST-1).

> No release can rest while the docs describe a different machine than the one that builds.

Documentation is not a description *of* the product that sits beside it. For a Genesis
Run, documentation is **part of the boundary** — a working production surface that must
be as true as the code. A doc that describes a phantom crate, a removed command, or a
future capability as a present one is the same defect as a command that works only in
theory: an [Oracle Gap](oracle-gaps.md), in prose.

## Docs fiction is legacy-path contamination

When two documents describe the machine — one true, one stale — readers hit the simpler
or more prominent one, and the two drift further apart with every edit. This is exactly
[legacy-path contamination](motion-does-not-count.md): the old path was never removed, so
it competes with the new one.

ggen's docs estate carried this defect directly. At the start of DOCS-REST-1 there were
**two competing Diátaxis trees**:

- `docs/diataxis/{tutorials,how-to,reference,explanation}` — older framing
  (e.g. "8-operator", "manufacturing your first artifact").
- top-level `docs/{tutorials,how-to,reference}` — packs/marketplace-era content.

Plus concrete fiction: a `reference/template-directives.md` for `ggen template *`
commands that were removed; `how-to/{a2a,mcp}/` guides for nouns being archived; and
architecture docs that once described ~68 crates when the real workspace is **15**.

## The standard: Diátaxis × combinatorial maximalism

The rewrite uses the [Diátaxis](https://diataxis.fr/) four-mode split — tutorials,
how-to guides, reference, explanation — because the four serve genuinely different reader
needs (learning, doing, looking up, understanding) and conflating them is its own kind of
drift.

To that, DOCS-REST-1 adds **combinatorial maximalism**: every release-critical surface
(install, headless CLI, LSP, MCP, A2A, pack manifest, field evidence, replay, metrics,
Oracle-Gap closure, release receipt, command proof) must have a place in **all four
modes** — plus a runnable example and a playground path — where it works, is explained,
and can rest. No surface gets only explanation; no surface gets only reference.

## What "matches the build" means operationally

- **Reference is generated or checked**, never written from memory. The crate list comes
  from `Cargo.toml` `members`; the command list from `cmds/mod.rs`; the feature flags from
  `[features]`. ([Crates reference](../reference/workspace/crates.md),
  [feature flags](../reference/workspace/feature-flags.md).)
- **Every documented command is proven** — it appears in the
  [command-proof matrix](../reference/cli/command-proof-matrix.md) with a real backing
  test, or it is removed from the release docs.
- **No example that does not run; no playground step that cannot be reset and rerun.**
  Examples and the playground are gated on the command proof they demonstrate — they are
  authored only once the command they show is proven, never before.

## See also

- [The Genesis Run](genesis-run.md) — docs as part of the boundary that must `complete` before `rest`
- [Sabbath-grade done](sabbath-grade-done.md) — "docs match reality" is a rest-gate condition
- [Documentation index](../README.md) — the one canonical Diátaxis surface
