# The Genesis Run

> Explanation. This page tells you *why* ggen is built and released the way it is.
> For *how* to do a thing, see [how-to](../how-to/). For *what* a thing is, see
> [reference](../reference/).

ggen is built and released as a **Genesis Run**: an ordered passage from undifferentiated
motion to a boundary that can rest. The ordering is not decoration — each step is a
precondition for the next, and the run does not rest until the last step holds.

```
separate → name → bind → fill → rule → inspect → complete → rest
```

| Step | Meaning in ggen | Concrete surface |
|------|-----------------|------------------|
| **separate** | Distinguish motion from value, contact from consequence | A command exiting `0` is motion; a durable state change is value |
| **name** | Declare the boundary explicitly | The v26.5.28 release boundary — which crates, which commands |
| **bind** | Fix each named thing to a real implementation | A noun in `cmds/mod.rs` binds to a verb that does real work |
| **fill** | Make the bound thing actually do the work | The μ₁–μ₅ pipeline; real Oxigraph; real lockfiles |
| **rule** | Constrain it so misuse fails loudly | Feature flags, `--locked`, profile gates, fail-loud refusals |
| **inspect** | Observe the real state by command, not by description | `cargo make check && lint && test`; command proof; OTEL spans |
| **complete** | No remaining work inside the boundary | No WIP, no Oracle Gap, no theory-only command |
| **rest** | Sign the receipt — and only then stop | The release receipt is the Sabbath signature |

## The equation

ggen precipitates code from an ontology:

```
A = μ(O*)
```

`A` (the artifact) is produced by applying the pipeline `μ` to a curated ontology `O*`.
The Genesis Run is the discipline that keeps `A` *true to* `O*` — that the artifact the
machine emits is the artifact the specification names, and that nothing is shipped that
the specification does not justify.

## Why the ordering is load-bearing

You cannot `fill` what you have not `bound`. You cannot `bind` what you have not `named`.
And — the rule this whole release turns on — **you cannot `rest` before you `complete`**.

The temptation in software is to rest at `fill`: the command runs, the screen says
success, so the work feels done. The Genesis Run refuses that. `fill` is followed by
`rule` (make misuse fail loudly), then `inspect` (prove it by command), then `complete`
(nothing left), and only then `rest`. Resting at `fill` is how Oracle Gaps — commands
that work only in theory — get shipped.

## See also

- [Motion does not count](motion-does-not-count.md) — why a passing command is not value
- [Sabbath-grade done](sabbath-grade-done.md) — what `rest` requires
- [Oracle Gaps](oracle-gaps.md) — what happens when you rest at `fill`
- [Why docs must match the build](why-docs-must-match-the-build.md) — the documentation corollary
