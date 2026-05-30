<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Sabbath-Grade Done](#sabbath-grade-done)
  - [The rest gate](#the-rest-gate)
  - [The receipt is the Sabbath signature](#the-receipt-is-the-sabbath-signature)
  - [The corollary for agents](#the-corollary-for-agents)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Sabbath-Grade Done

> Explanation. What "done" means for a ggen release, and why rest is a *consequence*,
> not a *decision*.

> God rested because the work inside the boundary was done.

"Sabbath-grade done" is the standard a ggen release must meet before it can rest. It is
not "good enough," not "shippable with known issues," not "we'll fix it in a patch." It
is: **the work inside the declared boundary is complete, and there is therefore nothing
left to do.** Rest follows from completion; it is never declared in spite of incompleteness.

## The rest gate

A release rests only when **all** of these hold, re-run and captured as the receipt's
evidence:

- `cargo make check` — compiles, whole workspace, no errors.
- `cargo make lint` — clippy clean, `-D warnings`, no `#[allow]` papering over signals.
- `cargo make test` — the full suite green; ignored tests triaged (each either runs or is
  lawfully, explicitly excluded — never silently skipped).
- `cargo make slo-check` — performance SLOs met.
- `cargo make audit` — no known vulnerabilities.
- `git status` clean — no WIP in the tree; every file committed, reverted, or explicitly
  classified out of boundary.
- **No Oracle Gap** — no `todo!`, no `unimplemented!`, no theory-only command. Every
  command in the boundary [works by executable proof or is removed from it](oracle-gaps.md).
- **Docs match the build** — every documented command/flag/crate exists and behaves as
  written. ([Why docs must match the build](why-docs-must-match-the-build.md).)

## The receipt is the Sabbath signature

The release receipt (`.ggen/receipts/<ts>.json`, Ed25519-signed, receipt-chained) is
written **only after** the rest gate is green — never before. It is not a promise that
the work will be done; it is the signature that it *is* done. An empty `signature` field,
or a receipt written ahead of the gate, is [contract drift](motion-does-not-count.md) —
the proof object lying about the world.

This is why a ggen release cannot be rushed by writing the receipt early. The receipt
derives its meaning from the gate it follows. Sign it before completion and it certifies
nothing.

## The corollary for agents

An agent operating on a ggen release has exactly one lawful terminal state per slice:
**"no remaining work in this slice."** Not "progress reported." Not "known issue filed."
Not "blocked, see above." If the gate is red, the agent keeps working until it is green —
because the easiest way to pass validation is to fix the issue.

## See also

- [The Genesis Run](genesis-run.md) — rest as the final step, lawful only after `complete`
- [Motion does not count](motion-does-not-count.md) — why a green command is not yet done
- [v26.5.28 boundary](../reference/release/v26-5-28-boundary.md) — what "inside the boundary" means for this release
