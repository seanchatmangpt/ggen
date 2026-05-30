<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Gall Foundation Retrofit — with `ggen sync` as the Only Actuation](#gall-foundation-retrofit--with-ggen-sync-as-the-only-actuation)
  - [The one-sentence thesis](#the-one-sentence-thesis)
  - [Sensing is not actuation](#sensing-is-not-actuation)
  - [The Chatman Equation, with μ pinned to sync](#the-chatman-equation-with-%CE%BC-pinned-to-sync)
  - [The house-repair calculus](#the-house-repair-calculus)
  - [What a Gall pier is (actuation-centric)](#what-a-gall-pier-is-actuation-centric)
  - [Rest condition](#rest-condition)
  - [Why "only actuation" is the bridge](#why-only-actuation-is-the-bridge)
  - [Product-category statement](#product-category-statement)
  - [See also](#see-also)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Gall Foundation Retrofit — with `ggen sync` as the Only Actuation

> Explanation. The bridge from a pre-Chatman project (value trapped in private
> implementation motion) to a post-Chatman project (a Gall foundation aligned to public
> ontologies). This rewrite states the load-bearing constraint the ggen run discovered:
> **`ggen sync` is the only actuation.** Everything else senses, routes, or inspects.

## The one-sentence thesis

> Every pre-Chatman project becomes post-Chatman through Gall Foundation Retrofit to
> public ontologies — and in a retrofitted project, **the only thing that actuates
> consequence is `ggen sync`.**

Not a rewrite. Not "add agents." Not "modernize the stack." A *foundation replacement* in
which exactly one command moves load.

## Sensing is not actuation

The ggen run proved a delivery plane: a real fixture
(`playground/proof/broken-construct.rq`) produced the **same `E0011` diagnostic and the
same `template.values-inline` route** through LSP, MCP, and A2A
([delivery-plane-proof](../reference/cli/delivery-plane-proof.md),
[GALL-INTEGRATION-1 receipt](../receipts/GALL_INTEGRATION_1_RECEIPT.md)). One route
engine, three transports, no drift.

But note what those three transports *did*: they **read** a file and **returned** a
diagnostic + a `route_id`. They wrote nothing. They changed no durable state. They emitted
no receipt.

| Surface | What it does | Does it actuate? |
|---------|--------------|------------------|
| LSP (`ggen lsp check`) | reads law surfaces, returns diagnostics + routes | **No** — read-only sensing |
| MCP (`repair_route`) | returns the canonical RouteEnvelope | **No** — proposes intent |
| A2A bridge | carries the same route to a remote actor | **No** — projection of intent |
| **`ggen sync`** | runs μ₁–μ₅, gates, writes artifacts, emits a receipt | **YES — the only actuator** |

LSP/MCP/A2A are the **author-time route surface** — they sense the work and name the
action. They are eyes and nerves, not hands. `ggen sync` is the hand. A `route_id` is a
*proposal to actuate*; the actuation happens only when `ggen sync` runs and a receipt is
written.

This is not an aspiration — it is observed behavior. `ggen lsp check` on the playground
returned `E0011` and exited without touching the tree. `ggen sync` on the playground ran
six quality gates, reached *DMAIC Phase 2: Measure*, and **refused to proceed** ("No
inference rules defined — measurement system not capable"). The sensing surfaces report;
the actuator gates and either manufactures or refuses. Refusal is the actuator's honest
"no artifact was produced."

## The Chatman Equation, with μ pinned to sync

Base equation:

```
A = μ(O*)
```

Receipt-bearing form:

```
R ⊢ A = μ(O*)
```

The retrofit pins the operator:

```
μ ≜ ggen sync          (the one and only manufacturing actuation)
O* = the closed, public-ontology-aligned work boundary (ggen.toml + ontology + queries + templates)
A  = the counted artifact actuation produced
R  = the receipt ggen sync emits, proving A followed from O*
```

So in a retrofitted project:

```
A_counted = ggen_sync(O*)
R ⊢ A_counted
```

Because μ is a single command, the rule "no counted work without a receipt" is
**mechanically enforceable**: every artifact traces to exactly one `ggen sync` actuation
bearing exactly one receipt. The route surfaces *cannot* manufacture phantom state,
because they structurally cannot write — actuation is funnelled through one gated
chokepoint.

Pre-Chatman projects collapse this distinction:

```
implementation path = authority path
help text = capability claim
a command exiting 0 = "it worked"
```

Post-Chatman, the only authority is the actuator's receipt:

```
sense (LSP/MCP/A2A route)
→ actuate (ggen sync)
→ gate (DMAIC / quality gates — refuse if incapable)
→ artifact + receipt (R ⊢ A = μ(O*))
→ replay / metrics
```

## The house-repair calculus

A cracked foundation is not repaired by decorating the rooms above it. The sequence:

| House | ggen |
|-------|------|
| shore the house | freeze the release boundary ([v26.5.28 boundary](../reference/release/v26-5-28-boundary.md)) |
| jack the load | inventory real commands / crates / docs / fixtures |
| expose cracks | find Oracle Gaps, phantom docs, fake success ([oracle-gaps](oracle-gaps.md)) |
| replace the footing | Gall integration tests aligned to public ontologies |
| transfer the load | route every command's *actuation* through `ggen sync` |
| inspect | receipts, replay, metrics, the [command-proof matrix](../reference/cli/command-proof-matrix.md) |
| build upward | capability-map — *only after the foundation rests* |

Gall's Law underneath: a working complex system grows from a working simple system; you
cannot patch a non-working whole into working. The retrofit obeys it by replacing the
foundation with simple, inspected piers — and by ensuring every pier's *consequence* is
produced by the same simple actuator (`ggen sync`), not by N divergent write paths.

## What a Gall pier is (actuation-centric)

```
GallPier(x) ⇔
  ∃ fixture f, public-ontology mapping o, test t, receipt r:
    sense(x, f)        → a route/diagnostic   (LSP/MCP/A2A — read-only)
    ggen_sync(O*_x, f) → evidence e + receipt r   (the ONLY actuation)
    Belongs(e, o)      (the evidence lands in public-ontology footing)
    t proves the expected durable state
    Replay(r) reconstructs e
```

The pier is not proven by the route surface alone (that is sensing). It is proven when
`ggen sync` actuates the change and a replayable receipt records it.

For author-time surfaces, transport parity is still required — but as a property of the
*sensing* layer feeding the one actuator:

```
fixture → {LSP, MCP, A2A} → same diagnostic, same route_id → one ggen sync actuation
```

## Rest condition

```
Rest(B) ⇔
  ∀ x ∈ B: GallPier(x)                    every kept surface has a pier
  ∧ EveryActuation(x) routes through ggen sync   no side-door writers
  ∧ DocsMatchBuild                         (why-docs-must-match-the-build)
  ∧ NoOracleGaps(B)
  ∧ ReplayComplete(R) ∧ MetricsReadable(M)
```

A release rests only when the only way to change its world is `ggen sync`, every actuation
leaves a receipt, and every receipt replays. ([sabbath-grade-done](sabbath-grade-done.md))

## Why "only actuation" is the bridge

Pre-Chatman reliability tactics — more tests, more docs, more observability, more agents —
all leave multiple uncontrolled write paths. Adding agents to that substrate only
accelerates the old failure mode: more motion, more claims, more drift. The retrofit
changes the substrate by making **one** gated actuator the sole producer of consequence:

```
private motion (many writers, no receipts)
→ public-ontology footing
→ Gall pier
→ ONE actuator: ggen sync
→ receipt / replay / metrics
→ counted consequence
```

That is why agents can scale safely *after* retrofit and not before: there is exactly one
place where the world changes, it is gated, and it leaves proof.

## Product-category statement

```
Gall Foundation Retrofit is the category.
ggen is the foundry.
The Chatman Equation (A = μ(O*), μ = ggen sync) is the calculus.
Public ontologies are the footing.
Gall tests are the inspection.
LSP/MCP/A2A are the author-time sensing/route surface.
ggen sync is the ONLY actuation.
Receipts, replay, and metrics prove the load moved.
Rest is the terminal condition.
```

ggen retrofits itself first — its own command surface proven by Gall piers whose
consequence is actuated only by `ggen sync` — then manufactures retrofit packs for other
projects.

> Every existing project needs a Gall Foundation Retrofit before autonomous work can
> scale. In a retrofitted project, the only hand that moves load is `ggen sync`.

## See also

- [The Genesis Run](genesis-run.md) — separate → … → actuate → inspect → rest
- [Motion does not count](motion-does-not-count.md) — why sensing ≠ consequence
- [Oracle Gaps](oracle-gaps.md) — claims without actuation
- [Sabbath-grade done](sabbath-grade-done.md) — the rest gate
- [Delivery-plane proof](../reference/cli/delivery-plane-proof.md) — the sensing-surface parity evidence
- [GALL-INTEGRATION-1 receipt](../receipts/GALL_INTEGRATION_1_RECEIPT.md) — the first load-bearing pier
