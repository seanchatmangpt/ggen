# GALL-CHECKPOINT-001B — Hard Gate: Stale-Clear Evidence

**This is a blocking reconciliation gate, not optional.** Promoted into the checkpoint
because it is the first proof the living LSP understands NEGATIVE consequence:
a diagnostic must DISAPPEAR when the broken relation is repaired — even when the
repair happens on a *different* source-law surface than the one the diagnostic
was published on.

## The invariant

> A project-graph diagnostic must be cleared on the URI where it was published,
> even when the repair happened on a different source-law surface.
>
> Diagnostic publication is REPLACEMENT, not append-only memory.

The unit of analysis is no longer file text — it is the **rule relation**
(ggen.toml rule: query ⇒ template ⇒ output). The diagnostic is published on the
template URI; the repair may occur in the .rq. The template URI must still be
recomputed and re-published (with an EMPTY set when now lawful).

## stale_clear_evidence — required scenario

```
Given:
  query SELECTs ?name
  template consumes row["title"]
  → template URI receives GGEN-TPL-001

When:
  query changes to SELECT ?name ?title
  template file is UNCHANGED

Then:
  server recomputes the project graph
  server publishes diagnostics for the affected TEMPLATE URI
  diagnostics for that template URI are empty / GGEN-TPL-001-free
  observe_diagnostics sees the disappearance
  RepairApplied → GatePassed → ReceiptEmitted path is available (state.rs:188-219)
  no emitted output file is written
```

## Agent 5 must inspect server.rs (Agent 1) for THREE things
1. Cross-surface recompute on `.rq` / `.sparql` / `ggen.toml` change (not just the edited file).
2. Affected-template-URI set is computed EVEN WHEN the resulting diagnostics are empty.
3. `publish_diagnostics(template_uri, empty_vec, None)` (or equivalent clear) IS called on repair.
   - Codebase precedent for the empty-publish idiom: `did_close` at server.rs:149
     already calls `publish_diagnostics(uri, Vec::new(), None)`.

## Two valid motions the living LSP must have
- **raise:** publish non-empty diagnostics for affected template URI when relation broken.
- **clear:** publish EMPTY diagnostics for affected template URI when relation repaired.

## If Agent 1 omitted the empty-publish-clear
Orchestrator patches `server.rs` directly (no running agent owns it post-completion),
routes the cleared set through `observe_diagnostics`, and adds the stale-clear
regression test. This is a reduce-drift fix, in scope for 001B.

## Checkpoint acceptance (unchanged standard)
- GGEN-TPL-001 fires live.
- `ggen lsp check` fails on invalid projection.
- Query-side repair clears the diagnostic from the template URI.
- observe_diagnostics remains the lifecycle path.
- LSP writes no emitted artifacts.
