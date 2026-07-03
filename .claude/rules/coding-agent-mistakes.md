---
version: 26.7.2
last_updated: 2026-07-03
gate: mandatory — read before every agent dispatch
---

# Coding-Agent Mistakes — Mandatory Gate

> **The strongest single rule:** Every coding-agent patch must either deepen authority or reduce drift.
>
> This means: the patch must make the authoritative path harder to bypass, make bypasses fail loudly, or remove a bypass that already exists. A patch that adds a feature while leaving the old bypass intact does not satisfy this rule.

---

## The Five Mistake Classes (quick reference)

1. **Decorative Completion** — a command exits 0 and prints success, but no
   durable state changed (e.g. `ggen sync` says "complete" but
   `.ggen/receipts/` gained no new file).
2. **Epistemic Bypass** — logic that should come from the RDF ontology/SPARQL
   query is hardcoded inline instead (the code "knows" something it should
   only "ask" `.specify/*.ttl` for).
3. **Fail-Open Behavior** — a missing required resource or violated
   constraint is logged as a warning instead of returning `Err(...)`.
4. **Legacy Path Contamination** — a new authoritative path was built, but
   the old bypass wasn't removed, and (being simpler) gets hit in practice.
5. **Contract Drift** — a receipt, lockfile, or other proof object no longer
   accurately describes what actually ran (stale/empty/default fields).

Real files to check against, not invented examples:
- `crates/ggen-cli/src/cmds/sync.rs` — sync command authoritative path
- `crates/ggen-graph/` — deterministic hashing, deltas, transition receipts
- `crates/ggen-marketplace/src/rdf/control.rs` — profile enforcement
- `.ggen/packs.lock`, `.ggen/receipts/` — actual local state (verify with
  `jq`, not assumptions, before claiming a field is populated correctly)

---

## The 6-Question Patch Contract

Every agent patch must answer all six questions before the patch is accepted.

1. **What real state changed?** Not stdout — name the file, DB row, or
   in-memory structure that differs after the patch runs.
2. **What authoritative path did this patch touch?** Name the actual
   module/file (see list above), not an invented stage name.
3. **What negative path now fails correctly?** Describe the sabotage
   condition and the expected non-zero exit / error message.
4. **What invariant protects this patch from drift?** State the concrete
   rule (e.g. "receipt `signature` must be non-empty and verified against
   the receipt body").
5. **What legacy path was removed or blocked?** If none, explain why none
   exists — silence here is a red flag.
6. **What proof object shows it worked?** Reference the receipt, a passing
   test (`just test`), or an OTEL span — not just "I read the code and it
   looks right."

---

## The Strongest Single Rule

> **Every coding-agent patch must either deepen authority or reduce drift.**

**Deepening authority**: the authoritative path becomes harder to bypass —
a `#[must_use]` return forces callers to handle a receipt, a typestate
prevents calling `render()` before `validate()` compiles, a SPARQL query
replaces a hardcoded `Vec`.

**Reducing drift**: proof objects more accurately reflect what ran — a hash
is computed at the real point in time instead of defaulting to `""`, a
signature is verified against the receipt body instead of trusted blindly.

**A patch that does neither is noise at best, contamination at worst.** If
you cannot answer "this deepens authority" or "this reduces drift" for your
patch, stop and reconsider before submitting.

---

**See also:**
- [Andon Signals](andon/signals.md) — stop-the-line protocol
- [OTEL Validation](otel-validation.md) — proof via spans for LLM/external services
- [Testing](rust/testing.md) — Chicago TDD; real collaborators for sabotage tests
