# LSP-Max Cognition: PRD/ARD

Product and architecture requirements for the direction that emerged from this session's
LSP/LSIF Gall-checkpoint work: using `lsp-max` not for its usual job (editor tooling) but as a
**push-shaped signal source for agent loops**, and the deterministic-vs-deliberative dispatch
question that direction raises once you ask what should consume those signals.

Full execution history, evidence, and file:line citations for everything summarized here live in
`/Users/sac/.claude/plans/80-20-gall-test-refactor-cheerful-quokka.md` (CP0-CP20). This document
is the durable statement of *why* and *what's next*; the plan file is the checkpoint-by-checkpoint
receipt log. lsp-max experiments will land under `examples/` later — this doc precedes that work,
not replaces it.

## 1. Problem statement

MCP's default interaction shape is pull: an agent calls a tool, waits, gets an answer. LSP's
diagnostic/notification surface is push: the server tells the client something changed without
being asked. The obvious "LSP for agents" move — definition/references, pull-shaped symbol lookup
— turned out to be a dead end: neither `tower-lsp` nor `auto-lsp` (real frameworks, both audited
this session) implement it; both leave symbol resolution to the language author, because it's
genuine language-specific work, not protocol boilerplate. The actual opportunity is the *easy*,
already-solved part of LSP nobody had pointed at MCP before: push signaling.

This session proved both halves of that pipeline are independently real and buildable:

- `lsp-max` already has a working `publishDiagnostics` push path (compositor deposit → merge →
  flush → real notification), and a second, narrower push (`lspMax/admissionChanged`, wired in
  CP10) triggered by a real state transition, not a poll.
- `ggen-mcp` already has, in the `rmcp` version it actually links (1.8.0, not 0.11.x), real
  server-initiated push methods (`Peer<RoleServer>::notify_resource_updated`) that were completely
  unused before CP12 wired one signal (`GGEN-TPL-001`) through a real end-to-end test — an actual
  MCP client, over a real `tokio::io::duplex` transport, receiving a real notification and reading
  back real content via `resources/read`.

Neither ecosystem had connected these two idle capabilities before. That's the concrete finding,
not "LSP can push" (true, not new) or "MCP can receive pushes" (true, not new) — it's that nobody
had wired the two together.

## 2. What's proven, what's missing

```
lsp-max: signal --> compositor merge/flush --> publishDiagnostics (real, tested editor push)
                                            \-> fitness-file write --> lsp-max-mcp (real, but
                                                                        POLLED, not pushed)
                          lspMax/admissionChanged --> ONE real push, CP10, nothing subscribes yet

ggen-lsp/ggen-mcp: GGEN-TPL-001 fires --> bridge::push_diagnostics_for_root
                                       --> Peer::notify_resource_updated --> real MCP client
                                           (mechanism proven; no production trigger wires it yet)

MISSING: notification arrives --> ??? --> action executes
                                   \-- this box does not exist. Push delivery is solved;
                                       deliberation-free DECISION-MAKING about a push is not
                                       even started, and getting that wrong is dangerous, not
                                       just slow.
```

See the plan file's CP15-CP20 for the concrete production-hardening list (watcher wiring, peer
lifecycle, backpressure, signal coverage, observability) and CP13/CP14 for the safety cleanup this
gap analysis already produced (removing `lsp-max-andon`, a confirmed stub with zero callers;
quarantining a real-but-dormant unattended-write hook chain found by direct audit).

## 3. Safety posture (audited, not assumed)

Two full read-only audits this session (grounded, file:line evidence, not doc-reading) found:
**no live path today lets a signal cause an unattended write with zero human/LLM decision step**,
in either repo. But two real facts lower the bar for a future dispatcher to be misused:

- `ggen_write_apply`'s only gate is a caller-supplied `confirm: true` boolean with zero
  independent corroboration, and a JSON-RPC-bypassing internal call into it already exists
  (`crates/ggen-mcp/src/selfplay/board.rs`, sandboxed to a test harness today).
- `lsp-max`'s `src/runtime/mesh_hooks.rs` contains a real, working "signal → unattended
  `std::fs::write`" hook chain (a customer-refund demo, unrelated to LSP-Max's purpose),
  confirmed dead on every live path but structurally one `register_hook` call from going live
  with no confirmation gate.

**Design rule for any future dispatcher**: the trigger→action mapping must be a declared,
inspectable fact (same pattern as CP6's stub contracts), never logic buried in a handler —
otherwise you've moved opacity from "the LLM decided" to "some unaudited Rust function decided,"
which is worse, not better.

## 4. Tool classification: lookup vs. decision

The dispatch question generalizes past diagnostics to MCP tools broadly. Splitting the ecosystem
along one axis:

- **Lookup tools** (recognition, not decision): `ggen_pack_capabilities`, `semantic_search`,
  schema introspection, dependency-graph queries, dry-run previews (`ggen_sync_dry_run` — read-
  only, but *about* a pending decision, so it belongs on the push side too: "here's what would
  happen if you synced now" is exactly the kind of thing worth surfacing unprompted). These don't
  need an LLM to decide *whether* to run, only *when the result matters* — the same trigger→
  dispatch gap named in §2.
- **Decision tools**: `ggen_write_apply`, anything committing to an action with consequences,
  anything requiring judgment about ambiguous intent. No amount of making these push-triggered
  removes the need for a decision-maker in the loop — it only changes who's watching for the
  trigger.

Two caveats that don't disappear under this framing:
- **Cost/staleness is a second, orthogonal axis.** A lookup tool with external cost (rate-limited
  API) needs the same backpressure work as CP18's diagnostic case. A lookup tool without a
  structured staleness signal (lumen's own docs: *"a stale index is UNKNOWN, not evidence of
  absence"*) risks pushing a confidently-wrong result, which is worse than not pushing — LSP's
  document version numbers exist precisely to prevent this class of bug; an equivalent is needed
  here.
- **The verifiability gap bounds the scope.** CP6/CP9's contract-tagging pattern worked because
  ggen controlled and could audit both the ontology and the implementation it described. Applying
  a lookup/decision tag to third-party MCP tools broadly means trusting self-reported metadata
  with no fidelity check available (most third-party MCP servers are opaque). The pattern is real
  and reusable, but only for locally-inspectable tools, not "the MCP ecosystem" unscoped.

## 5. Three-tier cognition framework for text diagnostics

A parallel generalization: this project's own steering rules (`CLAUDE.md`, `.claude/rules/*.md`)
are already LSP-shaped diagnostics over natural language — checked by an LLM self-applying them
every turn, the expensive path the push argument says shouldn't be the default for deterministic
checks. The corrected (not merely asserted) picture, verified against real prior art in this
session rather than left theoretical:

1. **Syntactic/lint** — regex, zero inference. Banned-phrase checks, missing required sections,
   formatting rules. Transfers cleanly; LSP already runs over prose today (`vale`, `markdownlint`
   both ship real LSP servers — the protocol layer needs no reinvention).
2. **Pattern/rule-based** — cheap, deterministic, not merely string-matching. Two real, *already-
   existing* implementations in this ecosystem, not hypothetical:
   - **ELIZA** (`~/wasm4pm/docs/breeds/eliza.md`, module `src/breeds/frame.rs`): priority-sorted
     keyword rules, first-match reassembly template, fallback response — Weizenbaum's actual 1966
     mechanism, real fixture transcribed from the original CACM paper. Handles *presence*-shaped
     violations (a bad phrase is or isn't there).
   - **N3/Datalog** (`praxis-graphlaw`, this workspace's real native N3/Datalog/SPARQL/SHACL
     engine): handles *absence-relative-to-a-claim* violations via negation-as-failure — the exact
     mechanism already in production this session as `gates/010_required.rq`'s
     `FILTER NOT EXISTS { ?subject rdfs:label ?label }`. Ported to text: `?subject` is the turn,
     the required correlate is an evidence marker, and "dismissal keyword present AND no evidence
     marker present" is one N3 rule, not a new mechanism.
3. **Genuine semantic judgment** — still needs a model. Not "was a referent cited" (tier 2 can
   check that), but "was the citation any good" — evaluating whether cited content actually
   supports the claim requires understanding meaning, which tiers 1-2 cannot do regardless of how
   well the rule is encoded.

The corrected boundary is not syntactic-vs-semantic (the original framing, wrong) but
presence-shaped vs. absence-relative-to-a-claim vs. content-evaluation-shaped. The first two tiers
are real, cheap, and already have working infrastructure in this ecosystem; only the third
requires an LLM in the loop, and it requires it close to every time that specific judgment is the
actual ask — not rarely, the way a genuinely ambiguous symbol-resolution case might be rare in the
code-diagnostics domain.

## 6. Open, unverifiable-from-here questions

Named explicitly rather than smoothed over, per this project's own evidence-first discipline:

- **The escalation-rate bet.** What fraction of real trigger events (diagnostics firing, gate
  transitions, admission changes) are genuinely deterministic vs. needing real judgment? This is
  the entire load-bearing assumption behind any "Nx cheaper" claim for a dispatcher — the
  multiplier is definitionally equal to the escalation rate's inverse, not an independent finding,
  and it hasn't been measured.
- **The ecosystem-adoption bet.** Whether the compounding-verification discipline (two-proof
  pattern, drift-injection tests, refusing to fake blocked wiring) pays off depends on this
  becoming a substrate other work builds on, not staying a one-off integration. Every test of the
  pattern *held* this session (real bugs caught, a fresh agent operating correctly on unfamiliar
  facts) — that's evidence of soundness, not evidence of payoff. A sound pattern can still lose to
  "ship fast" if the ecosystem never accumulates enough sessions to cash in the compounding.
- **In-generation vs. post-hoc enforcement.** Unlike a Rust file (static, diagnosed after a human
  writes it), the "document" under diagnosis in §5 is generated live by the same model that would
  consult the diagnostic. Whether tier-1/2 checks should run as a post-hoc lint pass (flag, don't
  prevent — what self-application already does) or as an in-generation constraint (closer to
  constrained decoding) is an open design fork with no code-diagnostics analog.

## 6a. Correction: the 5 `GGEN-*` codes are the cheap first tier, not a completeness claim

§5's three-tier framework was written in the abstract (for text/prose diagnostics). Applied back
to `ggen` itself, concretely: `ggen_check_project`'s 5 `GGEN-*`/`E00xx` codes plus the SHACL
analyzer are tier-1/2 work — regex/pattern and SPARQL/Tera variable-binding checks over static
project files, cheap and fast, with none of them running the real pipeline. An earlier version of
`ggen_check_project`'s tool description implied broader coverage than this by omission (it
enumerated the codes without saying what they can't see). That was corrected in
`crates/ggen-mcp/src/lib.rs`'s `tool_defs!` macro: a clean `ggen_check_project` result means "no
cheap-tier problems found," not "this project will sync cleanly." Graph-load failures, pack
resolution errors, receipt-chain tampering, and write-time refusals are invisible to this tier by
construction — they require escalating to `ggen_sync_dry_run` (CP26, runs the real pipeline
without writing), `ggen_receipt_verify` (CP27, checks an existing receipt's chain hash and
signature), or the `ggen-sync-refusal://` push notifications (CP28, real sync refusals surfaced as
they happen). This is the same tier-1/2-vs-tier-3 boundary §5 already drew for prose diagnostics,
not a new mechanism — the fix here was to state it plainly for `ggen`'s own tool surface rather
than leave it implicit.

## 6b. A fourth, narrower tier: bounded unattended writes (CP31-36)

§6a's tier-3 escalation path (`ggen_sync_dry_run`/`ggen_receipt_verify`/`ggen-sync-refusal://`
push) is entirely human/LLM-reviewed — every write still requires a decision step. A later EOD
requirement asked for a fully autonomous ggen loop, which surfaced a real tension already named in
§3's safety audit: a bare signal→`ggen_write_apply` call with zero decision step is the exact
anti-pattern that audit flagged as dangerous (the dormant `lsp-max` refund-receipt hook chain,
quarantined in that session's CP14, is a real, live-adjacent example of it). The resolution,
after being asked directly and choosing **bounded unattended writes**: a hardcoded dispatcher may
call `ggen_write_apply` with zero LLM/human step, but ONLY for a narrow, declared-safe class —
everything else still routes through §6a's tier-3 path unchanged.

**The eligible class** (`crates/ggen-mcp/src/tools/unattended_dispatch.rs`): a project's
frontmatter template must declare `unattended_write_eligible: true`, which the writer refuses to
parse unless `unless_exists: true` is also set (`crates/ggen-engine/src/template.rs::parse`,
`FM-TPL-027`) — the create-only guarantee `unless_exists` already enforces
(`crates/ggen-engine/src/write.rs:104`) is what makes zero-review safe: the write can only ever
create a file that doesn't exist, never clobber hand-written content. A dispatch attempt also
requires: the target isn't already on disk, it doesn't match a protected path (a Rust port of
`.claude/hooks/pre_tool_use_guard.sh`'s own check, `protected_paths.rs`), a fresh dry-run shows
zero project-wide refusals AND every path the real sync would write is covered by the eligible
set (whole-run-eligible, not just the one rule), and a circuit breaker (5 unattended writes per
60s per root, a working default) has budget. Every attempt — applied or refused — is logged to
`.ggen/unattended-dispatch-log.jsonl`.

This design was checked against three sibling projects with their own receipted pipelines before
being finalized (`~/mfw`'s branchless declared-risk-class admit-mask, `~/turbo-fieldfare/
kcj-mustar`'s independent-recheck-at-the-dispatch-boundary discipline, `~/wasm4pm`'s cautionary
finding that a severity classification which doesn't actually gate anything is worse than none) —
see the plan file's CP31-36 for the full research trail. It is deliberately narrower than the
original CP21 "any declared trigger→action mapping" dispatcher, which was assessed and rejected.

**Known, named limitation, not a silent gap**: this only fires from an `ggen-mcp` server session.
`ggen sync run --watch` (`crates/ggen-engine/src/watch.rs`) has its own, independent watch loop
with no dependency edge to `ggen-mcp` — a bare CLI watch process has zero unattended-dispatch
capability today (CP35). Closing that requires relocating the dispatcher into a crate both
`ggen-cli` and `ggen-mcp` depend on, scoped as future work, not attempted this pass.

## 7. Status and next steps

CP0-CP12 (ontology fidelity, generation pipeline, push mechanism proof) are complete and
independently re-verified — see the plan file. CP13/CP14 (safety cleanup) and CP15-CP20
(production hardening: watcher, peer lifecycle, write-gate strengthening, backpressure, signal
coverage, observability) are in flight as of this document's writing. None of §4-§6 above are
scheduled checkpoints yet — they are the next layer of design questions this session's execution
surfaced, recorded here so a later, less-context-loaded session can pick them up without
re-deriving them from a 100+-turn transcript.
