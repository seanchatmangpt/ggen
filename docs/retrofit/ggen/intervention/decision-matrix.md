# ggen Intervention Decision Matrix

Full table backing `decision.md`. Priority column: cheap-and-safe items rank first (do now,
implementable in one focused pass with real Chicago-TDD evidence); expensive-or-risky items are
named but explicitly deferred, per the task instruction not to default to rehabilitation.

| # | Gap | Class | Source evidence | Value | Risk | Priority | Continuity/Migration plan |
|---|-----|-------|------------------|-------|------|----------|----------------------------|
| 1 | ggen-verify-pack evidence producers not wired into ggen's own sync (6/8 receipt classes Unknown) | strengthening | receipt-system.md #1; PUBLICATION_JUDGMENT.md cond. 15 | High — closes the repo's own headline evidence gap | Low — additive, no path change | **Do now (cheap-safe)** | n/a (strengthening) |
| 2 | `andon` hardcoded `Green` in `sync.rs:1857` | repair | architecture-semantics.md #1; load-path.ttl Blocking | High — this is THE Blocking SPOF for the generation load path | Low — one function, existing gate results already computed | **Do now (cheap-safe)** | n/a (repair) |
| 3 | Stale pack-count table (`architecture.md`, 32 vs 33 documented vs disk) | repair | architecture-semantics.md #6; repository.md | Medium — recurring drift class, undermines doc trust | Low — regenerate via existing `ggen sync run` after ontology fix | **Do now (cheap-safe)** | n/a (repair) |
| 4 | Law XII cites nonexistent test `reasoner_independence_e2e` | repair | architecture-semantics.md #1 | Medium — a false evidentiary claim inside the constitution itself | Low — single TTL individual edit | **Do now (cheap-safe)** | n/a (repair) |
| 5 | Sabotage-fixture coverage gap, 12 fixtures across 3 packs | strengthening | receipt-system.md #6 | Medium — closes a real, named test gap | Low-medium — pattern already proven on sibling fixtures in same packs | Do soon (cheap-safe, larger volume) | n/a (strengthening) |
| 6 | Constitution ontology not wired into `praxis-core` admission | rehabilitation | architecture-semantics.md #1; PUBLICATION_JUDGMENT.md cond. 12 | High — closes condition 12 | High — live admission path, fail-open risk if rushed | Defer — phased, see decision.md #6 | Stated in decision.md #6 |
| 7 | No branch protection / single admin collaborator / no CODEOWNERS | strengthening | load-path.ttl Blocking (gh api evidence) | High — SPOF across PR + release + fleet paths | External-authorization-gated (branch settings + collaborator mutation) | **Named as authorization packet, not implemented** | n/a — awaiting user authorization |
| 8 | `just sync`/`just sync-dry` recipes pass invalid flags | repair | CLAUDE.md; reconfirmed live | Medium — blocks the documented workflow entry point | Low — recipe body only | **Do now (cheap-safe)** | n/a (repair) |
| 9 | Root `templates/` (90 files) disconnected from pack-scoped convention | rehabilitation | repository.md (liveness Unknown) | Medium — organizational debt | Medium — risk of deleting a live template before consumer trace completes | Defer — phased, see decision.md #9 | Stated in decision.md #9 |
| 10 | 34 root `.md` files violating "No Root Files" | repair | repository.md | Low-medium — self-inflicted rule violation | Low — pure file moves | Do soon (cheap-safe, larger volume) | n/a (repair) |
| 11 | `scripts/` 150+ one-off fix scripts, no archive split | repair | repository.md | Low — cosmetic/organizational | Low — file moves, verify no CI references first | Do soon (cheap-safe, larger volume) | n/a (repair) |
| 12 | Four differently-named `examples/` archive dirs | replacement | repository.md | Low-medium — organizational debt, one dir may reference deleted crate | Medium — must confirm `archive_ggen_core` dead-code status before merge | Defer — phased, see decision.md #12 | Stated in decision.md #12 |
| 13 | mermaid-pack does not exist (false premise in brief/PR #354) | preservation | architecture-semantics.md #4 | Low — documentation correction only | None — no code change | Do now (documentation-only) | n/a |

## Top 4 cheap-and-safe items (ranked, this pass's implementation portfolio)

1. **#2 — Fix hardcoded `andon: Green`** (highest value: it is the single Blocking SPOF named
   by the load-path survey for the generation path; smallest surface area of all high-value
   items).
2. **#1 — Wire ggen-verify-pack evidence producers into ggen's own sync** (closes the repo's own
   headline "eating your own dogfood" gap; the pack and its producers already exist and are
   proven elsewhere — this is invocation wiring, not new logic).
3. **#5 — Close the 12-fixture sabotage-coverage gap** (concrete, named, bounded list of
   fixtures; pattern already proven twice in the same packs, so risk is low even though volume
   is moderate).
4. **#8 — Fix `just sync`/`just sync-dry` broken flags** (trivially small diff, unblocks a
   documented entry point that is currently useless, directly falsifiable by running the recipe
   before/after).

Items #3, #4, #10, #11, #13 are also cheap-safe but smaller in value or more
documentation-flavored; they are good fill-in work for the same or a following pass but were not
selected as the primary 4 because #2/#1/#5/#8 have the clearest before/after evidence (a
receipt/andon field flips from a wrong/Unknown value to a correct one, or a broken command starts
working) and combined cover both of this session's two most load-bearing findings (the hardcoded
Green andon and the unwired evidence lane).

Items #6, #7, #9, #12 are correctly NOT in the cheap-safe set: #6 and #9 both carry a genuine
fail-open/data-loss risk if rushed (live admission path; possibly-live templates) and are
already given continuity plans rather than being scheduled now. #7 is hard-blocked by this
task's own external-authorization rule. #12 needs a one-time dead-code confirmation before its
migration is safe to execute.
