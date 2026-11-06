# Receipt — ggen-sjira complete.rs (PG-2 Slice 4 completion driver)

Standing: ALIVE

Date: 2026-11-03 · Written by: DS-10 lane (on coordinator PF-2 instruction —
the receipt debt for SE-3's 18:15 landing; DS-2 did not surface to claim it)

## Subject

`/Users/sac/ggen/crates/ggen-sjira/src/complete.rs` — Rust port of
`GgenIgniter.SemanticJira.Complete` (complete.ex, 349 lines). NOT written by
this lane (landed via SE-3, 18:15); this lane wrote only the receipt, per
PF-2 ("the receipt debt is DS-2's or yours").

## Surface (re-read from disk at receipt time)

- `run(orders, ledger_path, graph, opts) -> (u8, Value)` — three-exit
  vocabulary: 0 complete / 1 refused (`order_refused`, `max_iterations_
  exhausted` with `remaining_eligible`, `frontier_refused`,
  `authority_index_unavailable`) / 2 invalid invocation (pack_dir XOR
  packs_root exclusivity, before any ledger IO).
- `CompleteOpts`: max_iterations (default 1000), pack_dir/packs_root
  exclusivity boundary, `target_packs: BTreeMap<identity, pack>` — disclosed
  deviation: Elixir's loose `order["target_pack"]` shape does not port
  literally (typed WorkOrder refuses unknown fields), so resolution input
  rides the opts (identity-keyed), resolution unchanged.
- `sanitize(identity)`, `DEFAULT_MAX = 1000`.
- Zero shell-outs: the loop is function calls into ledger::fetch,
  frontier(), execute::run over a single-order slice; a per-order refusal
  mid-loop is structurally unreachable (driver and executor re-derive the
  identical frontier from the same ledger) but the branch exists fail-closed.
- Deferred (disclosed in module doc): SecurityReview.run/2 pre-step OUT
  (ggen-engine owns diff review); packs CARRIED not actuated
  (`resolved_pack_dir` surfaced in the receipt echo, never consumed).

## Gates (run this session)

- `cargo test -p ggen-sjira --test complete_test` → 8 passed, 0 failed,
  exit 0 (run 2026-11-03, this session).

## Standing

ALIVE — module compiled and its pinned court passed this session, on the
canonical checkout, by this session's own execution.

## Owner note for the coordinator

PF-2 says the receipt debt is DS-2's or DS-10's; DS-2 did not surface, so
DS-10 landed it. If DS-2 later produces its own receipt for complete.rs,
that document supersedes this one.
