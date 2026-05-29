# GALL-INTEGRATION-1 Receipt

## Final State

**GALL-INTEGRATION-1 COMPLETE** — the delivery-plane foundation pier is load-bearing.

## The house-repair doctrine

> A cracked foundation is not repaired by decorating the rooms above it.
>
> It is repaired by shoring the load, exposing the failing sections, replacing the
> footing, transferring the load, and inspecting the new foundation before building
> upward.
>
> For ggen, Gall integration tests are the shoring and inspection. They let us replace
> fragile assumptions with load-bearing proof.
>
> Capability-map resumes only after the delivery foundation holds.

## Foundation under test

```
playground fixture
→ LSP route surface   (ggen_lsp::check_files_in_root — headless gate)
→ MCP tool surface    (ggen_lsp_mcp::build_repair_routes_in)
→ A2A bridge surface  (ggen_lsp_a2a::dispatch_tool)
→ same diagnostic code
→ same route_id
→ replay/metrics reachable
```

Each test drives the **three distinct public entry points** (not one wrapping another),
so the parity assertion is meaningful, not tautological. They converge internally on the
same `RouteRegistry::seeded()` engine; the tests prove the public surfaces do not drift.

## Location

`crates/ggen-lsp-a2a/tests/gall_foundation_lsp_mcp_a2a.rs` — the apex crate of the
delivery dependency graph (depends on ggen-lsp + ggen-lsp-mcp + ggen-a2a-mcp), the only
place all three surfaces compose without a dependency cycle.

## Fixtures

- `playground/proof/broken-construct.rq` — unconstrained CONSTRUCT → **E0011** → seeded
  route `template.values-inline`. (Byte-identical to the bridge `E0011` constant, so the
  proof composes across independently-written surfaces.)
- `playground/thesis-ontology.ttl` — real valid Turtle; the clean-surface pier.
- `playground/proof/README.md` — documents each fixture's foundation pier.

## Tests proven (7/7 pass)

| Test | Foundation pier |
|------|-----------------|
| `gall_playground_broken_construct_routes_same_across_lsp_mcp_a2a` | LSP==MCP==A2A on E0011 + `template.values-inline` |
| `gall_clean_playground_surface_has_no_blocking_route` | clean .ttl → 0 errors, no route |
| `gall_mcp_tool_list_contains_repair_replay_metrics` | exactly 3 tools, each callable |
| `gall_a2a_bridge_matches_mcp_for_repair_route` | A2A == MCP byte-for-byte |
| `gall_lsp_mcp_a2a_preserve_route_id` | route_id identical across transports |
| `gall_delivery_plane_uses_v26_5_28_identity` | agent card + adapter + crate all 26.5.28 |
| `gall_replay_metrics_surface_is_reachable_for_captured_case` | capture → metrics + replay reachable |

## Commands run

```
$ cargo build -p ggen-cli-lib --features lsp --bin ggen     # 49.41s, green
$ cargo test -p ggen-lsp-a2a --test gall_foundation_lsp_mcp_a2a
running 7 tests
test result: ok. 7 passed; 0 failed; 0 ignored; finished in 0.01s
```

(Live transcripts of the underlying LSP/MCP/A2A surfaces are captured in
[delivery-plane-proof.md](../reference/cli/delivery-plane-proof.md).)

## Matrix updated

`docs/reference/cli/command-proof-matrix.md` — `lsp` is **PROVEN** (the delivery plane is
now backed by committed integration tests, not just a one-off transcript).

## Remaining work

The delivery-plane pier holds. The **command foundation** (init/sync/graph/pack/policy/
doctor/utils + archived-noun absence) is the next set of piers — **GALL-COMMAND-FOUNDATION-1**.
Capability-map resumes only after the command foundation also holds.
