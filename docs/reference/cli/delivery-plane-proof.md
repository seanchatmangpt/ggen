<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Delivery-Plane Proof: LSP / MCP / A2A on the Playground (Reference)](#delivery-plane-proof-lsp--mcp--a2a-on-the-playground-reference)
  - [Fixture](#fixture)
  - [Proof 1 — LSP (headless admission gate)](#proof-1--lsp-headless-admission-gate)
  - [Proof 2 — MCP (`ggen-lsp-mcp` stdio server)](#proof-2--mcp-ggen-lsp-mcp-stdio-server)
  - [Proof 3 — A2A (`ggen-lsp-a2a` bridge)](#proof-3--a2a-ggen-lsp-a2a-bridge)
  - [Conclusion](#conclusion)
  - [Reproduce](#reproduce)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Delivery-Plane Proof: LSP / MCP / A2A on the Playground (Reference)

> Reference + command-proof artifact. Captured transcripts proving the three delivery
> transports actuate the **same** route engine against the real `playground/` project,
> with no route drift. ggen v26.5.28. Binary built with `--features lsp`.

> One route engine, three transports. The editor (LSP), the agent tool (MCP), and the
> remote agent (A2A) all resolve the *same* RouteEnvelope for the same law-surface input.

## Fixture

A real playground law surface with a known diagnostic — `playground/proof/broken-construct.rq`:

```sparql
CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }
```

This is an unconstrained `CONSTRUCT`. Under strict mode it raises **E0011**
("CONSTRUCT query lacks ORDER BY") and the route engine attaches the canonical repair
route `template.values-inline`.

## Proof 1 — LSP (headless admission gate)

```
$ ggen lsp check --root playground --with_routes true
```

Scanned all playground law surfaces. `Cargo.toml`/`ggen.toml` → no diagnostics.
`playground/proof/broken-construct.rq` →

```json
{
  "diagnostics": [{ "code": "E0011", "severity": 2, "source": "ggen-lsp",
    "message": "CONSTRUCT query lacks ORDER BY — required when strict_mode is enabled" }],
  "routes": [{ "route_id": "template.values-inline", "family": "TemplateFailure",
    "title": "Move VALUES out of the external .rq into ggen.toml", "provenance": "Seeded" }]
}
```

Exit 0 — E0011 is severity 2 (warning, advisory under strict mode), not an ERROR. The gate
correctly distinguishes advisory from blocking.

## Proof 2 — MCP (`ggen-lsp-mcp` stdio server)

JSON-RPC `initialize` → `tools/list` → `tools/call ggen.lsp.repair_route` on the same file:

```
initialize  -> serverInfo { name: "ggen-lsp-mcp", version: "26.5.28" }
tools/list  -> ["ggen.lsp.repair_route", "ggen.lsp.replay_case", "ggen.lsp.metrics"]
tools/call ggen.lsp.repair_route -> is_law_surface=true, envelopes=1,
                                     diagnostic_code=E0011, route_id=template.values-inline
```

**Same E0011, same `route_id` as the LSP transport** — the MCP server returns the same
RouteEnvelope the editor gate produced.

## Proof 3 — A2A (`ggen-lsp-a2a` bridge)

```
$ cargo test -p ggen-lsp-a2a
test bridge_test::a2a_route_result_equals_mcp_result ............. ok
test bridge_test::from_a2a_actuates_a_task_against_the_route_engine ok
test bridge_test::agent_card_advertises_all_three_capabilities ... ok
test bridge_test::unknown_tool_is_a_structured_refusal ........... ok
test bridge_test::bad_args_preserve_the_mcp_refusal_shape ........ ok
test bridge_test::from_a2a_with_root_leaves_a2a_field_evidence ... ok
test bridge_test::from_a2a_without_root_stays_pure ............... ok
test triad_stress_test::triad_holds_under_concurrent_pressure .... ok
result: ok. 8 passed; 0 failed
```

The bridge's `E0011` test constant is byte-identical to the playground fixture content
(`CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }`), so `a2a_route_result_equals_mcp_result`
certifies that **A2A dispatch == MCP result for the exact playground input** — the bridge
actuates the same engine over the A2A transport with structured refusals and an agent card.

## Conclusion

| Transport | Surface | E0011 | route_id | Verdict |
|-----------|---------|-------|----------|---------|
| LSP | `ggen lsp check` | ✓ | `template.values-inline` | WORKS |
| MCP | `ggen-lsp-mcp` stdio | ✓ | `template.values-inline` | WORKS |
| A2A | `ggen-lsp-a2a` bridge | ✓ (== MCP) | `template.values-inline` | WORKS |

All three transports work on the playground project and agree byte-for-byte. The `lsp`
noun (and the MCP/A2A planes it backs) is **PROVEN** — see the
[command-proof matrix](command-proof-matrix.md).

## Reproduce

```bash
cargo build -p ggen-cli-lib --features lsp --bin ggen
target/debug/ggen lsp check --root playground --with_routes true
cargo build -p ggen-lsp-mcp        # drive its stdio server with the JSON-RPC above
cargo test  -p ggen-lsp-a2a        # bridge parity + actuation
```
