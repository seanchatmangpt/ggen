# ggen-a2a-mcp — Crate Audit

**Path:** `crates/ggen-a2a-mcp/`
**Lines:** 12+ modules, ~410 test markers
**Role:** Bridge between A2A protocol and MCP (Model Context Protocol). Handlers, adapters, YAWL bridge.

---

## STUBS

No explicit stubs found. The crate is one of the more complete implementations.

---

## DEAD CODE

| File:Line | Item | Status |
|-----------|------|--------|
| `client.rs:172,181` | Functions | `#[allow(dead_code)]` |
| `handlers.rs:1321` | Unknown | `#[allow(dead_code)]` |
| `ggen_server.rs.bak2` | Backup file | Already removed in workspace cleanup |

---

## ARCHITECTURE NOTES

- `MessageRouter` routes through 5 handler types: Text, File, Data, Multipart, Stream
- `A2aLlmClient` bridges A2A messages to LLM calls with retry
- `TaskMapper` converts YAWL tasks to A2A ConvergedMessages
- `YawlStateMapper` maps between YAWL states, A2A TaskStatus, and MessageState
- Three different `TaskStatus` enums across the A2A crates (ggen-a2a, a2a-generated, converged)

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **DELETE** | Dead code in client.rs, handlers.rs | P3 |
| **REFACTOR** | Three TaskStatus enums across crates — consider unified type | P3 |
