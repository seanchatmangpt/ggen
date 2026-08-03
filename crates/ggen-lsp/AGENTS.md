# ggen-lsp Subtree Contract

This contract applies to `crates/ggen-lsp/**` and extends the repository contract.

## Product boundary

`ggen-lsp` is an offline, static code-intelligence and law-diagnostic server for ggen surfaces. It does not require an LLM, network service, hosted telemetry collector, or external process-mining service to provide editor features.

The editor, headless checker, MCP bridge, and A2A bridge must project the same analyzer, route, diagnostic, and receipt semantics. A transport may change framing, not meaning.

## Protocol invariants

- Use real LSP JSON-RPC framing for protocol claims.
- Advertise only methods that return a well-formed, non-`method_not_found` response.
- Every advertised feature requires at least one protocol-level test through the spawned `ggen-lsp` binary.
- `didOpen`, `didChange`, and `didClose` must keep document state, published diagnostics, the lsp-max registry, and the Λ_CD gate mutually consistent.
- Diagnostic replacement is document-scoped: publishing a new set for one URI must remove stale registry entries for that URI without removing another URI's entries.
- A clean batch for one document must not open the gate while another document still has an active gating violation.
- Diagnostic identity must include document identity and location; message-only hashes are insufficient.
- Create the gate directory before writing. Gate writes are best-effort for editor continuity but must be directly testable.
- Prefer `workspaceFolders[0]`, then deprecated `rootUri`, then process cwd for the workspace root.
- Stdout is reserved for LSP frames. Logging goes to stderr.

## Analyzer invariants

- `.ttl`, `.nt`, `.nq`, `.rq`, `.sparql`, `.tera`, and `.toml` dispatch must remain explicit.
- Cross-surface diagnostics must use the open-buffer overlay before disk where the live buffer is authoritative.
- A single URI receives one coherent diagnostic publication per refresh. Do not transiently clear one species by publishing another species separately.
- Pure analyzer functions may use focused unit tests.
- Route selection and diagnostic lifecycle claims require integration or protocol evidence.
- Do not add an analyzer fallback that converts parse failure into an empty success.

## Completion standard

For a capability to be marked delivered:

1. the initialize response advertises it, or the server lawfully registers it with `client/registerCapability` when the client supports dynamic registration;
2. the corresponding `LanguageServer` method is implemented;
3. requests return a valid result rather than `method_not_found`;
4. at least one spawned-binary protocol test exercises it;
5. documentation matches the observed boundary.

An empty relation result is valid for hierarchy follow-up requests when the prepared symbol has no known edges. It is not valid to advertise a prepare method while leaving required follow-up methods unimplemented.

## Canonical verification

From repository root, use the pinned toolchain:

```bash
cargo fmt --check -p ggen-lsp
cargo check -p ggen-lsp --all-features
cargo test -p ggen-lsp --lib
cargo test -p ggen-lsp --test lsp_protocol_test
cargo test -p ggen-lsp --test lsp_contract_completion_test
cargo build -p ggen-lsp
python3 scripts/lsp-smoke.py
```

Run only the reachable prefix when a toolchain or dependency transport is unavailable. Classify the remainder; do not promote it to success.
