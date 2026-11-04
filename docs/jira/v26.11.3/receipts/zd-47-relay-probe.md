# ZD-47 — diagnostic_relay_probe hard-coded binary root-cause + fix

**Standing**: ADMITTED (all branches executed this session)
**Attribution**: CARRY-08 / sx-1-reds.md Red 4 (CARRIED-OPEN); cl-f attribution
(hardcoded external binary), CL-H suspected regression.
**Date**: 2026-11-04

## Root cause

`diagnostic_relay_probe.rs` lives in
`/Users/sac/lsp-max/crates/lsp-max-compositor/tests/` — not in ggen
`crates/ggen-lsp` as the dispatch assumed; `grep -rn diagnostic_relay_probe
/Users/sac/ggen/crates/` returns nothing. Line 19 hard-coded the child LSP
server path:

```rust
let child_bin = "/Users/sac/claude-code-config-lsp/target/release/claude-code-config-lsp";
```

That path does not exist on this machine (`target/` absent from the
claude-code-config-lsp checkout; the only real artifact is the dist binary
`/Users/sac/claude-code-config-lsp/dist/claude-code-config-lsp-26.6.24`,
sha256 `a46431fc5392337458d503192da73e885c0185a92a4f8fc45b7da81986316ea9`,
verified executable with `serve --stdio`). The compositor spawned a
nonexistent command, no child server ever published diagnostics, and the
probe died on the 20 s `publishDiagnostics` `recv_until` timeout — 2/2 runs,
exactly as recorded in sx-1-reds Red 4.

## Fix — options (a)+(b) combined

`/Users/sac/lsp-max/crates/lsp-max-compositor/tests/diagnostic_relay_probe.rs`:

- New `resolve_child_bin()` helper: (1) `CCC_PROBE_CHILD_BIN` env override
  (accepted only if the path exists on disk), (2) fallback search over known
  workspace locations (`target/release`, `target/debug`,
  `dist/claude-code-config-lsp-26.6.24`), (3) `None` → loud typed SKIP on
  stderr naming the env var and the build command — never a fake pass.
  `CCC_PROBE_ROLE` guard stays first so child-role re-exec is unaffected.
- No timeout bug found — the 20 s timeout was correct; the dependency was
  absent. Option (c) not applicable.

## Verification (all commands run this session)

| Command | Exit | Result |
|---|---|---|
| `cargo test -p lsp-max-compositor --test diagnostic_relay_probe` (run 1) | 0 | 2 passed, 0 failed |
| same, `-- --nocapture` (run 2) | 0 | PROBE3: init 114 ms; didOpen→publishDiagnostics round trip 14 ms — real relay witnessed, not silent skip |
| same (run 3) | 0 | 2 passed — green ×2 witnessed |
| Shim court: `mv` dist binary away → run → restore | 0 | `[SKIP] diagnostic_relay_probe::run_stdio_relay_works_end_to_end: external claude-code-config-lsp binary not found — set CCC_PROBE_CHILD_BIN or build /Users/sac/claude-code-config-lsp (cargo build --release)…` printed verbatim; 2 passed 0 failed (loud skip, not fake pass); restore byte-verified (sha256 `a46431fc…ea9` = baseline) |
| Env-override witness: dist binary hidden, `CCC_PROBE_CHILD_BIN=/tmp/ccc-probe-override-witness` (real copy) | 0 | 2 passed — override branch functions; restore byte-verified |

`rustfmt --check` on the modified file: clean. No `git` commands run, per
lane contract.

## Disclosure

- The dist binary in the sibling repo was twice temporarily renamed
  (`.shim-court-away`) and restored byte-identically to witness the skip and
  override branches; both restores sha256-verified.
- The mangled-path note in earlier drafts of this receipt was drafting error;
  this version is the authoritative text.
