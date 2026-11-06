# ZD-40 LSP-Contract EA Graph Receipt

Standing: ALIVE

Lane: ZD-40. Date: 2026-11-04. Repo: `/Users/sac/ggen`, branch `v26-11-3-wave` @ 58a3b4108.
Owned files: `self-host/lsp-contract/ea.graph.json` (new, untracked),
`docs/jira/v26.11.3/receipts/zd-40-lsp-contract-graph.md` (this file). No git
commands run; no other files touched.

## Finding (ZB-3, exact-head-producer-and-receiver + sync-compile-execute)

`self-host/lsp-contract` had no `ea.graph.json`; FM-LAW-015 (`crates/ggen-engine/src/sync.rs`,
fail-closed) refuses the root's sync. Reproduced before the fix:

```
$ cd self-host/lsp-contract && ../../target/debug/ggen sync run
ERROR: ... [FM-LAW-015] EA admission gate: `/Users/sac/ggen/self-host/lsp-contract/ea.graph.json`
is absent and no admission opt-out was given. ...
EXIT=1
```

The surface (`ggen-lsp-runtime-crown` workflow) runs `ggen sync run` in
`self-host/lsp-contract`, then installs the three projections into their repository
homes (`crates/ggen-lsp/generated/lsp-contract.json`,
`crates/ggen-lsp/src/generated_contract.rs`, `docs/generated/LSP_CONTRACT.md`) and
refuses drift. Both checks were blocked on this one missing artifact.

## Fix (XD-14 pattern, real SBB — not an empty-SBB bypass)

Modeled what the root actually manufactures, read from `ggen.toml` (3 generation
rules: `lsp-contract-json` → `generated/lsp-contract.json`,
`lsp-contract-rust` → `generated/generated_contract.rs`,
`lsp-contract-doc` → `generated/LSP_CONTRACT.md`, all rendered from
`ontology.ttl` + `templates/contract.{json,rs,md}.tera`):

- strategy `strategy:ggen-lsp-contract`, capability `cap:lsp-contract-manufacture`,
  ABB `abb:lsp-contract-manufacture`, contract `contract:lsp-contract-manufacture`
  (CONSTRUCT ceiling, `max_artifacts: 3`, three required ports matching the three
  rules), one SBB `sbb:lsp-contract-manufacture` carrying the three artifact paths.
- Digests **kernel-pinned, not assumed**: authored the graph with `PLACEHOLDER`
  digests, ran the read-only probe, pinned what the kernel computed:
  - `sbb_content_digest = sha256:0a10d4a40af64ff377581fd350409de38ec7424a4ce1051c5b0e61b7da5595d8`
  - `contract_digest    = sha256:f51c69dfbec29119946b25343404dbe2f67ca387fb4083405a3ac5478967d774`
  - final `graph_digest = sha256:723bd855cf13131885a7e12884445509a23f5727203307047c73e5d8d5525997`
  - qualification `qual:lsp-contract-manufacture` verdict `QUALIFIED` binding both.
- No `--skip-ea-admission` anywhere; the SBB was qualified, not the gate.

## Verification (all commands run this session)

1. Reproduction (pre-fix): `ggen sync run` in `self-host/lsp-contract` → exit 1,
   `[FM-LAW-015] ... ea.graph.json is absent`.
2. `cargo run -q -p ggen-abb-sbb --example validate_ea_graph -- self-host/lsp-contract/ea.graph.json`
   → exit 0; `schema ok: ggen.ea.graph.v1`, `validate: ok`,
   `ADMITTED sbb:lsp-contract-manufacture (qualification qual:lsp-contract-manufacture)`;
   plan decision table returns `Select { abb: ..., sbb: ..., authority: Select }`
   with zero `missing_ports` and zero refusals. Pre-pinning run of the same probe
   refused with the expected typed `SBB_DIGEST_MISMATCH` (non-vacuity: the gate
   flips on digest correctness).
3. `./target/debug/ggen sync run` (repo-built binary, reports version 26.11.3) in
   `self-host/lsp-contract` → **exit 0**; all 5 pipeline stages ran
   (`load`/`extract`/`validate`/`generate`/`emit`, `files_generated=3`); decisions
   `written` for all three projections (`generated/lsp-contract.json`,
   `generated/generated_contract.rs`, `generated/LSP_CONTRACT.md`); `skipped: []`.
4. No `ggen.lock` was minted for this root (sync's closure block printed
   ontology+template digests inline; nothing written to disk) — nothing to leave
   uncommitted on that front.

## Standing

ALIVE for `self-host/lsp-contract`: the exact-head-producer sync step and the
downstream sync-compile-execute chain in `ggen-lsp-runtime-crown` are unblocked at
this root (FM-LAW-015 satisfied with a kernel-admitted SBB). Generated outputs left
untracked under `generated/` (gitignored), matching the workflow's
sync-fresh-per-run contract. Not verified here (out of scope): full
`ggen-lsp` compile/test chain — the workflow runs it after install; the producer
gate this lane owns is green.
