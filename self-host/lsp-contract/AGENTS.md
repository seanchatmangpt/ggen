# Self-hosted LSP contract authority

This subtree is the authored authority for every generated LSP representation shared by `seanchatmangpt/ggen` and the independent `seanchatmangpt/ggen-legacy` receiver.

## Ownership

- Edit `ontology.ttl`, `ggen.toml`, templates, or `verify.py`.
- Do not hand-edit `crates/ggen-lsp/generated/lsp-contract.json`, `crates/ggen-lsp/src/generated_contract.rs`, or `docs/generated/LSP_CONTRACT.md`.
- Receiver copies must be byte-identical to the corresponding ggen projections.
- `ggen-legacy` may independently implement and verify the contract but cannot modify or certify this authority.

## Manufacture

```bash
cd self-host/lsp-contract
ggen sync run --config ggen.toml
python3 verify.py --ggen-root ../.. --legacy-root /path/to/ggen-legacy \
  --report ../../target/lsp-contract-sync.json
```

Execute the projection twice from the same ontology and require byte identity outside receipt state. The Python verifier is an independent deterministic projection checker; it is not a substitute for executing `ggen sync`.

## Standing

- Projection equality may be `ALIVE` after observed deterministic verification.
- Actual ggen manufacture remains `BLOCKED` when the ggen/Rust toolchain cannot execute.
- Runtime standing remains separate for both repositories.
