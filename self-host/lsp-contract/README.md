# ggen within ggen: LSP contract

This project uses ggen to manufacture the contract governing ggen's own LSP and the independent ggen-legacy reference receiver.

```text
ontology.ttl
→ ggen.toml + SPARQL/Tera templates
→ ggen Rust/JSON/Markdown projections
→ exact receiver copies in ggen-legacy
→ independent receiver verifier
→ two Rust runtimes on lsp-max
```

## Authority and projections

- authority: `ontology.ttl`
- generator: `ggen.toml`
- templates: `templates/*`
- kernel JSON: `../../crates/ggen-lsp/generated/lsp-contract.json`
- kernel Rust: `../../crates/ggen-lsp/src/generated_contract.rs`
- human projection: `../../docs/generated/LSP_CONTRACT.md`
- receiver JSON: `ggen-legacy:authority/lsp-contract.json`
- receiver Rust: `ggen-legacy:src/generated_contract.rs`

The contract currently defines 29 protocol methods, eight source surfaces, eleven diagnostics, ten invariants, and seven representations.

## Replay

```bash
cd self-host/lsp-contract
ggen sync run --config ggen.toml
cp ../../crates/ggen-lsp/generated/lsp-contract.json /path/to/ggen-legacy/authority/lsp-contract.json
cp ../../crates/ggen-lsp/src/generated_contract.rs /path/to/ggen-legacy/src/generated_contract.rs
cp ../../docs/generated/LSP_CONTRACT.md /path/to/ggen-legacy/docs/lsp/CONTRACT.md
python3 verify.py --ggen-root ../.. --legacy-root /path/to/ggen-legacy
python3 /path/to/ggen-legacy/scripts/verify_lsp_contract.py
```
