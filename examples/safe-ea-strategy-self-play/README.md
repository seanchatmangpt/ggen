# Non-LLM SAFe / EA Strategy Self-Play Example

The only authored inputs are `ggen.toml` and `ontology.ttl`. The real ggen compiler manufactures `generated/` and `docs/`.

```bash
../../target/debug/ggen sync run
../../target/debug/ggen receipt verify
cargo test --manifest-path generated/Cargo.toml --all-targets
```

The generated runner and verifier execute as separate processes. A second run must produce identical report bytes. A tampered move receipt must be refused by the independent verifier.
