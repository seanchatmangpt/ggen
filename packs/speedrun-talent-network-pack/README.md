# Speedrun Talent Network Pack

Ontology-first ggen pack for the public Speedrun Talent Network REST API and remote MCP server.

The pack manufactures a pure Speedrun specialization over the canonical generic `ggen-architecture` Building Block kernel. It does not perform network IO. Every REST or MCP call becomes a typed BRCE transport intent. `join_network` and `express_interest` require explicit, action-matched consent evidence. The model has no stealth-unmasking operation and no job-application submission operation.

## Generated surfaces

- standalone Rust consumer;
- REST contract covering eight OpenAPI operations;
- machine-readable contracts for `/llms.txt`, `/jobs.md`, and `/jobs.rss`;
- MCP contract covering eight reads and two consent-gated candidate actions;
- full-contract tests;
- non-self-promoting release standing.

## Verify

```bash
cd packs/speedrun-talent-network-pack
ggen sync run
cargo test --manifest-path consumer/speedrun-talent-network/Cargo.toml
```
