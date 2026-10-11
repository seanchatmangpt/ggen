# OntoStar + POWL v2 + MuStar Agent Pack

Ontology-first ggen pack for a three-layer agent framework over the verified Speedrun Talent Network Building Block.

## Authority split

- **OntoStar** admits evidence-backed observations. It adds no local process-mining mathematics.
- **POWL v2** owns the workflow grammar: activity, sequence, choice, parallel, and partial order. Structural validation and cycle refusal are deterministic.
- **MuStar** compiles admitted POWL plans into deterministic Motion Packets.
- **GPT-5.6 Luna, Terra, and Sol** manufacture bounded inference candidates for extraction, planning, and adversarial verification.
- **BRCE** remains the only external actuation path.

No model output, agent intent, POWL plan, or Motion Packet is execution authority.

## Model routing

| Phase | Framework | Capability tier |
|---|---|---|
| Observe and extract | OntoStar | GPT-5.6 Luna |
| Generate workflow candidate | POWL v2 | GPT-5.6 Terra |
| Adversarially verify | MuStar | GPT-5.6 Sol |

The generated crate emits model-inference intents only. It does not embed an OpenAI SDK, API key, HTTP client, or provider endpoint.

## Speedrun projection

The generated consumer depends on the separately generated `speedrun-talent-network-consumer`. Read operations become typed Speedrun transport intents. `join_network` and `express_interest` still require the Speedrun pack's explicit consent evidence and remain addressed to BRCE.

## Verify

```bash
cargo build -p ggen-cli-lib --bin ggen

(
  cd packs/speedrun-talent-network-pack
  ../../target/debug/ggen sync run
)

(
  cd packs/ontostar-mustar-powlv2-agent-pack
  ../../target/debug/ggen sync run
  cargo test --manifest-path consumer/ontostar-mustar-powlv2-agents/Cargo.toml
  cargo clippy --manifest-path consumer/ontostar-mustar-powlv2-agents/Cargo.toml --all-targets -- -D warnings
  ../../target/debug/ggen receipt verify
)
```
