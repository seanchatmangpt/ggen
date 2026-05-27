# Portfolio Cartographer Map

## 1. Scope
Inventory of the entire project portfolio located in `/Users/sac/`, classified by role in the Genesis-bearing interchangeable part architecture.

## 2. Universal Layer Definitions
1. **Byte/Motion Substrate:** Physical layout, bit-masks, raw Pair2 tuple execution.
2. **Genesis Kernel:** Lawful construction kernel (A = μ(O)), Need9/Need257 splits.
3. **Interchangeable Part Runtime:** AtomVM/WASM runtime bodies, edge custody.
4. **ggen Foundry/Membrane:** Adaptation, packaging, projection, CLI tools.
5. **Truex Lifecycle:** Execution trust, promotion, accounting, rollup.
6. **Process Intelligence:** wasm4pm, OTel, process-ISA (POWL8), conformance.
7. **Public Vocabulary / Validation:** open-ontologies, SHACL, RDF/OWL survivors.
8. **Vertical Proof Surface:** Domain-specific Genesis parts (ostar, CNS, DTeam).
9. **Downstream Consumer:** UI (Nuxt), lakehouses, AI contexts, reporting.
10. **System/Support/Archive:** Legacy tools, backups, support infrastructure.

## 3. Project Classification Matrix

| Project | Role | Layer | Status | Finish Gap |
|:---|:---|:---|:---|:---|
| **Genesis Core** | Lawful Construction Kernel | 2 | PARTIAL | Extract pure `genesis-core` crate; Need9/Need257 logic. |
| **ggen** | Foundry & Membrane | 4 | IMPLEMENTED | AtomVM/WASM packager logic integration. |
| **bitstar / bytestar** | Pair2 Tuple / Byte Substrate | 1 | IMPLEMENTED | Full integration with ggen-membrane. |
| **knhk** | Knowledge & Provenance Subsystem | 2 | PARTIAL | Migrate from monolith to isolated Genesis-bearing part. |
| **truex** | Lifecycle & Execution Trust | 5 | PARTIAL | Promotion API and receipt-chain validation hooks. |
| **wasm4pm** | Process Validation Engine | 6 | IMPLEMENTED | Real-time streaming bridge for ggen-projection. |
| **AtomVM / erlmcp** | Edge Actor Custody | 3 | IMPLEMENTED | Embed Genesis core as NIF/Port; MCP+ standard. |
| **a2a / a2a-rs** | Agent-to-Agent Protocol | 4/3 | PARTIAL | Finish MCP+ tool standard for cross-agent interop. |
| **cns / cns_forge** | Cognitive Backbone | 1/8 | PARTIAL | Migrate to receipt-backed Genesis construction. |
| **cre / bitactor** | Cognitive Runtime | 3/1 | PARTIAL | Transition from YAWL to AtomVM-backed Genesis parts. |
| **dteam** | Digital Teams (AutoML) | 8 | PARTIAL | High-rigor Rust implementation of team coordination. |
| **insa / proc** | Process ISA (POWL8) | 1/6 | PARTIAL | High-speed POWL64 execution; Pictl integration. |
| **open-ontologies** | Survivability Checkpoint | 7 | IMPLEMENTED | Automated SHACL CI gate for ggen projections. |
| **ostar** | Vertical Proof Surface | 8 | PARTIAL | 14-gate manufacturing proof integration. |
| **pictl** | Process Intelligence CLI | 6 | IMPLEMENTED | Continuous OCEL ingestion from Genesis parts. |
| **clap-noun-verb** | Universal CLI Grammar | 1 | IMPLEMENTED | Retrofit legacy CLI tools (kgn, unrdf). |
| **kgn / kgc-sidecar** | KGC / Knowledge Generation | 7 | PARTIAL | Align with open-ontologies public URI rules. |
| **knowd / knowtro** | Knowledge Sync | 7 | PARTIAL | Bridge to receipted Genesis construction. |
| **ktemp / mcpp** | Knowledge Geometry / MCPP | 1 | PARTIAL | Formalize KGC calculus in Genesis core. |
| **mcp_erl / mcp-mqtt** | AtomVM MCP Custody | 3 | PARTIAL | Full support for Pair2 primitives in Erlang actors. |
| **semantic_bit** | Atomic Unit of Construction | 2 | IMPLEMENTED | Standardize as the base unit for all Shard laws. |
| **s2s / sos** | Source Lowering / Swarm OS | 4 | PARTIAL | Clean membrane for heterogeneous agent swarms. |
| **speckit-ralph** | Autonomous Exploit Loop | 5 | PARTIAL | Hardening Truex acceptance/promotion gates. |
| **unrdf / unrdf-clean** | RDF Projection Substrate | 7 | PARTIAL | Replace custom triple store with ggen projections. |
| **yaml-server / cloud** | YAML-native Membrane | 4 | PARTIAL | Standardize YAML-to-RelationPage adaptation. |
| **yawl / yawlv6** | Legacy Workflow Engine | 3 | TRANSITION | Port valid workflow logic to POWL8 Genesis parts. |
| **zoela / zoeapp** | Legacy Agent Archive | 10 | DEAD_CODE | Subsume into ggen-mcp / ostar. |
| **ai / ai-chatbot** | Exploratory Archive | 10 | ARCHIVED | Concepts subsumed into MCPP / Claude. |
| **pigsty** | Database Downstream | 9 | IMPLEMENTED | Use as validator/consumer for projected DuckDB sets. |
| **nuxt-catalog** | Documentation UI | 9 | IMPLEMENTED | Project Genesis manifests into human-readable Nuxt. |
| **Legal** | RDF Authority Models | 7 | PARTIAL | Map authority laws to Genesis O* constraints. |

## 4. Key Architectural Insights
- **Kernel Concentration:** The scattered `Pair2` and `Construct8` logic in `knhk`, `bitstar`, and `bytestar` is the primary target for consolidation into `genesis-core`.
- **Membrane Proliferation:** Many projects (`kgn`, `knowd`, `unrdf`, `yaml-server`) are actually membranes that should be refactored as `ggen` adapters.
- **Body Standardization:** The path forward for `cre`, `bitactor`, and `yawl` is standardization on `AtomVM` (Custody) and `WASM` (Portability) runtime bodies.
- **Validation Authority:** `open-ontologies` and `wasm4pm` must remain independent of the construction kernel to provide credible GALL evidence.

## 5. Required Action: Consolidation
All projects marked as PARTIAL in the "Kernel" or "Membrane" layers must be prioritized for consolidation to avoid doctrine blur.
