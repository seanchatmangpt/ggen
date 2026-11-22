# Repo inventory — ECOSYSTEM-SPEC-27.1.1

Base: `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` (observed 2026-11-22).
Standing labels: ALIVE | PARTIAL_ALIVE | UNKNOWN | UNSUPPORTED | BLOCKED.

| Subject | Paths / evidence | Standing | Notes |
|---------|------------------|----------|-------|
| Truex (product / sibling) | `docs/gh-terraform/FLEET-CENSUS.md` → `seanchatmangpt/truex`; `ontology_catalogue/open-ontologies/ontology/truex-ecosystem.ttl`; archived `docs/archive/**/07_TRUEX_LIFECYCLE_SPEC.md` | **UNKNOWN** | Sibling not mounted; catalogue TTL is description, not runtime admission |
| `truex:` as private prefix | `ORIGINAL_REQUEST.md` (banned); `crates/ggen-graph/src/bin/gall_observe_worktree.rs` banned IRI tokens; marketplace registry id `truex` | **ALIVE** (ban) | Private-prefix ban is live; distinct from W3C `shui:` |
| `forbid_private_namespaces` / public prefix scan | `crates/ggen-graph/src/bin/gall_actuate_code_evaluation.rs` (`allowed_prefixes`, `check_uri`); `crates/ggen-lsp/assets/policies/no-private-namespace.shacl.ttl` | **PARTIAL_ALIVE** | Live scanners; `shui` absent until Gate-1 slice |
| `shui:` (W3C SHACL 1.2 UI) | *(none prior to this PR)* | **UNSUPPORTED** → **PARTIAL_ALIVE** after `vocab::shui` land | Spec IRI `http://www.w3.org/ns/shacl-ui/` |
| Taffy / AOT layout | `taffy` workspace dep + `ggen-graph::layout::taffy_aot` six-width seal; court admits all six + refuses width-local overflow | **PARTIAL_ALIVE** (ggen fence sealed; zcode/igniter consume open) | Gate 1 photolithography still open |
| `*.brand.toml` / DTCG rhythm | `layout::brand` + `docs/rfc/v27.1.1/fixtures/{fourth-watch,cindycat}.brand.toml` | **PARTIAL_ALIVE** (rhythm); token inventory Gate 2+ | Gate 1 schema / Gate 2+ inventory |
| SUPPLE / ORC | no product surface under live crates | **UNKNOWN** | Name collision noise only (ORCID, ORCHESTRATOR_ALIVE prose) |
| ui-layout / zcode-cli | not in workspace members; no `ui-layout-court.ts` | **UNSUPPORTED** | External sibling |
| Zero-DOM / WebGPU | no live crate surface | **UNSUPPORTED** | Mode A |
| FFB (UI Mode B) | no live crate surface | **UNSUPPORTED** | Unrelated “Mode A/B” in wasm4pm examples is process-mining, not FFB |
| CASTLE | no publish/court surface in-repo | **UNKNOWN** | Gate 2–3 |
| Affidavit | `packs/affidavit-pack/**`; `examples/affidavit-verify/` (when present on tip); `ontology_catalogue/affidavit` | **PARTIAL_ALIVE** | Pack manufacture + reference port; not Plane-5 DO |
| `.aod` | no `*.aod` files in tree | **UNSUPPORTED** | |
| BRCE / AS-01 doctrine | `AGENTS.md`; `docs/architecture/*TAI*`; `docs/architecture/CI-CD-ARD-PRD-v26.7.31.md` | **PARTIAL_ALIVE** | Doctrine ALIVE as text; WebAuthn AS-01 runtime **UNKNOWN** in ggen |
| `curve25519-dalek` | `Cargo.lock` 4.1.3 + 5.0.0; `ed25519-dalek` workspace 3.0 | **PARTIAL_ALIVE** | Transitive; Gate-1 pin/docs court missing |
| `verify_witness` WASM &lt;250KB | `ORIGINAL_REQUEST.md` mentions `gall_verify_witness_adequacy`; no size court | **UNKNOWN** | |
| Plane 5 / revops / cos / GCP | fleet census names only | **UNKNOWN** / **OUT OF SCOPE** | Do not claim ALIVE |

## Search method

Ripgrep over live crates + docs (excluding `ontology_catalogue/**` noise where noted),
fleet census, pack tree, and `Cargo.lock`. Sibling repos (`truex`, `zcode-cli`,
`revops`) are named but not checked out.
