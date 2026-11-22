# Repo inventory — ECOSYSTEM-SPEC-27.1.1

Base: `origin/main` @ `d173cad5792e837bd4fec730d120c582b9bc5d4c` (observed 2026-11-22).
Standing labels: ALIVE | PARTIAL_ALIVE | UNKNOWN | UNSUPPORTED | BLOCKED.

| Subject | Paths / evidence | Standing | Notes |
|---------|------------------|----------|-------|
| Truex (product / sibling) | `docs/gh-terraform/FLEET-CENSUS.md` → `seanchatmangpt/truex`; sibling admit-court on `cursor/v2711-truex-admit-court-bc54` @ `e28e68300` (truex#6); catalogue TTL remains description-only | **PARTIAL_ALIVE** | Admit-court 6/6 vitest exit 0 (`shui:` ADMITTED; private `truex:` REFUSED); not full `ALIVE` until merge to truex `main` |
| `truex:` as private prefix | `ORIGINAL_REQUEST.md` (banned); `crates/ggen-graph/src/bin/gall_observe_worktree.rs` banned IRI tokens; marketplace registry id `truex` | **ALIVE** (ban) | Private-prefix ban is live; distinct from W3C `shui:` |
| `forbid_private_namespaces` / public prefix scan | `crates/ggen-graph/src/bin/gall_actuate_code_evaluation.rs` (`allowed_prefixes`, `check_uri`); `crates/ggen-lsp/assets/policies/no-private-namespace.shacl.ttl` | **PARTIAL_ALIVE** | Live scanners; `shui` absent until Gate-1 slice |
| `shui:` (W3C SHACL 1.2 UI) | *(none prior to this PR)* | **UNSUPPORTED** → **PARTIAL_ALIVE** after `vocab::shui` land | Spec IRI `http://www.w3.org/ns/shacl-ui/` |
| Taffy / AOT layout | `taffy` workspace dep + `ggen-graph::layout::taffy_aot` six-width seal; court admits all six + refuses width-local overflow | **PARTIAL_ALIVE** (ggen fence sealed; zcode/igniter consume open) | Gate 1 photolithography still open |
| `*.brand.toml` / DTCG rhythm | `layout::brand` + `docs/rfc/v27.1.1/fixtures/{fourth-watch,cindycat}.brand.toml` + `brand_toml_completeness_court` | **ALIVE** (lineage tip-cargo completeness **5** + rhythm **12** EXIT 0 @ `c91b22561`; [`BRAND-TOML-TIP-CARGO-STANDING.md`](./BRAND-TOML-TIP-CARGO-STANDING.md)); token inventory Gate 2+; production theme **UNSUPPORTED** | Gate 1 schema / Gate 2+ inventory |
| SUPPLE / ORC | no product surface under live crates | **UNKNOWN** | Name collision noise only (ORCID, ORCHESTRATOR_ALIVE prose) |
| ui-layout / zcode-cli | not in workspace members; no `ui-layout-court.ts` | **UNSUPPORTED** | External sibling |
| Zero-DOM / WebGPU | no live crate surface | **UNSUPPORTED** | Mode A |
| FFB (UI Mode B) | no live crate surface | **UNSUPPORTED** | Unrelated “Mode A/B” in wasm4pm examples is process-mining, not FFB |
| CASTLE | no publish/court surface in-repo | **UNKNOWN** | Gate 2–3 |
| Affidavit | `packs/affidavit-pack/**`; `examples/affidavit-verify/` (when present on tip); `ontology_catalogue/affidavit` | **PARTIAL_ALIVE** | Pack manufacture + reference port; not Plane-5 DO |
| `.aod` | no `*.aod` files in tree | **UNSUPPORTED** | |
| BRCE / AS-01 doctrine | `AGENTS.md`; `docs/architecture/*TAI*`; `docs/architecture/CI-CD-ARD-PRD-v26.7.31.md`; `docs/rfc/v27.1.1/BRCE-WEBAUTHN-STANDING.md`; `ggen-abb-sbb::as01_seat` presence fence | **PARTIAL_ALIVE** | Doctrine + ungated-DO refusal court; WebAuthn/`prime256v1` verify **UNSUPPORTED**; broker/Plane-5 DO **BLOCKED**/out of ggen |
| `curve25519-dalek` | `Cargo.lock` 4.1.3 + 5.0.0; `ed25519-dalek` workspace 3.0; [`CURVE25519-DALEK-PIN-STANDING.md`](./CURVE25519-DALEK-PIN-STANDING.md); `curve25519_dalek_pin_court` | **ALIVE** | Lock + pin doc + court **6 passed; EXIT 0** @ tip `bebea5dd0` (`cursor/v2711-curve25519-court-bc54`) |
| `verify_witness` WASM &lt;250KB | `ggen_graph::witness` + `fixtures/witness/verify_witness.placeholder.wasm` (**35** B) + size court **ALIVE** @ `f5f44571f` (absorb base `9173c6529` post-Smalt; sources ≡); affidavit pin size court **ALIVE** @ `563eb322a` (`VERIFY-WITNESS-AFFIDAVIT-SIZE-COURT-STANDING.md`); production path doc `VERIFY-WITNESS-PRODUCTION.md` | Size court **ALIVE**; affidavit pin **ALIVE**; ggen production wire **UNSUPPORTED** until `GGEN_VERIFY_WITNESS_WASM` courted | Gate 1 |
| Plane 5 / revops / cos / GCP | fleet census names only | **UNKNOWN** / **OUT OF SCOPE** | Do not claim ALIVE |
| `packs/gh-terraform-pack` (Real-state **FM-PACK-001**) | path **PRESENT** (`test -d packs/gh-terraform-pack` → exit **0**; tree `c7799cd6`); consumers retargeted; crown `composed_packs_e2e` **ALIVE** (lineage tip-cargo) | **ALIVE** (lineage tip-cargo) | Lineage tip-cargo **2 passed; exit 0** @ evidence `19c08a3b0` / court tip `a508e6bae` (sources ≡ absorb tip `fbfaa3ad9`; log `/tmp/fm-pack-001-composed-e2e.log`). Land `cursor/v2711-fm-pack-001-alive-bc54` @ `ce613b05d`. Ledger: [`FM-PACK-001-ALIVE-STANDING.md`](./FM-PACK-001-ALIVE-STANDING.md). Replay: `CARGO_BUILD_JOBS=1 cargo test -p ggen-engine --test composed_packs_e2e --target-dir /tmp/ggen-fm-pack-001-alive-target --config build.jobs=1 -- --nocapture` → exit **0** |

## Search method

Ripgrep over live crates + docs (excluding `ontology_catalogue/**` noise where noted),
fleet census, pack tree, and `Cargo.lock`. Sibling repos (`truex`, `zcode-cli`,
`revops`) are named but not checked out.
