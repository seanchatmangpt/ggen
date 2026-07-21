# Fleet retrofit ledger

## Selection 1: seanchatmangpt/chicago-tdd-tools

| Field | Value |
|---|---|
| repository | seanchatmangpt/chicago-tdd-tools |
| selection reason | Real, known downstream consumer: ggen itself vendors a trimmed copy of this crate (packs/chicago-tdd-tools-pack, crates/chicago-tdd-tools — see CLAUDE.md PR #255 provenance note). Most recently pushed of the ggen-adjacent Rust crates in the census (2026-07-20T09:12:08Z). Public, Rust/cargo (clear build/test/lint commands), moderate GitHub config (10 labels, issues+wiki+projects on) to exercise gh-terraform-pack meaningfully without being trivial. |
| criticality | Medium — a dev-dependency/tooling crate, not a runtime-critical production service; no release-asset consumers found in the census beyond ggen's own vendored copy. |
| blast radius | Low-medium — public repo, no evidence of external production deployments; worst case is a broken CI/PR workflow on a tooling repo, not a live outage. |
| known consumers | ggen (vendored copy, not live dependency-edge); any other external consumers unconfirmed (Unknown, to be resolved in Phase 1). |
| known production effects | None observed at selection time — Unknown pending Phase 1 as-found survey. |
| proposed intervention depth | Preservation-or-repair leaning (small, active tooling repo) — final classification deferred to Phase 3 per repo-intervention-pack; not pre-judged here. |
| initial andon | YELLOW — protection state Unknown (census could not observe branch protection via unauthenticated/default read), several other facts Unknown pending Phase 1. |
| selection evidence | docs/gh-terraform/FLEET-CENSUS.md (generated 2026-07-21T03:57:00Z by scripts/gh/fleet-census.sh), row `seanchatmangpt/chicago-tdd-tools`. |

Rejected-for-now candidates (recorded, not silently dropped):
- `seanchatmangpt/wasm4pm` — branch protection already Present (higher existing governance, less retrofit learning value); multiple other ggen packs (wasm4pm-facts/cognition/algorithms) depend on its ontology surface, raising blast radius for a first attempt.
- `EpicGames/*`, `nuxt/ui-pro`, `IGP-AIoT/*`, `ORDADEV/*`, `Jonneal3/*`, `RadicalJustin/*`, `davidcolaco/*`, `petalframework/*`, `Miosa-osa/*` — not seanchatmangpt-owned; retrofitting a repository outside the operator's own ownership without a separate authorization is out of scope for an unattended first pass.
- `seanchatmangpt/cargo-cicd` — also a strong candidate (real dependency, referenced throughout this session's release-health work); deferred to a later retrofit round to keep this first pass isolated from the release-health-check investigation already in flight on `ggen` itself.

## Status
Phase 0 (selection): DONE, this record.
Phase 1 (as-found): IN PROGRESS — see .tcps/retrofit/as-found.ttl once generated.
