# ggen v6.1.0 — Marketplace and packs scope

**Status:** Engineering track (product slice)  
**Last updated:** 2026-04-01

This document defines the **marketplace / governed packs** slice targeted for the **v6.1.0** minor release. It complements (does not replace) security-focused items listed for v6.1.0 in [docs/security/V6_MIGRATION.md](../security/V6_MIGRATION.md) (API authentication, multi-tenant isolation, enhanced audit logging).

## Minimum shippable (v6.1.0 packs)

1. **Lockfile authoring** — `ggen packs install` updates `.ggen/packs.lock` in the current project when installation succeeds.
2. **Pack cache alignment** — `PackResolver` reads packs from `~/.ggen/packs` by default, overridable with `GGEN_PACK_CACHE_DIR` (tests and CI).
3. **μ₂ integration** — Pack-provided **CONSTRUCT-only** `.rq` queries under `<cache>/<atomic-pack>/queries/` run in μ₂ after standard tensor queries (see [PACK_QUERY_CONTRACT.md](PACK_QUERY_CONTRACT.md)).
4. **μ₃ staging** — Pack `.tera` files are copied to `<project>/.ggen/pack-stage/` for emission rules or follow-up work; receipt records contributed template paths.
5. **Receipts** — `PackProvenance` lists real query/template identifiers and a **content digest** for local packs (not placeholder `sha256:verified`).
6. **Trust configuration** — `GGEN_MARKETPLACE_PUBLIC_KEY` enables Ed25519 verification in `ggen-marketplace` when signature checks run.
7. **Epoch for pack-only runs** — Pipeline creates a synthetic epoch from the merged pack graph so receipts do not panic when no project ontology files are loaded.

## Explicit deferrals

- Restoring removed `ggen marketplace *` CLI commands.
- Public registry UX and community marketplace.
- Automatic emission rules generated from pack templates without project configuration.
- Slidev / XLSX and other non-code charter targets.

## Related docs

- [PACK_QUERY_CONTRACT.md](PACK_QUERY_CONTRACT.md) — SPARQL rules for pack queries.
- [PIPELINE_INTEGRATION.md](PIPELINE_INTEGRATION.md) — μ-stage overview (update when code matches).
- [FORTUNE_5_LEAN_SIX_SIGMA_CHARTER.md](FORTUNE_5_LEAN_SIX_SIGMA_CHARTER.md) — Strategic CTQs and tollgates.
