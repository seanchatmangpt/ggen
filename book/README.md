# ggen: Manufacturing Level Five Packs

**Version:** v26.7.19

This repository is the complete source for a 336-chapter mdBook and its companion Level Five pack laboratory. Its objective is mechanical: by the end of the book, a reader should be able to create a ggen pack that generates, verifies, safely regenerates, receipts, and evolves a complete subsystem for a zero-knowledge consumer.

## Canonical equation

```text
pack + ggen
    -> complete subsystem
    + independent proof
    + safe regeneration
    + drift refusal
    + lifecycle receipts
```

## Included products

- `src/` — the complete mdBook source.
- `src/listings/` — one companion code listing for every numbered chapter and appendix.
- `code/packs/` — canonical Level Five packs, including the TCPS core and release packs.
- `code/examples/` — real consumer layouts and certification laboratories.
- `code/case-studies/tcps-v26.7.19-reference/` — the complete 132-file production reference product.
- `scripts/` — structural checks, manifest generation, static assembly, and Level Five acceptance checks.
- `schemas/` — receipt and certification schemas.
- `dist/` — an assembled Markdown edition and a standalone HTML edition.

> **Admission-mechanism update (2026-07-19).** After this book's v26.7.19 authoring, the ggen
> engine replaced SHACL (`shapes.ttl` / `[law].shapes`) as its admission mechanism with SPARQL
> gates: per-pack `gates/*.rq` files (`# MESSAGE:` header + one ASK or SELECT query;
> `FM-PACK-012`/`FM-PACK-013`) and project-level `[law].gates`
> (`FM-LAW-012`/`FM-LAW-013`). A legacy `shapes.ttl` in a pack is now a loud typed refusal.
> The chapter text and listings in `src/` have been updated to teach the gates mechanism as
> current; SHACL is referenced only as history. See
> `crates/ggen-engine/src/sync.rs` (`parse_gate_source`/`evaluate_gate`) and
> `crates/ggen-engine/tests/reasoner_independence_e2e.rs` in the host repository.

## Standing

The original authoring environment did not contain `cargo`, `rustc`, `ggen`, or `mdbook`; the repository was structurally verified (every `SUMMARY.md` link resolves, all chapters contain code and acceptance gates, pack/reference inventories checked, manifests generated, ZIP tested). The book now lives inside the ggen repository, and the headline case-study results have since been independently rerun there — see `SOURCE_NOTES.md` for the 2026-07-19 rerun record. mdBook rendering and full-product synchronization must still be executed in an approved toolchain before assigning binary or generator standing to any artifact not covered by that rerun.

## Build

```bash
python3 scripts/check_book.py
python3 scripts/check_level_five.py
python3 scripts/assemble_book.py

# In a machine with mdBook and the target toolchain:
mdbook build
cd code/examples/canonical-level-five-consumer
ggen sync run
cargo test --all-targets
```
