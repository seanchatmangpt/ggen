# ggen: Manufacturing Level Five Packs

**Version:** v26.7.19

This book is the complete source for a 336-chapter mdBook and its companion Level Five pack laboratory. Its objective is mechanical: by the end of the book, a reader should be able to create a ggen pack that generates, verifies, safely regenerates, receipts, and evolves a complete subsystem for a zero-knowledge consumer.

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

## Standing

This environment does not contain `cargo`, `rustc`, `ggen`, or `mdbook`. The repository is structurally verified, every `SUMMARY.md` link resolves, all chapters contain code and acceptance gates, pack/reference inventories are checked, manifests are generated, and the ZIP is tested. Rust compilation, actual ggen synchronization, and mdBook rendering must be executed in an approved toolchain before assigning binary or generator standing.

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
