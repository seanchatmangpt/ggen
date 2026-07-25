# The ggen Pack Language

**Version:** v26.7.59 documentation branch

This mdBook is a pattern language for writing ggen packs that manufacture verified software. It does not treat every chapter as an already implemented feature. Each pattern must be read against the repository’s live ownership surfaces, committed packs, consumers, tests and explicit gaps.

Start with the [Repository Capability and Pack Map](CAPABILITY_MAP.md). It distinguishes:

- an implemented crate or command;
- a committed pack witness;
- a partially demonstrated capability;
- a normative target with no current witness;
- historical evidence that cannot establish current standing.

## Canonical equation

```text
A = μ(O*)

admitted observation
    → lawful manufacturing
    → consumer-visible artifact
    → independent verification
    → replayable receipt
```

For a pack writer, this expands to:

```text
pack law + ggen
    → admitted graph state
    → deterministic projection
    → bounded write
    → real consumer consequence
    → verifier evidence
    → receipt or typed refusal
```

## Repository-grounded reading rule

Every chapter must answer five concrete questions:

1. Which live crate or command owns this capability?
2. Which exact repository path can falsify the chapter’s claim?
3. Which committed pack demonstrates the pattern, when one exists?
4. Which consumer or test observes the consequence?
5. Is the chapter currently `IMPLEMENTED`, `PACK_WITNESS`, `PARTIAL`, or `TARGET`?

A marketplace descriptor, archived document, generated listing or case-study statement is not by itself proof of a live pack capability.

## Included products

- `src/` — the mdBook source and pattern chapters.
- `src/CAPABILITY_MAP.md` — current crate, pack, consumer and standing alignment.
- `src/listings/` — companion listings used by chapter laboratories.
- `code/packs/` — canonical Level Five laboratory packs.
- `code/examples/` — consumer layouts and certification laboratories.
- `code/case-studies/tcps-v26.7.19-reference/` — the TCPS reference product retained for comparison.
- `scripts/` — structural checks, corpus transformation and Level Five acceptance checks.
- `schemas/` — receipt and certification schemas.

The repository’s live `packs/`, `crates/`, `examples/` and `tests/` directories take precedence over book-local examples when establishing current capability standing.

## Standing

The book source and pattern grammar are structurally materialized. Capability standing is mixed:

- deterministic graph, manifest, engine, pack-resolution and marketplace primitives are implemented;
- several packs have real consumer and proof witnesses;
- complete multi-pack, multi-target Level Five substitution remains partial;
- some practicum and certification chapters are normative targets until their full current-head laboratories execute.

Do not promote the whole book to `ALIVE` from structural checks alone.

## Build and falsification

```bash
python3 book/scripts/check_book.py
python3 book/scripts/check_level_five.py
python3 book/scripts/assemble_book.py

# Representative live capability checks
cargo test -p ggen-graph
cargo test -p ggen-marketplace
cargo test -p ggen-engine --test reasoner_independence_e2e

# Book rendering
mdbook build book
```

Record exact command outcomes. A command printed here is a falsifier, not evidence that it passed in the reader’s environment.