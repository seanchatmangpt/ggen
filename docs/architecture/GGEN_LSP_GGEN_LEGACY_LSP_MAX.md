# ggen LSP triad: ggen × ggen-legacy × lsp-max

## Status

This document defines the bounded architecture for finishing `crates/ggen-lsp`.
It does not grant executable standing. Rust compilation and protocol execution
remain required falsifiers.

## Exact observations

| Role | Repository / package | Exact identity | Admission |
|---|---|---|---|
| Product and domain authority | `seanchatmangpt/ggen` | base `fe7fa2f14ba5a4d72c89a94fb61d18f8944377fc` | target implementation |
| Predecessor evidence | `seanchatmangpt/ggen-legacy` | head `70e599a599fedb7c62c965377cc2f80df1fa01ec` | evidence only |
| Stable predecessor coordinate | `ggen` recorded by Project 001 | `0f39227c102e0ac7519f0f27561356227a518653` | observable legacy contract |
| LSP runtime | `lsp-max` package family | exact pin `26.7.3` | runtime and protocol |

`ggen-legacy` Project 001 records the stable predecessor coordinate and keeps
replacement standing separate from predecessor retirement. The LSP rewrite uses
that corpus as provenance and equivalence authority; it does not import the
legacy repository into the runtime graph.

## Preserve

The established `ggen-lsp` paths remain authoritative for RDF, SPARQL, Tera,
TOML, project indexing, repair routing, OCEL events, receipts, MCP, A2A, and
stdio framing.

The legacy source-law contract preserves:

- `GGEN-SRC-002`: generated/non-editable caste banners are errors;
- `GGEN-SRC-003`: lesser-source instructions are errors;
- `GGEN-SRC-004`: generated Rust module edges require generation authority.

## Fence

The repositories have disjoint authority:

```text
ggen-legacy  -> observed predecessor contract and exact coordinates
ggen         -> project graph, generation rules, diagnostic policy
lsp-max       -> LSP protocol, incremental AST, MAX diagnostic envelope
```

Forbidden edges:

```text
ggen-legacy -/-> production runtime dependency
raw text      -/-> module authority
file exists   -/-> generation authority
dynamic path  -/-> proven absence
Diagnostic    -/-> standing
```

## Calculus

```text
Rust buffer
  -> lsp-max AutoLspAdapter
  -> Tree-sitter Rust mod_item nodes
  -> ggen ProjectIndex generated-output set
  -> GGEN-SRC-004 admission/refusal
  -> MaxDiagnostic
  -> legacy-contract provenance payload
  -> existing ggen route / OCEL / receipt lifecycle
```

Only formal `mod_item` nodes manufacture candidate module edges. Text inside
comments, ordinary strings, and raw strings has no execution or authority.

A module edge is admitted when one of its Rust candidates is owned by a static
`ggen.toml` generation rule. File existence alone is insufficient. A dynamic
Rust output keeps the identity `UNKNOWN`; the analyzer refuses to fabricate a
missing-authority diagnostic before construction resolves the path.

## Diagnostic provenance

Each GGEN-SRC diagnostic carries:

- contract schema and diagnostic code;
- `ggen` as product authority;
- exact `ggen-legacy` evidence head;
- exact stable predecessor `ggen` coordinate;
- exact `lsp-max` package version.

This payload is evidence lineage, not a self-issued receipt or standing claim.

## Exclusions

This rewrite does not:

- make `ggen-legacy` a Cargo dependency;
- claim equivalence for unrelated LSP features;
- advertise call/type hierarchy directions that are not implemented;
- infer concrete ownership for dynamic Tera output paths;
- replace Cargo compilation, LSP stdio execution, or replay evidence.

## Falsifiers

```bash
cargo check -p ggen-lsp --all-features
cargo test -p ggen-lsp source_law_analyzer
cargo test -p ggen-lsp --test ggen_src_004_living_loop
cargo test -p ggen-lsp
cargo clippy -p ggen-lsp --all-targets --all-features -- -D warnings
```

Behavioral falsifiers include:

1. a comment or string produces `GGEN-SRC-004`;
2. an unowned external `mod` does not produce `GGEN-SRC-004`;
3. an owned `foo.rs`, `foo/mod.rs`, or `#[path]` module is rejected;
4. an inline nested module resolves against the wrong directory;
5. an unsaved Rust or `ggen.toml` repair fails to clear the same episode;
6. diagnostic provenance omits any of the three repository roles;
7. existing RDF/Tera/TOML behavior changes.

## Standing

Until the commands above execute against the exact branch head, the rewrite is
`PARTIAL_ALIVE`. A compiler or test failure changes the state to `BUILD_BROKEN`.
