# genesis-schema-v2 — audit (generation-first, 2026-07)

**Purpose:** "KNHK V2 schema system — OpenAPI specs, RDF ontology, 43 YAWL pattern definitions, workflow schema validation." (`crates/genesis-schema-v2/Cargo.toml`).

## LOC

`tokei crates/genesis-schema-v2/src --output json` (tokei Rust code lines):

| File | Code LOC |
|---|---|
| src/lib.rs | 205 |
| **Total** | **205** |

## Consolidation status

`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §2 (line 30) + §3 (line 39): CONSOLIDATE into `genesis-types-v2` as a `schema.rs` module (single consumer: `genesis-core-v2`; zero tests, zero outgoing workspace deps). **Consolidation ≠ dead**: the code survives the merge, so it is classified for generatability, not DEAD-DELETE.

## Class breakdown

| Class | LOC (est.) | Basis |
|---|---|---|
| GENERATABLE-WITH-SPEC | 205 | whole-file read (not a sample): the file is 13 `#[derive(Serialize, Deserialize)]` structs/enums mirroring OpenAPI 3.1 (`OpenApiSpec`, `ApiInfo`, `PathItem`, `Operation`, `Parameter`, `RequestBody`, `MediaType`, `Response`, `SchemaRef`, `JsonSchema`, `Components`), a 3-field `RdfOntology` with a trivial `to_turtle()` string join, `PatternMetadata`, and a `HashMap`-backed `PatternRegistry` with register/get/list methods. No algorithm exceeds a match-on-HTTP-method or a filter. |

## Per-item rationale

- OpenAPI structs (lines 12–130): pure serde mirrors of a published spec — the canonical GENERATABLE-WITH-SPEC shape; an OpenAPI-vocabulary TTL would emit them mechanically.
- `RdfOntology` (132–158): 3 fields + `format!`-loop `to_turtle`; boilerplate.
- `PatternMetadata`/`PatternRegistry` (160–207): registry boilerplate; the 43-YAWL-pattern vocabulary is exactly the kind of data ggen's ontology layer should own (`genesis-schema-v2` even advertises an "RDF ontology" in its own doc comment while containing none as TTL).

## Nearest template/spec

No emitting template today. Nearest spec material: `.specify/codegen-annotations.ttl` / `.specify/cli-schema.ttl` families show the struct-from-TTL pattern; a `yawl-patterns.ttl` + serde-struct Tera template would cover this file. No `.specify/*yawl*` or `*openapi*` spec currently exists (checked `ls .specify`).
