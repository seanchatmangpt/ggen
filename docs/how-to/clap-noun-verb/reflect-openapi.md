# Reflect an OpenAPI spec into a zero-code CLI

`crates/openapi-cnv-reflect` derives a `cnv:Cli` ontology from an OpenAPI 3.x
document instead of hand-authoring one RDF fact block per command (see
[zero-code-cli.md](zero-code-cli.md) for the compiler this ontology feeds).
For a CLI wrapping an existing read API, this turns *N endpoints ×
(ontology-authoring time per endpoint)* into *one reflection pass + review*.

## What it covers (80/20 scope — read this before reflecting a real spec)

| Supported | Not supported (skipped with a reported warning, never silently dropped) |
|---|---|
| OpenAPI **3.x JSON** | YAML input, Swagger 2.0 |
| `GET` operations only | `POST`/`PUT`/`DELETE`/`PATCH`/etc. |
| Path and query parameters of scalar type (`string`/`integer`/`number`/`boolean`) | Request bodies, array/object schemas, auth headers |
| A required boolean **path** parameter, or a required boolean **query** parameter | Refused by the compiler's own closed argument model (booleans are always optional presence flags) — reflected as a skip instead of handing the compiler a provably-refused ontology |

Every reflected command carries `cnv:CustomBehavior` — the sole explicitly
admitted handler-seam escape hatch (see zero-code-cli.md's "Opting a command
out of closed zero-code" section) — because a real HTTP call is never one of
the six closed primitives. The reflector's leverage is eliminating the *N*
ontology-authoring actions and *N* typed-stub-writing actions; it does not
eliminate writing the HTTP call itself. That's deliberate, separable scope
(a future closed `cnv:HttpBehavior` primitive is a different piece of work).

## Usage

```bash
cargo run -p openapi-cnv-reflect -- path/to/openapi.json path/to/consumer-dir
```

Writes `path/to/consumer-dir/ontology.ttl` and reports every skipped
operation (and why) to stderr. Add a `ggen.toml` next to it composing the
six `clap-noun-verb-*-pack`s (same composition as any zero-code consumer —
see zero-code-cli.md §1), then:

```bash
ggen sync run
```

manufactures the complete crate, with one scaffolded `src/custom_handlers.rs`
stub per reflected command (`unless_exists: true` — never overwritten by a
later sync, so your hand-written domain logic is safe once you fill it in).

## Verified end to end

`crates/openapi-cnv-reflect/tests/reflect_e2e.rs` reflects a real fixture
(`tests/fixtures/petstore-slice.json`, 4 `GET` operations across 2 tags plus
one deliberately-included `POST` to prove the skip-with-warning path), feeds
the output through the **real** `ggen` binary and the real schema-pack
gates, and asserts the manufactured crate builds and its own generated tests
(including the per-command wiring-proof test) pass — no mocks.
