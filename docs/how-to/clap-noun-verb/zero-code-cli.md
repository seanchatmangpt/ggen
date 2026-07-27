# Manufacture a zero-code `clap-noun-verb` CLI

The universal compiler turns an admitted RDF command graph into a complete Rust
binary. The application author owns two source files:

```text
my-cli/
├── ggen.toml
└── ontology.ttl
```

`ggen sync run` manufactures the manifest, crate roots, typed routes, behavior
interpreter, boundary adapters, integration tests, and command reference.
Generated Rust is a product of the graph. Do not edit it.

## 1. Compose the compiler

```toml
[project]
name = "my-cli"

[ontology]
source = "ontology.ttl"

[ontology.prefixes]
cnv = "https://clap-noun-verb.dev/ontology#"

[packs]
clap-noun-verb-schema-pack = { path = "../../packs/clap-noun-verb-schema-pack" }
clap-noun-verb-crate-pack = { path = "../../packs/clap-noun-verb-crate-pack" }
clap-noun-verb-routing-pack = { path = "../../packs/clap-noun-verb-routing-pack" }
clap-noun-verb-behavior-pack = { path = "../../packs/clap-noun-verb-behavior-pack" }
clap-noun-verb-boundary-pack = { path = "../../packs/clap-noun-verb-boundary-pack" }
clap-noun-verb-verification-pack = { path = "../../packs/clap-noun-verb-verification-pack" }

[templates]
dir = "."
aggregate_modules = false
```

The schema pack contains law only. It has no command individuals. The optional
`clap-noun-verb-specimen-pack` is deliberately absent, so framework composition
cannot change the CLI's public surface.

## 2. Declare the CLI graph

```turtle
@prefix cnv: <https://clap-noun-verb.dev/ontology#> .

cnv:ExampleCli
    a cnv:Cli ;
    cnv:binaryName "examplectl" ;
    cnv:crateName "examplectl" ;
    cnv:version "0.1.0" ;
    cnv:edition "2024" ;
    cnv:rustVersion "1.85" ;
    cnv:about "Example generated CLI." ;
    cnv:hasNoun cnv:SystemNoun .

cnv:SystemNoun
    a cnv:Noun ;
    cnv:name "system" ;
    cnv:about "Inspect the system." ;
    cnv:hasCommand cnv:SystemPing .

cnv:SystemPing
    a cnv:Command ;
    cnv:name "ping" ;
    cnv:about "Return liveness." ;
    cnv:belongsToNoun cnv:SystemNoun ;
    cnv:hasBehavior cnv:PingBehavior .

cnv:PingBehavior
    a cnv:StaticJsonBehavior ;
    cnv:jsonValue "{\"status\":\"alive\"}" .
```

Static values are JSON data, not Rust source. Malformed JSON reaches a generated
real-binary test and fails. Arbitrary source text is never evaluated.

## 3. Add typed arguments

Arguments are first-class RDF nodes. Every argument carries a lexical test
witness so the verification pack can exercise the real parser.

```turtle
cnv:ItemName
    a cnv:Argument ;
    cnv:name "name" ;
    cnv:fieldName "name" ;
    cnv:valueKind "string" ;
    cnv:required true ;
    cnv:position 1 ;
    cnv:testValue "Widget" ;
    cnv:about "Item identifier." .
```

Options use position `0` and require a long flag:

```turtle
cnv:ItemTag
    a cnv:Argument ;
    cnv:name "tag" ;
    cnv:fieldName "tag" ;
    cnv:valueKind "string" ;
    cnv:required false ;
    cnv:position 0 ;
    cnv:testValue "production" ;
    cnv:longFlag "tag" ;
    cnv:shortFlag "t" ;
    cnv:environmentVariable "INVENTORY_TAG" ;
    cnv:about "Item tag." .
```

The compiler maps the closed value-kind set to Rust types:

| RDF value kind | Rust type |
|---|---|
| `string` | `String` |
| `i64` | `i64` |
| `u64` | `u64` |
| `f64` | `f64` |
| `bool` | `bool` |
| `path` | `PathBuf` |

Unsupported kinds fail before rendering.

## 4. Select one behavior

Every command has exactly one admitted behavior kind:

- `cnv:StaticJsonBehavior`
- `cnv:EchoBehavior`
- `cnv:ExpressionBehavior`
- `cnv:FilesystemWriteBehavior`
- `cnv:FilesystemListBehavior`
- `cnv:RefusalBehavior`
- `cnv:CustomBehavior` (see [Opting a command out of closed zero-code](#opting-a-command-out-of-closed-zero-code) below)

Generated commands dispatch through a Rust enum. There is no string-selected
handler and no `crate::handlers::*` seam for any of the first six kinds.

A mutating filesystem command must use a boundary that admits `write-json`,
requires a receipt, and declares a replay policy other than `none`:

```turtle
cnv:InventoryStore
    a cnv:FilesystemBoundary ;
    cnv:rootPath ".inventory/items" ;
    cnv:allowedOperation "read-json", "write-json" ;
    cnv:receiptRequired true ;
    cnv:replayMode "verify-observed-output" .
```

The generated adapter validates relative paths, refuses parent traversal and
symlink entries, writes a fully synced temporary inode, publishes it through a
no-clobber hard link, and returns a BLAKE3 digest bound to the persisted bytes.

## Opting a command out of closed zero-code

`cnv:CustomBehavior` is the sole, explicitly admitted escape hatch. It is
per-command, not per-CLI: most commands can stay on the closed six-primitive
set while one command opts out.

```turtle
cnv:PricingLookup
    a cnv:Command ;
    cnv:name "lookup" ;
    cnv:about "Look up a live price." ;
    cnv:belongsToNoun cnv:PriceNoun ;
    cnv:hasBehavior cnv:PricingBehavior .

cnv:PricingBehavior a cnv:CustomBehavior .
```

The first `ggen sync run` scaffolds `src/custom_handlers.rs` with one typed
stub per opted-out command (`pub fn <noun>_<verb>(inputs: Map<String, Value>)
-> Result<Value>`), each body a bare `todo!(...)`. That file is written with
`unless_exists: true` — every later `ggen sync run` leaves it alone, so your
hand-written domain logic is never clobbered. A missing or misnamed handler
function is a Rust compile error, not a silent generation gap; a generated
test (`every_custom_command_routes_to_its_own_handler` in
`src/generated_cli.rs`) additionally proves each custom command routes to its
*own* handler and not a neighboring command's.

A command opting into `cnv:CustomBehavior` is, for that command only, no
longer zero-code — it requires hand-written Rust, same as any ordinary
`crate::handlers::*` seam. The rest of the CLI stays fully closed and
receipted. This is the intended tradeoff for domain logic the six primitives
cannot express (calling an external API, querying a database, multi-step or
conditional business rules) — not a general-purpose way to bypass admission.

See `examples/zero-code-custom-handler-demo/` for a complete, runnable proof:
one closed `system ping` command alongside one `price lookup` command that
opts out and looks up a price from a small hand-written table (a real branch
on argument value, not expressible by any of the six primitives). Its
`src/custom_handlers.rs` is committed as authored source (not a scaffolded
`todo!()` stub) so `cargo test` after `ggen sync run` exercises real,
deterministic output.

## 5. Manufacture and verify

```bash
ggen sync run
cargo fmt --check
cargo clippy --all-targets -- -D warnings
cargo test --all-targets -- --nocapture
cargo run -- --help
```

The generated integration suite invokes the compiled binary through
`CARGO_BIN_EXE_*`. It does not call route functions directly and does not mock
boundaries. Required-argument and unknown-command paths must fail nonzero.
Filesystem tests read the real persisted bytes and recompute the emitted BLAKE3
digest.

Run sync a second time and compare generated bytes. A graph and pack closure that
did not change must manufacture the identical product.

## Authority boundaries

```text
ontology.ttl       application authority
compiler packs     manufacturing law
generated Rust     derived product
runtime boundary   admitted actuation
BLAKE3 response    content receipt
generated tests    executable falsifier
```

A command does not qualify as zero-code when it contains a consumer-authored
Rust handler that was not explicitly admitted via `cnv:CustomBehavior`, a
generated placeholder, an unadmitted boundary, or an unreceipted mutation.
