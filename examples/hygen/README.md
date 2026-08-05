# examples/hygen — ggen recreations of the Hygen docs' own examples

Four small, real, runnable ggen projects, each one the RDF-bound
equivalent of a specific example from Hygen's own documentation (see
`docs/research/hygen-and-hygen-create-reference.md` for the source
material and `docs/research/ggen-docs-hygen-parity.md` for the full
mechanic-by-mechanic mapping). Every example here was actually run
against the real `ggen` binary while building this directory — the
"Verified output" block in each section below is real captured output,
not a prediction.

The substitution these examples demonstrate throughout: **wherever Hygen's
docs read a value from a `--flag` or an interactive prompt, these examples
read the same value from an RDF individual, queried via SPARQL.** Nothing
else about Hygen's frontmatter vocabulary (`to:`, `inject:`, `skip_if:`,
`sh_before:`/`sh_after:`) changes.

## 1. `worker-example/` — the quick-start worker class

Mirrors Hygen's `Templates` doc walkthrough: a template reading `name` and
`message`, producing a class whose method body uses an uppercased
`message`. Hygen's blessed `Name` (auto-capitalized `name`) becomes
`name | pascal_case`; the manual `Message = message.toUpperCase()` EJS
scriptlet becomes a Tera `upper` filter.

```bash
cd examples/hygen/worker-example
ggen sync run
cat app/workers/foobar.rs
```

**Verified output** (`app/workers/foobar.rs`):
```rust
pub struct Foobar;

impl Foobar {
    pub fn work(&self) -> &'static str {
        "HELLO"
    }
}
```

## 2. `mailer-example/` — the mailer html/text generator

Mirrors Hygen's `Generators` doc walkthrough: `hygen mailer new --name
foobar --message hello --version 1`, which renders **two** files
(`html.ejs.t`, `text.ejs.t`) from one folder — the "folder is command
structure" idea. Here, one `ex:Campaign` individual drives two independent
templates (`templates/html.tmpl`, `templates/text.tmpl`), each with its
own `sparql:`/`for_each:` binding to the same fact.

```bash
cd examples/hygen/mailer-example
ggen sync run
cat app/emails/foobar.html app/emails/foobar.txt
```

**Verified output** (`app/emails/foobar.html`):
```html
<h1>Hello foobar</h1>
hello
(version 1)
```
**Verified output** (`app/emails/foobar.txt`):
```
Hello foobar
hello
(version 1)
```

## 3. `inject-example/` — dependency injection with idempotency

Mirrors Hygen's `Templates` doc injection example: add a dependency into
an existing manifest, `skip_if`-guarded so a repeat run never duplicates
the line. Hygen's example target is `package.json`; this one's is
`Cargo.toml` (`[dependencies]`), matching ggen's actual target ecosystem.

```bash
cd examples/hygen/inject-example
ggen sync run   # injects: serde = { version = "1", features = ["derive"] }
ggen sync run   # run again — proves skip_if idempotency
cat Cargo.toml
```

**Verified output**, run twice — the second run's real reported decision:
```json
{
  "written": [],
  "skipped": [["Cargo.toml", "skip_if: existing file already contains \"serde =\""]],
  "decisions": { "Cargo.toml": "skipped: skip_if: existing file already contains \"serde =\"" }
}
```
`Cargo.toml` after both runs (unchanged after the second, exactly one
`serde =` line, matching Hygen's own claim for its `package.json`
example — *"it will not add it twice"*):
```toml
[package]
name = "hygen-inject-example"
version = "0.1.0"
edition = "2021"

[dependencies]
serde = { version = "1", features = ["derive"] }
```

## 4. `react-component-example/` — case-conversion helpers

Mirrors Hygen's `Templates` doc change-case example: given `name =
HelloWorld`, derive every case variant a template needs from that one
value. This one is an **honest adaptation**, not a 1:1 port: Hygen's
example uses `h.changeCase.paramCase` to emit a kebab-case CSS class in a
JSX component; ggen does ship a `kebab_case` filter (Inflector-backed,
`crates/ggen-engine/src/template.rs`, alongside `snake_case`, `pascal_case`,
`camel_case`, `shouty_snake_case`, `title_case`, `pluralize`/`singularize`,
and five Rails-style inflection filters — `ordinalize`, `demodulize`,
`foreign_key`, `tableize`, `classify`), but the example still targets Rust,
not JSX, so it uses `snake_case`/`pascal_case` for the module path and
struct name rather than reproducing a CSS class. The example keeps Hygen's
core point (derive case variants from one canonical value) with the filters
that fit ggen's actual target language, stated as an adaptation rather than
presented as equivalent.

```bash
cd examples/hygen/react-component-example
ggen sync run
cat src/components/hello_world.rs
```

**Verified output** (`src/components/hello_world.rs`):
```rust
pub struct HelloWorld {
    pub children: String,
}

impl HelloWorld {
    pub fn render(&self) -> String {
        format!("<div class=\"{}\">{}</div>", "hello_world", self.children)
    }
}
```

## What's deliberately not here

`docs/research/ggen-docs-hygen-parity.md` §6 already states this
explicitly, repeated here for anyone browsing only this directory: there is
no example for Hygen's `.hygen.js` extensibility mechanism, because ggen
has no equivalent — extending ggen means extending the ontology and packs,
not registering an arbitrary helper function into the template engine.
There is likewise no `hygen-create`-style "derive a generator from existing
working code" example; see that doc's §10 for `ggen init-self`/`ggen pack
new`, which solves the adjacent problem (constructing a new pack without
hand-assembly) a structurally different way.

## Relationship to `crates/ggen-engine/tests/hygen_parity_e2e.rs`

This directory and that test file cover overlapping ground for different
audiences, and should be read together, not confused for one another.
`hygen_parity_e2e.rs` is the **authoritative regression gate** — four
compiled `CliHarness` subprocess tests (component + barrel-injection in one
run, `unless_exists`, `skip_if`, `sh_after`) that actually run in `cargo
test`/CI and fail the build if parity breaks. The four projects in this
directory are **hands-on, independently runnable walkthroughs** for a human
reading the docs — nothing here runs in CI. `docs/testing/
HYGEN_PARITY_AND_E2E_STRATEGY.md` is the fuller strategy doc behind that
test file, including the parity matrix with `file:line` evidence and the
Gall's-Law argument for building e2e coverage incrementally rather than all
at once; `docs/research/ggen-docs-hygen-parity.md` (this directory's
companion doc) covers the same parity ground with fuller Hygen-docs-side
narrative (installation, packages, extensibility) that the strategy doc
doesn't need for its CI-gate purpose. Start at whichever doc matches what
you're trying to do: extend the regression gate → `hygen_parity_e2e.rs` +
the strategy doc; understand or demo a specific mechanic → this directory.

## Every project here is real and independently runnable

Each subdirectory is a self-contained `ggen.toml` + `ontology.ttl` +
`templates/` project — `cd` into any one of them and run `ggen sync run`
yourself; nothing here depends on the others or on the parent repo's own
`ggen.toml`.
