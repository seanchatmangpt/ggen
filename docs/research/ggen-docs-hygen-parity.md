# ggen — Documentation (Hygen-Parity Edition)

> Organized to mirror `docs/research/hygen-and-hygen-create-reference.md`
> section-for-section, so the two can be read side by side. Every mechanic
> described here is real and verified against this repo's own source
> (`crates/ggen-engine/src/{template,sync,write,config,generation_rules}.rs`,
> `crates/ggen-config/src/manifest/types.rs`, and real files under
> `packs/*`) — not aspirational, not mirrored from Hygen's behavior by
> assumption. Where ggen has no equivalent to something Hygen does, that is
> stated explicitly rather than invented.

**Last compiled:** 2026-08-05

**Companion docs:** `docs/testing/HYGEN_PARITY_AND_E2E_STRATEGY.md` is the
CI-facing counterpart to this document — same parity ground, plus the four
compiled regression tests (`crates/ggen-engine/tests/hygen_parity_e2e.rs`)
that actually gate the build, a `file:line` evidence table, and the Gall's
Law argument for how e2e coverage should grow across the rest of the
workspace. This document has fuller Hygen-docs-side narrative (installation,
packages, extensibility, FAQ) that doc doesn't need for its narrower
purpose. `examples/hygen/` has four hands-on runnable projects mirroring
this document's own examples.

---

## Table of Contents

- [1. Overview](#1-overview)
- [2. Installation](#2-installation)
- [3. Quick Start](#3-quick-start)
- [4. Generators (Packs)](#4-generators-packs)
- [5. Templates](#5-templates)
- [6. Extensibility](#6-extensibility)
- [7. Packages (Marketplace)](#7-packages-marketplace)
- [8. Standalone Installation](#8-standalone-installation)
- [9. FAQ](#9-faq)
- [10. The `hygen-create` Equivalent: `ggen init-self` / `ggen pack new`](#10-the-hygen-create-equivalent-ggen-init-self--ggen-pack-new)
- [Appendix: Frontmatter Property Reference](#appendix-frontmatter-property-reference)

---

## 1. Overview

ggen is a specification-driven code generator: instead of a CLI argument or
an interactive prompt supplying the values a template renders, an RDF/
Turtle ontology does — queried via SPARQL, bound into a Tera template
context, and actuated through the same frontmatter vocabulary (`to:`,
`inject:`, `skip_if:`, `sh_before:`/`sh_after:`, …) that governs Hygen
templates. The formula this repo states for itself: **A = μ(O)** — the
generated Artifact is a deterministic function μ of the Ontology.

Where Hygen's unit of reuse is a **generator** (a folder under
`_templates/`), ggen's is a **pack** (a folder under `packs/`, or an
external marketplace package) — an `ontology.ttl` plus a set of `*.tmpl`
files, wired into a project's `ggen.toml`. A pack can also carry SPARQL
**gates** (`gates/*.rq`) that refuse a sync outright if the graph is
structurally wrong — there is no equivalent gate concept in Hygen; the
closest Hygen gets is a template author manually checking a condition
inside EJS.

Everything ggen writes is receipted: a chained BLAKE3 hash
(`.ggen-v2/receipt.json`, full history in `.ggen-v2/receipt-log.jsonl`)
records the graph state and every output's content hash for that run. Hygen
has no receipt concept at all — this is the largest structural difference
between the two tools, not a documentation gap.

## 2. Installation

ggen is not currently published to a package registry with a documented
`brew`/`npm`/`npx`-equivalent one-line install (UNVERIFIED beyond this
repo — no evidence of a crates.io release or Homebrew tap was found while
writing this doc). The verified path is building from source in this
workspace:

```bash
# From the repository root — builds the real ggen binary
cargo build -p ggen-cli-lib --bin ggen
# Binary lands at target/debug/ggen (or target/release/ggen with --release)
```

`rust-toolchain.toml` pins the required nightly toolchain; `cargo`/`just`
select it automatically (see this repo's own `CLAUDE.md`).

## 3. Quick Start

Hygen's `hygen init self` seeds a project with hygen's own
generator-of-generators. ggen's equivalent is `ggen init-self` +
`ggen pack new` — see [§10](#10-the-hygen-create-equivalent-ggen-init-self--ggen-pack-new)
for the full mechanism. For an ordinary project that already has an
ontology and wants to start generating code from it:

```bash
# Scaffold a new ggen project (writes ggen.toml, schema/domain.ttl, a
# Makefile whose build: target runs `ggen sync`, and git hooks)
ggen init --name my-project

# Preview what would be generated, without writing anything
ggen sync run --dry-run

# Generate for real — writes files and chains a receipt
ggen sync run
```

Basic invocation shape, once a project has packs wired in via `ggen.toml`'s
`[packs]` table: there is no `ggen <generator> <action>` positional command
the way Hygen has `hygen mailer new` — a ggen sync run always processes
*every* template a project's `ggen.toml` (project templates dir + every
declared pack) resolves to, in one pass, gated and receipted as a unit. To
run only part of that surface, see `ggen graph validate --files <ttl>` (a
narrower, ontology-only operation) or a project's own `[[generation.rules]]`
scoping (declarative-rules schema, §5).

## 4. Generators (Packs)

Every ggen pack shares one shape (confirmed against `packs/clap-noun-verb-pack`,
`packs/ggen-self-host-pack`, `packs/ggen-self-pack`, and others):

```
packs/my-pack/
├── pack.toml            # [pack] name/version/description/... metadata
├── ontology.ttl          # RDFS/OWL vocabulary + individuals, pack-owned namespace
├── gates/                # SPARQL ASK/SELECT law gates (optional)
│   └── 010_required.rq
├── queries/               # Named .rq files templates can reference (optional)
└── templates/
    └── *.tmpl             # Hygen-frontmatter + Tera body
```

A minimal pack — `pack.toml`:

```toml
[pack]
name = "greeter-pack"
version = "0.1.0"
description = "Generates a per-name greeting module."
category = "example"
```

`ontology.ttl` — the vocabulary and one individual:

```turtle
@prefix gr: <http://example.org/packs/greeter#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

gr:Greeting a rdfs:Class ; rdfs:label "Greeting" .
gr:name a rdf:Property ; rdfs:domain gr:Greeting ; rdfs:range xsd:string .
gr:message a rdf:Property ; rdfs:domain gr:Greeting ; rdfs:range xsd:string .

gr:HelloGreeting a gr:Greeting ;
    gr:name    "hello" ;
    gr:message "Hello, world!" .
```

`templates/greeting.rs.tmpl`:

```yaml
---
to: "src/greetings/{{ name }}.rs"
sparql:
  greetings: "PREFIX gr: <http://example.org/packs/greeter#> SELECT ?name ?message WHERE { ?g a gr:Greeting ; gr:name ?name ; gr:message ?message . } ORDER BY ?name"
---
{% for row in greetings %}
pub fn {{ row.name }}() -> &'static str {
    "{{ row.message }}"
}
{% endfor %}
```

Wired into a project's `ggen.toml`:

```toml
[packs.greeter]
path = "packs/greeter-pack"
```

`ggen sync run` then discovers this pack's templates automatically —
`crate::sync::discover_templates` walks the project's own `[templates].dir`
*and* every declared pack's template paths in one pass (confirmed: it is
literally `load_templates(project_dir)` chained with each pack's
`template_paths`, sorted for determinism), the same "drop a file, it's
live" ergonomics as Hygen's `_templates/` folder scan — the difference is
what supplies the per-instance values: Hygen reads CLI flags at invocation
time; ggen reads whatever the ontology currently states, every sync.

There is no Hygen-equivalent of "select part of a generator" (`hygen mailer
new:text`) — a ggen sync always evaluates every template's `when:` ASK
guard and named `sparql:` queries; scoping which output actually renders is
done through `when:`, `skip_empty:`, or the ontology's own content (a
`SELECT` returning zero rows renders zero outputs for that rule), not a CLI
sub-selector.

## 5. Templates

A ggen template is the same two-part shape as a Hygen template — YAML
frontmatter, then a body — but the body is [Tera](https://keats.github.io/tera/)
(Jinja2-family), not EJS, and frontmatter values are Tera-rendered before
the body is (matching Hygen's own "frontmatter is rendered too" behavior).

```yaml
---
to: "app/emails/{{ name }}.html"
---

Hello {{ name }},
{{ message }}
```

### Frontmatter is a closed, schema-validated set

Unlike Hygen's frontmatter (loosely typed YAML), ggen's `Frontmatter` struct
is `#[serde(deny_unknown_fields)]` — an unrecognized key is a hard parse
error, and the struct's field set is pinned 1:1 against
`schema/frontmatter-schema.ttl` by a real test
(`tests/frontmatter_schema_match.rs`, comparing `schemars::schema_for!`
output). The full property list is in the [Appendix](#appendix-frontmatter-property-reference).

### Template body

Tera, not EJS — but the same "you get free case-conversion helpers" idea:

```yaml
---
to: "src/workers/{{ name | snake_case }}.rs"
---
pub struct {{ name | pascal_case }};

impl {{ name | pascal_case }} {
    pub fn work(&self) {
        // your code here!
    }
}
```

`snake_case` and `pascal_case` are two of ggen's registered Tera filters
(`crates/ggen-engine/src/template.rs`) — the direct analog of Hygen's
blessed `Name` variable and its `h.changeCase.*`/`h.inflection.*` helper
families. ggen's full filter set, all backed by the `Inflector` crate
(migrated from hand-rolled implementations for consistency with a real,
tested library — see §6 below), is 13 filters: `snake_case`, `pascal_case`,
`camel_case`, `kebab_case`, `shouty_snake_case`, `title_case`, `pluralize`,
`singularize`, `ordinalize`, `demodulize`, `foreign_key`, `tableize`,
`classify`. Same idea throughout: derive a case/inflection variant of a
bound value without hand-rolled string logic in the template.

### SPARQL instead of CLI arguments

This is the core substitution the rest of this doc is organized around.
Where a Hygen template reads `<%= name %>` (a CLI flag or prompt answer),
a ggen template reads a **bound SPARQL query result**:

```yaml
---
to: "src/{{ item.slug }}.rs"
sparql:
  item: "PREFIX ex: <http://example.org/ex#> SELECT ?slug ?title WHERE { ?i a ex:Item ; ex:slug ?slug ; ex:title ?title . } LIMIT 1"
---
/// {{ item.title }}
pub struct {{ item.slug | pascal_case }};
```

Named queries in `sparql:` accept three forms (identical convention to
ggen's own predecessor, `ggen-core`'s `template_types.rs`): a bare string
(implicitly named `default`), a YAML sequence (`query_0`, `query_1`, …), or
the explicit `{name: query}` mapping form used above. Beyond the
frontmatter-declared `sparql:` map, the template body can also call SPARQL
directly via registered Tera functions: `sparql(query="...")`,
`sparql_first(...)`, `sparql_values(...)`, `sparql_empty(...)`,
`sparql_count(...)` — useful when a query is needed conditionally inside
the body rather than unconditionally in the frontmatter.

### Addition, injection, and shell actions — Hygen's vocabulary, extended

Every property Hygen documents (`to`, `force`, `unless_exists`, `inject`,
`before`/`after`, `skip_if`, `sh`) has a direct, real counterpart, plus
several ggen adds on top:

```yaml
---
inject: true
to: "Cargo.toml"
after: "[dependencies]"
skip_if: "serde ="
sh_after: "cargo fmt"
---
serde = { version = "1", features = ["derive"] }
```

`sh:` in Hygen is one property; ggen splits it into `sh_before:` (runs
before the write decision) and `sh_after:` (runs only after a successful
`Written`/`Injected` outcome, never after a `Skipped` one — a distinction
Hygen's single `sh:` does not make). Both are checked against a shell-
command safety denylist before execution (`crate::shell_safety::
check_shell_command_safe`) — refused, not silently sanitized, if a command
matches it.

`before`/`after`/`skip_if` accept either a bare string (Hygen's original
substring-match behavior, preserved exactly) or a structured `MatchSpec`
for exact/regex matching with explicit cardinality — a superset of Hygen's
"it's actually always a regex" behavior, opt-in rather than implicit.

### Beyond Hygen: RDF-native frontmatter properties

These have no Hygen equivalent at all, because Hygen has no ontology to
query or law to enforce:

| Property | What it does |
|---|---|
| `construct:` | An optional CONSTRUCT query whose result feeds the template — the Enrich-stage analog of a `sparql:` SELECT |
| `when:` | A SPARQL ASK guard: this template only generates when the graph satisfies it |
| `shape:` | SHACL shape file paths enforced against the active graph before this output is admitted — a non-conforming graph refuses the sync |
| `rdf:` / `rdf_inline:` | Extra Turtle files or inline Turtle text loaded into a *per-template* graph overlay (never mutating the shared project graph) |
| `determinism:` | When `true`, the sync pipeline renders this template's body twice and refuses if the bytes differ — an enforced, not merely claimed, determinism check |
| `freeze_policy:` / `freeze_slots_dir:` | Freezes an output against regeneration once written, tracked via BLAKE3 checksums in a slots directory |
| `unattended_write_eligible:` | Declares this rule safe for `ggen-mcp`'s bounded unattended-write dispatcher — requires `unless_exists: true` in the same block, since that already guarantees the write can never clobber hand-written content |
| `backup:` | Copies the existing file to `<path>.bak` before an overwriting write |

### Conditional rendering

Same idea as Hygen's `to: "<%= cond ? path : null %>"`, expressed via a
Tera conditional and, more idiomatically, `skip_empty:` or `when:`:

```yaml
---
to: "{{ 'src/' ~ name ~ '.rs' if is_public else '' }}"
skip_empty: true
---
```

## 6. Extensibility

Hygen's `.hygen.js` (custom helpers, bubbling-up config discovery) has
**no direct ggen equivalent** — this is a real, confirmed gap, not glossed
over. ggen's extensibility model is structural instead of a JS escape
hatch: new capability is added by extending the *ontology* (new classes/
properties a template can query) and the *pack* (new templates), not by
injecting an arbitrary function into the template engine's global scope.
The 13 registered Tera filters (§5) and five SPARQL functions (§5) are the
complete built-in surface; there is no project-level mechanism to register
additional ones without a code change to `ggen-engine` itself (UNVERIFIED
whether this is planned — nothing in this repo's docs states an intent to
add one).

## 7. Packages (Marketplace)

`ggen-marketplace` is the direct analog of Hygen's `hygen-add` + published
`hygen-*` npm packages, but native to this workspace (no separate install
tool):

```bash
# List available packs
ggen pack list

# Search
ggen pack search "clap"

# Install (copies into the project, records a provenance receipt binding
# pack id + version + digest — no Hygen equivalent of the receipt)
ggen pack add clap-noun-verb-pack

# Remove
ggen pack remove clap-noun-verb-pack

# Inspect one pack's metadata
ggen pack show clap-noun-verb-pack
```

Like Hygen's `hygen-add`, installing **copies** the pack's files into the
project (`.ggen/packs.lock` records the pinned content hash) rather than
maintaining a live reference to an external location — the same
resilience-over-liveness argument Hygen's own docs make for `hygen-add`.

Real packs shipped in this repo's own marketplace (`packs/*` — see this
repo's `CLAUDE.md` "Pack Inventory" table) span CLI-route generation
(`clap-noun-verb-pack`), CI workflow generation (`cargo-cicd-pack`,
`gh-terraform-pack`), test-harness generation (`chicago-tdd-tools-pack`),
and this repo's own self-referential packs (`ggen-self-host-pack`,
`ggen-self-pack` — see [§10](#10-the-hygen-create-equivalent-ggen-init-self--ggen-pack-new)).

## 8. Standalone Installation

No standalone (non-Cargo) binary distribution channel was found for ggen
while writing this doc — UNVERIFIED beyond "build from source" (§2). This
section exists only to keep the two documents' structure aligned; there is
currently no additional content to report here truthfully.

## 9. FAQ

**Why should I use ggen instead of Hygen directly?**
When the values a template needs are already facts your project maintains
elsewhere as structured data (an API surface, a capability catalog, a set
of CLI commands) rather than one-off human-supplied CLI flags, querying
that data via SPARQL is more consistent than re-typing it as `--flag`
arguments per invocation — and it composes with everything else already
expressed as RDF in the project. If the values genuinely are one-off,
per-invocation, human-supplied strings, Hygen's CLI-argument model is
simpler and ggen offers no advantage — see this repo's own
`.claude/rules/architecture.md`/`CLAUDE.md` "ggen-first" discipline for
when the tradeoff is judged worth it in this codebase specifically.

**How do I lowercase/uppercase/transform a bound value?**
Tera filters, the direct equivalent of Hygen's inline EJS transforms:
```yaml
---
to: "app/reducers/{{ reducer | lower }}.rs"
---
Hello {{ reducer | default(value="my-reducer") }}.
```
(`lower`/`upper`/`default` are Tera's own built-in filters, not ggen-
specific — see Tera's own documentation for the full built-in filter set.)

**Can a template see the results of a prior sync?**
Yes, opt-in: `[law].reflexive = true` in `ggen.toml` parses
`.ggen-v2/receipt-log.jsonl` at Stage 1 and inserts each prior receipt as a
fixed-shape `ggenr:Sync` fact cluster, queryable by any template's own
`sparql:`. No Hygen equivalent — Hygen has no receipt history to reflect
on.

**Should I check in my packs and ontology?**
Yes — exactly Hygen's answer for templates, for the same reason: they are
part of the codebase, reviewed like any other source.

**Can I force ggen to always overwrite?**
Per-template, via `force: true` in that template's own frontmatter
(§5) — there is no global `HYGEN_OVERWRITE`-style environment override;
overwrite behavior is a property of the template, not the invocation.

## 10. The `hygen-create` Equivalent: `ggen init-self` / `ggen pack new`

Hygen's own docs describe `hygen-create` as solving generator-maintenance
pain by deriving a generator from an *existing, working* set of files —
reverse-engineering EJS placeholders out of concrete code. ggen's answer to
"how do I get a new pack without hand-assembling one" is structurally
different — **forward construction from an ontology fact**, not reverse
derivation from working code — but addresses the same underlying problem:
don't require a human to remember and re-assemble a pack's required shape
by hand every time.

```bash
# One-time per project: materialize the canonical constructor
ggen init-self

# Create a new pack: binds one sp:Pack RDF individual from these CLI args,
# then runs the ordinary ggen-engine sync pipeline against it
ggen pack new my-pack \
  --description "What this pack generates and why" \
  --namespace "http://example.org/packs/my-pack#"
```

This is implemented as a real pack, `packs/ggen-self-pack/` — its own
`ontology.ttl` describes pack structure itself (`sp:Pack`, `sp:name`,
`sp:description`, `sp:namespace`, `sp:hasTemplateRole`), and
`gates/010_required_shape.rq` refuses to generate a pack whose bound fact
is missing a required property — a real SPARQL gate firing inside `ggen
sync run`, not a lint run afterward. See `packs/ggen-self-pack/README.md`
for the full design and `crates/ggen-cli/tests/self_pack_constructor_test.rs`
for its Chicago TDD coverage (8 real subprocess/filesystem tests, including
a sabotage test proving the gate refuses an incomplete fact).

The honest difference from `hygen-create`: there is no "iteratively improve
a generator by editing its generated output and re-running the deriver"
workflow in ggen today. `hygen-create`'s core loop — fix a bug in generated
code, run `hygen-create` again, the fix folds back into the generator — has
no ggen counterpart; a structural change to how ggen packs are shaped means
editing `packs/ggen-self-pack/ontology.ttl`/`templates/*.tmpl` by hand.

---

## Appendix: Frontmatter Property Reference

Source of truth: `crates/ggen-engine/src/template.rs`'s `Frontmatter`
struct (`#[serde(deny_unknown_fields)]`, schema-pinned against
`schema/frontmatter-schema.ttl`). All fields are Tera-rendered before use,
matching Hygen's own frontmatter-is-rendered-too behavior.

| Property | Type | Purpose |
|---|---|---|
| `to:` | String | Output path, relative to the project root |
| `sparql:` | map/seq/string | Named SPARQL SELECT queries bound into the Tera context |
| `for_each:` | String (query name) | Which named query governs per-row output fan-out vs. single aggregated rendering |
| `construct:` | String | Optional CONSTRUCT query feeding the template (Enrich-stage) |
| `inject:` | bool | Inject into an existing file instead of creating a new one |
| `before:` / `after:` | string or MatchSpec | Injection anchor — bare string (substring) or structured exact/regex matcher |
| `at_line:` | usize | Inject at this 1-based line number |
| `skip_if:` | string or MatchSpec | Skip the write when the existing file already satisfies this selector |
| `unless_exists:` | bool | Skip the write entirely when the target already exists |
| `unattended_write_eligible:` | bool | Opt into `ggen-mcp`'s bounded unattended-write dispatcher (requires `unless_exists: true`) |
| `force:` | bool | Overwrite an existing, differing file instead of failing closed |
| `when:` | String | SPARQL ASK guard — generate only if satisfied |
| `skip_empty:` | bool | Skip the write when the rendered body is empty |
| `from:` | String | Load the Tera body from another path (frontmatter still comes from this file) |
| `sh_before:` (alias `sh`) | String | Shell command run before the write decision |
| `sh_after:` | String | Shell command run only after a successful write/inject |
| `backup:` | bool | Copy the existing file to `<path>.bak` before overwriting |
| `shape:` | Vec\<String\> | SHACL shape files enforced against the active graph for this output |
| `determinism:` | Option\<bool\> | Render twice, refuse if the two renders differ |
| `freeze_policy:` | Option\<FreezePolicy\> | Freeze this output against regeneration once written |
| `freeze_slots_dir:` | Option\<String\> | Checksum directory for `freeze_policy: checksum` |
| `rdf:` / `rdf_inline:` | Vec\<String\> | Extra Turtle files/inline text loaded into a per-template graph overlay |
| `prefixes:` | map | Extra `@prefix` declarations for `rdf:`/`rdf_inline:` parsing |
| `base:` | Option\<String\> | RDF `@base` IRI for `rdf:`/`rdf_inline:` parsing |
