# ggen frontmatter lifecycle reference

This document is the canonical reference for the frontmatter schema implemented by
`crates/ggen-engine`.

The executable source of truth is:

- `crates/ggen-engine/src/template.rs` — closed YAML schema and defaults;
- `crates/ggen-engine/src/sync.rs` — graph, extraction, rendering, actuation, and receipt lifecycle;
- `crates/ggen-engine/src/write.rs` — Hygen-derived write law;
- `crates/ggen-engine/schema/frontmatter-schema.ttl` — machine-readable vocabulary;
- `crates/ggen-engine/tests/frontmatter_*.rs` — real-boundary evidence.

The parser accepts exactly 25 properties. Unknown keys fail closed. The historical
`sh` key is accepted only as a compatibility alias for `sh_before`; it is not a
26th property.

## 1. Complete template form

```yaml
---
to: "src/{{ row.module }}.rs"

sparql:
  00_driver: |
    SELECT ?module ?capability WHERE {
      ?artifact ex:module ?module ;
                ex:capability ?capability .
    }
    ORDER BY ?module ?capability
for_each: 00_driver

construct: |
  CONSTRUCT {
    ?artifact ex:normalizedName ?normalized .
  }
  WHERE {
    ?artifact ex:name ?name .
    BIND(LCASE(STR(?name)) AS ?normalized)
  }

when: |
  ASK { ?project ex:enabled true }

inject: true
before:
  pattern: '^\s*// GGEN:SLOT:{{ row.capability }}:END\s*$'
  matcher: regex
  scope: line
  occurrence: unique
skip_if:
  pattern: "pub mod {{ row.module }};"
  matcher: contains
  scope: file
  occurrence: first

unless_exists: false
force: false
skip_empty: true
backup: true

from: bodies/module.rs.tera
sh_before: "test -f src/lib.rs"
sh_after: "rustfmt src/{{ row.module }}.rs"

shape:
  - "shapes/{{ row.capability }}.ttl"
determinism: true
freeze_policy: checksum
freeze_slots_dir: ".ggen-v2/freeze/{{ row.capability }}"

rdf:
  - local-context.ttl
rdf_inline: |
  @prefix ex: <https://example.com/> .
  ex:thisTemplate ex:mode ex:Strict .
prefixes:
  ex: "https://example.com/"
base: "https://example.com/project/"
---
pub mod {{ row.module }};
```

This example intentionally combines nearly every property. Most templates should
use a much smaller subset.

## 2. Lifecycle model

Frontmatter is not one flat bag of options. Every property belongs to a specific
phase where its information and authority become available.

```text
parse
  -> resolve project, packs, templates, and closure
  -> enrich shared graph with global construct queries
  -> run project/pack law and SPARQL gates
  -> build optional template-private RDF overlay
  -> run optional overlay construct
  -> evaluate when and named sparql queries
  -> select explicit for_each cardinality or preserve legacy implicit rows
  -> establish fan-out, aggregate, or whole-output Tera context
  -> render body and output-phase properties
  -> validate shapes, determinism, duplicate targets, paths, and matchers
  -> run sh_before
  -> apply write decision
  -> run sh_after on successful mutation
  -> hash outputs, decisions, graph, packs, and input closure
  -> append chained receipt
```

### 2.1 Parse phase

A template must begin with an opening `---` line, contain YAML frontmatter, close
with another `---` line, then contain the Tera body. The key set is closed through
Serde `deny_unknown_fields`.

Parse failures include:

- missing, malformed, or unterminated delimiters;
- malformed YAML;
- unknown properties;
- invalid enum values;
- invalid structured matcher fields;
- a missing required `to` value.

### 2.2 Resolve and closure phase

The project ontology and resolved pack ontologies are inserted into one shared
graph. Templates are discovered deterministically: project templates in sorted
path order, followed by resolved pack templates in pack order.

The receipt input closure binds:

- the ggen actuator version;
- project identity;
- `ggen.toml`;
- the project ontology;
- pack ontologies and extra ontologies;
- every template file;
- every resolved `from` body;
- every resolved `rdf` file;
- law and pack gate inputs handled elsewhere in the sync pipeline.

Inline frontmatter values such as `rdf_inline`, `prefixes`, `base`, queries, hooks,
and matchers are indirectly bound through the template file hash.

### 2.3 Shared enrichment phase

Every template without `rdf` or `rdf_inline` may execute one `construct` query
against the shared graph. These constructs execute once, in deterministic template
order, and immediately insert their triples into the shared graph.

The implementation is deliberately single-pass. A construct can observe facts
inserted by an earlier template, but not facts that a later template will insert.
Construct closure is not iterated to a fixed point.

### 2.4 Law phase

After shared constructs, the configured law engine materializes rules and runs
project and pack gates. Therefore shared `construct` consequences can participate
in law evaluation.

Template-private overlays do not yet exist at this phase. Their private facts and
private construct consequences are not visible to project or pack gates.

### 2.5 Overlay, extraction, and cardinality phase

A template declaring `rdf` or `rdf_inline` receives a fresh overlay graph containing:

1. the current shared graph serialized into the overlay;
2. its resolved `rdf` files;
3. its `rdf_inline` content;
4. an optional template-local `construct` consequence.

The overlay is isolated. It does not mutate the shared graph and no other template
observes its additional facts.

`when` and all named `sparql` queries run against the active graph: either the
shared graph or the template-private overlay.

The render context contains:

- every named SPARQL result under its declared key;
- `results`, the driving row array;
- for per-row projections, `row` and every row binding promoted to a top-level key.

### 2.6 Output projection phase

The body and these output-phase values are Tera-rendered from the same context:

- `to` for per-row projections;
- matcher patterns inside `before`, `after`, and `skip_if`;
- `sh_before`;
- `sh_after`;
- every `shape` path;
- `freeze_slots_dir`.

The matcher configuration itself—kind, scope, occurrence, index, case sensitivity,
and trim—is structural configuration and is not Tera-rendered. Only its pattern is.

All templates render before any output write begins. This prevents a render or
query failure in a later template from leaving earlier rendered templates on disk.
It does not make the write and hook phase transactional.

### 2.7 Write and actuation phase

For each pending output, the implementation applies this effective order:

1. `skip_empty` may skip before matcher or path preflight;
2. matcher syntax and structured match cardinality are preflighted;
3. dry-run returns a non-mutating classification, or execution continues;
4. `sh_before` runs;
5. structured matchers are observed again against post-hook host state;
6. output size and target path are checked;
7. `unless_exists` may skip;
8. `skip_if` may skip;
9. `freeze_policy` may skip;
10. `inject` composes into an existing file;
11. otherwise `force` may overwrite;
12. otherwise a missing target is written, an identical target is skipped, and a
    differing target is refused;
13. `sh_after` runs only after `Written` or `Injected`.

The write stage is sequential and nontransactional. A later write or hook failure
can occur after earlier outputs have already changed. In that case the sync returns
an error and does not emit the final sync receipt.

### 2.8 Receipt and replay phase

A successful non-dry-run sync chains a receipt over:

- post-enrichment graph hash;
- hashes of every decision target that exists on disk;
- pack content hashes;
- per-target decisions;
- input closure hashes.

Structured matcher observations are appended to decision text, including matcher,
scope, occurrence, observed count, and selected line range. The whole decision map
is receipt-bound.

A `freeze_policy: always` output whose candidate bytes differ from the frozen file
creates a quarantined admission item and a `FROZEN-DRIFT:<path>` obligation.

Shell side effects outside declared decision targets are not itemized in the sync
receipt.

## 3. Property inventory

| Property | Type and default | Primary phase | Consequence | Standing |
|---|---|---|---|---|
| `to` | required string | cardinality/render/write | selects output path and fan-out/aggregate topology | ALIVE, bounded |
| `sparql` | string, sequence, or map; default empty | extract | produces named semantic views | ALIVE |
| `for_each` | optional named result; default absent | cardinality | explicitly selects rows for fan-out or aggregation | ALIVE |
| `construct` | optional string | shared enrich or overlay enrich | inserts derived triples | ALIVE, single-pass |
| `inject` | boolean; default `false` | write | composes body into an existing file | ALIVE |
| `before` | optional match declaration | render/preflight/write | inserts before selected host span | ALIVE |
| `after` | optional match declaration | render/preflight/write | inserts after selected host span | ALIVE |
| `at_line` | optional one-based integer | write | inserts at fixed line | ALIVE |
| `skip_if` | optional match declaration | render/preflight/write | idempotence/presence skip | ALIVE |
| `unless_exists` | boolean; default `false` | write | one-time scaffold skip | ALIVE |
| `force` | boolean; default `false` | write | overwrites a differing engine-owned file | ALIVE |
| `when` | optional ASK query | extract | template-level generation guard | ALIVE |
| `skip_empty` | boolean; default `false` | apply | suppresses empty rendered artifacts | ALIVE with path-preflight gap |
| `from` | optional relative path | resolve | replaces inline body with external UTF-8 body | ALIVE |
| `sh_before` | optional string; alias `sh` | render/actuate | pre-write shell command | ALIVE execution; sandbox/rollback unsupported |
| `sh_after` | optional string | render/actuate | post-success shell command | ALIVE execution; sandbox/rollback unsupported |
| `backup` | boolean; default `false` | write | writes `<target>.bak` before force/inject mutation | PARTIAL_ALIVE |
| `shape` | string list; default empty | render/preflight | declares and existence-checks governing paths | PARTIAL_ALIVE |
| `determinism` | optional boolean | extract/render | independently re-executes queries and compares projection | ALIVE for query/render determinism |
| `freeze_policy` | `never`, `always`, `checksum`; default absent/never | write/receipt | establishes regeneration ownership | PARTIAL_ALIVE |
| `freeze_slots_dir` | optional relative path | render/write | stores per-output BLAKE3 ownership slots | PARTIAL_ALIVE |
| `rdf` | string or list; default empty | resolve/overlay | adds template-private RDF files | ALIVE |
| `rdf_inline` | string or list; default empty | parse/overlay | adds template-private inline Turtle | ALIVE |
| `prefixes` | string map; default empty | overlay | creates Turtle prefix prolog | ALIVE |
| `base` | optional IRI string | overlay | creates Turtle base prolog | ALIVE |

`ALIVE` means executable behavior exists and is covered by real-boundary tests within
the stated boundary. It does not erase the explicit limitations recorded below.

## 4. Property reference and use cases

## 4.1 `to`

### Syntax

```yaml
to: src/generated.rs
```

```yaml
to: "src/{{ row.module | snake_case }}.rs"
```

### Lifecycle

`to` is required. With `for_each`, a value containing `{{` fans out one complete
output per selected row, while a static path aggregates one body rendering per selected
row into a single artifact. Without `for_each`, the historical behavior is preserved:
only a `to` containing `{{` fans out, using the first array-valued named SPARQL result
in `BTreeMap` key order.

Duplicate rendered targets are refused before writes begin. Zero selected rows emit zero
outputs. A path containing only Tera control tags such as `{% if %}` but no `{{` remains
static and therefore aggregates when `for_each` is declared.

Every eventual target is subject to the project-root path boundary during apply,
except for the skip-path gaps recorded in section 8.

### Use cases

- one deterministic file from the whole graph;
- one Rust module, schema, test, policy, workflow, or document per ontology row;
- language-specific or capability-specific output directories;
- projecting the same semantic row into multiple artifact families through
  multiple template files;
- generating a stable registry file with a static path while looping over
  `results` inside the body.

### Internal-pack guidance

Declare `for_each` explicitly and include a complete `ORDER BY` with a stable
tie-break in the named driver query. The alphabetic first-array convention is retained
only for backward compatibility.

### External-pack guidance

Keep output paths inside a clearly namespaced consumer directory. Do not claim
canonical host paths unless the pack contract explicitly grants that authority.

## 4.2 `sparql`

### Syntax forms

```yaml
sparql: "SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name"
```

The string form is named `default`.

```yaml
sparql:
  - "SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name"
  - "ASK { ?s a ex:Enabled }"
```

The sequence form is named `query_0`, `query_1`, and so on.

```yaml
sparql:
  00_driver: |
    SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name
  enabled: |
    ASK { ?s a ex:Enabled }
```

### Lifecycle

Every named query executes against the active graph. Query results are converted
for Tera:

- ASK becomes a Boolean;
- SELECT becomes an array of row objects with bare variable names;
- CONSTRUCT/DESCRIBE becomes an array of triple objects.

Every named result is inserted into the context. When `for_each` is declared,
`results` is that named array-valued result. Otherwise `results` preserves the historical
first-array-valued-result behavior in sorted key order.

`determinism: true` executes every declared query a second independent time.

### Use cases

- select artifact rows;
- expose lookup tables alongside a driver query;
- create feature flags with ASK;
- generate documentation or manifests from graph triples;
- compute counts, lists, joins, classifications, or dependency relationships;
- drive multiple artifact families from one admitted ontology view.

### Constraints

- Use `ORDER BY` for any query whose rows influence bytes or path ordering.
- Declare `for_each` whenever more than one array-valued result exists. Without it,
  an alphabetically earlier auxiliary result remains the legacy implicit driver.
- Row-level conditional generation belongs in the query `WHERE` clause; `when`
  guards the whole template.

## 4.3 `for_each`

### Syntax

```yaml
sparql:
  entities: |
    SELECT ?name WHERE { ?s ex:name ?name }
    ORDER BY ?name
for_each: entities
```

### Lifecycle

`for_each` explicitly names the `sparql` result governing projection cardinality. The
named result must exist and be array-valued. Missing names and scalar ASK results
refuse before output rendering, hooks, or writes.

The output path determines how the selected rows materialize:

```text
dynamic `to` containing `{{`
  -> render one complete output projection per selected row

static `to`
  -> render the body once per selected row
  -> concatenate those bodies in query-result order
  -> apply one invariant lifecycle law to the aggregate output
```

A static aggregate refuses if output-phase frontmatter differs across rows. One file
cannot honestly have row-dependent injection markers, hooks, shapes, freeze slots, or
other ownership semantics. Include a row binding in `to` when those properties must
vary per row.

When `for_each` is absent, ggen preserves the historical behavior: the first
array-valued named query in sorted key order supplies `results`, and fan-out occurs
only when `to` contains `{{`.

Zero explicitly selected rows produce zero outputs for both fan-out and aggregate
modes.

### Use cases

- manufacture one module, test, schema, policy, workflow, or document per ontology row;
- manufacture one registry, manifest, dispatch table, policy bundle, report, or index
  from all selected rows;
- aggregate row-rendered injection payloads into one host structural port;
- make auxiliary array-valued queries incapable of silently changing cardinality;
- give external packs a reviewable declaration of exactly which semantic view
  multiplies their infrastructure consequences.

### Combinatorial-maximalist law

`for_each` separates knowledge selection from representation topology:

```text
one admitted named view
  x many artifact families
  x fan-out or aggregate topology
  x all Tera-representable infrastructure surfaces
```

The property increases lawful projection combinations without increasing ambient
write or shell authority.

## 4.4 `construct`

### Syntax

```yaml
construct: |
  CONSTRUCT {
    ?service ex:hasNormalizedRoute ?route .
  }
  WHERE {
    ?service ex:route ?raw .
    BIND(LCASE(STR(?raw)) AS ?route)
  }
```

### Lifecycle

Without `rdf`/`rdf_inline`, `construct` executes during shared enrichment and inserts
its triples into the shared graph. Later constructs, law rules, gates, and all
templates can observe them.

With `rdf` or `rdf_inline`, the construct executes later against the template-private
overlay. Its consequences remain private and do not participate in project or pack
gates.

The query must produce graph results. SELECT or ASK is refused.

### Use cases

- normalize names or identifiers once before projection;
- derive capability, dependency, ownership, or route facts;
- convert source vocabularies into a canonical projection vocabulary;
- manufacture an intermediate projection index;
- enrich a private overlay without granting facts to other templates.

### Internal-pack guidance

Use shared constructs for constitutional or cross-template derived facts that must
be gate-visible. Make ordering explicit through template naming and avoid hidden
multi-pass dependencies.

### External-pack guidance

Prefer overlay constructs. A shared construct changes the semantic state consumed
by every pack and therefore requires a stronger admission contract.

## 4.5 `inject`

### Syntax

```yaml
inject: true
```

### Lifecycle

When true, the target must already exist. The body is inserted according to this
placement precedence:

1. `before`;
2. `after`;
3. `at_line`;
4. append at end.

`force` does not replace injection behavior. The write is classified `Injected`.

### Use cases

- add modules to a registry;
- extend a host manifest or dispatch table;
- add generated imports or exports;
- compose generated policy clauses into a stable host-owned document;
- mount multiple pack artifacts through explicit structural ports.

### External-pack guidance

External packs should inject only into host-declared slots. Use a structured
`unique` matcher plus `skip_if` so the host structure and idempotence condition are
both explicit.

## 4.6 `before`

### Compatibility form

```yaml
before: "// GGEN:SLOT:COMMANDS:END"
```

This means `contains + line + first`, case-sensitive and untrimmed.

### Structured form

```yaml
before:
  pattern: '^\s*// GGEN:SLOT:COMMANDS:END\s*$'
  matcher: regex
  scope: line
  occurrence: unique
```

### Lifecycle

The pattern is Tera-rendered per output. The selected match is observed before
actuation, and structured evidence enters the decision receipt. Injection occurs
before the selected span's starting line.

### Use cases

- insert immediately before an explicit end marker;
- insert before a closing generated region;
- add declarations before a host-defined sentinel;
- match a multi-line file-scoped structural region and insert before its first line.

### Constraints

- Meaningful only with `inject: true`.
- Missing insertion matches refuse.
- `unique` refuses multiple matches.
- Empty compatibility strings match every line and should not be used.

## 4.7 `after`

`after` has the same declaration, rendering, matching, and receipt semantics as
`before`. Injection occurs after the selected span's ending line.

### Use cases

- add an import after an import sentinel;
- append a module after a registry header;
- place generated configuration immediately after a named section;
- insert after a multi-line file-scoped block.

## 4.8 Matcher declaration shared by `before`, `after`, and `skip_if`

```yaml
pattern: "required"
matcher: contains       # contains | exact | regex
scope: auto             # auto | line | file
occurrence: first       # first | last | unique | nth
index: 1                # one-based, used by nth
case_sensitive: true
trim: false
```

### Defaults

- `matcher: contains`;
- `scope: auto`;
- `occurrence: first`;
- `index: 1`;
- `case_sensitive: true`;
- `trim: false`.

`scope: auto` resolves to line scope for `before` and `after`, and file scope for
`skip_if`.

Line scope counts matching lines, not every match inside one line. File scope counts
non-overlapping spans. File-scoped zero-width matches are refused.

Patterns over 64 KiB, malformed regex, structured empty patterns, and `index: 0`
are refused before shell execution.

## 4.9 `at_line`

### Syntax

```yaml
inject: true
at_line: 1
```

### Lifecycle

The value is one-based. Valid positions are `1..=line_count + 1`. Zero and values
past the append position are refused. `before` and `after` take precedence.

### Use cases

- insert a generated preamble at line one;
- modify a fixed-format file controlled by the same pack;
- perform bounded migrations where line structure is itself a contract.

### Guidance

Avoid `at_line` for human-edited or formatter-controlled host files. Structural
markers are more stable and more reviewable.

## 4.10 `skip_if`

### Compatibility form

```yaml
skip_if: "pub mod generated;"
```

This preserves whole-file substring semantics.

### Structured form

```yaml
skip_if:
  pattern: '^pub mod generated;$'
  matcher: regex
  scope: file
  occurrence: unique
```

### Lifecycle

`skip_if` runs before freeze and mutation. A selected match produces a `Skipped`
outcome. For structured `unique`, zero matches means “do not skip”; multiple matches
refuse. For `nth`, a missing nth match means “do not skip.” A compatibility empty
string is a historical no-op.

### Use cases

- idempotent injection;
- prevent duplicate declarations, imports, routes, or policy clauses;
- detect a semantic generated marker rather than compare the whole file;
- protect a host region already populated by another admitted pack.

## 4.11 `unless_exists`

### Syntax

```yaml
unless_exists: true
```

### Lifecycle

If the target exists, the write skips before `skip_if`, freeze, injection, force,
or normal byte comparison. `sh_before` still runs because hooks are outside the
writer's decision table. `skip_empty` short-circuits before the hook.

### Use cases

- first-run configuration scaffolds;
- example environment files;
- human-owned implementation stubs;
- one-time migration or bootstrap artifacts;
- files whose existence itself transfers ownership to the consumer.

## 4.12 `force`

### Syntax

```yaml
force: true
```

### Lifecycle

For non-injection writes, an existing target is overwritten rather than refused.
Freeze policy is evaluated first. If `backup` is true, the previous bytes are copied
to `<target>.bak` before overwrite.

With `force: true`, even byte-identical existing content follows the force branch and
is rewritten.

### Use cases

- engine-owned generated modules;
- generated lockstep manifests;
- snapshots whose canonical bytes must always be projected;
- synthetic aggregators and indexes;
- deterministic derived files that must never become human-owned.

### External-pack guidance

Do not use `force` against consumer-owned canonical files unless the installation
contract grants explicit ownership. Prefer namespaced outputs or structural
injection.

## 4.13 `when`

### Syntax

```yaml
when: |
  ASK { ?project ex:enablesFeature ex:Payments }
```

### Lifecycle

The query must be ASK. `true` continues extraction. `false` skips the whole template
before named SPARQL queries and output rendering.

`determinism: true` independently executes the guard again and refuses if the
second result differs.

### Use cases

- optional feature packs;
- generate only when a capability or policy fact exists;
- environment/profile selection represented as ontology facts;
- compatibility gates for optional artifact families;
- suppress a template when no governing concept is admitted.

### Constraints

`when` is template-level, not row-level. Filter rows in SPARQL for per-entity
conditions.

For a dynamic `to`, a false guard records the unrendered path expression as the skip
decision because no row context exists. See the implementation findings.

## 4.14 `skip_empty`

### Syntax

```yaml
skip_empty: true
```

### Lifecycle

After body rendering, whitespace-only output is skipped before matcher validation,
path preflight, hooks, or write decisions.

### Use cases

- optional sections whose query can return no content;
- avoid empty source modules, manifests, or documentation pages;
- keep a template generic while allowing a body-level conditional to suppress its
  artifact.

### Constraint

Because this is the earliest apply-stage short circuit, its decision path currently
bypasses the normal safe-target preflight. See FM-LC-001.

## 4.15 `from`

### Syntax

```yaml
from: bodies/service.rs.tera
```

### Lifecycle

The external UTF-8 file replaces the inline Tera body. All frontmatter remains in
the declaring `.tmpl` file. The path is resolved relative to the template file's
own directory and must remain inside that directory. Its bytes are separately bound
into the receipt closure.

### Use cases

- share a body across multiple frontmatter programs;
- separate long generated source from semantic control metadata;
- keep language-specific bodies next to one ontology-driven control template;
- reuse a stable body with different queries, paths, ownership, or hooks;
- package bodies cleanly inside external packs.

## 4.16 `sh_before`

### Syntax

```yaml
sh_before: "test -f src/lib.rs"
```

`sh` is accepted as an alias.

### Lifecycle

The command is Tera-rendered per output, checked by the bounded shell denylist, then
executed through `sh -c` with the project root as the working directory.

It runs after all templates have rendered but before this output's write decision.
It therefore runs even when `unless_exists`, `skip_if`, freeze, or unchanged-content
logic later skips the output. It does not run for `skip_empty` or dry-run.

Structured matchers are preflighted before the hook and again after it. A hook cannot
be relied upon to create a missing structured insertion slot because the first
preflight would already refuse.

### Use cases

- assert required host files or tools exist;
- validate a precondition not expressible in the graph;
- prepare a non-target auxiliary directory;
- run a bounded native verifier before mutation.

### Authority boundary

The denylist is not a sandbox. External packs with hooks are executable supply-chain
inputs and require explicit admission. Hook side effects are not itemized in the
sync receipt.

## 4.17 `sh_after`

### Syntax

```yaml
sh_after: "rustfmt src/{{ row.module }}.rs"
```

### Lifecycle

The command is rendered and safety-checked like `sh_before`, but runs only after a
`Written` or `Injected` outcome. It never runs after `Skipped` and never runs in
dry-run.

### Use cases

- format a generated source file;
- run a parser, compiler, schema checker, or targeted test;
- update an index derived from the written artifact;
- execute a pack-specific native verification step.

### Authority boundary

A failed `sh_after` occurs after the target has changed. There is no rollback and no
final receipt for the failed sync.

## 4.18 `backup`

### Syntax

```yaml
backup: true
```

### Lifecycle

`backup` is active only for injection and force overwrite. The current target bytes
are copied to `<target>.bak`. The backup path is overwritten on later operations;
there is no rotation or history.

### Use cases

- one-generation rollback for a controlled migration;
- preserve the pre-injection host file for review;
- protect a force-owned file while a pack transition is evaluated.

### Current limitation

For injection, the backup is currently written before a compatibility marker is
proved by `inject_into`. A missing bare-string marker can therefore leave a `.bak`
side effect even though the sync refuses. Structured matcher preflight closes this
for structured selectors, but not for the compatibility form. See FM-LC-003.

## 4.19 `shape`

### Syntax

```yaml
shape:
  - shapes/service-output.ttl
  - "shapes/{{ row.capability }}.ttl"
```

### Lifecycle

Paths are Tera-rendered per output and checked before writes and hooks. Current
behavior proves only that each joined path exists.

### Use cases

- declare the intended SHACL contract governing an artifact;
- associate capability-specific governance files with projected outputs;
- make a missing governance dependency fail before actuation;
- prepare future output validation and provenance reporting.

### Current boundary

- no SHACL engine evaluates the shape against the rendered artifact;
- the check accepts any existing path, not specifically a regular file;
- path containment is not yet routed through `resolve_target`;
- shape file bytes are not separately included in the receipt closure.

Therefore `shape` is a declaration plus existence gate, not proof of conformance.

## 4.20 `determinism`

### Syntax

```yaml
determinism: true
```

### Lifecycle

When true, the engine independently re-executes `when` and every named `sparql`
query. It rebuilds the context, re-renders the complete output-phase frontmatter,
and re-renders the body.

It refuses differences in:

- query result truth, count, values, or ordering;
- `to`;
- body bytes;
- `before`, `after`, and `skip_if` declarations after pattern rendering;
- hooks;
- shape paths;
- freeze slot paths;
- any other frontmatter field included in structural equality.

### Use cases

- release and publication packs;
- generated source committed to version control;
- proof, receipt, or policy artifacts;
- detect missing SPARQL `ORDER BY` clauses;
- ensure row-specific actuator commands and composition slots are stable.

### Boundary

This proves query and projection determinism. It does not re-run shell hooks, write
outcomes, host filesystem races, or external tools.

## 4.21 `freeze_policy`

### Values

```yaml
freeze_policy: never
```

Normal write law. Omitting the property is equivalent.

```yaml
freeze_policy: always
```

Once the target exists, generation never updates it. Candidate drift is observed
and receipt-quarantined.

```yaml
freeze_policy: checksum
freeze_slots_dir: .ggen-v2/freeze
```

After each successful ggen write, a BLAKE3 checksum is stored. Later runs proceed
while the current target still matches that checksum. If the target no longer
matches, it is treated as human-edited and regeneration skips.

### Use cases

- `never`: engine-owned derived artifacts;
- `always`: one-time scaffolds and immediate ownership handoff;
- `checksum`: generate repeatedly until a human edits, then preserve the human
  branch;
- migration packs that gradually transfer artifacts from generated to maintained;
- internal factories with mixed machine-owned and human-owned surfaces.

### Precedence

Freeze is checked after `unless_exists` and `skip_if`, but before injection, force,
or normal byte comparison.

### Current limitation

A checksum-slot read error other than missing-file is currently treated as “no prior
checksum,” allowing normal write law to continue. This is fail-open for an unreadable
governing slot. See FM-LC-004.

## 4.22 `freeze_slots_dir`

### Syntax

```yaml
freeze_slots_dir: ".ggen-v2/freeze/{{ row.capability }}"
```

### Lifecycle

The value is Tera-rendered per output. For checksum freeze, the slot path is:

```text
<freeze_slots_dir>/<to>.blake3
```

The combined path passes through the same root-containment resolver as output paths.
The property is required only for `freeze_policy: checksum` and ignored otherwise.

### Use cases

- separate ownership ledgers by pack, capability, language, or artifact family;
- retain target-relative checksum hierarchy;
- inspect and version a deterministic machine-ownership map.

## 4.23 `rdf`

### Syntax

```yaml
rdf: local-context.ttl
```

```yaml
rdf:
  - local-context.ttl
  - vendor-compatibility.ttl
```

### Lifecycle

Each path is relative to the template's own directory and must remain inside it.
The files are parsed into a fresh per-template overlay layered over the current
shared graph. Each file is separately receipt-closure-bound.

### Use cases

- package-local vocabulary mappings;
- vendor or platform compatibility facts;
- template-specific test fixtures;
- add knowledge needed by one artifact family without polluting other packs;
- ship an external pack with its own bounded semantic context.

### Boundary

The overlay uses the deterministic SPARQL graph implementation rather than the full
GraphLaw store. It receives the already-materialized shared facts, but template-local
facts are not independently subjected to the earlier law stage.

## 4.24 `rdf_inline`

### Syntax

```yaml
rdf_inline: |
  @prefix ex: <https://example.com/> .
  ex:template ex:mode ex:Strict .
```

A sequence of strings is also accepted.

### Lifecycle

Inline Turtle is parsed into the same private overlay as `rdf`. Its bytes are bound
through the template file hash rather than a separate closure entry.

### Use cases

- tiny template-local compatibility facts;
- declare one projection mode or profile;
- embed a small mapping table without another file;
- construct focused test or demonstration fixtures;
- parameterize a reusable body with admitted semantic constants.

## 4.25 `prefixes`

### Syntax

```yaml
prefixes:
  ex: "https://example.com/"
  qudt: "http://qudt.org/schema/qudt/"
```

### Lifecycle

Prefix declarations are emitted as a Turtle prolog before every `rdf` and
`rdf_inline` payload. `BTreeMap` ordering makes the prolog deterministic. The map is
ignored when no overlay RDF is declared.

### Use cases

- keep overlay Turtle compact;
- centralize vocabulary IRIs for several RDF inputs;
- make external pack mappings readable and reviewable.

Malformed names or IRIs fail when the composed Turtle is parsed.

## 4.26 `base`

### Syntax

```yaml
base: "https://example.com/project/"
```

### Lifecycle

An `@base` Turtle directive is emitted before prefix declarations and overlay
content. The value is ignored when neither `rdf` nor `rdf_inline` is present.

### Use cases

- resolve relative IRIs in packaged overlay data;
- relocate the same template-local RDF structure under a stable project base;
- reduce repeated absolute IRIs in external pack fixtures.

## 5. Composition recipes

## 5.1 Static engine-owned artifact

```yaml
---
to: src/generated/schema.rs
sparql:
  entities: |
    SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name
force: true
determinism: true
---
{% for entity in entities %}
pub struct {{ entity.name | pascal_case }};
{% endfor %}
```

Use for canonical generated files wholly owned by ggen.

## 5.2 One artifact per ontology row

```yaml
---
to: "src/generated/{{ row.name | snake_case }}.rs"
sparql:
  00_driver: |
    SELECT ?name WHERE { ?s ex:name ?name } ORDER BY ?name
force: true
determinism: true
---
pub struct {{ row.name | pascal_case }};
```

Use explicit driver naming and stable ordering.

## 5.3 External pack injection into a host-declared port

```yaml
---
to: src/lib.rs
sparql:
  00_driver: |
    SELECT ?module WHERE { ?m ex:module ?module } ORDER BY ?module
inject: true
before:
  pattern: '^\s*// GGEN:SLOT:MODULES:END\s*$'
  matcher: regex
  scope: line
  occurrence: unique
skip_if:
  pattern: "pub mod {{ row.module }};"
  matcher: contains
  scope: file
  occurrence: unique
backup: true
determinism: true
---
pub mod {{ row.module }};
```

The host exports authority through the slot; the external pack does not claim the
entire file.

## 5.4 One-time scaffold handed to a human

```yaml
---
to: src/custom_handler.rs
unless_exists: true
---
pub fn handle() {
    // Human-owned after first generation.
}
```

## 5.5 Generate until edited, then preserve

```yaml
---
to: config/service.toml
force: true
freeze_policy: checksum
freeze_slots_dir: .ggen-v2/freeze/service
---
# Generated until a human changes this file.
```

## 5.6 Template-private semantic adapter

```yaml
---
to: generated/vendor_adapter.rs
rdf:
  - vendor-mapping.ttl
prefixes:
  vendor: "https://vendor.example/schema/"
construct: |
  CONSTRUCT { ?x ex:canonicalName ?name }
  WHERE { ?x vendor:name ?name }
sparql:
  rows: |
    SELECT ?canonicalName WHERE {
      ?x ex:canonicalName ?canonicalName
    }
    ORDER BY ?canonicalName
determinism: true
---
{% for row in rows %}// {{ row.canonicalName }}
{% endfor %}
```

The adapter facts remain private to this template.

## 5.7 Optional artifact family

```yaml
---
to: docs/payments.md
when: |
  ASK { ?project ex:enables ex:Payments }
skip_empty: true
---
# Payments
```

## 6. Internal and external pack policy

Frontmatter has identical syntax for internal and external packs. Authority does not.

| Capability | Internal pack | External pack default |
|---|---|---|
| namespaced `to` output | allowed | allowed |
| canonical host `to` path | reviewed ownership | refuse unless explicitly granted |
| named `sparql` | allowed | allowed against admitted union graph |
| shared `construct` | reviewed semantic authority | refuse by default; prefer overlay |
| `rdf`/`rdf_inline` overlay | allowed | preferred isolation mechanism |
| `inject` | allowed into stable ports | only host-declared ports |
| `before`/`after` | structured `unique` preferred | structured `unique` required |
| `skip_if` | strongly recommended | required for idempotent injection |
| `force` | only engine-owned files | refuse by default |
| `unless_exists` | allowed | preferred for scaffolds |
| freeze policies | allowed | preferred ownership handoff mechanism |
| `sh_before`/`sh_after` | code-reviewed executable input | disabled/refused unless separately admitted |
| `shape` | governance pointer only | governance pointer only |
| `backup` | migration aid | not a substitute for host authorization |
| `determinism` | required for release packs | required for promoted external packs |

A pack hash proves which bytes were admitted. It does not make dangerous authority
safe. External-pack admission should evaluate the semantic authority requested by
its properties, especially shared construct, force, injection targets, and hooks.

## 7. Interaction laws

### 7.1 Output cardinality

```text
static to
  -> one pending output per template

dynamic to containing {{
  -> one pending output per driving row
  -> zero rows means zero outputs
```

### 7.2 Placement precedence

```text
before > after > at_line > append
```

Only the first configured placement mode is used.

### 7.3 Skip and ownership precedence

```text
skip_empty
  -> unless_exists
  -> skip_if
  -> freeze_policy
  -> inject
  -> force
  -> default create / identical skip / differing refuse
```

`sh_before` sits outside the writer's skip table and runs before these writer-level
decisions, except when `skip_empty` or dry-run short-circuits.

### 7.4 Freeze and force

Freeze wins over force. `force: true` does not override `always` or a checksum-detected
human edit.

### 7.5 Injection and force

Injection wins. When `inject: true`, `force` does not turn the operation into a whole
file overwrite.

### 7.6 Overlay and construct

```text
no rdf overlay:
  construct -> shared graph -> law/gates -> all templates

rdf overlay:
  shared graph after law -> private RDF -> private construct -> this template only
```

### 7.7 Receipt binding

- Template text binds all property declarations.
- `from` and `rdf` files receive separate closure hashes.
- Rendered output bytes bind existing decision targets.
- Decision text binds write outcome and structured matcher evidence.
- Hook side effects outside target paths are not bound.
- Shape file contents are not separately bound.

## 8. Implementation review findings

These findings describe the exact reviewed implementation. They are not claimed as
features.

### FM-LC-001 — decision paths can bypass target admission

**Standing: PARTIAL_ALIVE**

`skip_empty` returns before normal path preflight. A false `when` guard also records
a decision directly, before output rendering or target resolution. Receipt output
binding later joins decision keys to the project root without using the safe target
resolver.

Consequences:

- an unsafe static `to` can avoid the normal path refusal when skipped early;
- a false dynamic `when` records the literal unrendered `to` expression;
- receipt target lookup assumes a safety proof that these branches did not perform.

**Required checkpoint:** establish one admitted target type before any skip decision,
and use non-path evidence identifiers for templates that never materialize outputs.

### FM-LC-002 — `shape` is not yet an admitted governing input

**Standing: PARTIAL_ALIVE**

The current check uses project-root joining plus `exists`:

- no traversal/symlink containment proof;
- directories pass;
- shape bytes are absent from the input closure;
- no SHACL conformance evaluation occurs.

**Required checkpoint:** resolve safely, require a regular readable file, closure-bind
its bytes, then separately add actual shape evaluation.

### FM-LC-003 — injection backup precedes compatibility-marker proof

**Standing: PARTIAL_ALIVE**

The inject path writes `<target>.bak` before `inject_into` proves a bare-string marker
exists. A failed injection can therefore actuate a backup without producing a final
receipt.

Structured selectors are preflighted earlier and avoid this specific path, but the
compatibility form remains exposed.

**Required checkpoint:** compute and validate the complete prospective bytes before
any backup or target mutation.

### FM-LC-004 — unreadable checksum slots fail open

**Standing: PARTIAL_ALIVE**

Any checksum-slot read error is currently treated as if no checksum exists. Only
`NotFound` should have that meaning.

**Required checkpoint:** continue on `NotFound`; refuse permission, encoding, I/O,
and directory errors.

### FM-LC-005 — dry-run is not the complete write planner

**Standing: PARTIAL_ALIVE**

Dry-run suppresses hooks and mutations correctly, but it mainly reports unchanged
versus planned write. It does not fully classify:

- `unless_exists`;
- compatibility `skip_if`;
- freeze outcomes;
- missing injection targets;
- compatibility marker failures;
- backup consequences;
- force versus normal ownership.

Structured match preflight does run, making dry-run stronger for structured selectors
than compatibility strings.

**Required checkpoint:** separate a pure `plan_write` decision object from an
`execute_write` actuator and use the same plan in dry-run and execution.

### FM-LC-006 — selectors without `inject` have inconsistent consequences

**Standing: PARTIAL_ALIVE**

Placement selectors are meaningful only for injection. The writer ignores them when
`inject` is false, but structured selectors are still preflighted by the sync layer
and can refuse against an existing target. Compatibility strings are not preflighted
there.

**Required checkpoint:** either refuse placement properties unless `inject: true`, or
consistently ignore them before matcher observation. Refusal is the clearer contract.

### FM-LC-007 — projection cardinality is explicit

**Standing: ALIVE**

`for_each` now names the exact array-valued `sparql` result that governs multiplicity.
Dynamic targets fan out per row. Static targets aggregate row-rendered bodies into one
artifact while refusing row-varying lifecycle law. Missing or scalar drivers refuse
before hooks and writes. Omitting the property preserves the historical implicit
behavior for existing templates.

**Remaining extension boundary:** cross-products, joins between multiple named row
sets, grouping, and reduction operators are intentionally excluded until a bounded
use case demonstrates that they cannot be expressed more clearly in SPARQL.

### FM-LC-008 — hooks and writes are nontransactional

**Standing: UNSUPPORTED transactionality**

All rendering is atomic with respect to writes, but actuation is sequential. A hook
or later output failure can leave earlier mutations without a final receipt.

**Required extension:** staged writes, hook execution in a bounded broker, commit or
rollback, and itemized hook effect receipts.

### FM-LC-009 — overlay knowledge is outside the law stage

**Standing: ALIVE by current design**

Template-private RDF and its construct consequences are created after project and
pack law gates. The isolation is useful, but those facts do not receive the same
law-stage evaluation as shared graph facts.

**Required extension:** an optional overlay-law phase for packs that require local
facts to be admitted before projection.

### FM-LC-010 — checksum human edits are preserved but not quarantined

**Standing: ALIVE by current design**

`freeze_policy: checksum` records a skip reason when a manual edit is detected, but
only `freeze_policy: always` reasons containing `DRIFT:` create receipt quarantine
and obligations.

**Required decision:** declare checksum ownership transfer an admitted success, or
record a distinct human-ownership obligation. Do not leave the semantic distinction
implicit.

## 9. Recommended Gall checkpoints

1. **Target Admission** — one safe resolved target type used by every decision and
   receipt branch.
2. **Pure Write Plan** — calculate outcome, prospective bytes, backup, freeze, and
   matcher evidence before actuation; make dry-run execute the same planner.
3. **Shape Closure** — safe path, regular-file proof, closure hash, then SHACL
   evaluation.
4. **Hook Broker** — declared authority, sandbox/broker boundary, itemized effects,
   rollback or compensating receipt.
5. **Cardinality Algebra** — `for_each` closes the named-driver 80/20; add grouping,
   cross-products, or reductions only when concrete packs prove the need.
6. **Overlay Admission** — optional local law/gate pass for `rdf` overlays.
7. **Ownership Receipts** — distinguish generated ownership, human handoff,
   checksum-protected human edit, and frozen drift as explicit states.

## 10. Verification map

| Surface | Evidence |
|---|---|
| closed 24-property vocabulary | `frontmatter_schema_match.rs` |
| parsing, defaults, compatibility forms | `template.rs` unit tests |
| write ownership, injection, skips, force | `write.rs` unit tests and E2E suites |
| hooks, backup, shape existence, determinism, freeze, from | `frontmatter_fields_e2e.rs` |
| `rdf`, `rdf_inline`, `prefixes`, `base`, overlay isolation | `frontmatter_rdf_e2e.rs` |
| output-phase Tera rendering and hook asymmetry | `frontmatter_maximalism_e2e.rs` |
| typed matcher defaults, regex, cardinality, pre-actuation refusal | `frontmatter_matchers_e2e.rs` |
| explicit named driver, fan-out, static aggregation, invariant lifecycle law | `frontmatter_cardinality_e2e.rs` |
| schema/struct field equality | `frontmatter_schema_match.rs` |
| receipt payload and chained standing | sync and receipt E2E tests |

## 11. Minimal safe defaults

For an internal engine-owned artifact:

```yaml
to: generated/output.rs
force: true
determinism: true
```

For an external namespaced artifact:

```yaml
to: .ggen/vendor-pack/output.rs
determinism: true
```

For external host composition:

```yaml
inject: true
before:
  pattern: "// GGEN:SLOT:VENDOR:END"
  matcher: exact
  scope: line
  occurrence: unique
skip_if:
  pattern: "// GENERATED:vendor-pack:item"
  matcher: contains
  scope: file
  occurrence: unique
determinism: true
```

For human handoff:

```yaml
unless_exists: true
```

or:

```yaml
freeze_policy: checksum
freeze_slots_dir: .ggen-v2/freeze
```

Do not add hooks, force ownership, shared constructs, or canonical host injection to
an external pack merely because the schema permits them. Combinatorial maximalism
maximizes lawful projection combinations; it does not maximize ambient authority.
