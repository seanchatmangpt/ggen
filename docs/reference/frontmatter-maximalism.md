# Frontmatter maximalism

ggen frontmatter is a closed ontology-to-infrastructure projection program. Its Hygen lineage remains visible in `to`, injection, preservation, and shell lifecycle semantics. ggen extends that local generator grammar with admitted RDF, SPARQL extraction, semantic derivation, deterministic rendering, freeze ownership, and chained receipts.

The canonical field-by-field contract, lifecycle, use-case catalog, internal/external pack policy, and implementation review is [ggen frontmatter lifecycle reference](template-directives.md). This document states the architectural thesis; the lifecycle reference states the executable consequences and current gaps.

## Standing

This checkpoint makes output-phase frontmatter projection **ALIVE** through real filesystem and subprocess E2E tests. It does not claim shell sandboxing, transactional rollback, hook cardinality controls, itemized receipts for arbitrary hook side effects, SHACL conformance, or complete dry-run/write-plan equivalence. Those remain explicit extension boundaries rather than inferred guarantees.

## Phase law

Properties are evaluated in the phase where their information is available:

| Phase | Properties | Law |
|---|---|---|
| Resolve | `from`, `rdf`, `rdf_inline`, `prefixes`, `base` | Establish the template body and semantic overlay before query extraction. |
| Derive, extract, and cardinality | `construct`, `when`, `sparql`, `for_each` | Manufacture and select the admitted view and explicitly name the row set governing multiplicity. |
| Output projection | `to`, body, `before`, `after`, `skip_if`, `sh_before`, `sh_after`, `shape`, `freeze_slots_dir` | Tera-render from the same bounded query context for each materialized output. |
| Write law | `inject`, `at_line`, `unless_exists`, `force`, `skip_empty`, `backup`, `freeze_policy` | Decide ownership, composition, preservation, and mutation. |

A dynamic `to:` creates one output context per driving SPARQL row. All output-projection properties receive that same `row` plus its top-level bindings. This permits one ontology fact to specialize the path, structural slot, duplicate-prevention needle, native lifecycle command, governing shape, and checksum namespace together.

## Explicit projection cardinality

`for_each` is the highest-leverage combinatorial multiplier in the frontmatter
calculus. It names one array-valued `sparql` result as the projection row set:

```yaml
sparql:
  services: |
    SELECT ?service ?module WHERE { ... } ORDER BY ?service
for_each: services
```

A dynamic `to` manufactures one complete output per row. A static `to` concatenates
one body rendering per row into a single registry, manifest, policy bundle, report,
or injection payload. Static aggregation requires every output-phase authority field
to remain invariant across rows; content may multiply, but one output receives one
composition and actuation law.

This creates the bounded 80/20 relation:

```text
named ontology view x artifact family x topology(fan-out | aggregate)
```

Omitting `for_each` preserves the historical implicit driver contract.

## Fortune 5 hardening boundary

The projection calculus is now bounded before actuation:

```text
named query result <= 4,096 rows
materialized outputs <= 8,192 per sync
one rendered artifact <= 10 MiB
all rendered bodies <= 64 MiB per sync
```

Every prospective target is safely resolved before any shell hook. Canonical path
aliases are duplicate targets. Rendered `shape` documents must be safe, regular,
readable files and their bytes enter the receipt closure. Placement selectors require
`inject: true` and exactly one placement authority. Checksum ownership state fails
closed on every error except `NotFound`. Injection bytes are validated before backup.

These controls bound combinatorial multiplication without reducing the lawful ontology
views or artifact families available to internal and external packs.

## Structural ports

`before` and `after` are Tera-renderable structural ports. They let a host artifact export stable composition slots while a pack projects row-specific content into the appropriate slot. `skip_if` is rendered from the same context, so idempotence can name the exact consequence being injected.

```yaml
to: "src/{{ row.module }}/mod.rs"
inject: true
before: "// GGEN:SLOT:{{ row.capability }}:END"
skip_if: "pub mod {{ row.generated_module }};"
```

Bare-string markers retain the historical first-line substring behavior. Structured declarations can select `contains`, `exact`, or `regex` matching over line or file scope with explicit occurrence law. Missing insertion matches and ambiguous structured cardinality fail closed. Two outputs resolving to the same target in one sync are still refused rather than ordered implicitly.

## Shell lifecycle boundary

`sh_before` and `sh_after` are Tera-rendered per output, then checked by the existing bounded denylist before `sh -c` execution in the project root.

The phases are intentionally asymmetric:

1. Every template and every output-phase property renders before any filesystem mutation.
2. `sh_before` runs immediately before that output's write decision. It therefore still runs when `unless_exists`, `skip_if`, freeze policy, or unchanged-content logic later skips the write.
3. `sh_after` runs only after `Written` or `Injected`, never after `Skipped`.
4. Dry-run executes neither hook.

Shell hooks are not a sandbox and do not receive authority merely because a pack declares them. External packs containing hooks must be treated as executable supply-chain inputs and admitted accordingly.

## Determinism

`determinism: true` now re-executes query extraction and compares the complete output projection: path, body, structural markers, idempotence needle, hook commands, shape paths, and checksum-slot path. A stable body with an unstable actuator or composition target is a determinism violation.

## Hygen lineage and extension

Hygen established the project-local action grammar:

```text
add → preserve → inject → execute
```

ggen preserves that fence and adds semantic standing:

```text
admit → derive → project → compose → actuate → verify → receipt → replay
```

Frontmatter maximalism does not mean every property executes in every phase. It means every property is used to its maximum lawful consequence under explicit phase, ownership, authority, and receipt boundaries.

## Typed host-content matchers

`before`, `after`, and `skip_if` accept either the historical bare string or a structured matcher.

```yaml
before:
  pattern: '^\s*// GGEN:SLOT:{{ row.capability }}:END\s*$'
  matcher: regex
  scope: line
  occurrence: unique
```

Every optional matcher property has a deterministic default:

| Property | Default | Meaning |
|---|---|---|
| `matcher` | `contains` | Literal substring matching |
| `scope` | `auto` | `line` for `before`/`after`; `file` for `skip_if` |
| `occurrence` | `first` | Select the first admissible match |
| `index` | `1` | One-based occurrence used by `nth` |
| `case_sensitive` | `true` | Preserve byte-sensitive host conventions |
| `trim` | `false` | Match the original candidate boundaries |

Bare strings remain exactly backward compatible. For `before` and `after`, `unique` requires exactly one match. For `skip_if`, zero matches means the condition is false while multiple matches under `unique` refuse ambiguity. Invalid or oversized regex patterns refuse before shell hooks. Regex execution uses Rust's linear-time `regex` engine and a bounded compiled-program size; zero-width file matches are refused.

Matcher patterns are output-phase Tera projections. `determinism: true` therefore includes the rendered pattern and every matcher option. Structured match count and selected line range are appended to the sync decision so the receipt records the host-structure observation that authorized composition.
