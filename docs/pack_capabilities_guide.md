# Pack Capabilities Guide

How to declare `[capabilities]` in `pack.toml` under the ggen capability model.

## What capabilities are

`pack.toml` may carry a `[capabilities]` table with exactly three keys:
`types`, `provides`, `requires` — each a list of strings
(`crates/ggen-engine/src/pack.rs`, `PackCapabilities`). These are
candidate-routing facts: they narrow SELECT candidates during resolution
and are compiled into the canonical RDF graph by
`crate::pack_scope::topology_turtle`. They never bypass GraphLaw/SHACL
gates or BRCE, and they never grant execution authority.

- `types`: semantic class labels from the caller's public ontology.
- `provides`: capabilities this pack provides after its own admission
  succeeds.
- `requires`: capabilities admitted from two evidence scopes only —
  see "Requires satisfaction: the two-tier rule" below.

## The URN convention

Capability strings use the form `urn:ggen:pack:<pack-name>` — one URN per
pack identity. Corpus examples (all under `packs/`):

```toml
# packs/fortune5-required-capabilities-pack/pack.toml
[capabilities]
provides = ["urn:ggen:pack:fortune5-required-capabilities-pack"]
```

```toml
# packs/clap-noun-verb-pack/pack.toml
[capabilities]
provides = ["urn:ggen:pack:clap-noun-verb-pack"]
requires = ["urn:ggen:pack:praxis-core-pack", "urn:ggen:pack:star-toml-pack"]
```

```toml
# packs/chicago-tdd-tools-pack/pack.toml
[capabilities]
requires = ["urn:ggen:pack:clap-noun-verb-pack"]
provides = ["urn:ggen:pack:chicago-tdd-tools-pack"]
```

Declare `provides` for every pack (the corpus convention). Declare
`requires` only on evidence: a `requires` URN means the named pack must
also appear in the consumer's `[packs]` dependency closure, or resolution
fails.

## Strict schema: deny_unknown_fields

`PackCapabilities` is `#[serde(deny_unknown_fields)]`. Any key other than
`types`/`provides`/`requires` inside `[capabilities]` is a hard parse
error. Note the asymmetry: the top-level `pack` table and unknown
top-level tables are open (collected into `extra`, informational only) —
only `[capabilities]` is closed.

Pitfall — the fortune5 path-map history: the
`fortune5-required-capabilities-pack` historically used `[capabilities]`
as a path map (`ontology`, `queries`, `templates`, `schemas` keys). Those
keys are now refused by `deny_unknown_fields`; the path map lives under
the separate `[capability_paths]` table, and `[capabilities]` holds only
the provides/requires shape
(`crates/ggen-config/tests/pack_corpus_schema_test.rs`:
`fortune5_pack_parses_with_no_capabilities_path_map`).

## How composition arbitrates

`crates/ggen-marketplace/src/packs_registry/composer.rs` — `compose()`
builds each pack's capability surface from the marketplace `Pack` model:
`packages` entries become provided capabilities, non-optional
`dependencies` entries become requirements on named provider packs, and
`templates[].path` entries become artifact output paths. It refuses
unsound compositions with four typed refusals, checked in fixed order.

1. `DuplicateCapability` — two packs provide the same package name.

```toml
# Triggers: pack-a and pack-b both declare packages = ["my-lib"]
[pack]
id = "pack-b"
packages = ["my-lib"]   # pack-a already provides "my-lib"
```

2. `UnboundRequirement` — a non-optional dependency names a pack absent
   from the composed set.

```toml
# Triggers: chicago-tdd-tools-pack composed without clap-noun-verb-pack
[pack]
id = "chicago-tdd-tools-pack"
[[dependencies]]
pack_id = "clap-noun-verb-pack"   # not in composed set
optional = false
```

3. `DuplicateArtifactPath` — two packs write the same template output
   path; later writes would clobber earlier ones.

```toml
# Triggers: pack-b declares a template with the same path as pack-a's
[[templates]]
path = "src/generated/catalog.rs"   # pack-a already targets this path
```

4. `CyclicDependencies` — the declared dependency edges form a cycle;
   there is no valid install order.

```toml
# Triggers: a requires b and b requires a (transitively)
[[dependencies]]
pack_id = "pack-a"   # in pack-b/pack.toml, while pack-a requires pack-b
```

## Requires satisfaction: the two-tier rule (adjudication H2)

Engine-side admission lives in `validate_capability_requirements` and
`satisfied_by_declaration` (`crates/ggen-engine/src/pack.rs:472-523`,
reached from both `resolve` and `resolve_read_only`). A required
capability string is satisfied by exactly one of two scopes:

**Tier 1 — URN-form pack-identity requires
(`urn:ggen:pack:<name>`):** satisfied iff the CONSUMER's `ggen.toml`
`[packs]` table declares `<name>`. These URNs are consumer-advice
annotations, not dependency-closure edges. Why not closure edges:
mutual requires between packs would create false dependency cycles.
Real-corpus evidence: `packs/self-monitoring-pack` and
`packs/dogfood-lifecycle-pack` each `requires` the other's URN.

```toml
# packs/dogfood-lifecycle-pack/pack.toml (URN form)
[capabilities]
requires = ["urn:ggen:pack:self-monitoring-pack"]
```

```toml
# consumer ggen.toml — the declaration IS the satisfaction
[packs]
self-monitoring-pack = { path = "packs/self-monitoring-pack" }
```

This is the cross_pack_matrix fixture shape
(`crates/ggen-engine/tests/cross_pack_matrix.rs`): a consumer project
declaring sibling packs by name in `[packs]`, resolving green.

**Tier 2 — non-URN requires (any other string): strict
dependency-closure scope.** The provider must appear in the pack's
transitive `[dependencies]` closure and `provide` the capability.
Fail-closed: ambient/global providers are never admitted as hidden
dependencies. Only Tier 1 consults the consumer declaration; a bare
name or a different URN space falls through to Tier 2.

Worked refusal (`crates/ggen-engine/tests/annotated_pack_sync_smoke.rs`,
`unsatisfied_capability_requires_refuse_with_fm_pack_018`):
`a2a-hex-migration-pack` declares
`requires = ["urn:ggen:pack:ash-extension-pack"]` with no
`[dependencies]`. When the consumer's `[packs]` does not declare
`ash-extension-pack`, Tier 1 fails (not declared) and Tier 2 fails
(no dependency provides it) — the sync refuses with the typed error:

```text
[FM-PACK-018] pack 'a2a-hex-migration-pack' requires capability/
capabilities [urn:ggen:pack:ash-extension-pack], but no provider
exists in its declared dependency closure and no required
pack-identity URN names a consumer-declared pack. Ambient/global
providers are not admitted as hidden dependencies.
```

Remediation: add a dependency that provides the capability, declare
the required pack in ggen.toml `[packs]`, or remove the requirement.

### Capability ordering vs satisfaction

Satisfaction and ordering are separate concerns. URN-form Tier 1
requires between packs of the declared universe also feed
`admitted_capability_edges` (`crates/ggen-engine/src/pack.rs`), which
orders candidate scope resolution: providers are enqueued after their
requirers alongside ordinary `dependencies` edges. Two invariants:

- Capability edges order but never refuse. If adding them to the
  declared dependency graph would form a cycle, the edges are
  deterministically dropped (empty vector) and scoping falls back to
  the dependencies-only order — no error, no refusal.
- H2 is preserved: cycle refusal remains on the dependencies-only
  graph, so mutual URN requires (consumer-advice) can never create a
  dependency cycle.

Satisfaction itself is unchanged: Tier 1 stays
consumer-declaration-based and Tier 2 stays dependency-closure-based.
Verified in `crates/ggen-engine/tests/capability_topology_exp.rs`
(4 tests).

Status (landed, VERIFIED 11/0): SJIRA-10 part 2 extends the
composer's surfaces from `packages`/`dependencies` to also consume
`[capabilities]` (`PackCapabilitySurface::from_pack`,
`crates/ggen-marketplace/src/packs_registry/composer.rs`):
- provides universe = `packages` entries ∪
  `capabilities.provides` URNs.
- requires universe = non-optional `dependencies` pack ids ∪
  `capabilities.requires` URNs.
- `DuplicateCapability` fires on same-URN `capabilities.provides`
  (cross-corpus mirror pairs refuse BY DESIGN as a tripwire —
  within-corpus composition is clean; see the falsified-dedup note
  below).
- `UnboundRequirement` binding is widened: a requirement binds when
  any pack in the set carries the id OR provides the capability.

Open question (status after the falsified dedup design, see the
FALSIFIED section of `docs/pack_urn_namespace_proposal.md`): the URN
space is per-pack-name, and the same URN is declared in both this
repo's `packs/` and `~/ggen-marketplace/packs/` (71 cross-repo
same-name pairs; logical mirrors). The earlier 45-vs-0 census
reversed: 64 ggen.toml refs resolve to `ggen/packs` vs 1 to
`marketplace` — both corpora are live. The composer's
`DuplicateCapability` refusal on cross-corpus pairs is INTENTIONAL
for now (tripwire): within-repo composition is clean, cross-corpus
composition has zero observed consumers; revisit if one appears.
No deletion of `ggen/packs` copies is planned (SJIRA-13 withdrawn).

## Relation to ggen.toml: [rules] and [pack_sources]

`ggen.toml` (v26.10.10 §4.1) adds two optional sections
(`crates/ggen-config/src/manifest/types.rs`):

- `[rules]` — `n3` and `datalog` arrays of external rule file paths,
  relative to the manifest. Distinct from `[law].rules` (engine law-state
  inputs) and `[[generation.rules]]` (inline SPARQL CONSTRUCT codegen
  rules). Absent/empty = no rules. `RulesConfig` is also
  `deny_unknown_fields`.

```toml
# ggen.toml
[rules]
n3 = ["rules/routing.n3"]
datalog = ["rules/policy.dl"]
```

- `[pack_sources]` — map of pack name to source binding.

```toml
# ggen.toml
[pack_sources.my-pack]
source = "path"
location = "packs/my-pack"
```

Capabilities and these sections are complementary: `[pack_sources]`
declares where a pack's bytes come from; the pack's own `[capabilities]`
declares what it routes as once resolved. `[rules]` files feed inference
over the merged graph that pack capabilities were compiled into.

## CLI surface

`ggen pack capabilities <name>` exists in
`crates/ggen-cli/src/cmds/pack.rs` (`#[verb] pub fn capabilities`): it
prints one pack's `provides`/`requires` URNs plus `satisfied_by`, a live
scan mapping each required URN to the packs whose `provides` cover it
across both corpus roots. A pack without `[capabilities]` returns
`capabilities: null`; an unknown pack is a typed error. Engine-side
refusals (`[FM-PACK-018]`, composer refusals) still surface through
normal resolve/sync behavior.

`ggen pack compose <a> <b> ...` (same file, `#[verb] pub fn compose`)
takes packs as positional args — verified against the clap signature
(`packs: Vec<String>`, no `--packs` flag, no comma delimiter). It
resolves names against the same corpus roots, runs the deterministic
composition kernel, and prints `{pack_ids, provides, order,
artifact_paths}` JSON. Refusal classes: duplicate capability, unbound
requirement, duplicate artifact path, cyclic dependency (typed, non-
zero exit on stderr), plus duplicate input names refused pre-kernel.
Same input set yields the same plan byte-for-byte.

The plan also carries `self_satisfied` (composer.rs:140): a sorted
list of packs whose `requires` bind through their own `provides`
under union semantics. Self-satisfaction is legal — the requirement
is genuinely bound by the composed set — but it is surfaced for
audit so consumers can spot packs that silently depend on
themselves; see the self_satisfied e2e test
(`pack_composition_e2e_test.rs:535`).
Gap: the `ggen pack compose` JSON projection (cmds/pack.rs:1116)
omits `self_satisfied` — the field is kernel-visible only until
that projection is extended.

MCP surface: the `capability_status` tool
(`crates/ggen-mcp/src/tools/capability_status.rs`) adds an additive
`capabilities` key — `{provides, requires, unsatisfied,
annotated_packs}` — computed over the tool's project-local pack scope.
It is omitted entirely when no annotated pack is referenced
(serde `skip_serializing_if`), so existing consumers are unaffected.

## Evidence contract enforcement (2026-10-09)

A sample court audit found ~76% of annotated `requires` entries were
prose-only — systemic: every lane promoted sibling mentions into
requirements. A corpus-wide purge now enforces the contract: a `requires`
URN survives ONLY when the pack actually consumes it.

### Decision table

| Evidenced (survives) | Prose-only (purged) |
|---|---|
| `@prefix`/IRI vocabulary binding + actual use in the pack's TTL | Comments noting a sibling pack uses the URN |
| Template, query, or path imports of the referenced artifact | README/provenance (`sourcePath`) mentions |
| `[dependencies]` on a pack that `provides` the URN | "mirrors"/"ported"/"same pattern" phrasing |
| Script path-loads of the referenced file | Source-attribution notes |

Precedents: the purge kept requirements backed by real template imports
and `[dependencies]` edges; it removed entries justified only by
provenance notes like "ported from pack X, which provides Y".

### Going forward

Before adding a `requires` entry, point at the consuming artifact —
the import line, dependency edge, or script load. If the only support
is prose, do not add the requirement; describe the relationship in
documentation instead.

### Residual

Cross-corpus mirror URNs (71 pairs): intentional `DuplicateCapability`
tripwire; both corpora live, no dedup planned. See the FALSIFIED
section of `docs/pack_urn_namespace_proposal.md` (SJIRA-13 withdrawn).

Census (re-derived 2026-10-09, `[pack].name`-keyed, both repos,
current tree): 403 `pack.toml` manifests (95 `ggen/packs` +
308 `ggen-marketplace/packs`), 332 unique names, 71 mirror pairs
(all cross-repo; zero within-repo duplicates). The 72-pair /
331-name figure in SJIRA-261010-12 is stale.

Count correction (lane census-post-strata, 2026-10-10, supersedes the
2026-10-09 Residual census counts above): full-tree recount, both keyings,
two deterministic runs — **444** `pack.toml` (95 `ggen/packs` +
349 `ggen-marketplace/packs`; 408 top-level + 36 nested), **373** unique
`[pack].name` (also 373 unique dir basenames), **71** mirror pairs,
104 top-level requires entries / 71 distinct URNs / **0 dangling**.
Growth 439 → 444 = the 5 new strata packs; all nested manifests now carry
`[pack].name` (previously 30 nameless). Canonical numbers live in the
census-post-strata section of SJIRA-261010-12.

## Closing consistency pass (verifier V18, 2026-10-09)

- The guide's re-derived census (71 cross-repo mirror pairs / 332 unique names) is CONFIRMED by
  fresh count under both keying methods ([pack].name and directory); the phase1 receipt's 331/72
  figures are the error (corrected there).
- Live requires universe as-of this pass: 49 packs, 104 entries, 74 distinct URNs, 0 dangling,
  0 bare. FM-PACK-018 two-tier semantics and the composer 11/0 arbitration claims check out
  against the tree (`crates/ggen-marketplace/tests/cross_corpus_tripwire_test.rs` present).

### Count correction (lane census-recount, 2026-10-09, supersedes the Residual census above)

Full-tree recount (no maxdepth, both keyings, two deterministic runs): **439**
`pack.toml` manifests (95 `ggen/packs` + 344 `ggen-marketplace/packs`) —
**338** unique `[pack].name` (30 nested manifests carry no `[pack].name`),
**368** unique dir names, **71** mirror pairs, 105 requires entries with
**2 dangling** (both `chatman-ecosystem-v26-9-1-release-gate/pack.toml`).
Top-level-only scope (the earlier census's `maxdepth 2`): 403 / 332 / 332 / 71,
unchanged. The +36 growth is nested marketplace manifests
(`dfcm-pack/families/*` applications, `ggen-pack-spec-pack/qualification/*`
fixtures), not new top-level packs. Canonical numbers live in
SJIRA-261010-12 census re-count section.

## End-to-end walkthrough (executed 2026-10-09, ggen@26.10.8 debug build)

Every block below is copied from a real run: `cargo build -p ggen-cli-lib
--bin ggen`, then the commands executed in a `tempfile`-style throwaway
project under `/tmp/ggen-tutorial`. The worked example uses three real
annotated packs from `/Users/sac/ggen-marketplace/packs`:

- `standing-ladder-pack` — provider: `provides` its own URN only.
- `a2a-durability-pack` — provider: `provides` its own URN; its own
  `requires` also names `a2a-conformance-pack` (mutual with the consumer
  — legal precisely because Tier 1 URN requires are not closure edges).
- `a2a-conformance-pack` — consumer: `requires` both providers' URNs.

### The packs (verbatim `[capabilities]` tables)

```toml
# /Users/sac/ggen-marketplace/packs/a2a-conformance-pack/pack.toml
[capabilities]
provides = ["urn:ggen:pack:a2a-conformance-pack"]
requires = ["urn:ggen:pack:standing-ladder-pack",
            "urn:ggen:pack:a2a-durability-pack"]
```

```toml
# /Users/sac/ggen-marketplace/packs/a2a-durability-pack/pack.toml
[capabilities]
provides = ["urn:ggen:pack:a2a-durability-pack"]
requires = ["urn:ggen:pack:a2a-conformance-pack",
            "urn:ggen:pack:standing-ladder-pack"]
```

```toml
# /Users/sac/ggen-marketplace/packs/standing-ladder-pack/pack.toml
[capabilities]
provides = ["urn:ggen:pack:standing-ladder-pack"]
requires = []
```

### Step 1 — inspect the consumer: `ggen pack capabilities`

```text
$ ggen pack capabilities a2a-conformance-pack
{
  "name": "a2a-conformance-pack",
  "provides": [
    "urn:ggen:pack:a2a-conformance-pack"
  ],
  "requires": [
    "urn:ggen:pack:standing-ladder-pack",
    "urn:ggen:pack:a2a-durability-pack"
  ],
  "satisfied_by": {
    "urn:ggen:pack:a2a-durability-pack": [
      "a2a-durability-pack"
    ],
    "urn:ggen:pack:standing-ladder-pack": [
      "standing-ladder-pack"
    ]
  }
}
EXIT:0
```

`satisfied_by` is the live corpus scan: each required URN mapped to the
packs whose `provides` cover it. This is advisory — the admission
decision happens at resolve/sync time against the *consumer's declared*
pack universe, not the ambient corpus (that asymmetry is the two-tier
rule: ambient providers satisfy the scan but never admit a sync).

### Step 2 — plan the composition: `ggen pack compose`

The CLI takes a comma-delimited `--packs` list (verified live: the
positional form is refused — `error: unexpected argument
'clap-noun-verb-schema-pack' found ... Usage: ggen pack compose
[OPTIONS] --packs <PACKS>`). The composer consumes BOTH
`capabilities` URNs and
`[[dependencies]]` ids (SJIRA-10 part 2), so a partial set refuses —
real 2-of-3 attempt, verbatim (the missing pack here is bound by
`a2a-durability-pack`'s own `capabilities.requires`):

```text
$ ggen pack compose --packs standing-ladder-pack,a2a-durability-pack
ERROR: CLI execution failed: Command execution failed: composition
refused: unbound requirement: pack 'a2a-durability-pack' requires
provider pack 'urn:ggen:pack:a2a-conformance-pack', which is not in
the composed set
EXIT:1
```

The three-pack composition, verbatim real output (exit 0):

```json
$ ggen pack compose --packs standing-ladder-pack,a2a-durability-pack,a2a-conformance-pack
{
  "pack_ids": [
    "a2a-conformance-pack",
    "a2a-durability-pack",
    "standing-ladder-pack"
  ],
  "provides": {
    "urn:ggen:pack:a2a-conformance-pack": [
      "a2a-conformance-pack"
    ],
    "urn:ggen:pack:a2a-durability-pack": [
      "a2a-durability-pack"
    ],
    "urn:ggen:pack:standing-ladder-pack": [
      "standing-ladder-pack"
    ]
  },
  "order": [
    "a2a-conformance-pack",
    "a2a-durability-pack",
    "standing-ladder-pack"
  ],
  "artifact_paths": {}
}
EXIT:0
```

### Step 3 — sync a consuming project

Project layout in `/tmp/ggen-tutorial` (pure frontmatter schema —
mixing `project.version` + `[[generation.rules]]` with `[packs]` makes
the manifest schema-ambiguous, FM-CONFIG-101):

```toml
# /tmp/ggen-tutorial/ggen.toml
[project]
name = "pack-capabilities-walkthrough"

[ontology]
source = "ontology.ttl"

[packs]
standing-ladder-pack = { path = ".../packs/standing-ladder-pack" }
a2a-durability-pack = { path = ".../packs/a2a-durability-pack" }
a2a-conformance-pack = { path = ".../packs/a2a-conformance-pack" }

[templates]
dir = "templates"
```

```turtle
# /tmp/ggen-tutorial/ontology.ttl
@prefix ex: <http://example.org/> .
ex:one a ex:Thing .
```

```text
$ ggen sync run        # cwd = project root
{
  "written": [
    "out/smoke.txt",
    "tmp/a2a-durability/lib/durable_task_store.ex",
    "tmp/a2a-durability/config/a2a_pplan.exs",
    "docs/standing-ladder/audit-trail.md"
  ],
  "skipped": [
    ["@template//...a2a-conformance-pack/.../conformance_court_skeleton.exs.tmpl",
     "for_each (implicit row fan-out) produced 0 rows (...)"],
    ["@template//...a2a-conformance-pack/.../spec_corpus_entry.json.tmpl",
     "for_each (implicit row fan-out) produced 0 rows (...)"]
  ],
  "graph_hash_hex": "b5be50b1452523a70f233ff02e1b52523c7988992e0fd6ba5c53c958910d2ba8",
  "packs": {
    "a2a-conformance-pack": "93605710910bc88dd71e327509bf2dd9222fd40f72139ef6b540db20d2511e8a",
    "a2a-durability-pack": "c55fed068f9074b36b342aaf96d2b8583f66dfd661f6c5339a9a76d43fc5cb90",
    "standing-ladder-pack": "69f7979969a5a700d0619a34909af94e63c9968c5201cd1c625e752efaa5c0df"
  }
}
EXIT:0
```

The capability-closure admission passed because both required URNs are
Tier-1 satisfied: `a2a-conformance-pack` and `a2a-durability-pack` both
appear in the consumer's `[packs]` table — the declaration IS the
satisfaction, mutual requires notwithstanding. Pack templates rendered
against the project ontology (conformance templates skipped cleanly:
their queries matched 0 rows in this minimal ontology — skip, not
failure). `ggen.lock` recorded all three packs with blake3 content
hashes:

```text
# ggen.lock — generated by `ggen sync`. Do not edit.

[packs.a2a-conformance-pack]
source = "path:/Users/sac/ggen-marketplace/packs/a2a-conformance-pack"
content_hash = "blake3:93605710910bc88dd71e327509bf2dd9222fd40f72139ef6b540db20d2511e8a"
...
```

### Step 4 — the failure case: undeclared provider

A project declaring `a2a-hex-migration-pack`
(`requires = ["urn:ggen:pack:ash-extension-pack"]`, no
`[dependencies]`) WITHOUT `ash-extension-pack` in `[packs]` fails both
tiers — Tier 1: not consumer-declared; Tier 2: no dependency provides
it. The sync refuses (typed, exit 1). Verbatim real transcript from a
throwaway project whose only `[packs]` entries are `aaif-vanilla-pack`
and `a2a-hex-migration-pack`:

```text
$ ggen sync run        # cwd = /tmp/ggen-tutorial-fail
ERROR: CLI execution failed: Command execution failed: validation
error: [FM-PACK-018] pack 'a2a-hex-migration-pack' requires
capability/capabilities [urn:ggen:pack:ash-extension-pack], but the
consuming project does not declare the referenced pack(s).
Remediation: add the pack(s) to this project's [packs] table or
remove the unsatisfied requirement.
EXIT:1
```

The refusal is raised by the engine's
`validate_capability_requirements`
(`crates/ggen-engine/src/pack.rs`), exercised by the Chicago smoke
test `unsatisfied_capability_requires_refuse_with_fm_pack_018`
(`crates/ggen-engine/tests/annotated_pack_sync_smoke.rs`). Remediation
is in the message: declare the provider in `[packs]` (Tier 1), add a
providing dependency (Tier 2), or drop the requirement.
