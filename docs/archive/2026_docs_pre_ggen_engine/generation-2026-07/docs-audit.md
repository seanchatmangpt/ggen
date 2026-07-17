# Documentation Audit — Generation-First Audit (2026-07)

Scope: all `*.md` under `docs/` plus repo-root `*.md`. Method per `METHODOLOGY.md`:
bucket-by-directory with 2–4 file samples per bucket, reusing
`DOCUMENTATION_AUDIT_REPORT.md` (2026-06-30, 3,118 rows, one per file with
Purpose/Completeness/Action) as raw inventory rather than recounting.

## 1. Inventory

- `find docs -name '*.md' | wc -l` → **990**
- `find docs -maxdepth 1 -name '*.md' | wc -l` → **55** (root of docs/)
- `ls -1 *.md | wc -l` → **35** repo-root markdown files
- Per-directory counts (`find docs -maxdepth 1 -type d | while read d; do echo "$d $(find $d -name '*.md' | wc -l)"; done`):

Largest buckets: `docs/archive` 478, `docs/architecture` 48, `docs/receipts` 41,
`docs/interop` 39, `docs/marketplace` 29, `docs/thesis` 26, `docs/how-to` 23,
`docs/reference` 22, `docs/diataxis` 19, `docs/explanation(s)` 28, `docs/tutorials` 15,
`docs/crate-audits` 14, `docs/security` 12, ~20 smaller dirs of ≤11 files each.
Four dirs (`registry`, `proof-cycles`, `performance-dashboard`, `observability`) contain 0 md.

## 2. GENERATED: zero files

Verification: `grep -rl "DO NOT EDIT\|@generated\|doctoc" docs --include='*.md' | wc -l`
→ 866 matches, but `grep -rl "doctoc" docs --include='*.md' | wc -l` → **864 are doctoc
Table-of-Contents blocks** ("DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc"). The 2
non-doctoc matches (`docs/archive/2025_docs/.specify/README.md`, this audit's own
METHODOLOGY.md) merely *mention* generation markers. **No markdown file in docs/ carries
ggen provenance, and none is reproduced by any wired rule.**

### The "RDF is truth, .md is generated" doctrine has zero implementation today

`grep -n "output_file" ggen.toml` shows exactly **one** `[[generation.rules]]` entry
(`cli-commands-reference`), and its output is Rust, not markdown:
`output_file = "crates/ggen-cli/src/generated_commands.rs"`. There is no TTL→md emission
rule anywhere in `ggen.toml`. The TTL specs exist (`.specify/cli-commands.ttl`,
`.specify/cli-schema.ttl`, `.specify/specs/*` — 10 spec dirs), and the CLAUDE.md rule
"Edit `.specify/*.ttl` (source). Never edit `.md` (generated)" describes an architecture
that is currently aspirational: every one of the 990 docs files is hand-authored.

## 3. Bucket classification

Completeness distribution per bucket derived from `DOCUMENTATION_AUDIT_REPORT.md` via
`awk -F'|' '$2 ~ /^ docs\// {…count by dir+state…}'` (see numbers cited inline).
Samples read per bucket to verify character.

| Bucket | Files | Class | Disposition / evidence |
|---|---|---|---|
| `docs/archive/` | 478 | **STALE-DELETE** | Self-declared archive (2025_docs, legacy_structure, specs). 93 stubs + 28 TODO + 23 draft per prior report. Delete or move out of docs/. |
| `docs/receipts/` | 41 | **STALE-DELETE** | Dated closeout/handoff receipts (V26_5_29_CLOSEOUT_RECEIPT.md, NEXT_SESSION_HANDOFF.md). Superseded by `.ggen/receipts/*.json` cryptographic receipts. |
| `docs/crate-audits/`, `docs/audits/` (prior), root-of-docs status files (~35 of the 55: MASTER_TODO, PHASE5_*, WAVE2_*, PATH_A_*, *_SUMMARY, *_REPORT) | ~50 | **STALE-DELETE** | Version-stamped completion/status reports; AUDIT_DASHBOARD already superseded per supersessions.md. |
| Repo-root `*.md` (35) minus README/CHANGELOG/SECURITY/MANIFESTO/AGENTS | ~28 | **STALE-DELETE** | ORIGINAL_REQUEST.md, TEST_CATEGORIZATION_REPORT.md etc. — session artifacts, violate "no root files" rule. |
| `docs/reference/` (22), `docs/cli/` (1), `docs/api/` (2), `docs/QUICK_REFERENCE`/`API_REFERENCE`/`FEATURE_GATES_DETAILED_REFERENCE` (root of docs) | ~30 | **DERIVABLE-FROM-TTL** | `docs/reference/commands/{a2a,mcp}.md`, `docs/reference/01-commands.md`, `docs/cli/CONSTRUCT_COMMANDS.md` restate CLI surface already modeled in `.specify/cli-commands.ttl` — same query the one wired Rust rule uses. |
| `docs/marketplace/` | 29 | **DERIVABLE-FROM-TTL** (majority) | Pack catalogs/registry docs restate data owned by ggen-marketplace RDF control plane; 11/29 are stubs anyway. |
| `docs/features/`, `docs/metrics/`, `docs/validation/`, `docs/templates/` | 34 | **DERIVABLE-FROM-TTL** | Enumerations of feature gates, diagnostic codes (GGEN-TPL-001…), SLO tables — list-shaped, spec-owned facts. |
| `docs/CHANGELOG.md`, version-stamped release docs | ~5 | **DERIVABLE-FROM-TTL** | Release metadata is receipt/tag-derivable. |
| `docs/architecture/` (48), `docs/adr/` (2), `docs/thesis/` (26), `docs/ggen-v6-thesis/` (4), `docs/explanation(s)/` (28), `docs/gall/` (10), `docs/security/` (12), `docs/diataxis/` (19) | ~149 | **HAND-AUTHORED-KEEP** | Design rationale, ADRs, formal theory (3T-PhD-THESIS.md), COMPRESSED_REFERENCE.md. Curated human prose. |
| `docs/tutorials/` (15), `docs/how-to/` (23), `docs/how-to-guides/` (5), `docs/getting-started/`, `docs/troubleshooting/`, `docs/GETTING_STARTED.md`, `docs/FAQ.md` | ~48 | **HAND-AUTHORED-KEEP** | Diataxis tutorial/how-to quadrants; pedagogy doesn't precipitate from TTL. Merge the duplicate how-to dirs. |
| `docs/interop/` (39), `docs/mcp/`+`docs/mcp-rdf/` (20), `docs/agent/` (11), `docs/testing/` (8), `docs/performance/` (8), remainder small dirs | ~100 | **MIXED — mostly HAND-AUTHORED-KEEP, ~⅓ stale** | Protocol/testing guidance keep; dated kickoff briefs and stubs (6 interop stubs per prior report) delete. |

## 4. Headline numbers

Total docs-tree markdown: **990** (+35 repo-root). Approximate disposition
(bucket totals above; stale share of mixed buckets estimated from the prior report's
Placeholder/Stub + Has-TODOs + dated-title counts — the report contains 684 rows
matching `grep -c 'Superseded\|superseded\|Archive\|archive'`):

| Class | Files (approx) | Share |
|---|---|---|
| GENERATED | 0 | **0%** |
| STALE-DELETE | ~610 (archive 478 + receipts 41 + status/report files ~90) | **~62%** |
| DERIVABLE-FROM-TTL | ~100 | **~10%** |
| HAND-AUTHORED-KEEP | ~280 | **~28%** |

The single biggest coverage win in the docs tree is deletion, not generation: removing
`docs/archive/` alone drops the corpus by 48%.

## 5. Top-10 doc classes to convert to TTL-generation first

Ordered by (spec already exists) → (data already machine-owned) → (drift cost):

1. **CLI command reference** (`docs/reference/commands/*`, `docs/reference/01-commands.md`, `docs/cli/`) — `.specify/cli-commands.ttl` exists and already feeds the only wired rule; add a second `[[generation.rules]]` with an md Tera template. Cheapest possible first win.
2. **CLI configuration reference** (`docs/reference/configuration/`) — `.specify/cli-schema.ttl` exists.
3. **Diagnostic-code reference** (GGEN-TPL-001 etc., `docs/validation/`, QUICK_REFERENCE tables) — codes live in analyzer source; model once in TTL, emit both Rust and md.
4. **Feature-gate matrix** (`docs/FEATURE_GATES_DETAILED_REFERENCE.md`, `docs/features/`) — pure enumeration keyed to Cargo features.
5. **Crate map / README-per-crate** (`crates/*/README.md`, architecture crate tables) — currently duplicated in CLAUDE.md, .claude/rules/architecture.md, and docs; single TTL source kills three-way drift.
6. **Marketplace pack catalog** (`docs/marketplace/`) — pack registry is already RDF-native in ggen-marketplace.
7. **SLO / performance targets** (`docs/performance/`, rules/performance.md tables) — small TTL vocabulary, high drift cost when targets change.
8. **API reference index** (`docs/API_REFERENCE.md`, `docs/api/`) — derive from LSP documentSymbol sweep or TTL type-registry (`.specify/type-registry.ttl` exists).
9. **MCP/A2A tool schemas** (`docs/mcp/`, `docs/reference/commands/{mcp,a2a}.md`) — `.specify/mcp-a2a-protocol-example.ttl` exists; a2a_generated Rust is already the workspace's only GENERATED code.
10. **Release/changelog stubs** (`docs/CHANGELOG.md` skeleton, version banners in README/docs) — version stamp appears in 20+ files today; one TTL fact, one emission rule.

## Summary

The docs tree holds 990 markdown files of which exactly zero are generated by ggen — the
864 "DO NOT EDIT" markers are all doctoc TOC blocks, and ggen.toml's single generation
rule emits Rust, so the "RDF is truth, .md is generated" doctrine is presently
unimplemented. About 62% of the tree (docs/archive/, docs/receipts/, dated status
reports) is stale-delete material, ~28% is legitimately hand-authored (architecture,
ADRs, theses, Diataxis tutorials/how-tos), and ~10% (~100 files) is reference-shaped
content derivable from TTL — starting with the CLI command reference, whose spec
(`.specify/cli-commands.ttl`) and query already exist and only need a markdown Tera
template and a second `[[generation.rules]]` entry.
