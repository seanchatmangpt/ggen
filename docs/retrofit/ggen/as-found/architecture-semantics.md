# As-Found: Architecture, Documentation, and Semantics (Agent E)

Branch: `retrofit/ggen-self-g1`. Observation only — as-found precedes as-desired.

## 1. ggen-constitution-pack — receipt-admission wiring (publication condition 12)

Read `packs/ggen-constitution-pack/ontology.ttl` in full (14 `ccn:Law` individuals,
I–XIV) plus its two gates and `pack.toml`.

- **Confirmed PARTIAL, not fixed.** The pack is a Stage-1 "descriptive mirror" (its own
  Law XIV entry says exactly this): it generates `CONSTITUTION.md` at repo root via a
  SPARQL SELECT over `ccn:Law` individuals, and enforces two internal-consistency gates
  on the ontology itself:
  - `gates/010_every_law_has_statement.rq` — every `ccn:Law` must carry `ccn:statement`.
  - `gates/020_mechanized_laws_have_test.rq` — `ccn:mechanized true` requires
    `ccn:provenBy`.
- Neither gate, nor any other code path found, reads `ccn:Law`/`ccn:mechanized` facts to
  drive the actual receipt `andon` computation. `mcp__plugin_lumen_lumen__semantic_search`
  and direct reads of `crates/praxis-core/src/law.rs` and
  `crates/praxis-core/src/default_law.rs` show `DefaultLaw::admit`/`Obligation` as a
  free-standing admission mechanism with no dependency on this pack's ontology or
  generated output. `crates/ggen-engine/src/sync.rs:1857` (`write_receipt`) still
  hardcodes `andon: Andon::Green` unconditionally — the pack's own Law VII entry names
  this exact gap and marks itself `ccn:mechanized false` for it.
- The ontology is unusually honest about its own limits: Law XI, Law VII, and Law XIV are
  all explicitly `ccn:mechanized false` with named-gap notes, including one flagged
  divergence (Law XII cites a test `reasoner_independence_e2e` that a workspace-wide grep
  found zero matches for — the ontology itself surfaces this as unverified rather than
  hiding it).
- **Conclusion:** "wired to receipt admission" is not yet true. The pack governs its own
  ontology's internal consistency (every Law has a statement; mechanized claims cite
  proof), not the live receipt-admission decision in `ggen-engine`/`praxis-core`. This
  matches the last-checked PARTIAL status for publication condition 12 — no drift in
  either direction found this session.

## 2. Namespace split (public/private)

No `public`/`private` namespace split was found in `ggen-constitution-pack` or in the
pack ontologies inspected. All `ccn:*` predicates live under one namespace
(`http://seanchatmangpt.github.io/packs/ggen-constitution#`) with no visibility
qualifier on individuals. If a public/private split exists elsewhere in the workspace it
was not located under `packs/`; this session did not find one to report on.

## 3. Japanese canonical vocabulary usage across packs — ad hoc, not uniform

Checked `gh-terraform-pack`, `tcps-core-pack` (the canonical source cited by Law XIII),
and the constitution pack's own Law XIII text.

- `gh-terraform-pack/ontology.ttl` uses extensive Japanese TCPS-style vocabulary in
  individual labels/comments (e.g. 検査票を生成する, 標準作業を完全生成する,
  受入受領証連鎖, 提供者版管理) — 30 `ALIVE` standing markers found in that file.
- `tcps-core-pack` is the pack explicitly cited by both the root constitution
  (`.specify/memory/constitution.md`) and `ggen-constitution-pack`'s Law XIII as the
  demonstration of 原語保持 (24 Japanese-named `.tmpl` files generating equally
  Japanese-named output, e.g. `自働化.rs.tmpl`).
- By contrast, `ggen-release-pack/ontology.ttl` (24 `ALIVE` markers) mixes Japanese
  section headers/table rows (e.g. `現行ggenが候補ggenを生成する`) with mostly-English
  prose elsewhere in the same file, and other packs inspected (`ggen-verify-pack`,
  `ggen-constitution-pack`) use Japanese only for the small set of specifically-canonical
  terms the constitution names (自己申告, 原語保持, 訳語非権威, etc.), not for general
  labels.
- **Conclusion:** usage is per-pack, not governed by a shared style guide or lint. Packs
  whose domain is itself TCPS/manufacturing (`tcps-*-pack`, `gh-terraform-pack`) use
  Japanese pervasively for individual labels and prose; packs about ggen's own
  self-governance (`ggen-constitution-pack`, `ggen-verify-pack`) use it narrowly, quoting
  only the specific canonical terms the constitution names. No cross-pack rule enforces
  consistency (no gate grepped for Japanese-term coverage or a required glossary), so the
  degree of Japanese-vocabulary usage is a per-pack authoring choice, ad hoc rather than
  uniform.

## 4. mermaid-pack — does not exist on disk

The task brief's premise ("gh-terraform-pack, the 5 retrofit packs, mermaid-pack all use
extensive Japanese TCPS terms") is false for `mermaid-pack`: there is no
`packs/mermaid-pack` directory, and no `mermaid` string appears in any `pack.toml` or
`ontology.ttl` under `packs/` (the only two hits for "mermaid" are unrelated fixture
files under `packs/self-monitoring-pack/fixtures/`, coincidental substring matches, not a
mermaid-diagram pack). `packs/` contains 33 directories total, none named
`mermaid-pack` or containing Mermaid-diagram generation logic.

**Conclusion:** the claim "does mermaid-pack actually generate diagrams FOR ggen itself
yet, or only for external repos" cannot be evaluated — mermaid-pack is UNSUPPORTED
(does not exist), not PARTIAL or ad hoc. Either it was renamed, never created, or the
brief is describing aspirational/planned work as if already present. This should be
corrected at the source (whatever document asserted mermaid-pack's existence) rather than
carried forward.

## 5. Manuals — docs/gh-terraform/*.md

`docs/gh-terraform/` contains exactly one file, `FLEET-CENSUS.md` (408 lines). It is
**not** a Japanese manual: it is an English-language, machine-generated read-only
observation table (repo visibility/protection/labels census from `scripts/gh/
fleet-census.sh`), explicitly labeled "OBSERVATION, NOT DESIRED STATE." No Japanese
content, and no other file in that directory. The task brief's premise ("docs/gh-terraform/
*.md Japanese manuals", plural) is also not supported by what's on disk — one file,
English, not a manual in the instructional sense (it's a data table with a provenance
header).

## 6. CLAUDE.md / .claude/rules/architecture.md — GENERATED sections vs real crate/pack list

- **Crate count**: `.claude/rules/architecture.md`'s crate map still states 17 workspace
  members; `Cargo.toml`'s `[workspace] members` array plus the root `ggen` package was
  not re-counted independently this session but the doc's own re-verification note
  (2026-07-17) stands unchallenged by anything found here.
- **Pack count — new drift found.** `.claude/rules/architecture.md`'s "Pack Inventory"
  section header claims **32 pack directories on disk** and its table lists exactly 32
  `*-pack` rows. The real count on disk (`ls -d packs/*/`) is **33** directories. Diffing
  the doc's pack-name list against `ls packs/` shows exactly one pack present on disk and
  absent from the table: **`github-actions-pack`**. This is a second instance of the same
  drift class this session already found and fixed once (28-vs-32) — the table was
  updated to 32 but not re-verified against disk after `github-actions-pack` was added
  (or the table edit missed it). `CLAUDE.md` (root) does not carry its own copy of the
  Pack Inventory table (only `.claude/rules/architecture.md` does), so the drift exists
  in exactly one generated location.
- No other GENERATED-marked section in either file was checked byte-for-byte against
  disk this session; this is a targeted spot-check, not an exhaustive audit of every
  GENERATED region.

## 7. ALIVE-standing spot check — `ggen-release-pack` and `gh-terraform-pack`

Sampled 4 `ALIVE` entries from `ggen-release-pack/ontology.ttl` (conditions 19, 2, 3, 4
of its self-hosting-ladder table):

- Condition 2 (`現行ggenが候補ggenを生成する`) cites: commit hash `6585fcbe6` (v26.9.7),
  a real non-dry-run `ggen sync run`, `gen:G2` recorded in `.specify/generations.ttl`,
  and "14 tracked generated outputs came back content identical."
- Condition 3 cites a receipt chain-hash link (`prev_chain_hash_hex == G2's
  chain_hash_hex`) verified via `ggen receipt verify` with a quoted JSON fragment
  (`{"valid":true,...,"signature_valid":true}`).
- Condition 4 cites a literal `git diff --stat` comparison between G2 and G3 being empty,
  with the same exclusion pattern `publish-candidate.yml`'s docs-sync gate uses.

These three are evidence-backed (real commit hash, real receipt-verify output, a named
comparison method), not bare assertions — consistent with the repo's evidence-first
discipline. This session did **not** independently re-run `ggen receipt verify` or
re-diff G2/G3 to confirm the cited outputs still hold; the claims are UNVERIFIED by this
agent (not re-executed), only checked for internal specificity/citation quality versus
the "assertion without evidence" failure mode the task asked to screen for. No
bare/unsupported `ALIVE` markers were found in the 4 sampled entries; a full audit of all
54 `ALIVE` markers (24 + 30 across the two files) was not performed — this is a 4-sample
spot check, not exhaustive.

## Summary of findings requiring follow-up (WORK, not blockers)

1. `.claude/rules/architecture.md` Pack Inventory table: 32 listed vs 33 on disk, missing
   `github-actions-pack` row. Fix: regenerate from `.specify/repo-facts.ttl` per the
   file's own GENERATED-marker instructions, or add the row and re-verify the count in
   the section header.
2. Any document (outside this session's visibility) asserting `mermaid-pack` exists
   should be corrected — no such pack is on disk in this workspace.
3. `docs/gh-terraform/*.md Japanese manuals` claim (plural, Japanese) does not match
   what's on disk (singular, English, `FLEET-CENSUS.md`). Source of that claim should be
   traced and corrected, or the manual(s) it describes should be located if they exist
   under a different path.
4. `ggen-constitution-pack` remains genuinely Stage-1 (descriptive mirror only); wiring
   it into the live receipt-admission path (`crates/ggen-engine/src/sync.rs`'s
   `write_receipt`) is real, not-yet-done work, consistent with the pack's own honest
   `ccn:mechanized false` markers on Law VII/XI/XIV. Not a documentation defect — the
   pack already discloses this; flagging here only to confirm no regression/improvement
   since last check.
