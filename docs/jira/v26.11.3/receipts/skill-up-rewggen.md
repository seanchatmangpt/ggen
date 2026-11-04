# Receipt: SKILL-UP — run-ggen skill rewrite (RG-1..7 integration)

Date: 2026-11-03 · Lane: IMPLEMENTATION · Subject:
`/Users/sac/.claude/skills/run-ggen/SKILL.md` + `smoke.sh` (rewritten in
full / header-patched). No git operations. Other skills untouched.

## Ground truth re-verified live this session (not from lane prose)

- `ggen --version` → `ggen 26.10.11`
- `ggen sync run --help` → flag surface incl. `--consumer-mode`
- `ggen pack --help` / `ggen law --help` → verb surfaces transcribed
- 26-key frontmatter allowlist re-read from
  `~/ggen/crates/ggen-engine/src/template.rs` (`Frontmatter` struct)
- Live consumer re-read: `/Users/sac/arcus/ggen.toml` +
  `/Users/sac/arcus/packs/arcus-pack/pack.toml` (8 `[validation]` gates)

## Defect → fix table

| # | Defect (source) | Fix landed |
|---|---|---|
| 1 | Stale hardcoded version "26.8.18 / source 26.8.12" (RG-1 ground truth) | Header now says: run `ggen --version`; installed binary is ground truth; installed 26.10.11 (source 26.10.12) noted as drift example; last-verified 2026-11-03 |
| 2 | "complete flag surface — verified" overclaim (RG-1) | Flag surface listed incl. `--consumer-mode`; reader pointed at `ggen sync run --help` as surface of record; list marked last-verified-dated |
| 3 | FM-YIELD-002 retired; dangling "Verified section" ref in smoke.sh (RG-2) | Renamed `[FM-WRITE-002]` ("verified exact code"); smoke.sh header rewritten: staged-pipeline wording, no dangling cross-ref, points at SKILL.md contract |
| 4 | No two-schema dispatch section (RG-3, biggest gap) | New section: declarative-rules (minimal shape kept) vs pack-composition (`[project]` name only — no `version` per FM-CONFIG-101; `[ontology]` source+imports; `[templates]` dir `.tmpl`-only; `[packs]` table-of-tables); closed 26-key frontmatter allowlist enumerated; `results` not `sparql_results`; FM-CONFIG-101/102; arcus live-consumer reference incl. 8 sync-time gates (ar-1-gates-witness.md); local-pack=robust vs tpot2 cross-project=FRAGILE (RG-4) |
| 5 | JSON summary shape stale (RG-5) | `packs` key added; dry-run shape (`dry_run`, `planned_writes`, no `written`); `2>/dev/null` scripting advice; `.ggen-v2/receipt.json` + `receipt-log.jsonl` + `.ggen/keys/` + `ggen receipt verify`; `generation` epoch field explicitly NOT documented (source-only, v26.10.20+) |
| 6 | Gotchas stale/inverted (RG-5) | Filter-chain `~` still fails, new message quoted; `==` unbound gotcha REPLACED by per-column fill-forward silent-leakage trap + `[FM-GEN-008][TEMPLATE_VARIABLE_MISSING]` for never-bound; `is defined` caveat; GROUP_CONCAT broken in ALL forms (COALESCE no help) → COUNT/SAMPLE/Tera concat; typed-literal equality trap (`== "1"` vs `== 1`); no ggen post-processing / verbatim Tera whitespace |
| 7 | New verbs undocumented (RG-6) | New section: `ggen pack` (compose/capabilities/query/doctor/related/list/show/...), `ggen law` (validate/derive/explain/export/load — SHACL+fixpoint, typed FM-LAW refusals), `freeze_policy`/`freeze_slots_dir` drift protection; "requires ≥26.11.3" note lists ea/sjira/sbb/compose --out/blast-radius/journal/lsp-scip as source-only |
| 8 | Troubleshooting gaps (RG-2/3/5) | Added FM-CONFIG-101/102, FM-WRITE-002, FM-GEN-008 (VARIABLE_MISSING + PARSE_FAILED), FM-TPL-006/002, FM-CONFIG-002 (standard_only declarative-only) |
| 9 | 12-new-nouns + pipeline wording (RG-1) | New-nouns line (law, pack, capability, wizard, graph, receipt, bblock, agent, vision2030, telco, policy, init-self); pipeline described as staged (Stage 0 schema dispatch → resolve/enrich/extract/render → Stage 2c EA admission) |

## Verification ladder

1. `awk length>100` on both files → clean (frontmatter description line
   exempt — pre-existing house style, name+description shape preserved)
2. `bash ~/.claude/skills/run-ggen/smoke.sh` → **exit 0**,
   `SMOKE TEST PASSED`; manifest kept declarative-schema and validated
   clean. Live run re-confirmed the documented JSON summary shapes
   (`packs: {}` present on both runs; dry-run `dry_run:true` +
   `planned_writes` with no `written`; real run `written`) — the new
   §JSON text matches observed output, not prose.
3. H1 `# run-ggen` matches skill purpose; frontmatter is
   name+description only.

## Standing

ALIVE (smoke gate exit 0 on exact subject this session). Two session-
introduced defects (template `block=` typo; allowlist list dropped
`unless_exists` and had a mangled sentence) caught and fixed before the
gate run.
