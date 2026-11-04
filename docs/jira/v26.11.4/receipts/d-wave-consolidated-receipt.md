# d-wave-consolidated-receipt

Consolidated D-wave implementation-lane receipt index (E-n debt finding: D21-D55 impl
lanes carried receipts only in commit messages; no citable file existed).

## Verification table

Verified this session (2026-11-04) via `git branch -r --contains <sha>` per repo.

| lane | repo | commit | on-origin | one-line artifact |
|---|---|---|---|---|
| cgap | cgap | 74733e28 | y | ci(beam): pin OTP 28, drop continue-on-error, loud skip |
| cgap | cgap | b257a70f | y | test(crypto): verify_tag_var_aad negative legs + mldsa props |
| cgap | cgap | 67ea26c7 | y | chore: version-law sync 26.10.29→26.11.3, dead exports |
| cgap | cgap | 7b21f537 | y | E-h 5: cgap-local drift courts (dma_ring + isomorphism) |
| cgap | cgap | 9f668ce2 | y | fix(ocel2): fixtures to v3 wire law (1104B Finish, AppData) |
| cgap | cgap | 2c01194a | y | docs(receipts): bm6 claims table + F10 closing receipt |
| cgap | cgap | fbc0d9f3 | y | feat(cgap-aoq): Q6 M2b peer identity pin-table (P-f) |
| arcus | arcus | 9fec512 | y | feat(flight): Q6 M2b AoQ pin-table + persisted pin store |
| ggen | ggen | 56ccef60b | y | docs+ci: e8/zp-16 standing flips; PRE-FIX gate version fix |
| ggen | ggen | 04426e94c | y | docs(closure): v26.11.4 closure directory (register/seeds) |
| ggen | ggen | 899e7c7c1 | y | ci: weekly e2e backlog lane (202 ignored tests + fixtures) |
| ggen | ggen | ab4104e99 | y | docs: mint root KNOWN_DIVERGENCES.md (XC-8/ZD-38, R13) |
| ggen | ggen | d256aa383 | y | docs: marketplace bundle ruling (10 packages/ = templates) |
| mp | ggen-marketplace | 2d0064d50 | y | chore(strata): snapshot-and-pin templates, xref |
| mp | ggen-marketplace | 73eb07fde | y | docs(strata): FROZEN-ASOF 2026-11-04 hand-law note |
| smalt | smalt | 0acc51b0 | y | chore: remove tracked _impl14 build dir (259 files, 350M) |
| smalt | smalt | 434321b0 | y | merge: _impl14 build-artifact removal (branch 0acc51b0) |

## Loudly missing / not landed at receipt time

- smalt D33/D42/D43/D47/D48/D54/D55/D57/D60/D63/D64: no commits carrying these lane
  IDs (subject or body) found on smalt main, branches, or all-ref log this session.
  Work may be folded into checkpoint-pattern commits; lane attribution lives with the
  coordinator's lane map, not on disk.
- ggen D25r/D52/D69: not landed at receipt time (absent from `git log` this session).
- lsp-max D28: not landed at receipt time (working tree carries in-flight edits to
  workflows/tests/ontology/scripts; no D28 commit on any lsp-max branch).
- D63 receipt reference: the only D63 receipt file on disk is the smalt v26.10.16
  lineage (docs/jira/v26.10.16/receipts/r51-r57-r58-r68-r81-r95-r96-amqp-batch.md,
  D63/R95 atom-flood fence) — different ID lineage; no v26.11.4-wave D63 receipt
  exists yet.

## Standing

ALIVE — every commit in the table verified on origin this session by
`git branch -r --contains` (cgap/arcus/ggen-marketplace/smalt on origin/main,
ggen on origin/v26-11-3-wave).

## Receipt-debt note

Per-lane detail (courts, verification ladders, disclosures) lives in the cited commit
messages per the coordinator's lane-law; this file is the citable index over those
landings, not a replacement for the messages. Later lanes (D25r/D52/D69, D28, smalt
D-lanes) append rows here as they land.
