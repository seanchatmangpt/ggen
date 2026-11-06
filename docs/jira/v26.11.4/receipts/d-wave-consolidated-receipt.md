# d-wave-consolidated-receipt

Standing: ALIVE

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

ALIVE — every commit in the verification table and the addendum verified on origin this
session by `git branch -r --contains` (cgap/arcus/ggen-marketplace/smalt on origin/main,
ggen on origin/v26-11-3-wave, lsp-max on origin/master).

## Addendum (late tail, 2026-11-04)

Commits that landed after the D70 table snapshot above; verified this session via
`git branch -r --contains <sha>` per repo (all `y`).

| lane | repo | commit | on-origin | one-line artifact |
|---|---|---|---|---|
| cgap | cgap | b69ccabd | y | docs: fix publish-flag drift in CLAUDE.md (E-y row) |
| cgap | cgap | ef43dbee | y | chore(freeze): re-mint status_maps + cscb-targets blake3 pins (v3-era lawful edits) |
| cgap | cgap | f30e19f4 | y | chore: remove tracked build artifacts (wrapper_host.o, __pycache__ .pyc files) |
| cgap | cgap | 7b21f537 | y | E-h finding 5: cgap-local drift courts for dma_ring + isomorphism (dup of D70 row; also in late tail snapshot) |
| arcus | arcus | 23d313b | y | docs+test: errata blocks (18.1ns/G7 supersessions), digest scope-law note, cdata pinner-lease delegation test |
| arcus | arcus | 3c997ea | y | chore(freeze): re-mint blake3 pins for v3-regenerated aoq_ipc surfaces |
| arcus | arcus | 92e9a2a | y | docs: CLAUDE.md three-zone-law sentence resolved to scope-law (X4) |
| arcus | arcus | 9fec512 | y | feat(flight): Q6 M2b AoQ peer identity pin-table + persisted pin store (dup of D70 row; also in late tail snapshot) |
| ggen | ggen | 8d5ff1167 | y | docs(plans): ZP-19 row → EXECUTED (landed conditional) — bm8/br4/f6d0a5e evidence |
| ggen | ggen | 3afe33b2d | y | docs(plans): ZP-11..25 standings reconciliation — post-wave receipt flips folded in |
| ggen | ggen | be7afbf3c | y | docs(seed): v26.11.5 SEED.md — 17 rows, 4 dropped-at-compile, drain-verified statuses |
| ggen | ggen | 254668592 | y | chore: remove tracked tai-erlang-autonomics/_build tree (275 files, ~11MB) — _impl14 class |
| ggen | ggen | fa577dd6d | y | docs: rebuild root README (conflict artifact resolved), version 26.11.4 |
| ggen | ggen | 6716a741e | y | docs(closure): OI-8 reconstructed in-writing + CLOSED (merge 434321b0) |
| ggen | ggen | dc471a839 | y | fix(closure): replay script — labeled tags, real arcus court metric, smalt receipts row, trimmed counts |
| ggen | ggen | 6df04204a | y | docs: merge-region delimiters are generator-owned — marker-sweep whitelist note |
| ggen | ggen | a963f094d | y | docs(receipts): vuln-remediation closing receipt (NV10) |
| smalt | smalt | 434321b0 | y | merge: remove tracked _impl14 build artifacts (259 files) — C6 U3 (dup of D70 row; also in late tail snapshot) |
| smalt | smalt | 6bb10a36 | y | fix(gates): appconfig-etag-cas pin flips to 201-create (KD-149 parity flip 2026-10-30) |
| smalt | smalt | 038d3b0f | y | docs: CLAUDE.md sdk-* line updated to post-retirement reality (C6 U2) |
| smalt | smalt | a2c9b3fd | y | fix(workspace): air-gap fetch posture, dead component/deps removal, eslint selector (D83/D48 class) |
| smalt | smalt | 078ccf9f..62793177 | y | curriculum authoring chain, one row: 24 non-merge commits — module exercises/solution keys (M06/07/11/12/13-15/16/26/28/29-32/37-38/40/45), column-law renames bore_dev_um→bore_deviation_um (waves 1-2, re-sealed), stale count fix (m41 43→42), gate/shim-court fixes (existence guards precede grep; 7th invariant gate test-isolation-leak), KD-row staleness (KD-09/KD-08/KD-05), ending 62793177 course-completion-receipt with green final manifest gate |
| lsp-max | lsp-max | 5ad58c8 | y | chore: remove tracked __pycache__ .pyc files; ignore __pycache__/ |
| lsp-max | lsp-max | 81f7ad3 | y | docs: fix noun/crate-map drift in CLAUDE.md (E-y row) |
| lsp-max | lsp-max | 57b207a | y | docs: relabel PERFORMANCE.md figures as design targets (E-f audit) |
| lsp-max | lsp-max | a86d167 | y | D28: fix(law) victory-language reword + rg fail-loud guard + law workflow cache (was in-flight on the working tree at addendum start; landed mid-poll, ancestry-verified separately) |

## Receipt-debt note

Per-lane detail (courts, verification ladders, disclosures) lives in the cited commit
messages per the coordinator's lane-law; this file is the citable index over those
landings, not a replacement for the messages. Later lanes (D25r/D52/D69, D28, smalt
D-lanes) append rows here as they land.
