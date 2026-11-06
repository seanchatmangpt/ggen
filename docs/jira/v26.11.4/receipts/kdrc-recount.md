# KD-RC — Register Recount (audit-class lane)

Standing: PARTIAL

Lane KD-RC, 2026-11-05. All four register rows re-read live against their
repos; rows updated in place with dated provenance lines; zero silent
closures taken (the falsifier class — a KD closing without a witnessed
recovery receipt — was checked per row). No lib code touched; register .md
edits only; nothing committed.

## Per-row verdict table

| Register | Row | Verdict | Evidence (path, what was run) |
|---|---|---|---|
| cgap `docs/jira/v26.11.4/KNOWN_DIVERGENCES.md` | KD-1 | OPEN (close condition met in code, closure UNWITNESSED — FINDING KD-RC-1) | `status_maps.rs` carries `UNSUPPORTED_VERSION 0x4347A009`, `AOQ_STATUS_COUNT=9` (committed a216b486, 2026-11-04 21:22, tree clean); BOTH mapping sites remapped (`aoq_session.rs:110`, `aoq_sidecar.rs:340`, `// KD-COVERS: KD-1` markers). Repo-wide grep of `docs/jira/` for `FM-AOQ-009\|UNSUPPORTED_VERSION` → only the register itself: **no witness receipt**, L2/L3 legs not re-pinned. Row cannot flip per register law. |
| cgap | KD-2 | CLOSED (verified) | Receipt `docs/jira/v26.11.5/receipts/l27-kd2-claims.md` exists (mtime 2026-11-04 18:31), postdates the 2026-11-04 registration. |
| cgap | KD-3 | OPEN (confirmed) | `crates/cgap-core/src/frame.rs:44` `ATTESTATION_LEN: usize = 80` still pinned. |
| cgap | KD-4 | OPEN (confirmed) | `crates/cgap-ffi/src/oabi_lookup.rs` header still "typed UNSUPPORTED(scope) decision record". |
| cgap | KD-5 | CLOSED (re-verified) | `crates/cgap-arrow/src/transpose.rs:24` = `pub use crate::layout::Quad64`. |
| cgap | KD-6 | CLOSED-PREMISE-ROTTED (re-verified) | `git ls-files crates/cgap-blake3/tests/` has no bench_tmp.rs; dir listing confirms absence. |
| cgap | KD-7 | PARTIAL (confirmed, movement noted) | 3 doc-comment occurrences now exist (`sovereign.rs:362,1135,1151`, "stays internal") but no gate, no GAP-1 receipt; `include/cgap.h:168` has only `cgap_verify_p28`. |
| arcus `KNOWN_DIVERGENCES.md` | KD-1 | OPEN (confirmed) | grep for `cgap_import_arrow` over `/Users/sac/cgap/crates` (h+rs) = 0 hits. |
| arcus | KD-2 | OPEN (confirmed) | Evidence cite resolves: `docs/jira/v26.11.3/dx/03-replay-verb.md:109` carries the divergence note verbatim. |
| arcus | KD-3 | STALE (premise moved — finding KD-RC-2) | No `tools/` dir, no `tools/go.mod`; root `go.mod` (71 B) has no golangci tool directive; only workflow `security.yml` (no lint job); zero `golangci` references repo-wide; no `.golangci*`, no make/just lint target. The pin the row registers does not exist on disk. Not flipped (close condition presupposes a live pin); re-scaffold-or-delete decision left to owner. |
| arcus | KD-4 | OPEN (confirmed) | `workspace.dsl:29,41,89,90,101` still claims TLS 1.3/QUIC/mTLS; TLS/QUIC grep over `internal/` = 0 code hits. |
| ggen `KNOWN_DIVERGENCES.md` | XC-8/ZD-38 | OPEN (upstream), premise PARTLY moved | `process_mining` deleted from workspace/lock (commit cce1e8aa0, 0 occurrences); quick-xml 0.37.5 now enters via oxrdfxml 0.2.4, sparesults 0.3.4, wasm4pm-compat 26.8.7; a 0.41.0 copy also in lock. `cargo audit` exits clean on RUSTSEC-2026-0194/0195 **only via `.cargo/audit.toml` registered ignores** — registered divergence, not a fix. Close condition re-anchored to the new ingress deps. |
| ggen | R13 | OPEN (confirmed) | Flag remains a named opt-out wired in `crates/ggen-engine/src/verbs/sync.rs`/`handlers.rs`; no default-on bypass. |
| ggen | ZD-135 | PARTIAL (confirmed; one stale caveat — finding KD-RC-3) | Receipt `zd-135-committed-confirm.md` still 52/53 (FM-PACK-008 failing). Its "rg absent on this host" caveat is stale: `command -v rg` exits 0 this session (real binary on bash PATH). Sub-check UNVERIFIED as-run, not missing-binary. |
| smalt `posh/KNOWN_DIVERGENCES.md` | KD-01 | CLOSED (re-verified via PASS ledger) | §4.1 role_definition cell PASS; §6 closure text stands. |
| smalt | KD-02 | OPEN (no movement witnessed) | Row stands as written. |
| smalt | KD-03 | OPEN (stands) | Evidence exists: `docs/jira/posh/v26.10.30/gaps/23-secret-echo-staleness.md` (2026-10-30). |
| smalt | KD-04 | OPEN (subject stands; evidence path DANGLING — finding KD-RC-S1) | `find . -name "posh009-*"` (build dirs excluded) → only `docs/jira/posh/v26.10.30/receipts/parity/posh009-coverage-gate-{flip,green}.json`. `posh009-traffic-update-only.md` does not exist. |
| smalt | KD-05 | OPEN CORE half (re-confirmed); POSH-MEMBRANE half CLOSED (re-verified); evidence path DANGLING (KD-RC-S1) | `posh/internal/dialect/azure/family_translate.go:28` ParseMemoryGiB + `family_translate_test.go:60` mutation court present; `grep -rn "GiB\|MiB" lib/smalt --include="*.ex"` = only unrelated transport caps. `kd09-witness.md` exists (2026-11-04 17:32); `posh009-numeric-memory.md` does not exist. |
| smalt | KD-06 | OPEN (evidence path DANGLING — KD-RC-S1) | `posh009-api-version-ceilings.md` does not exist; subject not re-probed this session. |
| smalt | KD-07 | OPEN (stands) | Honesty-law markers in `adapters/terraform-provider-smalt/internal/provider/resource_container_app_environment.go`. |
| smalt | KD-08 | OPEN on FIC half (AMENDED row re-verified) | roleAssignments routes mounted (`resource_providers.ex:4432,4444,4463`); `federatedIdentityCredentials` grep over the plug = 0 hits. |
| smalt | KD-09 | CLOSED (re-verified) | Builtin GUIDs at `lib/smalt/policy/assignments.ex:21-25`; witnesses `r10b-policy-split.md` (2026-10-29 23:12) + `kd09-witness.md` (2026-11-04) exist. |

## Commands + exits (all run 2026-11-05)

- `grep AOQ_STATUS_COUNT crates/cgap-ffi/src/aoq/status_maps.rs` → 9 (band extended)
- `grep -B3 -A3 UnsupportedVersion` both mapping sites → remapped arms with KD-COVERS markers
- `git log --all -S "UNSUPPORTED_VERSION" -- crates/` → a216b486 (2026-11-04 21:22); `git status --porcelain` on the three files → clean
- `grep -rln "FM-AOQ-009\|UNSUPPORTED_VERSION" docs/jira/` → KNOWN_DIVERGENCES.md only (no witness receipt) → exit 0 with 1 hit
- `ls docs/jira/v26.11.5/receipts/l27-kd2-claims.md` → exists, Nov 4 18:31
- `grep ATTESTATION_LEN crates/cgap-core/src/frame.rs` → :44 = 80
- `grep Quad64 crates/cgap-arrow/src/{transpose,layout}.rs` → re-export at transpose.rs:24
- `git ls-files crates/cgap-blake3/tests/` + `ls` → no bench_tmp.rs
- `grep -rn "_cgap_p28_invariant_check" crates include tests` → 3 doc-comment hits in sovereign.rs
- `grep -rn cgap_import_arrow /Users/sac/cgap/crates` → 0 hits
- `ls tools/ go.mod` (arcus) → no tools dir; go.mod 71 B, no golangci
- `grep -rin "tls\|quic" internal/` (arcus) → 0 code hits; workspace.dsl claims intact
- `python3 -` lock parser (ggen) → quick-xml 0.37.5 dependents: oxrdfxml 0.2.4, sparesults 0.3.4, wasm4pm-compat 26.8.7; 0.41.0 copy present; process_mining: 0
- `cargo audit` (ggen) → "6 allowed warnings found", quick-xml unflagged; `.cargo/audit.toml` carries `ignore = ["RUSTSEC-2026-0194","RUSTSEC-2026-0195"]`
- `command -v rg` → exit 0 (binary on bash PATH)
- `find . -name "posh009-*"` (smalt, build dirs excluded) → only coverage-gate-{flip,green}.json
- `grep ParseMemoryGiB / TestMutationKD05` → family_translate.go:28 / family_translate_test.go:60
- `grep roleAssignments|federatedIdentityCredentials resource_providers.ex` → routes at 4432/4444/4463; FIC 0 hits
- `grep builtin GUIDs assignments.ex` → :21,:25
- `python3 scripts/coverage.py --self-test` (smalt posh, after my register edit) → SELF-TEST PASS, exit 0; gate still parses 8 KD rows; flip + restore witnessed

## Falsifier check

Any rowed KD closed without a witnessed recovery receipt → recount must catch
zero such closures (or report them as findings). Result: **one caught** —
cgap KD-1's close condition landed in committed code (band extension + both
remaps) with NO witness receipt anywhere in `docs/jira/`; reported as
KD-RC-1 and the row was NOT flipped (stays OPEN).

## Findings

1. **KD-RC-1 (cgap KD-1)** — closure unwitnessed: code-level close condition
   fully met (UNSUPPORTED_VERSION 0x4347A009, count=9, both mapping sites
   remapped, KD-COVERS markers, commit a216b486), but no receipt witnesses it
   and the L2/L3 court legs were not re-pinned to the new word. Row stays
   OPEN. Action: mint a sidecar wire-probe receipt asserting the version-skew
   status word = 0x4347A009 with re-pinned L2/L3 legs, then flip.
2. **KD-RC-2 (arcus KD-3)** — premise moved: the registered hermetic lint pin
   (tools/go.mod + ci.yml lint job) does not exist on disk; only
   security.yml ships, no golangci anywhere. Row marked STALE, not flipped —
   re-scaffold (row returns OPEN) or delete the row; both need an owner
   decision.
3. **KD-RC-3 (ggen ZD-135)** — stale caveat: the receipt's "rg absent on this
   host / silent skip" no longer describes the machine (`command -v rg`
   exits 0); the sub-check remains UNVERIFIED as-run until the ladder re-runs
   with rg live.
4. **KD-RC-S1 (smalt posh KD-04/KD-05/KD-06)** — evidence paths dangling: all
   three cited `posh/receipts/posh009-*.md` receipts do not exist anywhere in
   the repo (only the coverage-gate parity JSONs match `posh009-*`). Per the
   register's own law these rows carry invalid evidence cites; locate or
   re-mint the receipts. Subjects re-confirmed where probed, so no status
   flips were taken.
5. **XC-8 premise drift (ggen)** — registered ingress (process_mining) is
   gone from the lock; 0.37.5 now enters via oxrdfxml/sparesults/
   wasm4pm-compat, and the vuln gate's clean exit is carried by
   `.cargo/audit.toml` registered ignores, not by a fix. Row updated, stays
   OPEN (upstream).

## Standing

PARTIAL — all 22 rows across 4 registers recounted live with real commands;
registers updated in place with dated provenance lines; smalt coverage gate
re-verified post-edit (self-test PASS, exit 0). No register row was flipped
(this lane is audit-class; flips require witness receipts minted by their
owners). Nothing committed.
