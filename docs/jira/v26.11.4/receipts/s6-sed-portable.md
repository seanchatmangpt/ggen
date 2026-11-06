# S-6 Receipt — verify_v26.10.31.sh sed portability (ZB-65 ubuntu half)

Standing: PARTIAL_ALIVE

## Premise check (seed was stale)

Seed S-6 said the BSD `sed -i ''` at verify_v26.10.31.sh:80 still needed a ggen-side
fix. Re-derivation on disk:

- The script does NOT exist under /Users/sac/ggen — it lives at
  `/Users/sac/cgap/scripts/verify_v26.10.31.sh`.
- The ubuntu half already LANDED in cgap: commit `4d523705`
  ("fix(zd-141): portable in-place edit in verify_v26.10.31.sh classify branch (ZB-65)",
  2026-11-04), which replaced `sed -i '' ... "$TABLE"` with
  `sed 's/.../.../' "$TABLE" > "$TABLE.tmp" && mv "$TABLE.tmp" "$TABLE"` (script
  lines 106-107).
- Current state: `grep -c "sed -i" /Users/sac/cgap/scripts/verify_v26.10.31.sh` → `0`.
  Only POSIX sed usage remains (`sed 's/^/    | /'` at line 76, temp+mv at 106).
- Prior corroboration: `docs/jira/v26.11.4/receipts/gg1-closure.md:26` (zero
  `sed -i` matches) and `receipts/zp-16-publish-automation.md:83` ("Half 1 (ubuntu,
  BSD sed -i '') LANDED in cgap").

## Fix verification (no new edit required)

The landed pattern is sed-to-temp + `mv` — POSIX-portable, no in-place flag on
either sed flavor. The sed program is a plain POSIX BRE with no GNU/BSD
extensions (`|` and backtick-literals only, no `\|` alternation, no `-E` /
`--posix` dependence), so it parses identically under GNU sed.

## Commands + exits

- `find /Users/sac/ggen/docs/jira -name verify_v26.10.31.sh` → no output (absent
  in ggen).
- `git -C /Users/sac/cgap log --oneline -3 -- scripts/verify_v26.10.31.sh` → shows
  `4d523705` fix commit on top.
- `grep -c "sed -i" /Users/sac/cgap/scripts/verify_v.10.31.sh` → `0`.
- `./scripts/verify_v26.10.31.sh` in /Users/sac/cgap (BSD sed host, darwin) →
  see gate line below.
- GNU-sed static check: gsed absent on host; docker/colima start failed
  (`error starting docker: exit status 1`). GNU-side execution is therefore
  UNVERIFIED-ubuntu; statically the command form `sed prog file > out` is
  POSIX-defined and identical on GNU sed.

## Gate

- Local gate (BSD sed side): PENDING at receipt write time; final exit recorded
  in the wave ledger comment / amended below before landing.
- AMENDED 2026-11-05 (L6 re-verification): `bash -n scripts/verify_v26.10.31.sh`
  exit 0. Full script run in /Users/sac/cgap: GATE check-workspace PASS exit=0;
  test-cgap-arrow FAIL exit=101 (3 pre-existing band-size test failures,
  status_dictionary.rs:201 `BandSize(9)` vs frozen 8-entry band) and
  test-cgap-parquet FAIL exit=101 (same band mismatch, consumer court) — both
  PRE-EXISTING, unrelated to sed; run hit L6's 300s external timeout (exit 124)
  mid test-cgap-git, so no overall exit recorded. The line-106 sed classify
  branch was not exercised this run (check-workspace passed, no baseline
  mismatch). Isolation demo (temp file, darwin/BSD sed): old form
  `sed -i '' 's/| FAIL |/| PRE-EXISTING-FAIL |/' f` exit 0 with correct
  substitution; new form `sed 's/.../.../' f > f.tmp && mv f.tmp f` exit 0 with
  byte-identical result. GNU-side execution remains UNVERIFIED-ubuntu (no gsed,
  no container runtime this session); `sed prog file > out` + `mv` is
  POSIX-defined and flag-free, identical on GNU sed. S-6 falsifier ("verify
  script exits 0 on ubuntu runner") still UNVERIFIED — requires an ubuntu
  runner.

## Honest status

PARTIAL_ALIVE. The registered defect (ZB-65 ubuntu half) is already closed in the
owning repo (cgap); the ggen-side seed was stale. GNU-sed runtime execution
UNVERIFIED-ubuntu (no gsed, no container runtime available this session).
