# S-8 / MKT-04: pack-side standing re-verify at the v26.11.3 tag

Standing: PARTIAL_ALIVE

- Backlog row: S-8 (MKT-04), falsifier "a qualified pack fails at-tag re-verify", gated on
  CARRY-19 (v26.11.4 SEED.md:93-94, 124; next-wave-seeds.md:37-38).
- **Gate unmet**: CARRY-19 has not minted the v26.11.3 tag — `git tag -l` in
  /Users/sac/ggen shows no v26.11.3 tag (latest: v26.11.0 at 344c163b1). cgap and
  ggen-marketplace have no tags at all. The at-tag run is therefore not executable yet;
  S-8 remains OPEN.
- **Rehearsal executed at the closest lawful pin** — ggen-marketplace HEAD `ca16a013f`
  (fix(packs): force:true on idempotent-sync templates), under ambient ggen 26.11.3
  (the MKT-01 court identity, `ggen --version` = 26.11.3), 2026-11-05:

```
cd /Users/sac/ggen-marketplace
python3 scripts/qualify_packs.py --workers 8 --timeout-seconds 5 \
  --report /tmp/l8-s8-qualify-report.json   # exit 0
```

  Command exit 0. `--timeout-seconds 30` was REFUSED by the court's own bound
  (`GGEN_PACK_TIMEOUT_BOUND:must_be_gt_0_and_lte_5`) — first attempt exit != 0,
  retried at the lawful 5s bound. Full corpus: **348 packs** (up from 347 at zd-65).
- Disclosure: the marketplace working tree was dirty during the run (other lanes'
  in-flight edits: several pack.toml / templates / ontology.ttl / a workflow yaml).
  The at-tag replay after CARRY-19 must run from a clean archive at the tag SHA to
  be the lawful at-tag receipt.
- ## Per-pack verdict table (non-ALIVE rows only; 338/348 ALIVE)

| Pack | Verdict | Detail |
|---|---|---|
| automatic-autonomic-operations-pack | WARN (non-blocking) | REFUSED:GGEN_PACK_GENERATED_BUILD_FAILED — cargo build exit=101 on generated manifest |
| certification-assist-evidence-control-pack | WARN (non-blocking) | same code, exit=101 |
| certification-assist-pack | WARN (non-blocking) | same code, exit=101 |
| clap-noun-verb-crate-pack | WARN (non-blocking) | same code, exit=101 |
| ex4pm-wasm4pm-bindings-pack | WARN (at-HEAD only) | same code, exit=101 — the clap-noun-verb-schema-pack composition-closure residue family |
| ontostar-mustar-powlv2-agent-pack | WARN (non-blocking) | same code, exit=101 |
| otel-weaver-ocel-pack | WARN (non-blocking) | same code, exit=101 |
| speedrun-talent-network-pack | WARN (non-blocking) | same code, exit=101 |
| tcps-release-pack | WARN (non-blocking) | same code, exit=101 |
| tcps-pack | SKIPPED (typed, harness-limitation) | SKIPPED:GGEN_PACK_GENERATED_BUILD_NEEDS_WORKSPACE_ROOT |

- ## Falsifier verdict

  Falsifier NOT triggered: zero packs failed the court (no REFUSED-status pack, no
  ALIVE→non-ALIVE regression vs the zd-65 corpus). The 9 WARN + 1 SKIPPED are the
  same typed non-blocking classes MKT-03 already accepted/ratcheted. At-HEAD
  standing: **PASS** (exit 0, 348/348 admitted, 338 hard-ALIVE).
- ## Close condition

  S-8 closes when CARRY-19 mints the tags and this exact command re-runs from a
  clean tag archive with the same exit-0 outcome; falsifier remains armed until
  then. Replay command above; report JSON preserved at /tmp/l8-s8-qualify-report.json.
