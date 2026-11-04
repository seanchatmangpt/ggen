# GA-9 Receipt: SA-8 BLOCKED item — wasm4pm-facts-pack `[[capabilities.dialects]]`

Date: 2026-11-03. Lane GA-9. Binary: installed `ggen` 26.10.11 (`~/.local/bin/ggen`).
Subject: `/Users/sac/ggen/packs/wasm4pm-facts-pack/pack.toml` + `/Users/sac/ggen/examples/receiptctl/`.

## Defect

`pack.toml:24` carried `[[capabilities.dialects]]` (two entries, 6 lines total: turtle
`.ttl`, n3 `.n3`) — an unknown field under `[capabilities]` (schema allows only
`types`/`provides`/`requires`). This made `examples/receiptctl` `ggen sync run --dry-run`
fail with `[FM-PACK-003]`.

## BEFORE (exit 1, exact error)

```
[FM-PACK-003] pack `wasm4pm-facts-pack`: invalid pack.toml at
`.../packs/wasm4pm-facts-pack/pack.toml`: TOML parse error at line 24, column 16
   |
24 | [[capabilities.dialects]]
   |                ^^^^^^^^
unknown field `dialects`, expected one of `types`, `provides`, `requires`.
```

## Consumer check (lawful deletion, not relocation)

`grep -rn "dialects" packs/wasm4pm-facts-pack/ examples/receiptctl/` → the ONLY hits are
the two `[[capabilities.dialects]]` lines in pack.toml itself. No template, gate query,
ontology, or generated file referenced the dialect triples. Deletion is safe; no
schema-legal relocation needed.

## Fix

Deleted pack.toml lines 24-30 (the two 3-line `[[capabilities.dialects]]` entries plus
blank separator). Note: the dispatch described this as a "2-line deletion"; on disk it is
a 6-line deletion (two 3-line entries) — same data, disclosed here.

## AFTER — one blocker remained beyond the dialects defect (disclosed)

Dry-run exit 0 after deletion, but the real sync then failed on PRE-EXISTING state:
the example's `.ggen-v2/` receipts (`receipt.json`, `receipt-portable.json`,
`receipt-log.jsonl`, all dated Oct 20) were written by a different-format binary
(journal `version:1` record/activity schema, lines 12-15 of 19732 chars) that 26.10.11
cannot deserialize:

- `[FM-CHAIN-003] receipt log tail ... does not deserialize: unknown field 'generation',
  expected one of graph_hash, outputs, packs, decisions, closure, build_provenance`
- then, after archiving the log: `[FM-CHAIN-003] previous receipt 'receipt.json'
  malformed: unknown field 'generation' ...` Remediation text: "verify or remove the
  stale receipt."

## Fix 2 (disclosed, generated-artifact repair)

- Deleted `ggen.lock` (the binary's own FM-PACK-008 remediation: "delete ggen.lock to
  intentionally re-lock" — the lock pinned the pre-fix pack hash `7531919f...`; the real
  sync rewrote it to the post-fix hash `ab76d98e...`).
- Archived (not deleted — O preserved) the stale-format receipt set to
  `.ggen-v2/stale-2026-10-20-archive/` (`receipt.json`, `receipt-portable.json`) and
  `receipt-log.jsonl.stale-journal-format.bak`. The example now has a fresh 26.10.11
  receipt chain.

## Verification ladder (all real, this session, cwd = examples/receiptctl)

| Gate | Command | Result |
|---|---|---|
| BEFORE dry-run | `ggen sync run --dry-run` | exit 1, `[FM-PACK-003]` naming line 24 `dialects` (exact error above) |
| AFTER dry-run | `ggen sync run --dry-run` | exit 0, `dry_run: true`, `planned_writes: []` |
| Real sync 1 | `ggen sync run` | exit 0, 27 files skipped-unchanged, fresh receipt chain written |
| Real sync 2 (determinism law) | `ggen sync run` | exit 0, `written: []`, 27 skipped |
| Graph-hash determinism | sync1 vs sync2 `graph_hash_hex` | identical: `b59562f811da59dec3d8c89458fe0fa085f16afe11194cec20570f18efcf1488` |
| Example's own purpose | `cargo test` | exit 0 — 168 tests, 0 failed (15 suites: 20/16/3/3/9/4/9/12/57/17/6/12) |
| Relock witness | `grep facts-pack ggen.lock` | `content_hash = "blake3:ab76d98ed359...acffecb"` (matches on-disk pack hash from the FM-PACK-008 error text) |

## Standing

ALIVE. Dry-run + real sync green end-to-end for examples/receiptctl; determinism law
witnessed (sync x2, second `written: []`, identical graph hash); the example's 168
cargo tests (including both wasm4pm-facts-pack generated proof suites) pass. No git run.
