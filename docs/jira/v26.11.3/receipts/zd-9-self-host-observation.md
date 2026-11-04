# ZD-9 Self-Host Observation

XA-1/XD-14 carry-over: `ggen sync run` in `self-host/` exited 1 with
`[FM-PACK-013] GGEN_SELF_HOST_OBSERVATION_INCOMPLETE`, firing before the EA gate.

## Root Cause

Two independent facts, neither an engine defect:

1. **Missing observation state, not a gate defect.** Gate `010_observation_complete.rq`
   (pack `ggen-self-host-pack`) requires 16 exact-revision observation properties on
   `gsh:ggen` (`gsh:revision`, `gsh:observationDigest`, counts, finding counts).
   `self-host/ontology.ttl` held only the fail-closed bootstrap seed (3 triples,
   deliberately omitting revision/digest/counts). The lawful observation state is
   produced by the real mechanism that already existed:
   `self-host/scripts/observe_self_host.py` (canonical observer; writes
   `self-host/ontology.ttl`, `self-host/observed/repository.json`,
   `self-host/observed/observation-receipt.json`; read-only census, writes nothing
   into `lib` code). No engine code was touched.

2. **Stale ambient binary (discovered during verify).** The ambient
   `~/.local/bin/ggen` is v26.10.11 (built Oct 11); its `ReceiptPayload` predates the
   `generation` epoch field, so its `deny_unknown_fields` tail parse of
   `.ggen-v2/receipt-log.jsonl` refuses with `[FM-CHAIN-003]` (`unknown field
   generation`) — and the HEAD log tail already carried `generation`, so the root
   dogfood sync failure under that binary **pre-dates this session**. The repo-built
   binary (v26.11.3) has `generation: Option<u64>` (additive, `serde(default)`) in
   `ReceiptPayload` (crates/ggen-engine/src/sync.rs:328-334), so the same log parses
   cleanly. No gate weakening on either side; the old binary's refusal is correct for
   its own (older, closed) payload contract.

## Fix Applied

Ran the real observer, then synced with the repo-built binary.

## Commands / Exits

- `python3 self-host/scripts/observe_self_host.py` (cwd `/Users/sac/ggen`) → exit 0
  (~13 min full-repo census). Wrote `self-host/ontology.ttl` (7.8 MB),
  `self-host/observed/repository.json` (5.9 MB), `self-host/observed/observation-receipt.json`.
- `python3 self-host/scripts/verify_observation.py` → exit 0,
  `GGEN_SELF_HOST_OBSERVATION_VERIFIED` (independent verification against git HEAD: revision, per-file digests/sizes,
  path-set, counts, canonical digest, receipt output map, `actuation_performed=false`).
- `ggen sync run` (ambient 26.10.11) in `self-host/` → **exit 0** after the observation
  landed (gate 010 satisfied).
- Root `ggen sync run` (ambient 26.10.11) → exit 1 `[FM-CHAIN-003]` — pre-existing
  binary/log schema skew, present at HEAD before this session (HEAD log tail already
  carried `generation`).
- `/Users/sac/ggen/target/debug/ggen sync run` (repo-built **26.11.3**) in `self-host/`
  → exit 0 (23.4s).
- `/Users/sac/ggen/target/debug/ggen sync run` at root → exit 0 (4.14s) — dogfood green.

## Standing

ALIVE — self-host sync exit 0 and root dogfood sync exit 0 under the repo-built
v26.11.3 binary, with independently verified observation state. (Typo-guard: the
verifier string is `GGEN_SELF_HOST_OBSERVATION_VERIFIED`.)

## Falsifiers

- Re-run `observe_self_host.py --check` after a commit moves HEAD → drift exit 2
  (observation is revision-bound; must re-observe per revision).
- Ambient `~/.local/bin/ggen` 26.10.11 will keep refusing the root sync with
  FM-CHAIN-003 until it is reinstalled from this tree (outside this lane's ownership).
