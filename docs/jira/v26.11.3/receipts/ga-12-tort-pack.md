# GA-12 — tort-pack: the p99 Torture Matrix as ggen infrastructure

Standing: ALIVE

- **Lane**: GA-12, wave v26.11.3 (ggen-marketplace pack + cgap consumer wiring)
- **Subject**: `packs/tort-pack/` (new, ggen-marketplace) + `cgap/ggen.toml`
  `[packs]` wiring line + two rendered projections in cgap
  (`docs/torture-matrix.md`, `docs/torture-targets-open.md`)
- **ggen binary**: 26.10.11 installed (`/Users/sac/.local/bin/ggen`), skill
  contract read first
- **Status**: ALIVE — every command below ran this session against the exact
  files named; no git operations performed.

## What landed

The five sovereign torture injections are queryable RDF individuals
(`<urn:tort:>`, `packs/tort-pack/ontology.ttl`):

| Injection | Ceiling | Standing | Source |
|---|---|---|---|
| tort-1 bimodal-heap | <850 ns | TARGET-UNMEASURED | AF-37's injection bench (named, un-landed) |
| tort-2 stall-walk | <12 us | MEASURED | ab-10 (cgap `receipts/ab-10-writerv-stress.md`: 113,132 fps stall-injection soak = 8.84 us/frame under the Ok(0) injection, + headroom) |
| tort-3 cache-stride | <450 ns | TARGET-UNMEASURED | AF-37 |
| tort-4 tamper-inundation | <1.5 us | TARGET-UNMEASURED | AF-37 |
| tort-5 supervisor-churn | 5.8-7.4 ms | MEASURED | ab-17 (LANDED, lsp-max tree) + ab-18 (POLLED-NOT-LANDED) |

Plus 6 `CloudBlowout` individuals — the legacy cloud p99 contrast rows
verbatim from arcus `bare-metal-transit-ledger.md` — every one standing
TARGET-UNMEASURED per AF-18's census, and `courtLane`-shaped
`ceilingSource`/`prov:wasDerivedFrom` edges to the landed ab-* receipts.

## Gates (4 sync-time SPARQL gates, non-vacuity witnessed)

1. `010_injection_band_closure` — closed band tort-1..tort-5 + required
   properties.
2. `020_ceiling_source_closure` — every injection carries ceiling+source
   (MEASURED requires ceilingSource + ceilingExact + typed unit value) or
   TARGET standing (requires namedBench); standing outside
   {MEASURED, TARGET-UNMEASURED} refuses.
3. `030_cloud_blowout_target` — anti-fabrication: any CloudBlowout without
   TARGET standing = `[FM-PACK-013]`; also refuses a blowout used as a
   ceilingSource, or missing range/bench.
4. `040_measured_source_landed` — every MEASURED ceiling needs >=1 LANDED
   source lane (receiptPath + standing != POLLED-NOT-LANDED).

## Courts (real output this session)

- **Witness court** (`python3 qualification/verify.py`, pack root):
  `CONFORMANT`, gates=4, flips=4 — every gate 0 rows on its pass witness
  and >=1 on its fail witness (010: 3 fail rows; 020: 4; 030: 1; 040: 1).
  Exit 0.
- **Shim-court** (mutations injected into a COPY of the real
  `ontology.ttl`, gates run via rdflib over the shimmed graph):
  4/4 REFUSE with per-gate shims (fabricated `tort-6`; forged MEASURED
  stall-walk with no source; fabricated MEASURED cloud blowout; MEASURED
  ceiling anchored only on unlanded ab-18). RESTORE verified byte-exact
  sha256 `59b41bd9b43e8447...`; post-restore all 4 gates 0 rows. A
  court that cannot flip is FAIL — all four flip.
- **cgap sync x2** (`ggen sync run` from `/Users/sac/cgap`, after wiring
  `tort-pack = { path = "../ggen-marketplace/packs/tort-pack" }`):
  - run 1: exit 0, `"written": ["docs/torture-matrix.md",
    "docs/torture-targets-open.md"]` (all 4 pack gates green at sync time);
  - run 2 (replay): exit 0, `"written": []`, graph_hash `633987d55b51...`
    (checksum freeze — replay is byte-stable).
- **Marketplace validate**: `python3 scripts/marketplace.py validate` —
  exit 0, `packs=347 manifests=347` (tort-pack included).

## Render receipts (on disk, the receipt of record)

`/Users/sac/cgap/docs/torture-matrix.md` — 5-row injection table with
measured ceilings + source cites and TARGET-labeled targets, lane table
(ab-18 rendered as "— (not landed)"), 6 blowout rows all TARGET-UNMEASURED.
`/Users/sac/cgap/docs/torture-targets-open.md` — the 3 TARGET ceilings with
AF-37 named as the measuring bench + the 6 unmeasured blowouts.

## Honesty disclosures (read from disk this session)

1. **ab-18 has no receipt on disk** (searched cgap/arcus/ggen/lsp-max
   receipts, 2026-11-03). The lane contract mandates tort-5 as MEASURED
   citing ab-17/ab-18; the graph models this exactly: ab-17 is the LANDED
   anchor (gate 040 satisfied by ab-17 alone), ab-18 is POLLED-NOT-LANDED
   with no receiptPath and fences nothing. When ab-18 lands it supersedes.
   Note ab-17's receipt itself carries churn/leak witnesses, not a printed
   per-cycle ms number — the 5.8-7.4 ms range is carried in `ceilingExact`
   as the receipted-form range with its anchor disclosed, never as a
   per-cycle printed measurement.
2. **tort-2's <12 us is a fence over ab-10's receipted mean**, not a
   separately receipted p99 percentile: 113,132 fps = 8.84 us/frame mean
   under the Ok(0) stall injection; the <12 us ceiling discloses the
   derivation in `ceilingExact` and in the render.
3. **AF-37, AF-18, AF-30 have no receipts on disk** — they appear only as
   named benches (the work queue), never as evidence. tort-1/tort-3/tort-4
   ceilings and all blowout ranges stay TARGET-UNMEASURED, prov'd to the
   transit-ledger source individual (`tort:torture-matrix-source`).
4. Gate 030 makes disclosure mechanically enforceable: any future
   CloudBlowout (or blowout edit) that claims a non-TARGET standing
   refuses the sync with `[FM-PACK-013]`.

## Learned live

- rdflib refuses an unbound prefix (`w:`) appended mid-file — shim
  mutations must reuse prefixes the host ontology already binds.
- Single-shim courts under-witness: a fabricated-blowout shim flips only
  gate 030 (1/4). Per-gate shims (the fail-witness mutations, replayed
  against the real ontology graph) are the shape that proves 4/4.

## Standing

- tort-pack: **ALIVE** — witness court exit 0 (4/4 FLIP), shim-court 4/4
  refusal + byte-exact restore, cgap sync x2 exit 0 (written 2, then `[]`),
  marketplace validate exit 0, this session, exact subject.
- tort-1/tort-3/tort-4 ceilings + all cloud blowouts: TARGET-UNMEASURED
  pending AF-37's injection bench (and AF-18/AF-30 where named); never
  presentable as measured.
- ab-18: POLLED-NOT-LANDED; fences nothing.
