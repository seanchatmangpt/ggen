# GA-11 — cscb-pack: the CBMQS 7-gate standard as RDF (v26.11.3)

- **Lane**: GA-11 (CSCB program ggen infrastructure). Date: 2026-11-04.
  No git commands run.
- **Deliverable**: `/Users/sac/ggen-marketplace/packs/cscb-pack/` (new pack)
  wired into cgap's `ggen.toml` `[packs]` (one `[packs]` line; no imports —
  see FM-CONFIG-003 note below). Consumer: `/Users/sac/cgap`.
- **ggen**: 26.10.11 (`/Users/sac/.local/bin/ggen`), installed binary ground
  truth.

## What landed

The CBMQS standard (arcus
`docs/jira/v26.11.3/cbmqs-standard.md`, AF-13, PROPOSED v1) as a queryable
RDF graph: 7 `CscbGate` individuals (g1..g7, closed band), 11 `CscbLane`
individuals (ab-2/3/6/9/10/11/12/16/19 measured + ab-17/af-7
POLLED-NOT-LANDED), gate→lane `prov:wasDerivedFrom` reference edges, and the
6 AF-2 UNSUPPORTED rows (1, 5, 13, 14, 15, 16) as `CscbTarget` individuals
standing TARGET-UNMEASURED with named benches, provenanced to the AF-2 audit
entity (`dcterms:source` = the audit's file URL). Values are the
AF-2-corrected ones: G1's fence = AB-19's receipted 100,000 ops/s floor;
G7 is structural-no-ceiling per AF-2 row 1 UNSUPPORTED — the refuted
18.1 ns is never encoded as a threshold (gate 040 refuses the literal on any
non-target subject; a CscbTarget's verbatim UNSUPPORTED claim quoting it is
the lawful exception).

Files: `pack.toml` ([capabilities] provides `urn:ggen:pack:cscb-pack`;
[validation] 4 gates), `ontology.ttl`, `gates/010..040*.rq`,
`templates/gates_table.md.tmpl` → `docs/cscb-gates-table.md`,
`templates/targets_open.md.tmpl` → `docs/cscb-targets-open.md`,
`witnesses/{pass,fail}/` ×4, `qualification/verify.py` +
`gate-court.toml` (aoq-pack runner shape; schema
`ggen.semantic-gate-witness-court/1`).

## Gates (exact commands + exits, this session)

1. **Witness court** — `python3 qualification/verify.py` (pack root) →
   CONFORMANT, 4/4 FLIP-OK (pass 0 rows / fail ≥1 rows per gate), **exit 0**.
   A gate that cannot flip is VACUOUS-GATE (exit 1).
2. **cgap sync ×2** — `cd /Users/sac/cgap && ggen sync run`:
   - run 1: **exit 0**, `written: ["docs/cscb-gates-table.md",
     "docs/cscb-targets-open.md"]`,
     graph_hash `4af8cb3aa1b674e42c5dcec2453031723992c692b2bc487eec3b1c5ef2415b06`
   - run 2: **exit 0**, **`written: []`**, same graph_hash — deterministic.
3. **Shim-court (ar-1 protocol)** — baseline sha256 of all 4 gate files +
   `ggen.lock`; per gate: shim to always-violating `SELECT ?s WHERE { ?s ?p
   ?o } ORDER BY ?s LIMIT 1` marked `# MESSAGE: SHIM-COURT probe`, hold
   `ggen.lock` aside (shimmed pack content hash → FM-PACK-008 would preempt),
   `ggen sync run --dry-run`, expect `[FM-PACK-013] ... gate` naming the file;
   restore. **4/4 FLIP** (010/020/030/040 each refused with exit 1);
   `shasum -a 256 -c`: all 4 gates + ggen.lock **OK** (byte-exact restore,
   no residue); unshimmed dry-run exit 0. First shim attempt exposed my own
   shim bug (LIMIT before ORDER BY = SPARQL parse error) — the fail-closed
   path fired on the shim itself before the court ran; fixed the shim, not
   the gate.
4. **Post-court determinism** — `ggen sync run` again: **`written: []`**.

## Learned live (pack-wiring facts, 26.10.11)

- **`[ontology].imports` refuses `..` traversal** (`[FM-CONFIG-003]
  invalid_path`) — the cross-repo pack import shape I planned first is
  refused. Correct shape: list the pack under `[packs]`; its ontology.ttl
  composes into the union graph automatically. cgap's `ggen.toml` carries a
  comment recording this.
- **ORDER BY then LIMIT** in gate SPARQL (LIMIT-before-ORDER-BY is a parse
  error, FM-GRAPH-003).

## Deployment-matrix render sample (real, from sync run 1)

```
| Gate | Name | Threshold (exact receipted form) | Reference receipt | ...
| g1 | Zero-alloc hot path | 0 allocations per window on emit+read (counting
GlobalAlloc, min-of-400); throughput floor fence 100000 ops/s (AB-19's
receipted floor) | `docs/jira/v1...` (ab-19-slab-stress.md)
| g7 | Receipt chain | structural: chained receipt per admitted frame +
full-chain verify green + measured per-link cost recorded and disclosed; no
numeric ceiling | `file:///Users/sac/arcus/docs/jira/v26.11.3/dflss-aoq-claims-audit.md`
```

Targets render (docs/cscb-targets-open.md, all six rows TARGET-UNMEASURED
with named benches):

```
| 1 | 18.1 ns per-call receipt latency | TARGET-UNMEASURED | ab-12 court-1 per-call latency cell (64 B-1 KiB, release, min-of-3) |
```

> ERRATA 2026-11-04: the 18.1 ns target row and this receipt's TARGET-UNMEASURED
> standing are superseded by the v4 corrected-claims ledger
> (`ggen/docs/jira/v26.11.4/receipts/v4-corrected-claims-ledger.md` rows 1-2)
> and `cgap/docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md`.

(Verbatim rows as rendered above in the receipt body — the on-disk renders
at `/Users/sac/cgap/docs/cscb-{gates-table,targets-open}.md` are the
receipt of record.)

The prompts' "tactical deployment matrix" is now a SELECT: the 7-gate matrix
and the 6-target work queue are projections of the graph; the anti-fake law
(no TARGET standing as threshold evidence; no refuted 18.1 ns threshold;
every gate references a receipted lane) is enforced at sync time by the 4
gates, whose non-vacuity is witnessed (4/4 flip).

## Standing

- cscb-pack: **ALIVE** — witness court exit 0 (4/4 FLIP), cgap sync ×2 exit 0
  with `written: []` on replay, shim-court 4/4 refusal + byte-exact restore,
  this session, exact subject.
- g7/gate-040 law: the refuted 18.1 ns is fenceless (G7 structural) and
  gate-040 refuses it as a threshold — AF-2 row 1 stays UNSUPPORTED.
- ab-17, af-7: POLLED-NOT-LANDED (searched cgap/arcus/ggen receipts,
  2026-11-04); they fence nothing. When their receipts land they supersede
  per the standard's own law.
