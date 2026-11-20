# PRD — v26.11.4

Status: MATERIALIZED (2026-11-04, lane ZP-23). Corrections C1-C6 from the
34-audit Explore fan-out (ZB-40..74) are baked in; the corrections ledger and
per-correction falsifier sweeps live in §4. Grounding receipts were re-read on
disk at materialization time (§5 provenance table).

Standing: this PRD is a requirements document, not an execution receipt. Every
performance figure below carries its surface label and its receipt citation;
statuses use the standing vocabulary (ALIVE / PARTIAL / BLOCKED / UNVERIFIED /
UNSUPPORTED / REFUSED / MEASURED-MET / MEASURED-UNMET / TARGET-UNMEASURED).

## 1. Scope

v26.11.4 executes five work fronts, each grounded in the v26.11.3 landed
surface (see `docs/jira/v26.11.4/PLANS.md` for the full plan index):

1. **Membrane (PRD-01)** — ZP-11/12/13: 1M UDS round-trip court, arcus↔sidecar
   ML-KEM-768 handshake (go 1.23.4 → 1.24), zero-tag golden-window parity,
   AoqTagHook blake3 promotion (out-of-gengen ABB, cap removal, vector courts).
2. **Publish (PRD-03/05)** — ZP-16: crates.io Trusted Publishing automation
   (no automation exists today — ZB-48), tier DAG from the v26.11.2 runbook,
   environments `publish-tier0..5`/`publish-ggen` as the human admission edge.
3. **RFC-0089 / CBMQS (PRD-04)** — ZP-14/15/17/21/22: cbmqs-audit certificate
   bin, foreign-subject projection (namespaced P1-P8, gate-ID collision
   resolved by namespacing), Gate-10 settling experiment, durable seal,
   FIPS-204 signer.
4. **Perf provenance + gates** — ZP-18 (profile-guard 100% census), ZP-19
   (slab integration via TLS WindowBuffers + Slab64 identity plane), ZP-20
   (Gate-09, court-to-add per §3.4), and this document (ZP-23).
5. **Docs / hygiene** — ZP-24 (newline law), ZB-65 (portability), and this
   document (ZP-23).

## 2. Critical User Journeys (corrected)

CUJs were corrected per the provenance audit (ZB-40/61/67): two figures in the
draft CUJs were CONTRADICTED as stated and are corrected here with surface
labels and receipts.

### CUJ-1 — Receipt append latency (corrected: C1)

Claimed: "receipt in 167.0 ns". CONTRADICTED as stated (ZB-40/61/67; af-35).

Corrected claim: **chained BLAKE3 receipt 456.3 ns** on the landed surface
`AoqChain::append` (ab-7 chain law: `H(payload)` + `H(prev‖epoch‖idx‖
payload_digest)` — two BLAKE3 invocations plus a record write). Source: af-35
(transit audit), which re-measured the composed transit at 475.0 ns/frame
(2.105M fps) and ruled the 167 ns dossier row "does not correspond to any
landed surface".

The bare-hash 167.0 ns median (af-22: `cgap_blake3::hash` 64 B, best-of-3
median; p99 334 ns) is retained as a **labeled appendix row only** (§2.4) —
af-35 ruled it a no-landed-surface figure: it is not the receipt surface and
must not be quoted as one.

### CUJ-3 — Transit p99 under adversarial load (corrected: C2)

Claimed: "sub-10 µs ceiling". CONTRADICTED (only e2e adversarial p99 =
1.27-5.10 ms; per-stage MEETS rows are transport-excluded). Replaced by the
honest af-37 matrix (MEASURED, release, serial, aarch64 darwin; ranges across
green passes; per-run values printed by the bench, never rounded):

| injection | measured p99 (range) | target | verdict |
|---|---|---|---|
| I1 bimodal-size storm (processing p99) | 26,250-281,667 ns across 6 passes | <850 ns | UNMET |
| I1 64B class only | p99 3,625-4,041 ns | (component) | INFO |
| I1 3.8KB class only | p99 28,375-44,917 ns | (component) | INFO |
| I2 healthy-lane p99 under stall | 1,250-3,875 ns | <12 µs | MEETS |
| I3 decode p99 under cache pressure | 167-291 ns | <450 ns | MEETS |
| I4 admit p99 under 60% tamper | 583-584 ns | <1.5 µs | MEETS |
| I4 parity fence | p99(refuse)/p99(admit) 0.93-1.00 ≤ 1.5x | ≤1.5x | HOLDS |
| I5 relay e2e p99 vs ab-17/ab-18 band | 1.27-5.10 ms | band 5.8-7.4 ms | UNMET (below band) |

Root cause for I1 (af-37 court finding, quoted for budget honesty): ~4.5 µs of
the 64B class figure is digest work the 850 ns ceiling cannot include even at
zero overhead (blake3 over header+body twice per frame; ab-12's receipted
1768 MiB/s puts a 3.9 KB digest at ~2.2 µs floor each). The 850 ns
heap-storm ceiling is UNREACHABLE for the composed op as specified; it is
reachable only for a digest-free decode+gate path, which I1's structure does
not measure. The 0-alloc GC-immunity witness HOLDS on admit AND refusal paths.

### CUJ-1's original "167.0 ns receipt" — disposition

Corrected per C1 (above). The bare-hash appendix row:

#### Appendix row (labeled, per af-35 no-landed-surface ruling)

| surface | median | p99 | receipt |
|---|---|---|---|
| `cgap_blake3::hash` 64 B bare hash — NOT the receipt surface; af-35 ruled it no-landed-surface, appendix-only | 167.0 ns | 334.0 ns | af-22 |

## 3. Gate ladder (real names only)

Per ZB-70 (gate map): the PRD ladder is majority NEW work (3 renames, 3
partials, 6 no-equivalent). Wholesale replacement would silently drop
dod-001/007/008/009/010 — the PRD must ADOPT-AND-EXTEND the existing ladder,
never replace it. This section therefore uses REAL gate names from the landed
documents, with proposed new gates in a separate subsection.

### 3.1 Adopted: CBMQS G1-G7 (+ the 8th annotation row)

Source: arcus `docs/jira/v26.11.3/cbmqs-standard.md` (gate sections G1-G7;
prompt-vs-receipt rulings at the top of the file). The release court
`cbmqs_gates` asserts `gates.len()==8` — the 8th row is the aggregate
annotation row (dod-009; af-10-cbmqs; zd-36 re-run 8/8). Rulings carried:

- G1 zero-alloc hot path: MET (af-10 post-anti-DCE; ab-19 slab stress).
- G2 framing ceiling: MET (ab-6 codec, 10k single-byte mutation court).
- G7 correction: the "<25 ns, 18.1 ns" per-link cost is UNSUPPORTED; landed
  per-call 64 B bare-hash median is 167.0 ns (af-22) — labeled appendix row
  per C1.
- dod-009 gate table: `cargo test --release -p cgap-aoq --test cbmqs_gates`
  green (af-10-cbmqs; zd-36: gates.len()==8, n_fail==0).

### 3.2 Adopted: RFC-0089 §6 scorecard G1-G7

Source: arcus `docs/jira/v26.11.3/rfc-0089-cscb-p99.md` §6 scorecard (RX-3,
2026-11-03; implementation `cgap/crates/cgap-aoq/tests/rfc0089_scorecard.rs`,
receipt rx-3-scorecard.md). Re-read rows:

| Gate | Threshold | Verdict | Evidence |
|---|---|---|---|
| G1 | ≥20M pushes/s (slab) | MEASURED-MET | af-10 post-anti-DCE: 28.2M/28.8M pushes/s |
| G2 | ≤100 ns codec (enc+dec) | MEASURED-MET | af-25: 85.8/85.9 ns release-amortized; 84.0 ns per-call |
| G2b | <10 ns admission | UNSUPPORTED-CONTRADICTED | AF-2 row 13; af-35 first real measurement 2.1 ns timer-free — conservative but never measured as printed |
| G2b composed | — | MEASURED | af-35: 475.0 ns composed transit, 0-alloc census |
| G3 | 0 B/op | MEASURED-MET | ab-9, ab-19; af-35 alloc census 0 calls/0 bytes |
| G4 | p99 ≤10 µs | PARTIAL | af-26: 5,208 ns at 1M-scale MET; run 2 21,042 ns; the I3 cache-stride injection (§4.3) unrun — cannot close until run |
| G5 | Parity ≤50 ns | TARGET-UNMEASURED at RX-3 time | af-37 has since landed: parity p99 0.93-1.00 ≤ 1.5x — closes per AF-36's replacement law |
| G6 | Churn 0-leak | MEASURED-MET | ab-17: 200-cycle spawn/kill churn 0-leak; 8x500 fanout soak |
| G7 | ≤25 µs/link at 50k span | MEASURED-UNMET (exact) | ab-14: 50k span 1276.01 ms → 25.5202 µs/link — 0.5202 µs (2.08%) over; never rounded up |

NOTE (ZP-15): the runner's G1-G7 ≠ RFC §6's G1-G7 — different taxonomies. The
foreign-subject projection resolves the collision by namespacing (P1-P8 rows,
`projection_class ∈ {PROJECTABLE, ANALOGUE, NOT-PROJECTABLE}`, `attestation_class
∈ {SELF, ATTESTED, EXTERNAL}`), never by silently reusing gate IDs.

### 3.3 Adopted: cgap dod-001..012

Source: cgap `docs/jira/v26.11.3/dod-checklist.md` (all 12 rows MEASURED,
true). Adopt-and-extend: all 12 rows carry forward; zd-36 (DoD second pass)
re-ran dod-001..012, exit 0 on every row, and confirmed the dod-006
load-bound flake classification. Rows that wholesale replacement would have
silently dropped (ZB-70): dod-001 (arrow suite 242/0), dod-007 (riscv32imc
no_std check exit 0), dod-008 (wasm32 check exit 0), dod-009 (CBMQS 8/8
release gate), dod-010 (corridor join court 3/0, 100 iterations).

### 3.4 Proposed new gates (court-to-add; NOT yet adopted)

None of the gates in this subsection exists in the landed ladder. Each is a
proposal grounded in the ZB-70 map; each must land with its own court and
anti-vacuity legs before its status moves off PROPOSED.

| Proposed gate | Content | Grounding |
|---|---|---|
| Gate 09 (court-to-add) | bounded FD ≤16 + ΔRSS ≤2MB, per ZP-20: 3-run rerun decides in-place tighten (preferred — margins observed ~2 orders loose) vs new isolated court; "streams" → "requests" wording fix; quiet-machine poll via `pgrep -fl lsp-echo-server` (the `pgrep -f "cargo test"` pattern self-matches — zd-8 lesson) | ZP-20; zd-8-dod006-quiet.md |
| Gate 10 | **UNSUPPORTED at 25.5202 µs/link** (ab-14: 50k span 1276.01 ms / 50,000, 2.08% over the 25 µs threshold), pending the ZP-17 settling experiment. Fence the bench first (profile_guard + black_box in receipt_chain_scale.rs), min-of-7 release 50k re-measure; AB-14 receipt immutable (3 consumers parse it); all 3 §6 consumers move atomically in one diff. Branch (a) <25.0 → fence stands; (b) 25.0-25.5 → re-derivation receipt + TARGET_US_PER_LINK const move in the SAME diff + cert-court mutation base flips QUALIFIED→DISQUALIFIED; (c) >25.5 → UNSUPPORTED stands + optimization candidates (double-blake3 append fold, per-span pricing) | ZP-17; ab-14-chain-scale.md (ggen); RFC-0089 §6 G7 row |
> ERRATA 2026-11-04: this Gate-10 row is superseded by ZP-17's branch-(a)
> MEASURED-MET 1.26394 µs/link (fenced release min-of-7, POST-FOLD) —
> `cgap/docs/jira/v26.11.4/receipts/zp-17-gate10-settling.md`; v4 ledger
> `ggen/docs/jira/v26.11.4/receipts/v4-corrected-claims-ledger.md` row 5.
| CODEC-BUDGET | codec ceiling gate citing the ZB-70 map row (af-25 85.8/85.9 ns release-amortized as the base) | ZB-70 (session audit ledger); af-25 |
| SLAB-INGESTION | ArrowRecordSlab ingestion gate: slab is BENCH-ONLY for ingestion (ZB-64; CTQ-03 membrane 24 B/op NOT-MET receipted); 0 B/op scoped row per ZP-19 (TLS WindowBuffers for 6 alloc sites, Slab64 as metadata IDENTITY plane); G7-chain unfenced row stays explicitly open | ZB-64; ZP-19 |
| HEAP-CEILING | replaces the UNREACHABLE <850 ns composed heap-storm ceiling with a digest-floor-aware bound (af-37 root cause: ~4.5 µs digest floor) or scopes it to a digest-free decode+gate path | af-37; ZB-70 |
| TRANSPORT-UDS | UDS transport-floor gate (G4's transport floor + timer overhead observed at 21,042 ns run 2; the I3 cache-stride injection §4.3 must run before G4 closes) | af-26; RFC-0089 §6 G4 |
| PROCESS-CHURN | formalizes G6's 200-cycle spawn/kill churn as a standing gate (ab-17 pattern, bounded pgrep polling per zd-8) | ab-17; zd-8 |
| AUDIT-CHAIN | receipt-chain append gate at the landed surface: 456.3 ns chained (af-35), never the 167.0 ns bare-hash appendix row | af-35; af-22 (appendix); ab-7 |

ZB-68 (flip coverage): CBMQS 6/8, RFC-0089 3-4/8, DoD 0-2/12, marketplace
459/2,015 gate files (~23%) flip-witnessed. The wave's 100% flip objective is
the largest gap; each proposed gate must land with anti-vacuity legs (a court
that cannot flip is FAIL, a skip is loud).

## 4. Corrections ledger + falsifier sweeps

| ID | Correction | Was | Now | Receipts |
|---|---|---|---|---|
| C1 | Receipt figure re-surfaced | "receipt in 167.0 ns" | "chained BLAKE3 receipt 456.3 ns (AoqChain::append, ab-7)"; bare-hash 167.0 ns = labeled appendix row only (af-35 no-landed-surface ruling) | af-35, af-22, ab-7 |
| C2 | CUJ-3 ceiling replaced | "sub-10 µs ceiling" | honest af-37 I1-I5 matrix (§2 CUJ-3); CUJ-1's 167.0 ns corrected likewise | af-37, af-35 |
| C3 | Gate ladder uses real names | draft's unnamed/proposed ladder | CBMQS G1-G7 + G8 annotation, RFC-0089 §6 G1-G7, cgap dod-001..012 adopted; proposed gates separated into §3.4 | cbmqs-standard.md, rfc-0089-cscb-p99.md, dod-checklist.md, ZB-70 |
| C4 | Gate-09 court-to-add | Gate-09 as an existing gate | PROPOSED, court-to-add, per ZP-20 (3-run rerun decides tighten-vs-new; streams→requests; pgrep self-match fix) | ZP-20 (PLANS.md); zd-8 |
| C5 | ARD labeling nits | ambiguous ARD labels | digest-cover-span vs invariant-header distinguished; metadata-tail pad vs body-tail pad distinguished (ZB-41) | ZB-41 (session audit ledger) |
| C6 | Gate 10 standing | 25.5202 µs/link as a miss | UNSUPPORTED at 25.5202 µs/link pending the ZP-17 settling experiment (AB-14 receipt immutable; 3 consumers move atomically) | ab-14; ZP-17 (PLANS.md) |

### Falsifier sweeps (run at materialization, filters as shown)

Label law: a mention of a corrected figure is LABELED when it carries its
correction marker (surface label / CONTRADICTED marker / court-to-add marker).
PLANS.md hits are index text quoting the draft claims, not PRD claims — the
sweeps scope to PRD.md.

Sweep 1 — unlabeled "167.0" hits (labeled-only law; the sweep section's own
self-description lines are excluded by the `Sweep|grep -` filter):

```
$ grep -n '167\.0' docs/jira/v26.11.4/PRD.md | \
    grep -v 'appendix\|af-22\|bare\|CONTRADICTED\|CUJ-1\|Sweep\|grep -'
(no hits — every 167.0 mention carries the appendix/bare-hash/CONTRADICTED label)
```

Sweep 2 — unlabeled "sub-10" hits:

```
$ grep -ni 'sub-10' docs/jira/v26.11.4/PRD.md | \
    grep -v 'CONTRADICTED\|C2\|Sweep\|grep -'
(no hits)
```

Sweep 3 — unlabeled Gate-09 hits (every Gate-09 mention names it
court-to-add):

```
$ grep -n 'Gate-09\|Gate 09' docs/jira/v26.11.4/PRD.md | \
    grep -v 'court-to-add\|PROPOSED\|C4\|ZP-20\|Sweep\|grep -'
(no hits)
```

Falsifier discipline: each sweep must return zero unlabeled hits in PRD.md. A
non-zero sweep is a defect in this PRD — fix the prose, not the sweep.

## 5. Provenance (receipts re-read on disk 2026-11-04)

| Cite | Path (exists, verified on disk 2026-11-04) |
|---|---|
| ab-7 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/ab-7-chain-session.md |
| ab-12 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/ab-12-blake3-stress.md |
| ab-14 | /Users/sac/ggen/docs/jira/v26.11.3/receipts/ab-14-chain-scale.md |
| ab-17 | /Users/sac/lsp-max/docs/jira/v26.11.3/receipts/ab-17-churn-stress.md |
| ab-19 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/ab-19-slab-stress.md |
| af-10 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/af-10-cbmqs.md |
| af-22 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/af-22-percall-latency.md |
| af-25 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/af-25-g2-convergence.md |
| af-26 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/af-26-clearing.md |
| af-35 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/af-35-transit.md |
| af-37 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/af-37-p99-torture.md |
| zd-8 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/zd-8-dod006-quiet.md |
| zd-36 | /Users/sac/cgap/docs/jira/v26.11.3/receipts/zd-36-dod-final.md |
| ZB-40/41/48/61/64/67/68/70/73 | session audit ledger (34-audit Explore fan-out, ZB-40..74); index at docs/jira/v26.11.4/PLANS.md — no per-receipt files on disk |
