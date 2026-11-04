# SEED

Next-wave frontier carried from v26.11.4 (compiled 2026-11-04 by the
IMPLEMENT lane per P-o's compile plan; prior format: v26.11.4 SEED).
Standing vocabulary per the receipt-cited-counts law: every status below
was re-read live on disk at compile time — from the cited registers
(`docs/jira/v26.11.4/closure/open-items-register.md`, `PLANS.md` at
3afe33b2d, `closure/next-wave-seeds.md`, the D-wave consolidated receipt,
and the named receipts in each repo) — not carried from the prior SEED.
Premise corrections found at compile are disclosed inline per row.

## A. Wave rows (SD-1..SD-17)

### H (2)

- SD-1 (H, court+crypto): ML-DSA-65 signer (ZP-22) — 5-phase build on the
  landed field kernel (fips203/fips202); risk mass = branchless
  SampleInBall/SampleMask, FS-abort sign loop, 2420-B serializer onto the
  frozen Receipt shape. Register row OI-3 PLANNED/wave-3
  (PLANS.md:112-118). Falsifier: no FIPS-204 witness in any tree;
  sign-loop court flips on a faulted candidate count.
- SD-2 (H, mount): kv7 mount — mount the smalt-posh azure kv7/armkv
  dialect surface (see smalt
  docs/jira/v26.10.31/gaps/07-doc-migration-ledger.md:113, kv7 subpackage
  row) into the wave's Azure-dialect consumer surface. Falsifier: the
  kv7 wire court does not flip on a mutated armkv response shape.

### M (8)

- SD-3 (M, decision): CBP home — coordinator names one home for the
  sealed-certificate surface: ZP-14's `crates/cbmqs-cli` `[[bin]]`
  member vs ZP-21's feature-gated `[[bin]]` inside cgap-aoq
  (OI-6, PLANS.md:87-88); the losing plan retires its surface.
  Falsifier: two cbmqs-audit homes ship in one tree.
- SD-4 (M, version): cgap version retarget — cgap workspace is STILL
  `26.11.3` (live read /Users/sac/cgap/Cargo.toml:51 this compile;
  OI-7 still OPEN) — retarget 26.11.3 → 26.11.5 + tier-DAG consistency
  pass. Promoted from v26.11.4 seed S-2. Falsifier: version skew breaks
  a tier job's `version` guard.
- SD-5 (M, closure): route-smoke closure — D61 LANDED since the seed
  draft: smalt commit 4f440446 (route-smoke court — 14 pages + 6
  handlers + SSE + mutation flip, workspace/scripts/route-smoke-court.mjs).
  Premise correction: the seed's open premise is closed; this row is now
  a re-verify/carry-forward row, not a build row.
- SD-6 (M): async-flip — carried from the session coordination ledger;
  NO on-disk referent found this compile (no commit, receipt, or
  register row under that name in ggen/cgap/arcus/smalt). Per the
  reconstruct-or-demote law: reconstruct in-writing at drain or demote.
  Premise UNVERIFIED at compile.
- SD-7 (M): persistent_term — same status as SD-6: session-ledger row,
  no on-disk referent found (Elixir persistent_term work not witnessed
  in any wave tree). Reconstruct at drain or demote.
- SD-8 (M, unblock): rung-7 stress/chaos — zd-79 still not on disk
  (OI-4; v26.11.3 zb-24-final-index.md:46-48 rows it NOT-on-disk);
  BLOCKED stands. Falsifier: a zd-79 receipt lands in
  docs/jira/v26.11.3/receipts/.
- SD-9 (M, receipt): ZP-19 receipt-write — write the ZP-19 standing
  row from its actual receipts: arcus
  docs/jira/v26.11.4/receipts/bm8-pre-alloc-baseline.md (9 allocs per
  warmed Seal, budget 9, pre-C4 baseline at 9661bbf) +
  br4-alloc-isolation.md. PREMISE CORRECTION vs PLANS.md@3afe33b2d:
  PLANS.md:22-23 says ZP-19 UNVERIFIED / "no receipt found" — both
  receipts exist on disk now; the PLANS.md row is stale; this SEED
  cites the receipts directly.
- SD-10 (M, parity): error-code parity — session-ledger row, no on-disk
  referent found this compile (nearest referent: cgap KD-1
  version-refusal status-word collision, OE-4). Reconstruct at drain
  or demote; premise UNVERIFIED.

### L (7) — carried from v26.11.4 closure/next-wave-seeds.md (S-1..S-8; S-2 promoted
### to SD-4, S-5 superseded by SD-9+SD-14)

- SD-11 = S-1: OI-1 follow-up — ggen tag-push publish rehearsal now
  that engine pins are `=26.11.4` (crates/ggen-engine/Cargo.toml:118,
  121); the zp-16 receipt's BLOCKED claim is stale (register
  correction). Falsifier: pre-fix-gate still refuses on a rehearsal.
- SD-12 = S-3: OI-6 dispute (same surface as SD-3; SD-3 is the
  decision, SD-12 the retire-the-loser execution leg).
- SD-13 = S-4: ZP-15 foreign-subject projection still UNVERIFIED
  (PLANS.md:15; no receipt in ggen/cgap/arcus trees). Falsifier:
  projection court renders P1-P8 rows with `projection_class` on a
  real subject.
- SD-14 = S-5: ZP-19 integration leg (slab/TLS WindowBuffer) — receipt
  exists now (see SD-9); remaining leg is the 0 B/op CTQ-03-sidecar
  row flipping on a real soak (G7-chain unfenced row, PLANS.md:137).
- SD-15 = S-6: ZB-65 ubuntu half — verify_v26.10.31.sh:80 BSD
  `sed -i ''` still open (PLANS.md:171-174). Falsifier: verify script
  exits 0 on ubuntu runner.
- SD-16 = S-7: E-2 mechanicalize self-host observation re-pin gate
  bound to the observation commit, not literal HEAD (v26.11.4
  SEED.md:101-106; ZB-21 C21-class binding). Falsifier: gate flips on
  an observation-commit-shifted replay.
- SD-17 = S-8: MKT-04 pack-side standing re-verify at the v26.11.3
  tag, gated on CARRY-19 (v26.11.4 SEED.md:93-94). Falsifier: a
  qualified pack fails at-tag re-verify.

## D. Dropped-at-compile rows (4) — closed since the seed draft, carried as citations

- D1. Column drift (D55/D59/D60): LANDED — smalt column-law waves 1-2:
  e027dbaf (bore_dev_um → bore_deviation_um, M23/M25 + twins,
  re-sealed) + dd713577 (µm singles, M27/31/37/40, re-sealed).
  Disclosure: the D55/D59/D60 lane IDs are not git-reachable in any
  checked repo (the D-wave consolidated receipt already rows smalt
  D55/D60 as "loudly missing" at receipt time); the on-disk landing
  commits are the two column-law commits — lane attribution stays with
  the coordinator's lane map, not disk.
- D2. Pin-table (Q6 M2b): LANDED — cgap fbc0d9f3 (peer identity
  pin-table, P-f) + arcus 9fec512 (AoQ pin-table + persisted pin
  store); both verified on-origin in
  docs/jira/v26.11.4/receipts/d-wave-consolidated-receipt.md.
- D3. BEAM gate: LANDED — cgap 74733e28 (ci(beam): pin OTP 28, drop
  continue-on-error, loud skip; on-origin per the consolidated
  receipt).
- D4. KD registers: LANDED — ggen root KNOWN_DIVERGENCES.md mint
  ab4104e99 (XC-8/ZD-38, R13) + smalt posh KD staleness pass 53193fac
  (KD-09 flip, KD-08 amendment, KD-05 dialect split); index:
  docs/jira/v26.11.4/closure/kd-register-index.md.

## F. PRD omissions (carried loudly — the PRD's own gap)

SD-6, SD-7, SD-10 (the three session-ledger rows with no on-disk
referent), SD-8 (rung-7 unblock — no PRD phase rows it), and SD-16
(E-2 mechanicalize — scheduled by no PRD phase) are carried by this
SEED explicitly; the v26.11.5 PRD must adopt, reconstruct, or demote
each. The v26.11.5 PRD does not exist yet; until it lands, this
directory carries no plan and no receipts — only this SEED.

## G. Drain-time verification

Not filled at compile (no closing receipt is minted in this lane).
At drain: re-read every carried status above against disk (register
rows, PLANS.md, and each cited receipt body — never from memory),
flip any row whose premise moved, and only then fill the wave-closing
receipt. Rows SD-6/SD-7/SD-10 MUST be reconstructed in-writing at
drain or demoted, per the reconstruct-or-demote law.
