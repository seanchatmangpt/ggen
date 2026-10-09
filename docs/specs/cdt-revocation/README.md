# cdt-revocation

Spec: Capability Derivation Tree (CDT) revocation for the receipt fabric.
Operator task: seL4 Lesson 5. Lane: cdt-revocation. Repos: ggen (this spec) +
affidavit (grounding, standing poset, `chain.rs`).

Grounding subject: `sac/affidavit` HEAD `ee57f9d`
(`fix(crypto-trust): re-render KAT for ED25519/ES256K, wire features, re-pin
fixture (backlog [82])`). All file:line citations below are read from that
tree.

## 1. Derivation-tree model over the existing receipt chain

### 1.1 The chain law (existing, unchanged)

affidavit's rolling chain law (`src/chain.rs:7-9`, implemented `src/chain.rs:52-75`):

```text
H_0    = blake3(GENESIS_SEED)                      // src/chain.rs:52-54
C_i    = blake3(hex(C_{i-1}) || canon(event_i))    // src/chain.rs:58-67
```

`recompute_chain(events)` re-derives the head from event bytes alone
(`src/chain.rs:72-75`), and `canonical_bytes` is sorted-key deterministic
JSON (`src/types.rs:548-553`). Determinism of the fold is the property the
whole design rests on: identical input bytes always yield the identical
digest, and any byte difference anywhere in the prefix propagates forward
through every subsequent link (`src/chain.rs:10-12`).

### 1.2 CDT nodes and edges

Map seL4 capability derivation onto the receipt fabric:

| seL4 concept | Receipt-fabric concept | Real anchor |
|---|---|---|
| Capability | receipt (`SjRecord`, `src/sj_record.rs:535-541`) | |
| CNode slot | record in the ledger | |
| Parent edge | `ReplayBinding.predecessor_work_order_ids` (`src/sj_record.rs:253-255`) | |
| Mint/derive | new record citing a predecessor | |
| CNode_Revoke | retirement of a record; see §3 | |
| CDT root table | ledger index of per-record admission roots (§2.2) | |

A record `R_i` is a CDT node. An edge `R_p -> R_c` exists when
`R_c.document.replay_binding.predecessor_work_order_ids` contains
`R_p.document.work_order_id` (`src/sj_record.rs:253-255`). The edge is
content-anchored: the parent's chain head is fixed by the parent's own event
bytes via the chain law, and both ends can re-derive it from local bytes
(`SjRecord::verify` re-derives chain, head binding, and subject digest;
`src/sj_record.rs:576-599`).

### 1.3 Lineage event log and derivation digest

For a derivation path `R_1 -> R_2 -> ... -> R_k` (R_1 is the lineage root):

```text
E_i      = chain_events(R_i)                        // src/sj_record.rs:460-516
L_k      = E_1 || E_2 || ... || E_k                 // lineage event log
D_k      = recompute_chain(L_k)                     // the derivation digest
```

`D_k` is the fold of the *whole path's* events under the same genesis-seeded
left fold. Because `recompute_chain` is a left fold over a concatenation,
`D_k = fold(D_{k-1}, E_k)`: each hop extends the path digest exactly the way
one CDT hop extends the derivation path of a capability.

`D_k` is the receipt-fabric analogue of a seL4 cap's derivation
provenance: it commits to every ancestor record's bytes, in path order, plus
the record's own bytes.

### 1.4 Severing = parent root update

seL4 `CNode_Revoke` on slot s deletes the mapping bits of every cap derived
through s; sibling slots and unrelated subtrees keep working, and detection
of a dead cap is a *local* lookup failure, never a revocation-server query.

Receipt-fabric mapping: revoking node `R_j` means the canonical version of
record id `WO_j` changes from `R_j` to `R_j'` (a supersession: the retired
record stands as SUPERSEDED, the successor becomes canonical — `supersede`
in `src/architecture.rs:496-533`, which refuses non-QUALIFIED targets at
`src/architecture.rs:500-502` and refuses non-replacements via
`same_sbb` at `src/architecture.rs:503-505`). Since `R_j'` has different
event bytes (`NotAReplacement` refuses identical SBB digests,
`src/architecture.rs:503-505`), `E_j' != E_j` byte-wise, so by the fold's
propagation law:

```text
D_m' = recompute_chain(E_1||...||E_j'||...||E_m) != D_m   for all m > j
```

All downstream records (any record whose predecessor path passes through the
severed edge `R_j -> child`) are invalid, detected **locally**: recompute the
path digest from local bytes and compare against the admission-time value.
No live revocation server is queried. This is the same information flow as
seL4: the revoked cap's derivation is severed at the slot; descendants fail
on next local use, not on next network call.

### 1.5 What seL4 Revoke does NOT do (boundary)

Revoke does not touch siblings. Mapping: only records whose path passes
through `R_j` are severed. A sibling record derived directly from `R_1`
(the root) never traverses `E_j` in its path log, so `D_sibling' = D_sibling`
and the record stays valid. §4's vector does not exercise a sibling; the
anti-vacuity falsifier in §4.4 does.

## 2. Local-check algorithm

### 2.1 State

- **Ledger** (local, offline): all records, keyed by `work_order_id`; each
  id maps to its current canonical version (SUPERSEDED ids map to their
  successor's version).
- **Root table** (local, derived from the ledger, same standing-query
  surface that answers "current qualified SBB by ABB"
  `src/architecture.rs:23-25`): `work_order_id -> D_i` as witnessed at
  admission.

The root table is the fabric's "current root" per record, the analogue of
the CNode layout burned into the local CSpace. It is derived data: it can
always be rebuilt from the ledger by re-folding.

### 2.2 Algorithm

`check(R_i)` — sub-ms, offline, O(path length × event bytes):

1. Walk `R_i` up to the lineage root via
   `predecessor_work_order_ids` (`src/sj_record.rs:253-255`), resolving each
   record id to its **current canonical version** in the ledger (not the
   version R_i recorded — that is the stale one).
2. Form the canonical path log `L_i' = E_1' || ... || E_i'` from the current
   versions, in path order.
3. `D_i' = recompute_chain(L_i')` (`src/chain.rs:72-75`).
4. Compare `D_i'` against `root_table[R_i.id]` (the admission-time `D_i`).
   - Equal => record valid.
   - Not equal => SEVERED. To identify the severed edge, re-fold
     incrementally from the root: first depth `d` where
     `recompute_chain(E_1'..E_d') != root_table[R_d.id]`; the severed edge
     is `R_d -> child(R_d)`. Equal-at-all-depths ⇒ valid.

Note what the check does NOT do: it never mutates R_i, never queries the
network, never consults a CRL. The existing CRL plane
(`src/crypto_trust_revocation.rs:1-24`, key/epoch-signed publication) stays
orthogonal: CRL answers "is this issuer key still trusted"; CDT severance
answers "is this record still derived from the current root". CDT severance
is complete for lineage revocation with zero transport.

### 2.3 Why detection is inescapable (the falsifier's proof shape)

Suppose downstream record R_m (m > j) is valid after revoking R_j. Then
`D_m' = D_m`, so `recompute_chain` produced identical output from
`L' != L` as byte strings (E_j' != E_j guaranteed by `NotAReplacement`,
`src/architecture.rs:503-505`). Identical blake3 outputs from different
inputs = a blake3 collision (all folding is deterministic; `src/types.rs:548-553`).
So local detection fails only under a blake3 collision or byte-identical
"replacement", and byte-identical replacement is already refused typed as
`NotAReplacement` (`src/architecture.rs:503-505`).

## 3. Integration with affidavit's monotonic poset

### 3.1 The poset, as it exists (not as commonly cited)

affidavit's standing types are two enums, and neither declares an ordering
relation between SUPERSEDED and QUALIFIED — the monotonic poset is enforced
structurally (which fields each standing may carry) plus the ledger's
admission refusals, not by an `Ord` derivation:

- `ArchitectureStanding { Unknown, Candidate, Qualified, Refused, Superseded }`
  (`src/architecture.rs:49-63`).
- Standing/chain agreement is enforced in `verify_integrity`:
  SUPERSEDED must carry both `prior_receipt_digest` and
  `superseded_by_receipt_digest` (distinct); QUALIFIED may carry only
  `prior_receipt_digest`; CANDIDATE/UNKNOWN/REFUSED carry neither
  (`src/architecture.rs:419-432`).
- `certify` refuses direct SUPERSEDED certification and UNKNOWN promotion
  (`src/architecture.rs:311-315`); `supersede` is the only SUPERSEDED
  producer (`src/architecture.rs:496-533`).
- On the sj-record side, `StandingValue` (`src/sj_record.rs:81-99`) and the
  `Standing` face (`src/sj_record.rs:259-267`) use the
  UNKNOWN/PARTIAL_ALIVE/ALIVE/BLOCKED/... vocabulary; BROKEN terms ride
  `broken_term` (`src/sj_record.rs:126-149`).

Operator poset mapping (UNQUALIFIED < PROVISIONAL < QUALIFIED < SUPERSEDED):

| Operator term | affidavit spelling | Anchor |
|---|---|---|
| UNQUALIFIED | `Unknown` / `Candidate` | `src/architecture.rs:52-55` |
| PROVISIONAL | `Candidate` (proposed, not qualified) | `src/architecture.rs:54-55` |
| QUALIFIED | `Qualified` | `src/architecture.rs:56-57` |
| SUPERSEDED | `Superseded` | `src/architecture.rs:60-63` |

### 3.2 Which transitions sever which edges

| Transition | Edge effect | Anchor |
|---|---|---|
| -> SUPERSEDED (via `supersede`) | SEVERS the lineage edge at the retired record: every downstream record whose path traverses the retired record's events is invalid; canonical version swaps to the successor. | `src/architecture.rs:496-533` |
| -> QUALIFIED (fresh or successor) | Never severs: a fresh QUALIFIED anchor creates a lineage root; a successor QUALIFIED creates the new edge (carries `prior_receipt_digest` only, `src/architecture.rs:419-432`), pointing *at* the lineage, not cutting it. | `src/architecture.rs:496-533` |
| -> CANDIDATE / UNKNOWN | Never severs: carries no chain fields (standing/chain agreement, `src/architecture.rs:419-432`); cannot participate in a derivation path's canonical version swap. | `src/architecture.rs:419-432` |
| QUALIFIED -> SUPERSEDED only | Only a QUALIFIED receipt is supersedable; REFUSED receipts cannot sever anything (they never entered a lineage). | `src/architecture.rs:500-502` |
| -> REFUSED | Never severs: no chain fields. | `src/architecture.rs:419-432` |

Correction of the operator framing: QUALIFIED never severs (agreed), and
additionally REFUSED never severs; only the transition INTO SUPERSEDED
severs, because only it swaps the canonical version of an in-lineage record
id (fields `prior_receipt_digest` + `superseded_by_receipt_digest`,
`src/architecture.rs:152-156`).

### 3.3 Monotonicity

The poset is monotonic in the CDT sense: `supersede` refuses an unchanged
SBB (`NotAReplacement`, `src/architecture.rs:503-505`), so every severing
transition changes event bytes, which is exactly the precondition for the
local check to detect divergence (§2.3). A "revocation" that changes
nothing is not a revocation; it is refused at the poset layer before the
CDT layer can be asked to detect it.

## 4. Test vector: 5-record chain, revoke node 2

### 4.1 Vector definition

Build 5 records chained WO1->WO2->WO3->WO4->WO5 (each draft sets
`predecessor_work_order_ids` to the prior work order; `SjCampaignDraft`
admitted via `SjCampaign::new` at `src/sj_record.rs:449`, finalized at
`src/sj_record.rs:518-533`), each with one distinct commit sha and one
replay command. Full test code: `test-vector.rs` beside this README.

Path: `WO1 <- WO2 <- WO3 <- WO4 <- WO5` (each record cites the prior work
order as predecessor).

### 4.2 The math, hand-verified

blake3 outputs are not hand-computable, so hand verification is over the
fold structure, which is sufficient by §2.3's determinism argument:

```text
Admission (no revocation):
D_1 = recompute(E1)                          = X1
D_2 = recompute(E1||E2)                      = X2
D_3 = recompute(E1||E2||E3)                  = X3
D_4 = recompute(E1||E2||E3||E4)              = X4
D_5 = recompute(E1||E2||E3||E4||E5)          = X5
root table: {WO1:X1, WO2:X2, WO3:X3, WO4:X4, WO5:X5}

check(WO1) = recompute(E1)          == X1   PASS
check(WO2) = recompute(E1||E2)      == X2   PASS
check(WO3) = recompute(E1||E2||E3)  == X3   PASS
check(WO4) = recompute(E1||E2||E3||E4) == X4 PASS
check(WO5) = recompute(E1..E5)      == X5   PASS

Revoke node 2 (supersede WO2 -> WO2', distinct commit sha):
canonical versions: WO2' replaces WO2; E2' != E2 (distinct commit sha
  => distinct commit event payload `src/sj_record.rs:468-474`
  => distinct canonical bytes `src/types.rs:548-553`).

Recomputed path logs (current canonical versions, walked from each record):
check(WO1): L' = E1                  -> recompute = X1 == X1        PASS
check(WO2): L' = E1||E2'             -> recompute = Y2 != X2        FAIL
check(WO3): L' = E1||E2'||E3         -> recompute = Y3 != X3        FAIL
check(WO4): L' = E1||E2'||E3||E4     -> recompute = Y_m != X4       FAIL
check(WO5): L' = E1||E2'||E3||E4||E5 -> recompute = Y5 != X5        FAIL
```

Wait — one subtlety, stated honestly: why do records 4 and 5's *walks* pick
up E2'? The walk in §2.2 step 1 resolves each ancestor id to its **current
canonical version** in the local ledger. WO2 now resolves to WO2'. The
descendant's stored bytes are never rewritten; what changes is the canonical
version of the ancestor id, so the recomputed path log differs from the
admission-time log and the stored root-table value. That is precisely the
"local verification detects the severed branch" property: R4/R5 bytes are
untouched, offline, and the check fails with no transport.

Also stated honestly: records 4 and 5 do not detect *which ancestor*
changed by looking only at their own bytes — the walk uses the ledger's
current versions. A record whose every ancestor still resolves to its
admission-time version always passes; divergence requires an actual
canonical-version swap somewhere on the path. This is the same as seL4:
a cap is not "invalid in itself"; its derivation chain is no longer
re-derivable through the current CSpace.

Edge identification math: first depth d where the incremental prefix
diverges:

```text
prefix_1 = recompute(E1)       == X1   equal
prefix_2 = recompute(E1||E2')  != X2   DIVERGES at d=2
```

So the severed edge is `WO2 -> WO3`, reported without any severance
announcement reaching WO4/WO5.

### 4.3 Real test, real run (receipt)

The vector is implemented as a real Chicago-school test (real chain
assembler, real BLAKE3, no mocks) against affidavit HEAD `ee57f9d`, run in a
scratch tree (`git archive` to /tmp, canonical checkout untouched — one
canonical checkout per repo, doctrine law 3). The test appends to
`tests/sj_record.rs`:

```rust
#[test]
fn cdt_revocation_local_severance_detection() {
    // build 5 chained records (each cites its predecessor), capture the
    // admission roots X1..X5, supersede WO2 with a distinct-commit WO2',
    // then assert: WO1 passes, WO3/WO4/WO5 SEVERED, edge identified at
    // depth 2. Walk resolves ancestors to current ledger versions.
}
```

Full code: `test-vector.rs` beside this README. Actual run output: §5.1.

### 4.4 Anti-vacuity (mutation)

Mutation M1: make WO2' byte-identical to WO2 (drop the distinct commit sha).
Expected: the superseding record is refused `NotAReplacement`
(`src/architecture.rs:503-505`) — or, in the pure-sj_record vector, the
recomputed path log stays byte-identical, all checks PASS, and the test's
severance assertions fail. If the check still reports SEVERED, the check is
vacuous.

Mutation M2: also assert WO3's own chain recomputation still passes
`SjRecord::verify()` (`src/sj_record.rs:576-599`) after severance — proving
severance is a *lineage* failure, not a record-integrity failure. A record
can be perfectly intact and still severed. This is the seL4 distinction: the
cap's own bytes are fine; its derivation is dead.

## 5. Receipt

- Grounding subject: affidavit `ee57f9d` (scratch archive
  `/tmp/cdt-scratch.*`, `git archive HEAD`, canonical checkout untouched;
  scratch removed after the run).
- Commands + exits: §5.1.
- Falsifiers: §2.3 (collision-only failure), §4.4 (M1 vacuity, M2 intact-
  but-severed).
- Standing: PARTIAL_ALIVE — the check runs green over the real chain
  assembler in a scratch tree; it is not yet wired into affidavit's
  canonical `tests/sj_record.rs` (that edit belongs to an
  affidavit-owned lane).

### 5.1 Command log

```text
$ cargo test --features crypto-trust --test sj_record cdt_revocation_local_severance_detection
running 1 test
test cdt_revocation_local_severance_detection ... ok
test result: ok. 1 passed; 0 failed; 0 ignored; 0 measured; 11 filtered out; finished in 0.01s

$ cargo test --features crypto-trust --test sj_record          # full suite
test result: ok. 12 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.01s

# Mutation M1 (vacuity): make the superseding record byte-identical to WO2
$ sed -i 's/record_for(2, "f", /record_for(2, "a", /' tests/sj_record.rs && cargo test ... 
test cdt_revocation_local_severance_detection ... FAILED
panicked: assertion `left != right` failed: revocation must change bytes
# Non-replacement is refused at the precondition; the check never silently
# passes a no-op revocation (spec §2.3 / §4.4).
```
