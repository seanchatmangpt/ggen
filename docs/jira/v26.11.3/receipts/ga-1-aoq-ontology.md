# GA-1: AOQ Canonical Ontology (PG-A6 landing step 1 / AOQ-01 semantic spec)

Standing: **ALIVE** (parsed + asserted this session).

## Subject

`/Users/sac/ggen-marketplace/packs/aoq-pack/ontology.ttl` (new file, canonical
AOQ graph, 271 triples).

## What landed

R-1 ruling encoded as wire law: AOQ1 header is **64 bytes**, magic
`0x414F_5131` ("AOQ1"), version **2**, FNV-1a32 digest over bytes
`[0x00,0x3C)` stored at `0x3C`. AOQ-01's 24-byte form is recorded only as a
superseded `prov:Entity` (`aoq:src-aoq-01`, `aoq:status "superseded"`) — it is
not wire law.

Classes: `Frame`, `Header`, `HeaderField`, `StatusConstant`, `KemParam`,
`Epoch`, `StreamType`.

AOQ-02 field table (full, each field carries offset / sizeBytes / wireKind /
cName / rustName / goName, plus a `aoq:nextOffset` contiguity chain):

| offset | size | kind | C | Rust | Go |
|---|---|---|---|---|---|
| 0x00 | 4 | u32 magic = 0x414F5131 | magic | MAGIC | Magic |
| 0x04 | 2 | u16 version = 2 | version | VERSION | Version |
| 0x06 | 2 | u16 epoch | epoch | EPOCH | Epoch |
| 0x08 | 8 | u64 key_id | key_id | KEY_ID | KeyID |
| 0x10 | 4 | u32 frame_ordinal | frame_ordinal | FRAME_ORDINAL | FrameOrdinal |
| 0x14 | 4 | u32 seq | seq | SEQ | Seq |
| 0x18 | 36 | reserved-zero | reserved | RESERVED | Reserved |
| 0x3C | 4 | fnv1a32 digest over [0x00,0x3C) | header_digest | HEADER_DIGEST | HeaderDigest |

Chain: magic → version → epoch → key_id → frame_ordinal → seq → reserved →
header_digest; first offset 0, last field ends exactly at 64.

StatusConstants `FM-AOQ-001..008` at `0x4347A001..A008` (frozen band; gate
070 refuses 0x4347_0001..0006). `KemParam` individuals carry
`aoq:seam "hand-law"` + `aoq:status "blocked"` — BLOCKED-class, never
projected. Closed labeled enums: `Epoch` (GENESIS 0, ROTATED_1, ROTATED_2,
RETIRED 65535), `StreamType` (DATA 0, CONTROL 1, EOS 2). `owl:Ontology` header
present; `prov:wasDerivedFrom` on every individual.

## Verification (real gate + actual output)

- rdflib parse: `parsed OK, 271 triples` (syntax-clean = the gate).
- Contiguity assert: every field offset == running cursor, chain ends at 64 —
  passed.
- Frozen band assert: StatusConstant values exactly
  `0x4347A001..0x4347A008`, no `0x4347 000x` member — passed.

## Assumptions (flagged, not silent)

- No AOQ-01/AOQ-02 document exists on disk anywhere searched (`~/ggen`,
  `~/ggen-marketplace`, `~/cgap`, `~/arcus`, `~/lsp-max`, repo-wide sweep) —
  the R-1 ruling in the lane prompt is the sole ground truth; field widths/
  offsets for epoch (u16 @0x06), key_id (u64 @0x08), frame_ordinal (u32
  @0x10), seq (u32 @0x14) and the 36-byte zero region are authored here as
  the canonical table consistent with the ruling's constraints (64B envelope,
  named fields, FNV over [0x00,0x3C), IPC1 prefix shape: magic u32 + two u16
  in [0x00,0x08), digest-at-tail discipline).
- Status constant semantic labels (OK/PENDING/BUSY/…) and enum member lists
  are authored; ids and values are the frozen pins.

## Standing vocabulary

ALIVE = syntax parse + structural asserts executed this session on the exact
subject. Not claimed: ggen sync against this ontology, gate 070 execution
(gate not in scope of this lane), template projection.

Receipt: GA-1, lane GGEN, wave v26.11.3. No git operations performed.
