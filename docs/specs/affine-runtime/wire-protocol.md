# Affine Runtime Wire Protocol (ARW/1) — Normative Spec

Status: NORMATIVE
Version: 1.0 (2026-10-08)
Grounding: ggen @ fcfd6349 (branch `feat/integrate-graphlaw-engine`), affidavit @ ee57f9d, bcinr @ 3fe4ba75, ex4pm, ash_a2a, wasm4pm

## 0. Scope and Grounding

ARW/1 is the closed-domain wire packet format for the four-language affine
runtime (Rust/WASM, BEAM/AtomVM, Node/wasm-bindgen, JCS-JSON control plane).
It is grounded in four real transports:

| Grounding transport | Artifact | Anchor |
|---|---|---|
| WASM kernel (Rust host) | `praxis_graphlaw.wasm` | ggen `crates/praxis-graphlaw/src/parser/mod.rs:307` names the WASM target; ash_a2a `lib/ash_a2a/sa2a/graphlaw.ex:60-70` (`wasm_path/1`) pins `priv/graphlaw/praxis_graphlaw.wasm` via `MANIFEST.json` |
| BEAM to WASM (Elixir host) | `Wasmex` transport | ex4pm `lib/ex4pm_engine/wasm/real_transport.ex:98-111` — digest-pinned `Admission.admit/2` then `Wasmex.Store.new(nil, admitted.engine)` |
| Node subprocess (wasm-bindgen ABI) | hand-implemented ABI driver | ash_a2a `lib/ash_a2a/sa2a/graphlaw.ex:20-24` (moduledoc): driver instantiates the wasm directly, implements the wasm-bindgen ABI by hand |
| Record envelope | sj_record JSON wire form | affidavit `src/sj_record.rs:549` (`SjRecordWire`, flattened) with `from_json` at `:612` |

## 1. Design Law

1. **Closed-domain, Type-3 grammar.** Every ARW/1 packet is a byte sequence
   accepted by a single regular grammar (Chomsky Type-3), decodable in one
   left-to-right pass of a fixed DFA. No recursion, no backtracking, no
   nested structure on the wire. Nested content is opaque length-prefixed
   payload; the DFA never descends into it.
   The DFA kernel is the bcinr branchless DFA primitive
   (`/Users/sac/bcinr/crates/bcinr-logic/src/dfa.rs:54` `dfa_advance`,
   `:88` `dfa_run`, `:116` `dfa_is_accepting`): flat table
   `table[state * alphabet_size + input]`, bounds masked branchlessly,
   ill-formed input degrades to the sink state — never a panic
   (dfa.rs module doc, lines 1-10, "CC=1 ... deterministic latency").
2. **O(n) parse, O(1) allocations.** One byte pass. The only allocation is
   the payload buffer, sized once at the BODY_LEN field boundary. Per-byte
   cost is one table load plus one mask; no conditional jumps.
3. **Closed scalar domain.** The only scalars on the wire are: closed enums
   (u8, values defined in this spec), u64 little-endian, and 32-byte hashes.
   The only hash algorithm is BLAKE3 truncated to 32 bytes
   (affidavit `src/architecture.rs:227` `blake3_digest`; affidavit-core
   `src/digest.rs:15` `pub struct Digest(pub [u8; 32])`). Binary sections
   never carry strings; identity strings live in the JSON envelope outside
   the packet, per the sj_record document/wire split (affidavit
   `src/sj_record.rs:271` `SjRecordDocument`).
4. **Refusal, not panic, not silent truncate.** Any byte sequence the DFA
   steers to the sink state is refused with a typed code (Section 6). This
   mirrors the affidavit law that a tampered base "cannot become a record"
   (`src/sj_record.rs:352` `SjRefusal::ChainTamper`) and the ex4pm law that a
   nil digest pin is refused `:wasm_digest_unpinned`
   (`lib/ex4pm_engine/wasm/real_transport.ex:84-89`).
5. **Digest-pinned transport.** A packet stream is always paired with an
   artifact pin (SHA-256 of the wasm artifact; ex4pm
   `real_transport.ex:83-96` with `:wasm_digest_unpinned` refusal; MANIFEST
   pin: ash_a2a `priv/graphlaw/MANIFEST.json` and `WASMEX_HOST_MANIFEST.json`).
   The wire format is transport-independent; the pin is per-transport, not
   per-packet.

## 2. Packet Layout (fixed offsets)

All integers little-endian. Header is exactly 128 bytes, followed by one
opaque payload. There is no other framing on the wire.

```
offset  size  field
0       4     MAGIC        0x41 0x52 0x57 0x31 ("ARW1")
4       1     VERSION      0x01
5       1     PKT_TYPE     enum (Section 3.1)
6       1     TIER         enum (Section 3.2)
7       1     FLAGS        bit 0: payload is a BLAKE3 preimage; bits 1..7 reserved, must be 0
8       8     BODY_LEN     u64, payload length, 0..=MAX_BODY
16      8     SEQ          u64, monotone per-stream sequence
24      8     FUEL         u64 fuel budget for the handler, 0..=FUEL_CEILING
32      32    DOMAIN_ID    BLAKE3-32 of the domain tag string
                           (e.g. BLAKE3("affidavit-campaign/v1"); domain law:
                           affidavit src/sj_record.rs:39 CAMPAIGN_DOMAIN)
64      32    CHAIN_PREV   BLAKE3-32 of the previous packet in the stream
                           (all-zero for the first packet)
96      32    BODY_HASH    BLAKE3-32 over the payload bytes
128     N     PAYLOAD      opaque, exactly BODY_LEN bytes
```

MAX_BODY = 1_048_576 (1 MiB), same order as the wasm4pm boundary ceiling
(`/Users/sac/wasm4pm/src/boundary_generated.rs:2`
`pub const CEILING: u64 = 1000000;`).

Fixed offsets are deliberate: the header has zero variable-length fields, so
header parse is exactly 128 byte-steps on every packet regardless of content
(deterministic latency, bcinr dfa.rs:4-7).

## 3. Closed Enums

### 3.1 PKT_TYPE

| value | name | payload |
|---|---|---|
| 0x01 | HELLO | empty (BODY_LEN = 0) |
| 0x02 | ACK | empty |
| 0x03 | DATA | opaque; BODY_HASH must verify after parse |
| 0x04 | EVENT | JCS JSON event body (affidavit `src/sj_record.rs:555` canonical_json law) |
| 0x05 | SEAL | empty; closes a stream (CHAIN_PREV + BODY_HASH over zero-length body) |
| 0x06 | ERROR | 8-byte error code (Section 6), little-endian |
| 0x07 | FUEL_EXHAUSTED | 8-byte consumed-fuel count, little-endian |
| 0x08..0xFF | — | refused at parse time (closed domain) |

### 3.2 TIER

Carries the two-tier attestation partition (attestation.md Section 2).

| value | name | meaning |
|---|---|---|
| 0x00 | TIER_NONE | unattested transport packet |
| 0x01 | TIER1_ED25519 | Tier-1 classical signature over BODY_HASH |
| 0x02 | TIER2_MLDSA | Tier-2 post-quantum signature over BODY_HASH |
| 0x03 | TIER_DUAL | both halves; hybrid composition law per affidavit `src/crypto_trust_pqc.rs:201` |
| 0x04..0xFF | — | refused E_BAD_TIER (0xE1A2) |

## 4. The DFA

### 4.1 Alphabet: byte classes (position-derived, closed)

The 128 header bytes are partitioned into classes by offset; payload bytes
form one class. Class of a byte = function of its position, not of value
matching against a string.

- p in [0,4): C_MAGIC (each byte must equal MAGIC[i])
- p = 4: C_VERSION (0x01)
- p = 5: C_PKT_TYPE (0x01..0x07)
- p = 6: C_TIER (0x00..0x03)
- p = 7: C_FLAGS (bit 0 free; bits 1..7 = 0)
- p in [8,16): C_LEN (bytes free; boundary predicate at p = 15)
- p in [16,24): C_SEQ (free)
- p in [24,32): C_FUEL (bytes free; boundary predicate at p = 31)
- p in [32,128): C_ANY (free; DOMAIN_ID, CHAIN_PREV, BODY_HASH)
- p >= 128: C_PAYLOAD while payload budget k < BODY_LEN

### 4.2 States

| state | meaning |
|---|---|
| 0 | SINK (refusal) |
| 1..7 | collecting MAGIC[i], VERSION, PKT_TYPE, TIER, FLAGS, BODY_LEN, SEQ+hash span (see 4.3 granularity) |
| 8 | payload sweep |
| 9 | ACCEPT |

The implementation may collapse consecutive free-field positions into a
single state with a step counter, provided the observable behavior — accepted
set, refusal codes, byte-step count — is identical.

### 4.3 Boundary predicates (one per field boundary, branchless)

At each field boundary the accumulated value is tested with
comparison-select primitives, not branches — the bcinr kernels
`ct_lt_u32` (`ct.rs:161`), `ct_select_u64` (`ct.rs:71`),
`ct_eq_u64` (`ct.rs:142`):

- BODY_LEN <= MAX_BODY, else SINK with E_BAD_LEN (0xE2A0)
- FUEL <= FUEL_CEILING (1_000_000, wasm4pm boundary_generated.rs:2), else SINK with E_BAD_FUEL (0xE2A2)
- HELLO / ACK / SEAL imply BODY_LEN = 0, else SINK with E_BAD_LEN
- TIER_DUAL implies FLAGS.bit0 = 1, else SINK with E_BAD_FLAGS (0xE2A3)

Sink transitions are data-selected (`ct_select_u64(sink, next, violation)`),
so transition targets are values, not branches.

### 4.4 Transition table (normative)

`i` = index of the next MAGIC byte (0..3). `k` = payload bytes consumed.

| state | class | next | action |
|---|---|---|---|
| S_MAGIC | C_MAGIC | (i<3) S_MAGIC(i+1); (i=3) S_VER | none |
| S_MAGIC | other | SINK | E_BAD_MAGIC (0xE1A0) |
| S_VER | C_VERSION | S_TYPE | none |
| S_VER | other | SINK | E_BAD_VERSION (0xE1A4) |
| S_TYPE | C_PKT_TYPE | S_TIER | latch pkt_type |
| S_TYPE | other | SINK | E_BAD_PKT_TYPE (0xE1A1) |
| S_TIER | C_TIER | S_FLAGS | latch tier |
| S_TIER | other | SINK | E_BAD_TIER (0xE1A2) |
| S_FLAGS | C_FLAGS | S_LEN | none |
| S_FLAGS | other | SINK | E_BAD_FLAGS (0xE2A3) |
| S_LEN | C_LEN (7 bytes) | S_LEN | assemble u64 LE |
| S_LEN | C_LEN (8th byte) | S_SEQ | boundary: BODY_LEN <= MAX_BODY else SINK E_BAD_LEN (0xE2A0); allocate payload buffer once here |
| S_LEN | other | SINK | E_BAD_LEN (0xE2A0) |
| S_SEQ | C_SEQ (8 bytes) | S_FUEL | assemble u64 LE |
| S_SEQ | other | SINK | E_BAD_LEN-class (0xE2A0) |
| S_FUEL | C_FUEL (7 bytes) | S_FUEL | assemble u64 LE |
| S_FUEL | C_FUEL (8th byte) | S_HASHES | boundary: FUEL <= 1_000_000 else SINK E_BAD_FUEL (0xE2A2) |
| S_FUEL | other | SINK | E_BAD_FUEL (0xE2A2) |
| S_HASHES | C_ANY (96 bytes) | S_PAYLOAD | DOMAIN_ID, CHAIN_PREV, BODY_HASH latched; BODY_HASH is verified cryptographically after parse, not by the DFA |
| S_PAYLOAD | C_PAYLOAD | S_PAYLOAD while k < BODY_LEN | copy payload byte |
| S_PAYLOAD | C_PAYLOAD at k = BODY_LEN | S_ACCEPT | payload complete |
| S_ACCEPT | any byte | SINK | E_OVERLONG (0xE2A1): trailing bytes after BODY_LEN are a refusal, never a silent truncate |

State S_ACCEPT (9) is the only accepting state
(`dfa_is_accepting(state, &[9])`, bcinr dfa.rs:116).

### 4.5 Stream-level checks (after frame acceptance)

The DFA accepts a frame; the stream layer then enforces:

- CHAIN_PREV = BLAKE3-32(previous accepted packet), else E_CHAIN_BREAK (0xE1A8) — the journal chain law (affidavit `src/crypto_trust_journal.rs:12-19` entry_hash, `:130` `ChainBroken`)
- SEQ = previous SEQ + 1, else E_SEQ_GAP (0xE1A9) — the journal `SeqGap` law (crypto_trust_journal.rs:119)
- BODY_HASH = BLAKE3-32(payload), else E_BODY_HASH (0xE1AA)
- DOMAIN_ID must match the handler's admitted domain, else E_DOMAIN (0xE1AB)

## 5. Capability-Passing Interface Contract

The ARW/1 packet is the only argument surface. Capabilities are passed as
admitted imports, never ambient.

1. **Import allowlist admission precedes instantiation.** The ex4pm law
   (real_transport.ex:85-89, verbatim: "Instantiation imports are stubs
   derived from the SAME allowlist admission judged, so allowlist and stubs
   cannot drift"). A module whose imports exceed the allowlist is refused
   before `Wasmex.Store.new/2` (real_transport.ex:101).
2. **No ambient authority.** The handler sees no clock, filesystem, or env.
   Time enters as a u64 field or as an explicitly allowlisted import. This
   is the host-agnostic runtime boundary law (ash_a2a graphlaw.ex:25-28) and
   the affidavit law that `confers_do_authority` is always false
   (architecture.rs:163).
3. **Monotonic fuel.** FUEL is a header u64 bounded by FUEL_CEILING
   (wasm4pm boundary_generated.rs:2). Fuel is monotone non-increasing within
   a packet; no refuel within a packet. Exhaustion emits PKT_TYPE
   FUEL_EXHAUSTED with the consumed count. Metering is the Wasmtime
   store-level mechanism (ex4pm real_transport.ex:45).
4. **Share-nothing confinement (BEAM/AtomVM).** One process per handler
   instance; one `Wasmex.start_link/1` per admitted artifact
   (real_transport.ex:99-106). No shared heap; the only cross-handler
   channel is a new ARW/1 packet. A shared mutable log is a refused design —
   the journal is append-only per domain with typed `ChainBroken`/`SeqGap`
   refusals (crypto_trust_journal.rs:111-130).
5. **Replay pair requirement.** Every compute export ships a replay export
   pair — the ex4pm registry law: "one entry per `<algo>_v1`/`<algo>_replay_v1`
   export pair" (real_transport.ex:127-143 `algo_specs/0`). A handler
   without its replay pair is refused at admission, E_NO_REPLAY_PAIR
   (0xE1A5).
6. **Memory trio.** Handlers expose the alloc/dealloc/free export trio the
   transport already requires (real_transport.ex:500-522). Missing exports
   refuse E_NO_ALLOC_TRIO (0xE1A6).

Capability summary:

| capability | form | refusal if absent |
|---|---|---|
| artifact identity | digest pin (SHA-256, ex4pm real_transport.ex:83-96) + in-packet DOMAIN_ID | E_DIGEST_UNPINNED (0xE0A0) |
| memory | alloc/dealloc/free exports | E_NO_ALLOC_TRIO (0xE1A6) |
| fuel | header FUEL, ceiling 1e6 | E_BAD_FUEL (0xE2A2) |
| time | SEQ field or allowlisted import; never an ambient clock | E_NO_AMBIENT_CLOCK (0xE1A7) |
| signature tier | TIER field (Section 3.2) | E_BAD_TIER (0xE1A2) |
| replay pair | `<algo>_v1` / `<algo>_replay_v1` | E_NO_REPLAY_PAIR (0xE1A5) |

Any capability absent means a typed refusal before instantiation, never at
first call.

## 6. Error Codes (closed set)

| code | name | meaning |
|---|---|---|
| 0xE1A0 | E_BAD_MAGIC | magic mismatch |
| 0xE1A1 | E_BAD_PKT_TYPE | pkt_type outside closed domain |
| 0xE1A2 | E_BAD_TIER | tier outside {0x00..0x03} |
| 0xE1A4 | E_BAD_VERSION | version != 0x01 |
| 0xE1A5 | E_NO_REPLAY_PAIR | replay export pair missing at admission |
| 0xE1A6 | E_NO_ALLOC_TRIO | alloc/dealloc/free exports missing |
| 0xE1A7 | E_NO_AMBIENT_CLOCK | allowlist violation (any import outside the admitted set) |
| 0xE1A8 | E_CHAIN_BREAK | CHAIN_PREV mismatch (journal `ChainBroken` law) |
| 0xE1A9 | E_SEQ_GAP | SEQ not +1 (journal `SeqGap` law) |
| 0xE1AA | E_BODY_HASH | BODY_HASH mismatch after parse |
| 0xE1AB | E_DOMAIN | DOMAIN_ID not admitted for this handler |
| 0xE2A0 | E_BAD_LEN | BODY_LEN > MAX_BODY, or non-empty body with empty-body type |
| 0xE2A1 | E_OVERLONG | trailing bytes beyond BODY_LEN |
| 0xE2A2 | E_BAD_FUEL | FUEL > FUEL_CEILING |
| 0xE2A3 | E_BAD_FLAGS | reserved flag bits set, or TIER_DUAL without FLAGS.bit0 |
| 0xE0A0 | E_DIGEST_UNPINNED | artifact digest pin missing (ex4pm `:wasm_digest_unpinned`) |
| 0xE0AC | E_VACUOUS_QUALIFICATION_REJECTED | anti-vacuity gate; shared with attestation.md Section 5 |
| 0xE0AD | E_STANDING_REGRESSION | standing rank decrease for a fixed subject key without the lawful QUALIFIED -> SUPERSEDED transition; defined in attestation.md Section 3, surfaced on the ARW/1 wire |
| 0xE009 | RESERVED_HYGIENE | formally reserved for ontology metamodel-hygiene refusals (implemented in the alpha-gamma kernel as the typed string-coded error `E_METAMODEL_HYGIENE_VIOLATION`, ggen_law.rs:84, checker `check_hygiene` at :195); no wire emission yet |
| 0xEA01 | RESERVED_HYGIENE | formally reserved for ontology namespace-leak refusals (implemented in the alpha-gamma kernel as the typed string-coded error `E_NAMESPACE_LEAK`, ggen_law.rs:95); no wire emission yet |

The set is closed. The 16-bit ranges 0xE0xx, 0xE1xx..0xE2xx, and 0xEAxx are
reserved; new codes require a spec revision. Reserved entries carry no
runtime semantics until a spec revision defines their emission; a RESERVED
code arriving on the wire is refused like any other unallocated code. ERROR packet payloads carry the code as
u64 LE; unknown codes arriving on the wire are refused at parse (closed
PKT_TYPE domain), never surfaced.

## 7. Cross-Transport Determinism

The same packet bytes decode to the same value in all four languages.
Determinism law: every field little-endian; BLAKE3-32 everywhere; JCS
(RFC 8785) canonical JSON for EVENT payloads — affidavit
`src/sj_record.rs:555-561`: "No 'approximately canonical' serialization",
integers beyond 2^53 refuse typed as `SjRefusal::Canonical`. Cross-runtime
vector discipline is the affidavit KAT registry law (affidavit
`src/crypto_trust_kat.rs:59` `KAT_ALGORITHM_REGISTRY`, fixture
`fixtures/crypto_trust_kat.json`); ARW/1 conformance vectors use the same
golden-literal pattern.

## 8. Falsifiers

The refusal semantics of this spec are testable. Each falsifier below names
the observation that would refute the spec's claim; a passing conformance
suite must include the positive path plus every listed mutation.

1. **Closed enum domains.** Feed a packet with `PKT_TYPE = 0x08` and
   `TIER = 0x04`. If either is accepted (no SINK refusal: E_BAD_PKT_TYPE
   0xE1A1 / E_BAD_TIER 0xE1A2), the closed-domain law is refuted.
2. **DFA boundary completeness.** For each byte position p in 0..N, inject
   one out-of-class byte. If any position accepts an out-of-class byte
   without reaching SINK with the table's code (Section 4.4), the
   branchless boundary claim is refuted.
3. **Overlong refusal.** Append one trailing byte after a complete frame.
   If the parser truncates and accepts instead of refusing E_OVERLONG
   (0xE2A1), the "never a silent truncate" law is refuted.
4. **Chain and sequence laws.** Flip one bit of CHAIN_PREV (expect
   E_CHAIN_BREAK 0xE1A8); replay a packet with SEQ not +1 (expect
   E_SEQ_GAP 0xE1A9); corrupt one payload byte (expect E_BODY_HASH
   0xE1AA). Acceptance of any mutated stream refutes the journal-chain
   grounding.
5. **Fuel monotonicity.** Send a stream whose second packet declares FUEL
   greater than the first packet's remaining fuel, and a packet with
   FUEL > FUEL_CEILING (expect E_BAD_FUEL 0xE2A2; exhaustion must emit
   FUEL_EXHAUSTED with the consumed count, never a hang or silent stop).
6. **Capability admission ordering.** Present a module whose imports exceed
   the allowlist and one missing the alloc/dealloc/free trio. If either is
   instantiated (no pre-instantiation E_NO_AMBIENT_CLOCK 0xE1A7 /
   E_NO_ALLOC_TRIO 0xE1A6 refusal), the "refusal before instantiation"
   law is refuted.
7. **Cross-transport determinism.** Decode the same mutation corpus with
   all four language parsers. If any two disagree on accept/refuse or on
   the refusal code, the cross-transport determinism law (Section 7) is
   refuted.
8. **Closed error-code set.** Send an ERROR packet carrying an unallocated
   code (e.g. 0xE1A3 or a RESERVED entry: 0xE009, 0xEA01). If it is
   surfaced to a handler instead of refused at parse, the closed-set law
   is refuted.
