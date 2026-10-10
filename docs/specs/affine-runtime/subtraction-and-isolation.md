# Subtraction and Isolation — seL4 Lessons Operationalized (normative extension)

Status: NORMATIVE (extends ARW/1 `wire-protocol.md` and `attestation.md`)
Version: 1.0 (2026-10-08)
Grounding: ggen @ 5d209290c (tree), bcinr working tree @ 3fe4ba75,
ggen-marketplace @ 2de3d52fe (branch `hdit-v2-structs`), wasm4pm working tree.
Sibling specs: `wire-protocol.md` v1.0, `attestation.md` v1.0 (same directory).

## 0. Purpose

seL4's central result is that a microkernel's guarantees come from what it
*omits*. This spec fixes, for the affine runtime work set, three subtraction
boundaries: what memory a fiber may have (Lesson 1), what the synthesis
kernel may know (Lesson 2), and how fibers remain mutually non-interfering
(Lesson 4). Every element cites a real file:line footprint; anything not
cited is design, not grounding.

## 1. Lesson 1 — Capability-Based Spatial Quotas

seL4 grants memory only by explicit capability transfer; there is no ambient
heap. The affine runtime adopts the same law for WASM fibers: a fiber is
spawned with a pre-budgeted memory contract, and growth beyond it is
refused, not clamped.

### 1.1 WASM fiber spawning contract

- A fiber spawn request carries an explicit page count `P` in 64 KiB WASM
  linear-memory pages, bounded by `GATES = 150` (wasm4pm
  `src/boundary_generated.rs:1`). Maximum fiber memory is
  `P × 64 KiB ≤ 150 × 64 KiB = 9 600 KiB`; the ARW/1 `MAX_BODY` of 1 MiB
  (wire-protocol.md:81) always fits.
- There is no ambient growth: memory growth beyond `P` is refused at the
  boundary — the same sink-on-violation law as ARW/1 fuel
  (E_BAD_FUEL, wire-protocol.md:156, ceiling `CEILING = 1_000_000`,
  wasm4pm `src/boundary_generated.rs:2`).
- Every fiber receives one payload buffer, allocated once at spawn — the
  same discipline as the ARW/1 S_LEN transition ("allocate payload buffer
  once here", wire-protocol.md:180). No fiber ever allocates on its own
  control path.

### 1.2 AtomVM reduction-loop memory slices

AtomVM schedules BEAM processes by reduction count, so a memory slice is a
*consequence of the reduction contract*, not a separate allocator policy.
For each BEAM-side process hosting a fiber bridge:

- the memory slice is fixed at spawn and its size is a pure function of
  the fiber's page budget `P` (no per-fiber policy knobs — Lesson 2);
- when the process yields its reduction budget, the slice is returned
  whole to the runtime pool.

### 1.3 bcinr arena recycling (the proven steady-state shape)

The real implementation of "pre-budgeted, recycled whole, never grown" is
bcinr's `OcelEmitArena`: a bump arena of exactly 4096 frames allocated once
(bcinr `crates/bcinr-powl/src/receipt/ocel_emit.rs:18` `ARENA_CAPACITY`
= 4096; alloc_zeroed at :32) that panics when full (:62-66, "arena full")
and is *replaced, not grown*: the rdtsc benchmark recycles arenas at the
4090 mark (`crates/bcinr-powl/benches/rdtsc.rs:169`,
`if arena.len() >= 4090 { *arena = OcelEmitArena::new(); }`; rationale at
:155-157 — "mirroring receipt_bench.rs's warm steady state — a fresh arena
per sample would measure first-touch page faults, not `emit`"). This is
the exact runtime law: allocate once, fill to a mark strictly below
capacity, replace whole; never grow.

### 1.4 Parameters

| Parameter | Value | Anchor |
|---|---|---|
| Fiber page budget ceiling | 150 × 64 KiB = 9 600 KiB | wasm4pm `src/boundary_generated.rs:1` |
| Fuel ceiling | 1_000_000 | wasm4pm `src/boundary_generated.rs:2`; wire-protocol.md:156 |
| Arena capacity / recycle mark | 4096 / 4090 frames | bcinr `ocel_emit.rs:18`, `rdtsc.rs:169` |
| ARW/1 MAX_BODY | 1 MiB | wire-protocol.md:81 |

### 1.5 Falsifier (Lesson 1)

The spec is refuted if any of these is observed:

- a fiber's linear memory grows beyond its spawn budget `P` without a
  refusal (an ambient growth path exists);
- a payload buffer is allocated on a fiber control path other than the
  single at-spawn allocation;
- an arena is grown in place, or recycled at any mark that is not a fixed
  pre-budgeted mark strictly below capacity;
- measured steady-state `emit` throughput changes materially when a fresh
  arena is allocated per sample instead of recycled — i.e. the recycle law
  fails to abstract first-touch page faults, falsifying §1.3's premise.

## 2. Lesson 2 — Mechanism, Not Policy

seL4's microkernel implements mechanisms; policy lives in user space. The
affine runtime synthesis kernel is bounded the same way.

**The kernel may know:**

- **topological ordering over a DAG** — the kernel computes ordering and
  detects cycles via Bellman-Ford-style relaxation with cycle check
  (ggen `crates/praxis-graphlaw/src/datalog.rs:83` `validate_rules`,
  safety check at :104-118, iteration bound
  `while changed && iteration <= num_predicates` at :296);
- **DAG acyclicity** — a cyclic order is a typed refusal, not a policy
  decision;
- **port matching** — a packet's declared port must match its handler
  binding; mismatch is a refusal in the same closed-domain class as ARW/1
  E_BAD_TYPE / E_BAD_TIER (wire-protocol.md Section 3);
- **SHACL/Datalog falsification** — the kernel may evaluate a loaded rule
  against data and return admit/refuse; *which* rules and *which* shapes
  are loaded is not kernel knowledge.

**The kernel may NOT know:**

- which packs are installed, what a pack's rules encode, or which vendor
  vocabulary a pack carries. The mechanism-not-policy audit of the kernel
  crates (`praxis-core`, `praxis-graphlaw`, `ggen-engine`, `ggen-graph`)
  found zero real cloud/vendor hits (all raw matches are substring false
  positives: `aws` inside `laws`/`Laws`, `s3` inside stage vars `s1..s5`)
  and ~13 `ledger` hits that are the fabric's own admission-ledger
  vocabulary — verdict **"mechanism-not-policy holds for the kernel
  crates"** (ggen-marketplace `docs/sjira/v26.10.8/TCB-INVENTORY.md:37`);
- end-to-end flow policy — routing is declared in packs/graph, never
  synthesized by the kernel;
- any vocabulary beyond the fixed wire grammar (ARW/1 is a closed domain;
  packet types 0x08..0xFF refused at parse time, wire-protocol.md §3.1) —
  the kernel sees bytes and loaded rules, never intent.

**Honest note (parser).** TCB-INVENTORY.md row 2 records that no crate
named `ex4pm` exists and "Type-3 wire DFA" does not appear verbatim; the
real transport parser is a hand-rolled recursive-descent parser
(ggen `crates/praxis-graphlaw/src/parser/mod.rs:26` `parse_triples`, :109
`parse_triple`, :147 `parse`, :216 `parse_rules`, :225
`parse_n3_document`). The mechanism/policy split here is defined over the
parser that exists, not the named DFA.

### Falsifier (Lesson 2)

- a real (non-substring) cloud/IAM/vendor import or configuration in the
  kernel crates falsifies the verdict at TCB-INVENTORY.md:37 — inherited
  verbatim from the inventory's own falsifier list;
- a rule or shape change that requires a kernel source change (rather than
  a pack/graph change) falsifies the split;
- the kernel emitting or consuming vocabulary outside the ARW/1 closed
  domain, or consulting installed-pack identity rather than only loaded
  rules, falsifies §2;
- mirror violation: packs re-deriving topo ordering themselves instead of
  consuming kernel verdicts falsifies the split from the pack side.

## 3. Lesson 4 — Non-Interference

seL4 proves non-interference: one component's behavior is unobservable by
another except through declared channels. Three real mechanisms enforce
this here.

### 3.1 BEAM share-nothing

BEAM/AtomVM processes share no memory; a fiber bridge process observes a
sibling only through message passing. With §1.2's fixed slices, one
fiber's memory pressure cannot reallocate another's. Share-nothing is the
isolation substrate; constant-time machinery (§3.2) covers the residual
channel inside shared kernel machinery.

### 3.2 Constant-time verification (bcinr ct.rs)

Verification and boundary checks must not branch on content-dependent
values. bcinr provides the primitives, all real:

- `ct_byte_slice_eq` (bcinr `crates/bcinr-logic/src/ct.rs:212`) — "Always
  processes all bytes even when a mismatch is found — this is the defining
  property that makes it constant-time with respect to the content"
  (ct.rs:200-201); used for BODY_HASH comparison: the verifier touches
  every byte of the hash regardless of where a mismatch falls;
- `ct_lt_u32` (ct.rs:161) — unsigned less-than without a comparison opcode
  (Hacker's Delight borrow-propagation trick, ct.rs:156-157) — budget
  checks (BODY_LEN ≤ MAX_BODY, FUEL ≤ FUEL_CEILING) compare without
  data-dependent branches;
- `ct_select_u64` (ct.rs:71) and `ct_conditional_swap_u64` (ct.rs:239)
  for data-selected sink transitions — the same law as
  wire-protocol.md:160 ("Sink transitions are data-selected
  (`ct_select_u64(sink, next, violation)`)") and for content-independent
  reordering of in-flight packets;
- `ct_eq_u64` (ct.rs:142) for chain-head equality checks.

The ARW/1 header parse is exactly 128 byte-steps on every packet
regardless of content (wire-protocol.md:86); with ct.rs primitives the
verifier's total work is a function of packet shape (fixed header +
BODY_LEN), never of packet content.

### 3.3 Static reduction budgets as timing-channel defense

A timing channel exists when a timing difference carries information. The
runtime closes it by bounding work statically:

- every BEAM process hosting a fiber bridge receives a static reduction
  budget per scheduling slice; the budget is a pure function of the
  fiber's page budget `P` — no wall-clock or host-load term;
- verification cost per packet is bounded by shape: 128 header byte-steps
  (wire-protocol.md:86) plus one fixed pass over BODY_LEN bytes with
  `ct_byte_slice_eq` (§3.2) — so a fiber cannot learn another fiber's
  packet content from verifier timing;
- budget exhaustion is signaled by the ARW/1 FUEL_EXHAUSTED packet
  (wire-protocol.md §3.1, type 0x07, 8-byte consumed-fuel count) —
  exhaustion is a fixed-cost event, not a variable-length penalty.

### Falsifier (Lesson 4)

- two fibers exchanging traffic of identical shape (same BODY_LEN) but
  different content show verifier latency correlated with content
  (statistically significant distribution shift between content classes),
  falsifying §3.2;
- one fiber's memory-growth attempt changes another fiber's allocation
  latency or reduction-yield timing, falsifying §3.1/§3.3;
- a same-BODY_LEN packet whose verification path length differs (e.g. an
  early-exit mismatch in a hash comparison) falsifies §3.2's slice-eq law;
- the reduction budget depends on anything other than `P` (host load,
  wall clock), falsifying §3.3's purity requirement.

## 4. Consistency with sibling specs

No new wire constants, packet types, error codes, or tier semantics are
introduced. Every constant in §1.4 is an existing ARW/1 constant
(FUEL_CEILING 1_000_000, MAX_BODY 1 MiB) or an existing bcinr/wasm4pm
constant (GATES 150, ARENA_CAPACITY 4096, recycle mark 4090). The
refuse-not-clamp law (§1.1) reuses ARW/1 sink-on-violation transitions
(wire-protocol.md §5) and the E_BAD_TIER refusal class (wire-protocol.md
§3.2, 0x04..0xFF refused E_BAD_TIER 0xE1A2). Attestation tiers
(TIER1_ED25519 / TIER2_MLDSA / TIER_DUAL, wire-protocol.md §3.2;
attestation.md §2) are consumed as-is. Budget exhaustion maps to the
existing FUEL_EXHAUSTED packet (0x07), not a new type.

### Falsifier (consistency)

- any parameter in §1.4 diverging from wire-protocol.md or attestation.md
  refutes the self-consistency gate; re-verify anchors on disk at
  citation time.

## 5. Grounding receipt

Anchors re-read from disk 2026-10-08:

- wasm4pm `src/boundary_generated.rs:1-2` (GATES 150, CEILING 1_000_000;
  the file is 2 lines — the entire generated boundary surface);
- bcinr `crates/bcinr-powl/src/receipt/ocel_emit.rs:18,32,62-66`
  (ARENA_CAPACITY 4096, alloc_zeroed, full-panic);
- bcinr `crates/bcinr-powl/benches/rdtsc.rs:155-170` (4090 recycle mark,
  page-fault rationale);
- bcinr `crates/bcinr-logic/src/ct.rs:71,142,156-157,161,200-201,212,239`;
- ggen `crates/praxis-graphlaw/src/datalog.rs:83,104-118,296`;
- ggen `crates/praxis-graphlaw/src/parser/mod.rs:26,109,147,216,225`;
- ggen-marketplace `docs/sjira/v26.10.8/TCB-INVENTORY.md:37` plus the
  "Mechanism-not-policy grep (Lesson 2 audit)" section (zero real cloud
  hits; `ledger` = own admission-ledger vocabulary);
- wire-protocol.md:81,86,156,160,180 (in-repo sibling spec).
