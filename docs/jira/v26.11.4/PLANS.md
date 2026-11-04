# PLANS

v26.11.4 implementation plans (ZP-11..25, Plan agents 2026-11-04), grounded
in the 34-audit Explore fan-out (ZB-40..74). Full contract text lives in the
session coordination ledger; this index records each plan's load-bearing
decisions and deltas. Standing: all plans UNVERIFIED until executed.

## Membrane (PRD-01)

- ZP-11 (PRD-01 closure): G1 = 1M UDS round-trip court
  (`aoq_sidecar_soak_1m_court.rs`, 10 conn x 100k, alloc census, anti-vacuity
  flipped-tag mutation; ~60s wall from ab-8's 579k fps baseline) +
  `aoq_tagtool` blake3 bridge bin. G2 = arcus↔sidecar full ML-KEM-768
  handshake — **go.mod `go` directive must bump 1.23.4 → 1.24** (crypto/mlkem
  API floor; toolchain already 1.26.10); hand-law `aoq_handshake.go` +
  drift court parsing session.rs. G3 = byte-image parity via a ZERO-TAG
  golden window (arcus writes golden, cgap court byte-compares non-tag
  bytes + recomputes the tag slot; inverse leg + flipped-byte mutation).
  G4 = plan materialization note.
- ZP-12 (W2-F supervision): Option A — in-arcus exec.Command supervisor
  with a 17-reliability amendment row (supervision-of-sidecar ≠ the
  forbidden supervision-of-arcus); ordinal/seq reset = **derive rule**
  (fresh AoqBoundary per reconnect, new `ErrBoundaryStaleSession` typed
  fault + supervisor generation counter); stdin-EOF pipe = the only lawful
  stop; arcus never unlinks the socket (sidecar janitor owns it); states
  Spawning/Ready/BackingOff/Fault (no auto-escape from Fault); courts
  C1 kill-mid-stream / C2 clean-stop / C3 boot-churn / C4 stale-boundary /
  C5 no-sleep scan.
- ZP-13 (AoqTagHook): promote the court-local blake3 port to
  `internal/arcus/adapter/blake3.go` as a new OUT-OF-GENGEN ABB
  (zero-dependency law forbids third-party blake3); remove the 1024B cap
  (multi-chunk CV-stack per cgap_blake3::hash law); boot pre-flight +
  empty-input self-test vector (`af1349b9…`); vector courts + the
  cross-language 4096B pin through the real sidecar tag gate.

## Publish (PRD-03/05)

- ZP-16 (publish automation): **no automation exists today** (ZB-48) —
  construct crates.io Trusted Publishing: one `publish.yml` per repo,
  tag-push keyed, `id-token: write`, zero secrets; operator registers
  (repo, workflow filename, environment) per crate on crates.io; GitHub
  environments `publish-tier0..5`/`publish-ggen` with operator review as
  the human admission edge. Tier DAG = the v26.11.2 runbook's Tier 0-5
  (NOT the ARD's): integration is Tier 4 gated on cgap-audit; wasm-footprint
  Tier 5 conditional (pin rewrite + D9 tarball gate). ggen: graph →
  (abb-sbb, scip, config) → marketplace → engine; PRE-FIX: engine's
  `ggen-scip "26.10.12"` + `ggen-abb-sbb "26.10.11"` reqs must move to
  26.11.3. 26.15.2 anomaly: poll `newest`, never `max_version`; exact/range
  pins only, never `^26.x`. Manual fallback runbook retained (§7).
- ZB-55: ggen-scip READY (no sibling deps); ggen-sjira BLOCKED on its
  path-only `ggen-scip` dep (needs `version = "26.11.3"`).

## RFC-0089 / CBMQS (PRD-04)

- ZP-14 (cbmqs-audit bin): new `crates/cbmqs-cli` member (a `[[bin]]`
  inside cgap-aoq would regress its empty-deps embedded claim); `#[path]`
  shared kernel; `--emit-certificate <path>` + required `--run-seq`;
  `schema_version` (first key) + `seal_algorithm: "BLAKE3-256"`; seal =
  digest tier now, ML-DSA-65 deferred to the Wave-3 gate; courts C7
  tamper / C8 SKIP-fail-closed / C9 bin↔test parity.
  COORDINATION: ZP-21 proposes a feature-gated `[[bin]]` inside cgap-aoq
  instead — coordinator decides one home.
- ZP-15 (foreign-subject projection): **gate-ID collision** — the runner's
  G1-G7 ≠ RFC §6's G1-G7 (different taxonomies); the projection uses
  namespaced P1-P8 rows with `projection_class ∈ {PROJECTABLE, ANALOGUE,
  NOT-PROJECTABLE}` + `attestation_class ∈ {SELF, ATTESTED, EXTERNAL}`;
  NOT-PROJECTABLE register for kernel-internal gates (compile-court
  spec-drift, ≤25 µs/link receipt chain); protocol v2 additive legs
  (ADVERTISE/LEDGER); Kafka adapter = real broker transport, replies
  NO_CENSUS honestly (P1 → CONTRACT_UNSUPPORTED → PROVISIONAL cap — the
  expected honest outcome for JVM incumbents).
- ZP-17 (Gate-10 settling): fence the bench first (profile_guard +
  black_box in receipt_chain_scale.rs), min-of-7 release 50k re-measure,
  AB-14 receipt immutable (3 consumers parse it), all 3 §6 consumers move
  atomically in one diff; branch (a) <25.0 → fence stands; (b) 25.0-25.5 →
  re-derivation receipt + TARGET_US_PER_LINK const move in the SAME diff +
  the cert court's mutation base flips QUALIFIED→DISQUALIFIED (the rx-5
  hazard again); (c) >25.5 → UNSUPPORTED stands + optimization candidates
  (double-blake3 append fold, per-span pricing).
- ZP-21 (durable seal): promote the certificate surface into cgap-aoq
  `src/rfc0089_certificate.rs` (alloc-only no_std-safe); `cbmqs-audit`
  emit/verify-certificate verbs; `canonical_form: "fixed-declaration-order-v1"`
  named INSIDE the sealed artifact; disk-tamper court with anti-vacuity
  legs (delete/corrupt sidecar → exit 2; RunSeqCollision law);
  timestamp stays NONE (three grounds, argued).
- ZP-22 (FIPS-204 signer): 5-phase build reusing the landed field kernel
  (fips203/fips202); the real risk mass = branchless SampleInBall/SampleMask
  (fixed-trip + ct-select), the FS-abort sign loop (constant candidate
  count + masked index fold), and the 2420-B serializer onto the FROZEN
  Receipt shape ("derive from the spec; the const assert is the fence");
  Wave-3 gate de-risk: §10's risk named third-party crate availability —
  the in-repo answer makes it vacuous.

## Perf provenance + gates

- ZB-40/61/67 (provenance): 41.5 ns decode MEASURED (ab-6, 3 lanes agree);
  475.0 ns composed MEASURED (af-35, 2.105M fps); "receipt in 167.0 ns"
  CONTRADICTED as stated (bare-hash median; sealing surface = 456.3 ns
  chained); "sub-10 µs ceiling" CONTRADICTED (only e2e adversarial p99 =
  1.27-5.10 ms; per-stage MEETS rows are transport-excluded).
- ZP-18 (profile-guard 100%): 45 timing test files workspace-wide (ZB-73
  undercounted); wire the 13 cgap-aoq files (2-line compose each), census
  gate (`scripts/profile_guard_census.py` + `--self-check` non-vacuity)
  wired into verify_v26.10.31.sh, disclosed allowlist seeded for the
  32 cross-crate files; Phase C records the landed no_run doctest fix.
- ZP-19 (slab integration): ArrowRecordSlab is BENCH-ONLY for ingestion
  (ZB-64; CTQ-03 membrane 24 B/op NOT-MET receipted); plan = TLS
  WindowBuffers for the sidecar's 6 alloc sites (not the slab — 60B rows
  cannot hold 120B meta or 1MiB bodies), Slab64 as the metadata IDENTITY
  plane (2 rows/window, seal + sweep), CTQ-03-sidecar scoped 0 B/op row;
  G7-chain unfenced row stays explicitly open.
- ZP-20 (Gate-09): 3-run rerun decides in-place tighten (preferred, margins
  observed ~2 orders loose) vs new isolated court; "streams" → "requests"
  wording fix; quiet-machine poll uses `pgrep -fl lsp-echo-server` (the
  `pgrep -f "cargo test"` pattern self-matches — zd-8 lesson).
- ZB-70 (gate map): the PRD ladder is majority NEW work (3 renames, 3
  partials, 6 no-equivalent); wholesale replacement would silently drop
  dod-001/007/008/009/010 — the PRD must ADOPT-AND-EXTEND dod, never
  replace.
- ZB-68 (flip coverage): CBMQS 6/8, RFC-0089 3-4/8, DoD 0-2/12,
  marketplace 459/2,015 gate files (~23%) flip-witnessed; the PRD's 100%
  objective is the wave's largest gap.

## Docs / hygiene

- ZP-23 (PRD corrections): materialize at ggen docs/jira/v26.11.4/PRD.md
  with the C1-C6 corrections ledger (167→456.3 chained; honest I1-I5
  matrix; real gate names; Gate 09 court-to-add; ARD labeling nits; Gate
  10 UNSUPPORTED pending ZP-17) + per-correction falsifier sweeps.
- ZP-24 (newline law): Tera has NO Jinja-style `-}}` on expression tags
  (trim_blocks is control-tag-only) — the prompt's fix mechanism was
  Jinja2; real census 841 files/40+ packs; mermaid-pack minimal fix =
  template restructure; engine-default `trim_blocks` needs a two-sided
  render census including control-tag-EOL (F2).
- ZB-65 (portability): one live ubuntu failure (verify_v26.10.31.sh:80 BSD
  `sed -i ''`); one macOS-side break (lean4 script GNU-only).
