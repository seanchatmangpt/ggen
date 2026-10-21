# PRD Deviations Register — ggen v26.10.20 (H01)

Verdicts re-read from disk 2026-10-20 (graphlaw at `/Users/sac/graphlaw`, ggen at
`/Users/sac/ggen`). Each row: the admitted PRD claim, the landed reality with a
file:line receipt, and a disposition. Dispositions are requested verdicts carried
forward, not invented here. No silent re-derivation of numbers.

| # | PRD claim | Landed reality (file:line) | Disposition |
|---|---|---|---|
| 1 | SHACL validation depth = 64 | `MAX_SHACL_VALIDATION_DEPTH: usize = 16` — `/Users/sac/graphlaw/src/shacl_guard.rs:40` (guard recursion bounded at MAX+2, graphs deeper than cap refused with observed depth, shacl_guard.rs:63-151) | deviation-accepted (V09 kept 16, disclosed in RECEIPTS.md slice-1 "3 gaps closed") |
| 2 | Datalog derived-facts cap = 500,000 | Two distinct landed caps: `MAX_DATALOG_FACTS: usize = 100_000` (input-fact array bound, `/Users/sac/graphlaw/src/abi.rs:62`, enforced abi.rs:691) and `N3_MAX_DERIVED_FACTS: usize = 500_000` (N3 closure bound, `/Users/sac/graphlaw/src/law.rs:446`, wired law.rs:505, negative test law.rs:528). purrdf 2.0.2 (pinned `=2.0.2`, graphlaw Cargo.toml:34) exposes **no** `MAX_STORED_FACTS` — grep over registry src returned zero matches | deviation-accepted for the 100k input cap; PRD's 500k number exists verbatim but scopes the N3 closure, not the Datalog store — PRD wording conflated two constants |
| 3 | N3 max iterations = 4,000 | `N3_MAX_ITERATIONS: usize = 4_000` — `/Users/sac/graphlaw/src/law.rs:440`; wired into law config (law.rs:500), refusal envelope (law.rs:533), ABI manifest (`registry.rs:995`, `abi_manifest.rs:129`) | verified-match |
| 4 | Dialect matrix: Chomsky layering, JSON-LD reduced (no new parser deps, remote contexts fail-closed), N3 constrained | Landed as admitted: DIALECT-ADMISSION.md (ADMITTED 2026-10-20); parser probe 7/7 — purrdf 2.0.2 parses probed formats, oxrdfio REJECTED, remote `@context` at `https://example.invalid/...` fails closed with zero network (`/Users/sac/graphlaw/tests/parser_capability_probe.rs:107-146`, probe 7 at L125-146); N3 constrained via constants in rows 2-3 | verified-match (receipt: RECEIPTS.md slice-1, V04) |
| 5 | Resource hardening (wasm bounds) | 8/8 boundary courts GREEN, 3 gaps closed (V09): `/Users/sac/graphlaw/tests/{abi_bounds_court,resource_limits,parse_refusal_court,shacl_depth_guard_court,tier_court}.rs`; differential harness 35 cases, zero divergences (V12); ABI manifest + 10-test court (V11). Additional landed bounds not named in PRD: `MAX_JSON_DEPTH=64` (abi.rs:50), `MAX_ATOMS_PER_FIELD=10_000` (abi.rs:54), `MAX_DATALOG_RULES=10_000` (abi.rs:60), `N3_MAX_TERM_BYTES=64KiB` (law.rs:442) | verified-match (PRD under-specified; landed bounds are a superset) |
| 6 | "Zero unhandled errors" in law paths | `unwrap()`: 0 sites in `graphlaw/src/{law,abi,shacl_guard}.rs` and `crates/ggen-engine/src/law_engine.rs` (grep receipts). `expect()`: 3 sites — `law.rs:968` (authority-lookup over a closed registry, infallible by construction), `abi.rs:180` (serde of an already-built `Value`, infallible), `abi.rs:1082` (`call_json`, doc-marked test/native convenience) | deviation-accepted: 0 unwraps verified; 3 expects are infallible-by-construction or test-only — disclosed, not silent |
| 7 | "Bit-for-bit pack.lock" determinism | No bit-for-bit pack.lock court exists on disk — grep over `crates/ggen-engine/tests/` for `pack.lock`/`pack_lock` matches only unrelated suites (`gall_checkpoint_001_replay_e2e.rs`, `framework_packs_e2e.rs`); H06 is landing that court in this wave | to-fix — row PENDING until H06's court lands and runs GREEN |
| 8 | Wasm size ≤ 25 MiB | 6,654,496 B = 6.34 MiB, re-pinned at 26.10.13 source state, digest `cbc80f07…` byte-verified (`/Users/sac/graphlaw/registry/ARTIFACTS.blake3` line 2; ceiling `26_214_400` at `/Users/sac/graphlaw/tests/wasm_size_court.rs:9`) | verified-match |
| 9 | Success criteria: court suites GREEN | `just courts` all GREEN except one flake — now structurally fenced (G05); graphlaw `cargo test` exit 0; clippy 0 errors (FINAL_STATUS.md §6) | verified-match |
| 10 | Success criteria: publishable | graphlaw publish dry-run clean (26.10.13); ggen-engine blocked on publish ladder — needs ggen-abb-sbb 26.10.12 on crates.io first (FINAL_STATUS.md §5) | deviation-accepted: standard publish-order dependency, not a wave defect; disclosed |

## Cross-lane disclosures

- Row 2 wording fix is docs-only here: the PRD number 500,000 is real but names
  `N3_MAX_DERIVED_FACTS`; the Datalog input-fact cap landed at 100,000 by V09's
  security review. Any future PRD revision should split the two constants.
- Row 7 closes only when H06's court is on disk and GREEN; until then this
  register must not cite a pack.lock determinism receipt.

## H05 fail-open audit (2026-10-20, lane H05)

KPI: "Zero Fail-Open Paths". Grep sweep (`.unwrap()`, `.expect(`, `let _ =`,
`unwrap_or*`, `if let Ok`) over every file landed this wave that no in-flight
lane owns (G02 law.rs region, G07 justfile, G12 mutants report excluded).
Hits per file, each classified: (a) test code, (b) genuinely-infallible or
display-only (justified), (c) fail-open (fixed), (d) G02 region (skip-loud).

### ggen (/Users/sac/ggen)

| File | Hits | Classification |
|---|---|---|
| `crates/ggen-engine/src/envelope.rs` | 5 | L173 `unwrap_or_default()` = (b) falls back to conservative default caps (DEFAULT_MAX_* family; inner caps min()'d, never raised); L231-252 = (a) `#[cfg(test)]` (mod at L217) |
| `crates/ggen-engine/src/canonical/mod.rs` | 6 | (a) all inside `#[cfg(test)]` (mod at L231) |
| `crates/ggen-engine/src/court/mod.rs` | 0 | clean |
| `crates/ggen-engine/src/court/catalog.rs` | 1 | (a) `#[cfg(test)]` (mod at L163) |
| `crates/ggen-engine/src/court/compile.rs` | 2 | (b) L106/110 `unwrap_or_else("0")` — display timestamps in rendered runner template, verdict logic never reads them |
| `crates/ggen-engine/src/receipt_dialect.rs` | 2 | (b) L294/307 — tokenizer extent fallbacks and error-message char display; malformed input still returns `Err` |
| `crates/ggen-engine/src/dataset_bridge.rs` | 0 | clean |
| `crates/ggen-engine/src/generated/refusals.rs` | 0 | clean (static refusal table) |
| `crates/ggen-engine/src/write.rs` (lease fns) | 0 new | diff-added lines contain zero unwrap/expect/`let _ =`; lease reads fail closed per module doc (L81-82: corrupt/unreadable lease = unknown state, never admits) |
| `crates/ggen-engine/src/graph.rs` (dialect sections) | 5 | L184 `if let Ok(parse)` = (b) doc-justified (L176-179): only refines the error message on an already-refused query, refusal path unchanged; L522/820/833/847 = (b) display/label defaults in error text and blank-node relabeling |
| `crates/ggen-engine/src/sync.rs` (dialect sections) | 5 diff-added | L~1005/1084 `prior_generation.unwrap_or(0)` = (b) absent receipt log = generation 0 is the correct semantic; `read_receipt_log_generation` `Err` propagates via `map_err` (L994). L694 `let _ = writeln!(String)` = (b) infallible `fmt::Write to String`. L966/433 = (b) filename/display defaults inside an active refusal message. Pre-existing (not diff-added): L553/561 closure-hash `if let Ok` (best-effort dependency hashing, P2-era), L586/592 reflexive log parse — malformed lines surfaced as `receipt-log:MALFORMED-{n}` graph facts (L622), not swallowed |

### graphlaw (/Users/sac/graphlaw)

| File | Hits | Classification |
|---|---|---|
| `src/tier.rs` | 0 | clean |
| `src/abi_manifest.rs` | 4 | L172 `expect("manifest is JSON")` = (b) serde of statically-built `Value`, infallible; L89 `let _ = write!(String)` = (b) infallible; L192/194 = (a) tests (mod at L186) |
| `src/dialect.rs` | 5 | L280 = doc-comment example; L351/652/658 = (b) snippet-offset/extent fallbacks in refusal *message* construction — the refusal itself still raised; L963 `unwrap_or("")` = (b) empty snippet on non-UTF8 input, `ParseRefusal` still returned (fail-closed preserved) |
| `src/abi.rs` | 4 src | L180 `expect("json serializes")` = (b) infallible Value serde; L560 `unwrap_or("shexc")` = (b) documented default dialect, unknown dialect still errors (`bad(...)`, L563); L649 = (b) infallible strip; L1082 = (b) doc-marked native/test convenience (`call_json`) |

### Module-scope allows (predating the wave — listed, not fixed)

Zero `#[allow(clippy::unwrap_used)]`/`expect_used` in any wave-owned file
(both repos). Pre-existing module/item-scope allows live in
`crates/ggen-config/src/config/{template_config.rs:165,../config_lib/{parser.rs:473,validator.rs:312}}`,
`crates/ggen-config/src/receipt/envelope.rs:320`,
`crates/ggen-cli/src/progress.rs:5` (crate-level, mutex-poisoning rationale),
`crates/ggen-engine/src/lint.rs:337`, `crates/ggen-abb-sbb/src/lib.rs:434/614/834`
(each fail-loud-annotated), plus generated/vendored code
(`ggen-lsp/src/a2a_mcp/**`, `powl2-decompose`, `bcinr-pddl`, `ggen-mcp`,
`ggen-marketplace/src/marketplace/v3.rs:129`). graphlaw `src/` has zero allows.

### Verdict

Fixes applied: **0** — no category (c) fail-open site found in wave-owned
files. Every hit is test code, display-only fallback, infallible-by-construction
serde/fmt, or a documented fail-closed refinement. No G02-region skips were
required (law.rs excluded from scope by lane contract and not swept).

## See Also

[FINAL_STATUS.md](FINAL_STATUS.md) · [RECEIPTS.md](RECEIPTS.md) ·
[DIALECT-ADMISSION.md](DIALECT-ADMISSION.md)
