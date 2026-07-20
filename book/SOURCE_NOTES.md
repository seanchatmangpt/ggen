# Source notes

This book was manufactured from the conversation's v26.7.19 TCPS framework work, the supplied project execution record, the complete 132-file TCPS production-source archive, and the approved 336-chapter `SUMMARY.md`.

## Admission-mechanism update (2026-07-19)

After the book's authoring, the ggen engine replaced SHACL admission (`shapes.ttl`,
`[law].shapes`) with SPARQL gates: per-pack `gates/*.rq` files evaluated in sorted filename
order against the union graph (`# MESSAGE:` header lines + exactly one ASK or SELECT query;
ASK `true` or any SELECT row = violation; codes `FM-PACK-012`/`FM-PACK-013`, project-level
`FM-LAW-012`/`FM-LAW-013`; a legacy `shapes.ttl` refuses loudly). The chapter text and
listings in `src/` were updated on 2026-07-19 to teach the gates mechanism as current, with
SHACL kept only as history. Ground truth: `crates/ggen-engine/src/sync.rs`
(`parse_gate_source`, `evaluate_gate`, `list_gate_files`) and
`crates/ggen-engine/tests/reasoner_independence_e2e.rs` in the host repository.

## Recorded case-study results (as authored)

The book originally preserved these results as **project-recorded, not independently rerun**
(the authoring environment lacked the Rust and ggen toolchains):

- Original TCPS reference tests: 14/14 against generated core code.
- `receiptctl`: 20/20.
- `ggen-engine --lib`: 130/130.
- `framework_packs_e2e`: 11/11.
- Real defects found: inert packs, cross-pack shared-class contamination, FM-WRITE-008 output collisions, Tera self-interpretation, package-identity drift, mirrored-reference regressions, and module/struct shadowing.

## Independently rerun 2026-07-19 (host ggen repository)

Rerun this session in `/Users/sac/ggen` on branch `feat/reflexive-law`, actual commands and
observed results:

- `cargo test --manifest-path examples/tcps-generated/Cargo.toml --lib` — **18 passed, 0
  failed** (the generated core including the original Japanese `試験.rs` tests; the suite has
  grown from the recorded 14 to 18 tests).
- `cargo test --manifest-path examples/receiptctl/Cargo.toml` — **150 passed, 0 failed across
  all suites**, including the historically cited lib suite at **20/20**.
- `cargo test -p ggen-engine --lib` — **130 passed, 0 failed**.
- `cargo test -p ggen-engine --test framework_packs_e2e` — **12 passed, 0 failed** (one more
  test than the recorded 11/11; the current suite includes the loud legacy-`shapes.ttl`
  refusal and the pack-gate union-graph refusal among its 12).

Not rerun green: the compiled product-reference workspace under
`packs/tcps-core-pack/reference/製品版` did not compile in this session's in-flight working
tree (`cargo test -p tcps-core` → 7 module/struct-shadowing compile errors). This is a real,
permanent characteristic of the shipped reference itself — its own `RELEASE_STANDING`
discloses "structural inspection passed, compilation not verified" — and is exactly the
defect the generating pack fixes via the disclosed `_impl` module renames (see
`packs/tcps-core-pack/templates/lib.rs.tmpl`'s header comment).

Transient, since resolved: `examples/tcps-generated/tests/tcps_conformance_e2e.rs` was
observed failing 7/7 mid-session while the ggen-verify-pack wiring was in flight (its
scaffold did not yet copy the two newly wired packs). Fixed the same session; the suite's
final state is **7 passed, 0 failed**, and the full consumer workspace is 26/26 — verified
by rerun after the fix, plus a live end-to-end demonstration of the evidence gates refusing
a red check (`FM-PACK-013`, `test-workspace exit 101`) and passing after re-verification.
