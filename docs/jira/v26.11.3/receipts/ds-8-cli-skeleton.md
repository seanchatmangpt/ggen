# DS-8 CLI Skeleton — Descope Receipt (PF-2)

Date: 2026-11-03 · Lane: DS-10 (PF-2 addendum obligation 3) · Crate: ggen-sjira

## Descope record

DS-8's full CLI verb surface is DESCOPED to the next wave per coordinator
PF-2. This wave lands SKELETON ONLY: arg-parse + ONE happy-path subcommand.

## Subject

`/Users/sac/ggen/crates/ggen-sjira/src/bin/sjira.rs` (new file; bin target
`sjira` auto-discovered by cargo, no shared-file Cargo.toml edit needed).

## Surface

- `sjira admit <work-order.json>` — real file read → JSON parse →
  `admit_work_order` (the real kernel law, unchanged). Prints
  `{admitted, identity, definition_digest}` on success.
- Exit-code law (non-conflicting: aligned with complete.rs's three-exit
  vocabulary, distinct from ggen CLI's exit-5 Andon code):
  - `0` — admitted.
  - `1` — typed kernel refusal (`REFUSED: <admit refusal line>`).
  - `2` — invalid invocation: usage error, unknown subcommand, missing
    argument, unreadable file, non-JSON payload — all BEFORE the kernel is
    consulted.
- Absent verbs refuse typed (unknown subcommand names the next-wave item)
  rather than silently succeeding.

## Gates (run this session, real invocations)

- `cargo build -q -p ggen-sjira --bin sjira` → OK.
- `sjira admit /tmp/wo-valid.json` → exit 0, prints admitted JSON with
  `definition_digest: sha256:7d30f4ca...fcb`, identity `CLI-1`.
- `sjira admit` (missing-field payload) → exit 1, `REFUSED: Deserialization:
  missing field 'title' ...`.
- `sjira admit /tmp/nonexistent.json` → exit 2 (unreadable file).
- `sjira frontier x` → exit 2, typed unknown-subcommand refusal naming the
  next-wave descope.
- `sjira` (no args) → exit 2, usage line.

## Named next-wave items

1. Full verb surface: `sjira frontier` / `execute` / `complete` / `lease
   admit` / `epoch check` / `pack` — each mapped to the library APIs that
   already exist in the crate.
2. Wire the CLI admit exit code into the mutation battery as a criterion.
3. Optional: clap-noun-verb generation via ggen-marketplace (per the ggen
   skill's six-pack pipeline) instead of the hand-rolled std-only parse —
   deliberate skeleton choice to avoid a shared-file Cargo.toml dep edit
   while other lanes are landing concurrently.