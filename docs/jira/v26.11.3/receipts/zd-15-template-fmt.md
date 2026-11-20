# ZD-15: typed-boundary-refusal template emits rustfmt-clean output

Standing: PASS

Date: 2026-11-04 · Branch: `v26-11-3-wave` · Repo: `/Users/sac/ggen`
Upstream finding: ZD-14 fmt-gate flap — every `ggen sync run` re-armed the fmt
gate because `templates/typed-boundary-refusal-rust.tera` emitted
rustfmt-dirty bytes. Fix is template-side per GGM-SRC-002 (fix the generator,
never hand-patch the generated file).

## Root cause

1. Plain `{% for %}`/`{% endfor %}` tags left their own newline literals in the
   render, producing blank lines after `pub(crate) enum Code {`, between enum
   variants, between match arms, before the closing `}` of the enum/match, and
   a doubled blank line between `Refusal::new` and the first generated ctor.
2. Doc line was `` /// `{{ c.wire }}` — {{ c.doc }}. `` — registry docs already
   end in `.`, so every doc comment carried a doubled period.

## Template changes (`templates/typed-boundary-refusal-rust.tera`)

- Enum loop and `wire()` match loop: `{%- for c in codes %}` … `{%- endfor %}`
  (whitespace-control trims the tag newline; render is rustfmt-clean).
- Ctor loop: `{%- for c in codes %}` (trims the doubled blank after `new()`;
  each iteration's own leading newline provides the single rustfmt-required
  blank line), and the in-loop blank line before `{% endfor %}` removed.
- Doc terminator made idempotent:
  `{{ c.doc }}{% if c.doc is not ending_with(".") %}.{% endif %}` — single
  period regardless of whether the registry doc carries one.

Generated file `crates/ggen-abb-sbb/src/generated/refusals.rs` regenerated via
the real engine (`./target/debug/ggen sync run`); no hand edits.

## Verification (commands/exits)

| Command | Result |
|---|---|
| `./target/debug/ggen sync run` (run 1) | exit 0; `refusals.rs` in `written` |
| `rustfmt --check crates/ggen-abb-sbb/src/generated/refusals.rs` | exit 0 (PASS) |
| `cargo fmt --all -- --check` | exit 0 (PASS) |
| `shasum -a 256 refusals.rs` after run 1 | `793fddd99ca2ba03e95219756136629b133ee3e36491edcbf21f3bb8e7913227` |
| `./target/debug/ggen sync run` (run 2) | exit 0 |
| hash after run 2 | `793fddd9…7913227` — byte-identical (flap close condition met) |
| `rustfmt --check` after run 2 | exit 0 (PASS) |
| `cargo test -p ggen-abb-sbb` | exit 0 — 16 + 3 crosswalk + 3 doc-tests (2 compile_fail + 1 positive control) all green |

## Standing

ALIVE. The sync+fmt pair is now a fixed point: second sync re-emits
byte-identical output, so the fmt gate cannot re-arm. Pre-existing baseline
dirty hash before fix: `a48de987a3d558e09e4582e483560aee44760c6dae7a9274aa629e401e2546b2`.

Note: CLAUDE.md was also listed in sync's `written` set (concurrent lanes; not
owned by this lane, disclosed per lane law). Not committed — coordinator owns
git transitions.
