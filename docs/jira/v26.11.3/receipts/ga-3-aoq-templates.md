# GA-3: AOQ Pack Templates (PG-A6 step 3)

Standing: ALIVE

## Subject

`/Users/sac/ggen-marketplace/packs/aoq-pack/templates/*.tmpl` — 5 files, new:

- `status_maps.rs.tmpl` → `to: crates/cgap-ffi/src/aoq/status_maps.rs`
- `kind_enums.rs.tmpl` → `to: crates/cgap-ffi/src/aoq/kind_enums.rs`
- `frame_rs.tmpl` → `to: crates/cgap-ffi/src/aoq/frame.rs`
- `header_h.tmpl` → `to: include/cgap_aoq.h`
- `offsets_tbl-md.tmpl` → `to: docs/aoq-header-offsets.md`

## What landed

5 closed-frontmatter `.tmpl` templates in the canonical aoq-pack. Frontmatter
key set is exactly {`to`, `force`, `freeze_policy`, `freeze_slots_dir`,
`sparql`} per file — all inside the skill's closed 26-key allowlist (verified
on disk by per-file key grep). Every `sparql:` block ends `ORDER BY`
(strict-mode validation accepted all queries; a missing ORDER BY is
`error[E0013]` before the pipeline runs, and sync ran to a real written
receipt).

Gate-060 law honored: **no template SPARQL selects `aoq:KemParam`** (grep
receipt: KemParam appears only as prose in the offsets doc — 1 mention,
declaring the never-projected hand-law seam; zero SPARQL mention anywhere).

Tera discipline: every query binds every column on every row (no OPTIONAL →
zero fill-forward exposure); no `{% set %} ~` filter chains (header_h uses
plain per-branch `{% set ct = "..." %}`); typed-literal care (aoq:value hex
strings compared/rendered as plain strings).

## Verification (real gate + actual output)

Staged single-pack scratch manifest (schema-2 shape, no `[packs]` table; real
`ggen sync run`, ggen 26.10.11) against the landed GA-1 ontology:

- Run 1: `"written": [5/5]`, exit 0. Run 2 immediately after: exit 0,
  byte-identical SHA-256 set across all 5 outputs (DETERMINISTIC-X2 via
  `diff` of sorted `shasum -a 256` listings → empty diff).
- Banners: all 5 outputs begin with the canonical banner in the language's
  comment form (`//` ×3 Rust, `/* */` C, `<!-- -->` markdown).
- Non-empty: 2.9–5.2 KB per output.
- `include/cgap_aoq.h` freestanding-C99: `cc -std=c99 -ffreestanding -Wall
  -Wextra -fsyntax-only` → **no errors**, only expected unused-function
  warnings for a header with no consumer TU. Satisfies the
  will-join-GATE0_FILES constraint.
- Rendered Rust modules (`frame.rs`, `kind_enums.rs`, `status_maps.rs`):
  `rustc --edition 2021 --crate-type lib --emit=metadata` → exit 0, no
  errors (one non-blocking naming warning on projected ontology label
  `ROTATED_1` — an ontology label, not a template defect).
- Offsets in rendered output match the AOQ-02 table exactly (0x00/0x04/0x06/
  0x08/0x10/0x14/0x18/0x3C, terminal end 64).

## Assumptions / flagged

- PG-A6's field table is not on disk anywhere searched (home-wide `PG-A6`
  grep in *.md: zero hits). GA-1's receipt field table (the AOQ-02 table
  under the R-1 ruling) is the table authored against; the lane prompt
  named the same offsets.
- `AOQ1_MAGIC`/`AOQ1_VERSION` constants in frame_rs/header_h are hardcoded
  in the template body — querying `aoq:value` would bind on only 2 of 8
  field rows (fill-forward trap per run-ggen skill). Flagged as drift risk;
  gate 020-class closure is the backstop.
- Gates 060/070 (GA-2, in flight) have not landed; honored the stated 060
  law (no KemParam projection) ahead of its gate landing.
- Lumen semantic search was down this session (embedding servers unhealthy);
  filesystem search used instead.

## Standing vocabulary

ALIVE = real `ggen sync run` ×2 (exit 0, byte-identical) this session on the
exact 5-template subject against the landed GA-1 ontology and with GA-2
gates 010/020/030 present in the pack. Not claimed: gates 060/070 (not
landed), an actual cgap consumer sync (nothing under /Users/sac/cgap was
touched), compile of rendered Rust inside the real cgap crate tree
(scratch-only), git operations (none performed).

## Leftover lease

Scratch staging dir `/tmp/aoq-render.Psjk` could not be deleted (rm denied
for this lane) — coordinator should delete it.

Receipt: GA-3, lane GGEN, wave v26.11.3. No git operations performed.
