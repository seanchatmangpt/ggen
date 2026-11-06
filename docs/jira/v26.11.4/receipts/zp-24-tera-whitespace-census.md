# ZP-24 Tera Whitespace Census — L1 lane receipt (OI-2)

Standing: PASS

## Scope

OI-2 (PARTIAL) — falsifier: "dual-engine render-diff court still passing after
tera 2.4.0 becomes a real ggen-engine dev-dep."

State found on disk (working tree, no commits made):
- `crates/ggen-engine/tests/tera_whitespace_census.rs` — census + poisoned
  fixture + dual-engine court already authored.
- `crates/ggen-engine/Cargo.toml:180-185` — `tera2 = { package = "tera", version = "2.4" }`
  dev-dep already landed.
- `crates/ggen-engine/tests/fixtures/templates/newline-law-poisoned.tmpl` — present.

## Defects fixed this session (why the court was red before)

The dual-engine leg (`grammar_law_is_textually_grounded_in_both_engine_versions`)
did not compile and then did not hold:

1. tera 2.4.0 API drift: `templates` is `pub(crate)` and there is no public
   `build_inheritance_chains`. Replaced the v2 twin with `Tera::add_raw_template`
   (`tera.rs:790-805`, registry source), which is the lawful 2.x equivalent of
   the 1.x per-file registration discipline: `Template::new` per file, insert,
   `finalize_templates` (inheritance chains). Context built via
   `Context::from_serialize` (2.x `Context::insert` requires `Cow<'static>`).
2. Leg-3 court rendered every subject with an empty context; the poisoned
   fixture needs `{"rows":[{"source":"X"}]}` and panics without it, and pack
   bodies legitimately fail rendering context-free (SPARQL vars). Helpers are
   now fallible (`Result<String, String>`); a hold requires Ok/Ok byte-equality
   OR Err/Err outcome-parity — a one-engine error is a MISMATCH and flips the
   gate. Fixture subject gets its real context.

## Commands + exits

```
cargo test -p ggen-engine --test tera_whitespace_census
→ test result: ok. 3 passed; 0 failed; 0 ignored; 0 measured (finished in 1.57s)
```

Census output (`--nocapture`, real):
- ggen/packs: 674 template files, 12 with expression-tag `-}}` dashes (= receipted 12)
- marketplace/packs: 2004 template files, 18 with expression-tag `-}}` (= receipted 18)
- ggen/templates: 184 files, 8 expr-tag; ggen/examples: 421 files, 13 expr-tag
  (both extra roots censused, not receipt-pinned)
- Dual-engine court: 13 subjects rendered under tera 1.20.1 vs tera 2.4.0 —
  "COURT HELD: all 13 subjects render byte-identically under both engines"
- Grammar receipts: tera-1.20.1 `src/parser/tera.pest` `variable_end = { "-}}" | "}}" }`
  + tera-2.4.0 `src/parsing/lexer.rs` `Token::VariableEnd(true)` — both present.

## Falsifier result

DISCHARGED. The dual-engine render-diff court compiles and passes with tera
2.4.0 as a real ggen-engine dev-dep. OI-2 can flip PARTIAL → CLOSED.

## Files touched (this lane)

- `crates/ggen-engine/tests/tera_whitespace_census.rs` (fixed v2 render twin +
  leg-3 court parity semantics)
- `docs/jira/v26.11.4/receipts/zp-24-tera-whitespace-census.md` (this receipt)

No other lane files touched. Working tree left uncommitted per lane law
(coordinator owns git transitions).
