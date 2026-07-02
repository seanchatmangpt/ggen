# star-toml — audit (generation-first, 2026-07)

**Purpose:** "Framework for loading, layering, and validating any *.toml configuration file" (`crates/star-toml/Cargo.toml`): layered Loader, deep-merge, env expansion, Pydantic-style Validator + declarative Schema.

## LOC

`tokei crates/star-toml/src --output json` (tokei Rust code lines):

| File | Code LOC | Class |
|---|---|---|
| validation.rs | 1,097 | IRREDUCIBLY-CUSTOM (dominant) |
| schema.rs | 405 | IRREDUCIBLY-CUSTOM |
| loader.rs | 366 | IRREDUCIBLY-CUSTOM |
| merge.rs | 128 | IRREDUCIBLY-CUSTOM |
| expand.rs | 113 | IRREDUCIBLY-CUSTOM |
| error.rs | 49 | GENERATABLE-WITH-SPEC |
| lib.rs | 16 | GENERATABLE-WITH-SPEC |
| **Total** | **2,174** | |

## Consolidation status

`CRATE_CONSOLIDATION_ANALYSIS_2026-07-01.md` §2 (line 26) + §3 (line 35): CONSOLIDATE into `ggen-config` as a `star_toml` module — "real, exercised code, not dead weight, but single-consumer." **Not DEAD**; classified for generatability.

## Class breakdown

| Class | LOC | Share |
|---|---|---|
| IRREDUCIBLY-CUSTOM | ~1,810 (files above minus type-def regions) | ~83% |
| GENERATABLE-WITH-SPEC | ~365 | ~17% |

The ~365 generatable estimate = error.rs (49) + lib.rs (16) + the type-definition regions of validation.rs (~300 LOC: `LocSegment`/`Loc`, `Severity` + code table, `ErrorKind` 20-variant enum + code table, `ValidationError` fields + `repair_hint` string table, `ValidationErrors` container plumbing, lines ~114–560). Basis: symbol-level read of every file; validation.rs region estimate from its declaration map. Per METHODOLOGY one-class-per-file, validation.rs carries its dominant class (custom) in the TSV and the split lives here.

## Per-file rationale

- **validation.rs (1,097, IRREDUCIBLY-CUSTOM dominant):** the `Validator` check-event engine (descent via `field`/`index`, pass+fail counting), conformance `fitness()`, `variant_id()` fingerprinting, severity stratification, DECLARE cross-field constraints — the crate's algorithmic core (self-described "Pydantic-grade + Van der Aalst-grade"). Error/severity/kind type defs inside are generatable (~300 LOC, see above).
- **schema.rs (405, IRREDUCIBLY-CUSTOM):** declarative `Constraint` interpreter (`Constraint::check` over `toml::Value`, `FieldBuilder`, nested-section descent) — an interpreter, not boilerplate.
- **loader.rs (366, IRREDUCIBLY-CUSTOM):** layer composition/merge ordering, env-prefix override parsing, parent-dir config discovery — behavior-bearing framework core.
- **merge.rs (128, IRREDUCIBLY-CUSTOM):** recursive `deep_merge` + `set_dotted` + typed env-string coercion.
- **expand.rs (113, IRREDUCIBLY-CUSTOM):** hand-rolled `${VAR}`/`$VAR` scanner with UTF-8-safe slicing and partial-expansion semantics.
- **error.rs (49, GENERATABLE-WITH-SPEC):** thiserror enum + constructor helpers — canonical error-enum boilerplate.
- **lib.rs (16, GENERATABLE-WITH-SPEC):** mod/re-export declarations.

## Nearest template/spec

None. After the ggen-config merge, the generatable slice (error enums, ErrorKind/Severity code tables, repair-hint strings) would fall under whatever config-schema TTL ggen-config adopts; ggen-config already consumes this API through its `config_lib/{parser,error,validator,schema}` modules (consolidation analysis §2).
