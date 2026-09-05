# vision-2030-phase-change-pack

**Status: CONSUMED (2026-09-04) — exactly one Rust consumer, zero `ggen.toml` consumers.**
`ggen vision2030 report --manifest <manifest> --output <dir>` (`crates/ggen-cli/src/cmds/vision2030.rs`)
`include_str!`s this pack's `templates/vision-2030-report.md.tera` and renders the evaluated
`Report` through it, writing `<dir>/vision-2030-report.md`. Because the template is compiled
into the binary and rendered from `Context::from_serialize(&report)`, a template variable that
stops matching a `Report` field is a build/render failure, not silent drift.
`crates/ggen-config/tests/vision_2030_pack_orphan_test.rs` asserts this consumer set exactly
(one file, by name) — it is the same governance test that previously asserted zero.

The catalog→manifest bridge exists twice, on purpose: `tools/catalog_to_manifest.py` (Python,
reference) and `ggen vision2030 project --catalog <catalog> --output <dir>` (Rust, in-binary,
no shared code). `project_matches_committed_python_generated_manifest` in
`crates/ggen-cli/src/cmds/vision2030/tests.rs` asserts the two agree field-for-field on the
committed manifest — a cross-implementation falsifier, and a drift check if the catalog changes
without regenerating. See "If you are picking this pack back up" below for what the projection
does and what it deliberately does not claim.

## Evidence of the prior orphan status (verified 2026-08-03; superseded above)

- `find packs/vision-2030-phase-change-pack -type f` returns only `pack.toml`,
  `catalog/vision-2030-capabilities.json`, `catalog/vision-2030-maximalist-capabilities.json`,
  `schema/vision-2030-maximalism.schema.json`, `schema/vision-2030-program.schema.json`, and
  `templates/vision-2030-report.md.tera` — no `ontology.ttl`, no `queries/*.rq`, no `ggen.toml`.
- `grep -rl "vision-2030-phase-change-pack" --include="ggen.toml" .` (repo-wide) returns only
  this pack's own directory is absent — **zero** `ggen.toml` files anywhere reference it.
- `grep -rl "vision-2030-phase-change-pack" crates/` (repo-wide, all Rust source) now returns
  **one** file: `crates/ggen-config/tests/vision_2030_pack_orphan_test.rs` — a governance
  regression test (added after this note was first written) that asserts this pack stays
  orphaned. It excludes itself from its own assertion (it necessarily names the pack in prose)
  and still finds **zero** *other* Rust consumers, so the orphan status this README documents
  is unchanged; only the literal grep-output count above was stale.

## Do not confuse this with the live `ggen vision2030` CLI

There is a real, live, tested `ggen vision2030` command surface at
`crates/ggen-cli/src/cmds/vision2030.rs` (verbs) plus `crates/ggen-cli/src/cmds/vision2030/` (`evaluation.rs`, `receipts.rs`,
`tests.rs`). It is a **separate, independent implementation** from this pack:

- The live CLI's `inspect`/`validate`/`roadmap`/`blue_ocean`/`dx`/`qol`/`doctor`/`receipt`/
  `replay` verbs all take a `manifest: String` CLI argument — a path to a JSON file matching
  the `Manifest` struct in `crates/ggen-cli/src/cmds/vision2030.rs`, whose required
  `schema` field must equal the constant `MANIFEST_SCHEMA = "ggen.vision2030.program.v1"`.
- This pack's `schema/vision-2030-program.schema.json` **does** carry that same
  `"schema": {"const": "ggen.vision2030.program.v1"}` — so that one file is at least
  schema-compatible in principle.
- However, this pack's actual data files —
  `catalog/vision-2030-capabilities.json` and `catalog/vision-2030-maximalist-capabilities.json`
  — declare `"schema": "ggen.vision2030.catalog.v1"` (a **different** schema), are missing the
  live `Manifest`'s required `program`/`required_domains`/`horizons` fields entirely, and use a
  `depends_on` key on each capability where the live code's `Capability` struct requires
  `dependencies` plus an `evidence` map. **Neither catalog file in this pack can be passed
  as-is to the live `ggen vision2030` CLI as a `--manifest` argument; it will fail to parse.**
- The document `docs/architecture/VISION-2030-PHASE-CHANGE-ARD-PRD-v26.8.3.md` lists this
  pack's schema/catalog files in its "Implementation map" (§9) alongside the live CLI files,
  which can read as though they cooperate today. As of this note, they do not: nothing in the
  live CLI reads from this pack, and this pack's catalog JSON is not a valid input to it.

## If you are picking this pack back up

Before wiring it in, decide (and update this README when you do):

1. ~~Either make the catalog files conform to the live `Manifest` schema
   (`ggen.vision2030.program.v1`, with `program`/`required_domains`/`horizons`/`dependencies`/
   `evidence`), or make the live CLI accept the pack's `ggen.vision2030.catalog.v1` shape.~~
   **DONE 2026-09-04** — `tools/catalog_to_manifest.py` is a deterministic projection of
   `catalog/vision-2030-capabilities.json` into `catalog/vision-2030-program.manifest.json`
   (the live `Manifest` shape). The catalog stays the human-authored source; the manifest is
   committed generated output, so drift is a visible `git diff`. Every capability lands with
   `evidence: {}` — the honest state — and the live evaluator reports it as such:

   ```
   $ ggen vision2030 validate --manifest catalog/vision-2030-program.manifest.json --format json
   standing=DESIGNED achieved=false phase_change_multiplier=0.000 violations=416   # 52 caps x 8
   ```

   Two drifts were found and fixed in `schema/vision-2030-program.schema.json` along the way —
   it was stricter than the code it documents: it forbade `program.trusted_issuers`/
   `trusted_brokers` (which the Rust `Program` struct *requires*, no `#[serde(default)]`), and it
   required all 7 evidence roles on every capability (the evaluator legally accepts an empty
   map as `DESIGNED`). The schema now matches `vision2030.rs`; a falsifier run confirms it still rejects
   a missing registry, a non-hex key, an unknown evidence role, a non-blake3 digest, and an
   illegal authority.
2. ~~Add a `ggen.toml` (or a generation rule in an existing one) that actually consumes
   `templates/vision-2030-report.md.tera` against real query results…~~
   **DONE 2026-09-04, but not as a `ggen.toml` rule — deliberately.** `[[generation.rules]]`
   feeds a Tera template a SPARQL *result set*; this template consumes the live evaluator's
   `Report` (`domains.*.alive`, `capabilities[].standing`, per-capability `violations` …),
   which is computed from evidence digests and issuer signatures, not queried from an
   ontology. Forcing it through a SPARQL rule would mean re-implementing the evaluator in
   SPARQL, or generating a fake result set — either is a mock of the real thing. So the
   consumer is the CLI itself: `ggen vision2030 report` evaluates the manifest and renders
   this template with ggen's own Tera. The template is `include_str!`'d, so it is a real
   compile-time dependency, not a runtime file lookup that could silently miss.
3. ~~Remove or update the orphan notice in this file once a real consumer exists.~~
   **DONE 2026-09-04** — see the status banner at the top; the orphan governance test now
   asserts the consumer set is exactly `[crates/ggen-cli/src/cmds/vision2030.rs]`.

**What is still honestly open:** every one of the 52 capabilities is `DESIGNED`. The
report verb makes that state *legible* (one Markdown file with every violation named); it
does not advance any capability. Advancing one means binding real evidence — an SBB
density report, positive/negative/verifier artifacts, a receipt, a replay, and an
independently-signed external acceptance from a key registered in `program.trusted_issuers`
(currently empty on purpose) — and the evaluator will refuse anything less.

## The live CLI was itself unreachable until 2026-09-04

Worth knowing if you are reading old receipts: the sentence above "there is a real, live,
tested `ggen vision2030` command surface" was true of the *source* but not of the *binary*.
`vision2030/`, `maximalism/`, and `sbb/` were all `<dir>/mod.rs` modules, and the `#[verb]`
macro (clap-noun-verb-macros 26.7.4) infers the noun from `file!()` at runtime, so all three
registered their verbs under a noun literally named `mod` — colliding on `schema`/`inspect`/
`validate`/`receipt`/`replay`, with link order deciding which one you got. `ggen vision2030 <verb>`
returned `unexpected argument`. Fixed by moving each `mod.rs` up to `<name>.rs` (submodule dirs
stay); no macro or attribute changes. If you see `ggen mod` in an old `--help` capture, that is
this bug, not a real noun.

Per this repo's Evidence-First principle, no claim above is asserted without a citable
file or command; re-run the greps in "Evidence" above if this file's claims might have gone
stale.
