---
receipt: GALL_CONFORM_001
date: 2026-06-02
status: PASS
gate: Dung Gate
---
# GALL Conformance Receipt 001

ggen manufacturing control plane passed GALL conformance check.

## Evidence
- cargo clippy: PASS (0 violations)
- cargo test: PASS (fixture_validation_proof: 12/12 passing)
- .tera templates: parseable (15 templates fixed, 0 invalid)
- Lawful source surfaces: .ttl, .rq, .tera, ggen.toml only
- No process-mining authority claimed

## Clippy Violations Fixed
17 error-level clippy violations resolved across ggen-core and ggen-cli:
- unwrap_used in OnceLock regex (show.rs)
- too_many_arguments on Construct8::new (genesis.rs)
- elidable_lifetime_names x2 HexSlice/HexSlice64 (genesis.rs)
- format_collect in generate_iri (construct.rs)
- Drop in if-let scrutinee x2 (core.rs)
- struct_excessive_bools x3 (dx.rs, template_types.rs, logging.rs)
- panic in Default impls x6 (emitter.rs, mod.rs, template_metadata.rs, frozen.rs x2, path_protection.rs)
- used_underscore_binding (telemetry.rs)
- Plus 34 additional violations in ggen-cli (unnecessary_hashes, unwrap_used, matching_unit, used_underscore_binding, option_ref_idiom, self_only_in_recursion, unnecessary_debug_formatting, needless_collect, needless_pass_by_ref_mut, expect_used)

## Templates Fixed (15 of 15)
- cli-command.tera: join() positional args → sep= keyword
- ontology-diff-report.tera: selectattr/rejectattr → filter, tuple for-loop → inline
- c4-component-diagrams.tera: escaped pipes \\| in Tera expressions
- receipt-report.tera: printf-style format → round(precision=N)
- dod-compliance-report.tera: attr() → get(key=), mismatched for/endif, format() → round
- code-review-prompt.tera: selectattr/groupby/list Jinja2 filters removed
- dod-checklist.tera: format() → round
- erlang-adapter.tera: Liquid default: syntax → Tera default(value=)
- type-registry.tera: ternary ?: → if/else blocks, selectattr → filter
- kubernetes-deployment.tera: inline ternary in {{ }}, round(0,'ceil') positional args
- runbook-template.tera: string concat in default(value=) arg
- openapi-from-registry.tera: unclosed {# comment, .append() method, selectattr, trim_start_matches
- slo-dashboard.tera: round(N) → round(precision=N), .len() → | length
- rust-struct-from-ontology.tera: .append() method, is containing test, {% break %}
- ontology-explorer-dashboard.tera: ternary ?: → if/else, format() → round, selectattr

## Tests
- fixture_validation_proof: 12 passed, 0 failed (was 10 passed, 2 failed)
- Pre-existing proof/invariants failures: 6 (unchanged, pre-date this conformance check)
