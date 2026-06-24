# Action Plan: ggen star-toml Config Migration

## Milestone 1: Exploration & Analysis
- **Goal**: Survey the `star-toml` library's existing structures, validator, loader, and error traits. Survey `ggen-config`'s custom TOML parsing and validation system, locating `GgenConfig`, its sub-configs, `ConfigValidator`, and related tests. Identify the exact custom validations currently run (semver, IP/domain, path, and all other fields).
- **Worker**: `teamwork_preview_explorer`

## Milestone 2: Implement star-toml Validation Extensions
- **Goal**: Implement validation helper methods (semver, IP/domain hostname, path validation) in `star-toml` (within `crates/star-toml/src/validation.rs` or appropriate place) on the `Validator` struct. Ensure unit tests are added or updated in `star-toml` to verify these extensions.
- **Worker**: `teamwork_preview_worker`
- **Reviewer**: `teamwork_preview_reviewer`
- **Adversarial**: `teamwork_preview_challenger`
- **Auditor**: `teamwork_preview_auditor`

## Milestone 3: Implement star_toml::Validate for GgenConfig & Sub-configs
- **Goal**: Implement the `star_toml::Validate` trait for `GgenConfig` and all sub-configs in `crates/ggen-config`, using the new `star-toml` validation helpers. Ensure all previous checks from `ConfigValidator` are fully ported.
- **Worker**: `teamwork_preview_worker`
- **Reviewer**: `teamwork_preview_reviewer`
- **Adversarial**: `teamwork_preview_challenger`
- **Auditor**: `teamwork_preview_auditor`

## Milestone 4: Refactor ggen-config to use star-toml
- **Goal**: Refactor `crates/ggen-config` loader/parser to use `star-toml` library for loading (and env-var expansion if done in star-toml/ggen-config). Delegate to `star-toml`'s validation engine. Map/wrap `star_toml::ValidationErrors` to/in `ConfigError`. Ensure all workspace tests compile and pass cleanly.
- **Worker**: `teamwork_preview_worker`
- **Reviewer**: `teamwork_preview_reviewer`
- **Adversarial**: `teamwork_preview_challenger`
- **Auditor**: `teamwork_preview_auditor`
