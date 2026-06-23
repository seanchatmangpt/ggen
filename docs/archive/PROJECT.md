# Project: ggen star-toml Config Migration

## Architecture
- `star-toml` (`crates/star-toml`): Parsing, deserialization, env-var expansion, and structured validation of TOML configs.
- `ggen-config` (`crates/ggen-config`): Configuration schema (`GgenConfig` and sub-configs), custom validation rules, and config loading logic.
- Integration: `ggen-config` relies on `star-toml` for loading files, resolving environment variables, and running validations by implementing the `star_toml::Validate` trait.

## Milestones
| # | Name | Scope | Dependencies | Status |
|---|------|-------|-------------|--------|
| 1 | Exploration & Survey | Inspect star-toml and ggen-config to map validation rules and types | None | DONE |
| 2 | star-toml Extensions | Implement semver, IP/domain, path validation helpers on star-toml's Validator | M1 | DONE |
| 3 | Validate Trait Implementation | Implement star_toml::Validate for GgenConfig and all sub-configs | M2 | DONE |
| 4 | ggen-config Migration | Refactor loader, parser, validator in ggen-config to use star-toml and verify tests | M3 | IN_PROGRESS |

## Interface Contracts
### `star-toml` ↔ `ggen-config`
- `star_toml::Validate`: Trait implemented by configurations to perform validation checks using a `star_toml::Validator`.
- `star_toml::Validator`: Provides validation helpers (e.g. `check_semver`, `check_ip_domain`, `check_path`).
- `star_toml::ValidationErrors`: Returned when validation fails; mapped to `ggen_config::ConfigError`.

## Code Layout
- `crates/star-toml/`: TOML validation/parsing library
- `crates/ggen-config/`: Ggen configurations library
- Coord Metadata: `.agents/orchestrator/`
