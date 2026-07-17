# Project Scaffolding Port

Part of [00-OVERVIEW](00-OVERVIEW.md) — Phase 2, depends on
[01-PUBLISH-SAFETY-AND-CRATE-RENAME](01-PUBLISH-SAFETY-AND-CRATE-RENAME.md). Smallest, most
standalone ticket in the set — no other ticket depends on it, and it depends on nothing
beyond Phase 0.

## File reference table

`cli_generator` and `project_generator` are **directories**, not single files:

| Path | LOC |
|---|---:|
| `/Users/sac/ggen/crates/ggen-core/src/cli_generator/mod.rs` | 88 |
| `/Users/sac/ggen/crates/ggen-core/src/cli_generator/ontology_parser.rs` | 36 |
| `/Users/sac/ggen/crates/ggen-core/src/cli_generator/types.rs` | 202 |
| `/Users/sac/ggen/crates/ggen-core/src/cli_generator/domain_layer.rs` | 149 |
| `/Users/sac/ggen/crates/ggen-core/src/cli_generator/cli_layer.rs` | 186 |
| `/Users/sac/ggen/crates/ggen-core/src/cli_generator/dx.rs` | 368 |
| `/Users/sac/ggen/crates/ggen-core/src/cli_generator/workspace.rs` | 225 |
| **`cli_generator/` total (7 files)** | **1,254** |
| `/Users/sac/ggen/crates/ggen-core/src/project_generator/mod.rs` | 353 |
| `/Users/sac/ggen/crates/ggen-core/src/project_generator/rust.rs` | 393 |
| `/Users/sac/ggen/crates/ggen-core/src/project_generator/nextjs.rs` | 430 |
| `/Users/sac/ggen/crates/ggen-core/src/project_generator/common.rs` | 155 |
| **`project_generator/` total (4 files)** | **1,331** |
| **Combined scaffolding total (11 files)** | **2,585** |

Destination: new module under `/Users/sac/ggen/crates/ggen-cli/src/scaffolding/` (or
similar), consuming both directories' logic — see target design below.

Consumers (re-pointed in [08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md)):

| Path | LOC |
|---|---:|
| `/Users/sac/ggen/crates/ggen-cli/src/cmds/init.rs` | 1,133 |
| `/Users/sac/ggen/crates/ggen-cli/src/cmds/wizard.rs` | 1,744 |

## The gap

Project scaffolding (`cli_generator/`, 1,254 lines across 7 files; `project_generator/`,
1,331 lines across 4 files — both directories, not single files, under
`/Users/sac/ggen/crates/ggen-core/src/`) is absent in praxis — there is no `init`/`new`/
`scaffold` noun in `~/praxis/crates/ggen`'s CLI surface. This capability backs `ggen-cli`'s
`init`/`wizard` commands (`/Users/sac/ggen/crates/ggen-cli/src/cmds/init.rs`, 1,133 lines,
and `/Users/sac/ggen/crates/ggen-cli/src/cmds/wizard.rs`, 1,744 lines — see
[08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md), where these two files are scheduled late
in the phase-3 ordering since they also depend on the sync pipeline).

## Target design

Project scaffolding becomes a `ggen-cli`-internal module rather than a separate library
crate: it is presentation/UX logic invoked only by `ggen init`/`wizard`, not a generally
reusable library capability. First-principles design here means not manufacturing a library
abstraction for a single caller — this is the one gap in the whole migration that stays
entirely inside the CLI crate rather than being re-homed into an engine or sibling crate.

## Definition of done for this ticket

- All 11 files under `/Users/sac/ggen/crates/ggen-core/src/{cli_generator,project_generator}/`
  (2,585 lines total) ported into a new `ggen-cli`-internal module (not a new standalone
  crate).
- `ggen init` and `ggen wizard`
  (`/Users/sac/ggen/crates/ggen-cli/src/cmds/init.rs`,
  `/Users/sac/ggen/crates/ggen-cli/src/cmds/wizard.rs`) re-pointed at the new internal
  module once their other dependencies (ontology/validation, the sync pipeline) are also
  migrated — see [08-GGEN-CLI-MIGRATION](08-GGEN-CLI-MIGRATION.md).
