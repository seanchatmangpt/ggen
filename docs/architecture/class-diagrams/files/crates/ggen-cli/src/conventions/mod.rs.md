# `crates/ggen-cli/src/conventions/mod.rs`

Source SHA-256: `4e159aac574b2e5308c8574f2436ad48ca5d733e88e462ab1d48df19661c4a3b`

```mermaid
classDiagram
    class mod_planner {
      <<mod>>
    }
    class mod_presets {
      <<mod>>
    }
    class mod_resolver {
      <<mod>>
    }
    class mod_watcher {
      <<mod>>
    }
```

## Dependencies

- `planner::{GenerationPlan, GenerationPlanner, GenerationTask, TemplateMetadata}`
- `presets::ConventionPreset`
- `resolver::{ConventionResolver, ProjectConventions}`
- `watcher::ProjectWatcher`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
