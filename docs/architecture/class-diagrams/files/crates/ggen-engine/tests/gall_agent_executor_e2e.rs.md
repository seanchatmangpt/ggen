# `crates/ggen-engine/tests/gall_agent_executor_e2e.rs`

Source SHA-256: `f6e46792878f2a3d5fbe0f6054ad0a10f3feda8c38d402bef153f363cb279411`

```mermaid
classDiagram
    class fn_packs_dir {
      <<fn>>
    }
    class fn_copy_tree {
      <<fn>>
    }
    class fn_git {
      <<fn>>
    }
    class fn_run_agent {
      <<fn>>
    }
    class fn_ontology {
      <<fn>>
    }
    class fn_scaffold {
      <<fn>>
    }
    class fn_authorized_agent_change_passes_in_isolated_worktree {
      <<fn>>
    }
    class fn_forbidden_agent_change_is_refused_even_when_agent_exits_zero {
      <<fn>>
    }
```

## Dependencies

- `ggen_engine::sync::{sync, SyncOptions}`
- `std::path::{Path, PathBuf}`
- `std::process::{Command, Output}`
- `tempfile::TempDir`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
