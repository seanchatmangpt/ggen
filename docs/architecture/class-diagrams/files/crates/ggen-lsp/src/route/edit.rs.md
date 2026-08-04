# `crates/ggen-lsp/src/route/edit.rs`

Source SHA-256: `c981a180e37746693c2b0e48852c6e306a6b150a44fda76646b071ae92d7f76e`

```mermaid
classDiagram
    class fn_render_tmpl {
      <<fn>>
    }
    class fn_render_edit {
      <<fn>>
    }
    class fn_anchor_line {
      <<fn>>
    }
    class fn_workspace_edit_from_route {
      <<fn>>
    }
    class fn_route_plan {
      <<fn>>
    }
    class fn_ordered_steps {
      <<fn>>
    }
    class mod_tests {
      <<mod>>
    }
```

## Dependencies

- `lsp_max::lsp_types::{Position, Range, TextEdit, Url, WorkspaceEdit}`
- `std::collections::HashMap`
- `super::*`
- `super::model::{Anchor, EditTemplate, PartialOrder, RepairRoute, RouteBindings}`
- `super::plan::{DiagnosticRef, RoutePlan, RoutePlanStep}`
- `super::super::model::{ EditTemplate, PartialOrder, Provenance, RepairFamily, RepairRoute, RepairStep, RouteId, StepId, }`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
