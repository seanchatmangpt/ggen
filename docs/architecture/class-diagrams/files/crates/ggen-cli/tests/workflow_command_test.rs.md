# `crates/ggen-cli/tests/workflow_command_test.rs`

Source SHA-256: `d6bd808a110668604ef981e7166c103044cc526fe44106dbdbe5808d9bfb9d14`

```mermaid
classDiagram
    class struct_WorkflowInitOutput {
      <<struct>>
      +"workflow_name: String"
      +"path: String"
      +"status: String"
    }
    class struct_WorkflowAnalysisOutput {
      <<struct>>
      +"workflow_name: String"
      +"total_cases: usize"
      +"total_events: usize"
      +"unique_activities: usize"
      +"average_duration_minutes: f64"
      +"median_duration_minutes: f64"
      +"variant_count: usize"
      +"most_common_variant: Option~String~"
    }
    class struct_WorkflowDiscoveryOutput {
      <<struct>>
      +"workflow_name: String"
      +"total_edges: usize"
      +"pareto_edges: usize"
      +"graph_mermaid: String"
      +"top_paths: Vec~String~"
    }
    class mod_workflow_init_tests {
      <<mod>>
    }
    class mod_workflow_analysis_tests {
      <<mod>>
    }
    class mod_workflow_discovery_tests {
      <<mod>>
    }
    class mod_event_report_tests {
      <<mod>>
    }
    class mod_integration_tests {
      <<mod>>
    }
```

## Dependencies

- `serde_json`
- `std::path::PathBuf`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
