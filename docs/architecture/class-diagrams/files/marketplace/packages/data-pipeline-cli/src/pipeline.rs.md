# `marketplace/packages/data-pipeline-cli/src/pipeline.rs`

Source SHA-256: `f6f7907e941492ecde960ea31f140cd0d29d3872e7e7ba45bfd136355b39c4e3`

```mermaid
classDiagram
    class struct_PipelineConfig {
      <<struct>>
      +"name: String"
      +"description: Option~String~"
      +"batch_size: usize"
      +"parallelism: usize"
      +"enable_checkpointing: bool"
    }
    class struct_Pipeline {
      <<struct>>
      +"config: PipelineConfig"
      +"sources: Vec~Arc~RwLock~dyn DataSource~~~"
      +"transforms: Vec~Arc~dyn DataTransform~~"
      +"sinks: Vec~Arc~RwLock~dyn DataSink~~~"
    }
    class struct_PipelineResult {
      <<struct>>
      +"total_records: usize"
      +"successful_records: usize"
      +"filtered_records: usize"
      +"error_count: usize"
      +"duration_seconds: f64"
      +"api_calls: usize"
      +"joined_records: usize"
      +"validation_errors: usize"
      +"sample_record: Option~String~"
    }
    class struct_PipelineBuilder {
      <<struct>>
      +"name: Option~String~"
      +"description: Option~String~"
      +"batch_size: usize"
      +"parallelism: usize"
      +"enable_checkpointing: bool"
    }
    note "Pipeline"
    note "PipelineBuilder"
    note "PipelineResult"
```

## Dependencies

- `anyhow::Result`
- `crate::{DataSource, DataTransform, DataSink, Batch, PipelineError}`
- `serde::{Deserialize, Serialize}`
- `std::sync::Arc`
- `tokio::sync::RwLock`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
