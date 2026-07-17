// Lane B: Contract Tests
// Crate-boundary contracts and public API verification

#[cfg(test)]
mod cli;
#[cfg(test)]
mod manifest;
// ARCHIVED (ggen-core disconnect, 2026-07-16): drives
// `ggen_core::pipeline_engine::pipeline::{PipelineConfig, StagedPipeline}`
// directly. No ggen-engine/ggen-graph equivalent exists (verified via
// workspace-wide search, 2026-07-16 investigation). File retained on disk at
// tests/contract/pipeline.rs, not deleted, per this project's fix-forward
// doctrine.
// #[cfg(test)]
// mod pipeline;
