use std::path::PathBuf;
use crate::graph::Graph;
use ggen_utils::error::Result;
use tracing::instrument;

#[derive(Debug, Clone)]
pub struct OperatorContext {
    pub workspace_root: PathBuf,
    pub artifact_id: String,
    pub graph: Graph,
}

pub trait ManufacturingOperator: Send + Sync {
    fn name(&self) -> &str;
    fn execute(&self, ctx: &OperatorContext) -> Result<()>;
}

pub struct ValidateOntologyOperator;

impl ManufacturingOperator for ValidateOntologyOperator {
    fn name(&self) -> &str { "ValidateOntology" }

    #[instrument(skip(self, ctx), fields(artifact_id = %ctx.artifact_id))]
    fn execute(&self, ctx: &OperatorContext) -> Result<()> {
        // Real logic will be ported here
        Ok(())
    }
}

pub struct ProjectArtifactOperator;

impl ManufacturingOperator for ProjectArtifactOperator {
    fn name(&self) -> &str { "ProjectArtifact" }

    #[instrument(skip(self, ctx), fields(artifact_id = %ctx.artifact_id))]
    fn execute(&self, ctx: &OperatorContext) -> Result<()> {
        // Real logic will be ported here
        Ok(())
    }
}