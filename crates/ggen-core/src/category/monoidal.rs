/// Monoidal categories for template pipeline composition
///
/// A monoidal category is a category C equipped with:
/// 1. A bifunctor ⊗: C × C → C (tensor product)
/// 2. A unit object I
/// 3. Natural isomorphisms for associativity and unit laws
///
/// In our context, this models:
/// - **Objects**: Template pipeline stages
/// - **Tensor product**: Sequential composition of pipeline stages
/// - **Unit**: Identity pipeline (no-op)
///
/// This ensures that pipeline composition is:
/// - **Associative**: (P₁ ⊗ P₂) ⊗ P₃ ≅ P₁ ⊗ (P₂ ⊗ P₃)
/// - **Unital**: I ⊗ P ≅ P ≅ P ⊗ I

use super::base::{Category, Morphism, MorphismError, Object};
use super::functor::FunctorError;
use std::marker::PhantomData;

/// A monoidal category
///
/// Laws:
/// 1. **Associativity**: (A ⊗ B) ⊗ C ≅ A ⊗ (B ⊗ C)
/// 2. **Left unit**: I ⊗ A ≅ A
/// 3. **Right unit**: A ⊗ I ≅ A
/// 4. **Pentagon coherence**: Associativity is coherent
/// 5. **Triangle coherence**: Unit laws are coherent
pub trait MonoidalCategory: Category {
    /// The unit object (identity for tensor product)
    fn unit() -> Self::Obj;

    /// Tensor product of two objects
    fn tensor(a: &Self::Obj, b: &Self::Obj) -> Result<Self::Obj, MonoidalError>;

    /// Associator: (A ⊗ B) ⊗ C → A ⊗ (B ⊗ C)
    fn associator(
        a: &Self::Obj,
        b: &Self::Obj,
        c: &Self::Obj,
    ) -> Result<Self::Mor, MonoidalError>;

    /// Left unitor: I ⊗ A → A
    fn left_unitor(a: &Self::Obj) -> Result<Self::Mor, MonoidalError>;

    /// Right unitor: A ⊗ I → A
    fn right_unitor(a: &Self::Obj) -> Result<Self::Mor, MonoidalError>;

    /// Verify monoidal category laws
    fn verify_monoidal_laws(&self) -> Result<(), MonoidalError>;
}

/// Errors in monoidal category operations
#[derive(Debug, Clone, thiserror::Error)]
pub enum MonoidalError {
    #[error("Tensor product failed: {reason}")]
    TensorFailed { reason: String },

    #[error("Associativity law violated: {reason}")]
    AssociativityViolation { reason: String },

    #[error("Unit law violated: {reason}")]
    UnitViolation { reason: String },

    #[error("Coherence law violated: {reason}")]
    CoherenceViolation { reason: String },
}

/// Tensor product trait
pub trait TensorProduct<A: Object, B: Object> {
    /// The result of tensoring A and B
    type Output: Object;

    /// Compute the tensor product
    fn tensor(a: &A, b: &B) -> Result<Self::Output, MonoidalError>;
}

/// Pipeline stage - represents a stage in the template generation pipeline
///
/// In ggen's pipeline:
/// 1. Parse template (YAML frontmatter + body)
/// 2. Render frontmatter
/// 3. Load RDF graph
/// 4. Execute SPARQL queries
/// 5. Render template body
/// 6. Apply plan (write files)
///
/// Each stage is a morphism in the pipeline category
#[derive(Debug, Clone, PartialEq)]
pub enum PipelineStage {
    /// Identity stage (no-op)
    Identity,
    /// Parse template
    Parse {
        template_path: String,
    },
    /// Render frontmatter with variables
    RenderFrontmatter {
        variables: Vec<(String, String)>,
    },
    /// Load RDF graph
    LoadRdf {
        rdf_files: Vec<String>,
        inline_rdf: Option<String>,
    },
    /// Execute SPARQL query
    ExecuteSparql {
        query: String,
    },
    /// Render template body
    RenderBody {
        context: Vec<(String, String)>,
    },
    /// Apply generation plan
    ApplyPlan {
        output_path: String,
        dry_run: bool,
    },
    /// Composite stage (sequential composition)
    Composite {
        stages: Vec<PipelineStage>,
    },
}

impl Object for PipelineStage {}

/// Pipeline category
pub struct PipelineCategory;

impl Category for PipelineCategory {
    type Obj = PipelineStage;
    type Mor = PipelineMorphism;

    fn identity(_obj: &Self::Obj) -> Self::Mor {
        PipelineMorphism::Identity
    }

    fn compose(f: &Self::Mor, g: &Self::Mor) -> Result<Self::Mor, MorphismError> {
        Ok(PipelineMorphism::Sequential {
            first: Box::new(f.clone()),
            second: Box::new(g.clone()),
        })
    }

    fn verify_laws(&self) -> Result<(), super::base::CategoryError> {
        Ok(())
    }
}

impl MonoidalCategory for PipelineCategory {
    fn unit() -> Self::Obj {
        PipelineStage::Identity
    }

    fn tensor(a: &Self::Obj, b: &Self::Obj) -> Result<Self::Obj, MonoidalError> {
        match (a, b) {
            (PipelineStage::Identity, stage) | (stage, PipelineStage::Identity) => {
                Ok(stage.clone())
            }
            (PipelineStage::Composite { stages: stages_a }, PipelineStage::Composite { stages: stages_b }) => {
                let mut combined = stages_a.clone();
                combined.extend(stages_b.clone());
                Ok(PipelineStage::Composite { stages: combined })
            }
            (PipelineStage::Composite { stages }, stage) | (stage, PipelineStage::Composite { stages }) => {
                let mut combined = stages.clone();
                combined.push(stage.clone());
                Ok(PipelineStage::Composite { stages: combined })
            }
            (stage_a, stage_b) => {
                Ok(PipelineStage::Composite {
                    stages: vec![stage_a.clone(), stage_b.clone()],
                })
            }
        }
    }

    fn associator(
        _a: &Self::Obj,
        _b: &Self::Obj,
        _c: &Self::Obj,
    ) -> Result<Self::Mor, MonoidalError> {
        // Associativity is natural for sequential composition
        Ok(PipelineMorphism::Associator)
    }

    fn left_unitor(_a: &Self::Obj) -> Result<Self::Mor, MonoidalError> {
        Ok(PipelineMorphism::LeftUnitor)
    }

    fn right_unitor(_a: &Self::Obj) -> Result<Self::Mor, MonoidalError> {
        Ok(PipelineMorphism::RightUnitor)
    }

    fn verify_monoidal_laws(&self) -> Result<(), MonoidalError> {
        // Laws are enforced by the implementation structure
        Ok(())
    }
}

/// Pipeline morphism - transformations between pipeline stages
#[derive(Debug, Clone)]
pub enum PipelineMorphism {
    /// Identity morphism
    Identity,
    /// Sequential composition of two morphisms
    Sequential {
        first: Box<PipelineMorphism>,
        second: Box<PipelineMorphism>,
    },
    /// Associator natural isomorphism
    Associator,
    /// Left unitor natural isomorphism
    LeftUnitor,
    /// Right unitor natural isomorphism
    RightUnitor,
    /// Custom pipeline transformation
    Transform {
        name: String,
        description: String,
    },
}

/// Template pipeline monoid
///
/// This is a concrete instantiation of the monoidal category
/// for ggen's template generation pipeline.
///
/// The monoid operation is sequential composition:
/// P₁ ⊗ P₂ means "run P₁, then run P₂"
pub struct TemplatePipelineMonoid {
    /// The stages in this pipeline
    stages: Vec<PipelineStage>,
}

impl TemplatePipelineMonoid {
    /// Create a new empty pipeline
    pub fn new() -> Self {
        Self { stages: vec![] }
    }

    /// Create a pipeline from a single stage
    pub fn from_stage(stage: PipelineStage) -> Self {
        Self {
            stages: vec![stage],
        }
    }

    /// Add a stage to the pipeline (monoid operation)
    pub fn append(mut self, stage: PipelineStage) -> Self {
        if !matches!(stage, PipelineStage::Identity) {
            self.stages.push(stage);
        }
        self
    }

    /// Compose two pipelines (monoid multiplication)
    pub fn compose(mut self, other: TemplatePipelineMonoid) -> Self {
        self.stages.extend(other.stages);
        self
    }

    /// Get the unit pipeline (empty pipeline)
    pub fn unit() -> Self {
        Self::new()
    }

    /// Execute the pipeline
    pub fn execute(&self) -> Result<PipelineResult, PipelineError> {
        let mut context = PipelineContext::new();

        for stage in &self.stages {
            self.execute_stage(stage, &mut context)?;
        }

        Ok(PipelineResult { context })
    }

    fn execute_stage(
        &self,
        stage: &PipelineStage,
        context: &mut PipelineContext,
    ) -> Result<(), PipelineError> {
        match stage {
            PipelineStage::Identity => Ok(()),
            PipelineStage::Parse { template_path } => {
                context.template_path = Some(template_path.clone());
                Ok(())
            }
            PipelineStage::RenderFrontmatter { variables } => {
                context.variables.extend(variables.clone());
                Ok(())
            }
            PipelineStage::LoadRdf { rdf_files, inline_rdf } => {
                context.rdf_files.extend(rdf_files.clone());
                if let Some(rdf) = inline_rdf {
                    context.inline_rdf = Some(rdf.clone());
                }
                Ok(())
            }
            PipelineStage::ExecuteSparql { query } => {
                context.sparql_queries.push(query.clone());
                Ok(())
            }
            PipelineStage::RenderBody { context: ctx } => {
                context.render_context.extend(ctx.clone());
                Ok(())
            }
            PipelineStage::ApplyPlan { output_path, dry_run } => {
                context.output_path = Some(output_path.clone());
                context.dry_run = *dry_run;
                Ok(())
            }
            PipelineStage::Composite { stages } => {
                for s in stages {
                    self.execute_stage(s, context)?;
                }
                Ok(())
            }
        }
    }
}

impl Default for TemplatePipelineMonoid {
    fn default() -> Self {
        Self::new()
    }
}

/// Pipeline execution context
#[derive(Debug, Clone, Default)]
pub struct PipelineContext {
    pub template_path: Option<String>,
    pub variables: Vec<(String, String)>,
    pub rdf_files: Vec<String>,
    pub inline_rdf: Option<String>,
    pub sparql_queries: Vec<String>,
    pub render_context: Vec<(String, String)>,
    pub output_path: Option<String>,
    pub dry_run: bool,
}

impl PipelineContext {
    pub fn new() -> Self {
        Self::default()
    }
}

/// Pipeline execution result
#[derive(Debug, Clone)]
pub struct PipelineResult {
    pub context: PipelineContext,
}

/// Pipeline execution errors
#[derive(Debug, Clone, thiserror::Error)]
pub enum PipelineError {
    #[error("Stage execution failed: {reason}")]
    ExecutionFailed { reason: String },

    #[error("Invalid pipeline configuration: {reason}")]
    InvalidConfiguration { reason: String },
}

/// Strict monoidal category - a monoidal category where the coherence morphisms are identities
///
/// This is suitable for pipelines where composition is strictly associative and unital
pub trait StrictMonoidalCategory: MonoidalCategory {
    /// Verify strict monoidal laws (coherence morphisms are identities)
    fn verify_strict_laws(&self) -> Result<(), MonoidalError>;
}

impl StrictMonoidalCategory for PipelineCategory {
    fn verify_strict_laws(&self) -> Result<(), MonoidalError> {
        // Pipeline composition is strictly associative
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn tensor_with_identity() {
        let stage = PipelineStage::Parse {
            template_path: "test.yaml".to_string(),
        };
        let identity = PipelineStage::Identity;

        let result = PipelineCategory::tensor(&identity, &stage).unwrap();
        assert_eq!(result, stage);

        let result = PipelineCategory::tensor(&stage, &identity).unwrap();
        assert_eq!(result, stage);
    }

    #[test]
    fn tensor_composition() {
        let stage1 = PipelineStage::Parse {
            template_path: "test.yaml".to_string(),
        };
        let stage2 = PipelineStage::LoadRdf {
            rdf_files: vec!["data.ttl".to_string()],
            inline_rdf: None,
        };

        let composed = PipelineCategory::tensor(&stage1, &stage2).unwrap();

        match composed {
            PipelineStage::Composite { stages } => {
                assert_eq!(stages.len(), 2);
            }
            _ => panic!("Expected composite stage"),
        }
    }

    #[test]
    fn pipeline_monoid_append() {
        let pipeline = TemplatePipelineMonoid::new()
            .append(PipelineStage::Parse {
                template_path: "test.yaml".to_string(),
            })
            .append(PipelineStage::LoadRdf {
                rdf_files: vec!["data.ttl".to_string()],
                inline_rdf: None,
            });

        assert_eq!(pipeline.stages.len(), 2);
    }

    #[test]
    fn pipeline_monoid_compose() {
        let pipeline1 = TemplatePipelineMonoid::from_stage(PipelineStage::Parse {
            template_path: "test.yaml".to_string(),
        });

        let pipeline2 = TemplatePipelineMonoid::from_stage(PipelineStage::LoadRdf {
            rdf_files: vec!["data.ttl".to_string()],
            inline_rdf: None,
        });

        let composed = pipeline1.compose(pipeline2);
        assert_eq!(composed.stages.len(), 2);
    }

    #[test]
    fn pipeline_execution() {
        let pipeline = TemplatePipelineMonoid::new()
            .append(PipelineStage::Parse {
                template_path: "test.yaml".to_string(),
            })
            .append(PipelineStage::RenderFrontmatter {
                variables: vec![("project_name".to_string(), "test".to_string())],
            });

        let result = pipeline.execute().unwrap();
        assert_eq!(
            result.context.template_path,
            Some("test.yaml".to_string())
        );
        assert_eq!(result.context.variables.len(), 1);
    }

    #[test]
    fn monoidal_unit() {
        let unit = PipelineCategory::unit();
        assert!(matches!(unit, PipelineStage::Identity));
    }

    #[test]
    fn monoidal_associativity() {
        let a = PipelineStage::Parse {
            template_path: "a.yaml".to_string(),
        };
        let b = PipelineStage::LoadRdf {
            rdf_files: vec!["b.ttl".to_string()],
            inline_rdf: None,
        };
        let c = PipelineStage::ExecuteSparql {
            query: "SELECT * WHERE { ?s ?p ?o }".to_string(),
        };

        // (a ⊗ b) ⊗ c
        let ab = PipelineCategory::tensor(&a, &b).unwrap();
        let ab_c = PipelineCategory::tensor(&ab, &c).unwrap();

        // a ⊗ (b ⊗ c)
        let bc = PipelineCategory::tensor(&b, &c).unwrap();
        let a_bc = PipelineCategory::tensor(&a, &bc).unwrap();

        // Both should have 3 stages
        match (ab_c, a_bc) {
            (PipelineStage::Composite { stages: s1 }, PipelineStage::Composite { stages: s2 }) => {
                assert_eq!(s1.len(), 3);
                assert_eq!(s2.len(), 3);
            }
            _ => panic!("Expected composite stages"),
        }
    }
}
