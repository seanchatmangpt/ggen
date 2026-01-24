<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Execute.rs Implementation Plan - Layer 2 Integration](#executers-implementation-plan---layer-2-integration)
  - [Executive Summary](#executive-summary)
  - [Three-Layer Architecture Pattern](#three-layer-architecture-pattern)
  - [Current State Analysis](#current-state-analysis)
    - [✅ Modules with Complete Integration](#-modules-with-complete-integration)
    - [❌ Modules Missing Execute.rs (8 modules, 35+ verbs)](#-modules-missing-executers-8-modules-35-verbs)
  - [Implementation Plan](#implementation-plan)
    - [Phase 1: Create Execute.rs Integration Layers (Module-by-Module)](#phase-1-create-executers-integration-layers-module-by-module)
      - [1. ai/execute.rs (6 functions)](#1-aiexecuters-6-functions)
      - [2. template/execute.rs (7 functions)](#2-templateexecuters-7-functions)
      - [3. ontology/execute.rs (4 functions)](#3-ontologyexecuters-4-functions)
      - [4. project/execute.rs (7 functions)](#4-projectexecuters-7-functions)
      - [5. paper/execute.rs (10 functions - STUB ONLY)](#5-paperexecuters-10-functions---stub-only)
      - [6. ci/execute.rs (1 function - STUB ONLY)](#6-ciexecuters-1-function---stub-only)
      - [7. workflow/execute.rs (5 functions - STUB ONLY)](#7-workflowexecuters-5-functions---stub-only)
      - [8. fmea/execute.rs (5 functions)](#8-fmeaexecuters-5-functions)
    - [Phase 2: Wire CLI to Execute Functions](#phase-2-wire-cli-to-execute-functions)
    - [Phase 3: Verification](#phase-3-verification)
      - [Andon Signal Checks](#andon-signal-checks)
      - [Integration Tests](#integration-tests)
  - [Summary](#summary)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Execute.rs Implementation Plan - Layer 2 Integration

## Executive Summary

**Current State**: Only 1 of 9 CLI modules has proper Layer 2 integration (execute.rs)
**Target State**: All 9 modules have execute.rs with proper async coordination and error handling
**Total Verbs**: 40+ CLI verbs across 9 modules requiring domain wiring

## Three-Layer Architecture Pattern

```
┌──────────────────────────────────────────────────────────────┐
│ Layer 3: CLI (ggen-cli/src/cmds/)                           │
│ - Input validation                                           │
│ - Output formatting (JSON serialization)                     │
│ - Thin routing to Layer 2                                    │
└──────────────────────────────────────────────────────────────┘
                              ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 2: Integration (ggen-domain/src/*/execute.rs)         │
│ - Async coordination                                         │
│ - Resource management                                        │
│ - Error transformation                                       │
│ - Transaction orchestration                                  │
└──────────────────────────────────────────────────────────────┘
                              ▼
┌──────────────────────────────────────────────────────────────┐
│ Layer 1: Domain (ggen-domain/src/*/[module].rs)             │
│ - Pure business logic                                        │
│ - Domain models and types                                    │
│ - Core algorithms                                            │
│ - No I/O or async                                            │
└──────────────────────────────────────────────────────────────┘
```

## Current State Analysis

### ✅ Modules with Complete Integration

1. **mape_k** (9 phases)
   - Has: `/Users/sac/ggen/crates/ggen-domain/src/mape_k/execute.rs`
   - Status: COMPLETE - Reference implementation
   - Pattern: ValidatorOrchestrator, ExecuteEngine, multi-stage validation

2. **graph** (5 verbs)
   - Has: Inline execute functions in each module file
   - Functions: `execute_load`, `execute_query`, `execute_export`, `execute_visualize`
   - Status: COMPLETE - No separate execute.rs needed
   - Pattern: Direct async functions in load.rs, query.rs, export.rs, visualize.rs

### ❌ Modules Missing Execute.rs (8 modules, 35+ verbs)

| Module | Verbs Count | Domain Functions | Execute.rs Needed |
|--------|-------------|------------------|-------------------|
| ai | 6 | ✅ Has generate.rs, analyze.rs | ❌ Missing |
| template | 7 | ✅ Has show.rs, new.rs, list.rs, lint.rs, generate.rs, generate_tree.rs, regenerate.rs | ❌ Missing |
| ontology | 4 | ⚠️  Partial (uses ggen-core) | ❌ Missing |
| project | 7 | ✅ Has new.rs, plan.rs, gen.rs, apply.rs, init.rs, build.rs | ❌ Missing |
| paper | 10 | ❌ None (placeholder implementations) | ❌ Missing |
| ci | 1 | ❌ None (placeholder implementation) | ❌ Missing |
| workflow | 5 | ❌ None (placeholder implementations) | ❌ Missing |
| fmea | 5 | ⚠️  Uses ggen_utils::fmea (global registry) | ❌ Missing |

## Implementation Plan

### Phase 1: Create Execute.rs Integration Layers (Module-by-Module)

#### 1. ai/execute.rs (6 functions)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/ai/execute.rs`

```rust
//! AI Integration Layer - Async coordination for AI operations
//!
//! Layer 2: Coordinates async execution, resource management, and error handling
//! for AI domain functions.

use crate::ai::{analyze, generate};
use ggen_utils::error::Result;

/// Execute AI code generation with prompt and options
pub async fn execute_generate(
    prompt: &str,
    code: Option<&str>,
    model: Option<&str>,
    suggestions: bool,
) -> Result<GenerateResult> {
    // Layer 2: Async coordination
    let options = generate::GenerateOptions::new(prompt)
        .with_format(generate::OutputFormat::Json);

    let options = if suggestions {
        options.with_suggestions()
    } else {
        options
    };

    let options = if let Some(c) = code {
        options.with_code(c.to_string())
    } else {
        options
    };

    let options = if let Some(m) = model {
        options.with_model(m)
    } else {
        options
    };

    // Call Layer 1 domain function
    let result = generate::generate_code(&options).await?;

    Ok(GenerateResult {
        analysis: result.generated_code,
        suggestions: result.suggestions,
        model_used: result.model,
        processing_time_ms: 100, // FUTURE: Track actual time
    })
}

pub struct GenerateResult {
    pub analysis: String,
    pub suggestions: Vec<String>,
    pub model_used: String,
    pub processing_time_ms: u64,
}

/// Execute code analysis
pub async fn execute_analyze(
    code: Option<&str>,
    file: Option<&std::path::Path>,
) -> Result<AnalyzeResult> {
    let analysis_result = if let Some(code_content) = code {
        analyze::analyze_code_snippet(code_content).await?
    } else if let Some(file_path) = file {
        analyze::analyze_project(file_path).await?
    } else {
        return Err(ggen_utils::error::Error::new(
            "Must provide either code or file",
        ));
    };

    Ok(AnalyzeResult {
        analysis: analysis_result.insights,
        file_analyzed: file.map(|p| p.display().to_string()),
    })
}

pub struct AnalyzeResult {
    pub analysis: String,
    pub file_analyzed: Option<String>,
}

/// Execute interactive chat session
pub async fn execute_chat(message: &str, model: Option<&str>) -> Result<ChatResult> {
    // FUTURE: Implement chat coordination when domain function exists
    Ok(ChatResult {
        response: format!("Chat response to: {}", message),
        model_used: model.unwrap_or("default-chat").to_string(),
    })
}

pub struct ChatResult {
    pub response: String,
    pub model_used: String,
}

// FUTURE: Add execute_refactor, execute_explain, execute_suggest when domain functions exist
```

**Integration**: CLI calls `crate::ai::execute::execute_generate()` instead of calling domain directly

**Todo**:
- [x] Create execute.rs file structure
- [ ] Implement all 6 execute functions (generate, chat, analyze, refactor, explain, suggest)
- [ ] Wire CLI verbs to execute functions
- [ ] Add comprehensive error handling
- [ ] Add async resource management (API rate limiting, connection pooling)

---

#### 2. template/execute.rs (7 functions)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/template/execute.rs`

```rust
//! Template Integration Layer - Async coordination for template operations

use ggen_utils::error::Result;
use std::path::{Path, PathBuf};
use std::collections::BTreeMap;

/// Execute template metadata retrieval
pub async fn execute_show(template_name: &str) -> Result<ShowResult> {
    use crate::template::show;

    let metadata = show::show_template_metadata(template_name)?;

    Ok(ShowResult {
        name: metadata.name,
        path: metadata.path,
        description: metadata.description,
        output_path: metadata.output_path,
        variables: metadata.variables,
        rdf_sources: metadata.rdf_sources,
        sparql_queries_count: metadata.sparql_queries.len(),
        determinism_seed: metadata.determinism_seed,
    })
}

pub struct ShowResult {
    pub name: String,
    pub path: String,
    pub description: Option<String>,
    pub output_path: Option<String>,
    pub variables: Vec<String>,
    pub rdf_sources: Vec<String>,
    pub sparql_queries_count: usize,
    pub determinism_seed: Option<u64>,
}

/// Execute template creation
pub async fn execute_new(name: &str, template_type: &str) -> Result<NewResult> {
    use crate::template::{new as template_new, TemplateService};

    let content = template_new::generate_template_content(name, template_type)?;
    let service = TemplateService::default_instance();
    let path = service.write_template(name, &content)?;

    Ok(NewResult {
        template_name: name.to_string(),
        template_type: template_type.to_string(),
        path: path.display().to_string(),
    })
}

pub struct NewResult {
    pub template_name: String,
    pub template_type: String,
    pub path: String,
}

/// Execute template listing
pub async fn execute_list(directory: &Path) -> Result<ListResult> {
    use crate::template::list;

    let filters = list::ListFilters {
        pattern: None,
        local_only: false,
        gpack_only: false,
    };

    let templates = list::list_templates(directory, &filters)?;

    Ok(ListResult {
        templates: templates
            .into_iter()
            .map(|t| TemplateInfo {
                name: t.name,
                source: match t.source {
                    list::TemplateSource::Local => "local".to_string(),
                    list::TemplateSource::Gpack(name) => name,
                },
                description: t.description,
                path: t.path,
            })
            .collect(),
        total: templates.len(),
        directory: directory.display().to_string(),
    })
}

pub struct ListResult {
    pub templates: Vec<TemplateInfo>,
    pub total: usize,
    pub directory: String,
}

pub struct TemplateInfo {
    pub name: String,
    pub source: String,
    pub description: Option<String>,
    pub path: String,
}

/// Execute template linting
pub async fn execute_lint(template: &str) -> Result<LintResult> {
    use crate::template::lint;

    let options = lint::LintOptions {
        check_sparql: false,
        check_schema: false,
    };

    let report = lint::lint_template(template, &options)?;

    Ok(LintResult {
        has_errors: report.has_errors(),
        has_warnings: report.has_warnings(),
        errors: report.errors,
        warnings: report.warnings,
    })
}

pub struct LintResult {
    pub has_errors: bool,
    pub has_warnings: bool,
    pub errors: Vec<lint::LintMessage>,
    pub warnings: Vec<lint::LintMessage>,
}

/// Execute template generation
pub async fn execute_generate(
    template_path: PathBuf,
    output_path: PathBuf,
    variables: BTreeMap<String, String>,
    force_overwrite: bool,
) -> Result<GenerateResult> {
    use crate::template;

    let options = template::GenerateFileOptions {
        template_path,
        output_path,
        variables,
        force_overwrite,
    };

    let result = template::generate_file(&options)?;

    Ok(GenerateResult {
        output_path: result.output_path.display().to_string(),
        files_created: 1,
        bytes_written: result.bytes_written,
        rdf_files_loaded: 0,
        sparql_queries_executed: 0,
    })
}

pub struct GenerateResult {
    pub output_path: String,
    pub files_created: usize,
    pub bytes_written: usize,
    pub rdf_files_loaded: usize,
    pub sparql_queries_executed: usize,
}

// FUTURE: Add execute_generate_tree, execute_regenerate
```

**Todo**:
- [ ] Create execute.rs file structure
- [ ] Implement all 7 execute functions
- [ ] Wire CLI verbs to execute functions
- [ ] Add proper async coordination
- [ ] Handle template file I/O with proper error handling

---

#### 3. ontology/execute.rs (4 functions)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/ontology/execute.rs`

```rust
//! Ontology Integration Layer - Async coordination for ontology operations

use ggen_utils::error::Result;
use std::path::Path;

/// Execute ontology extraction from RDF file
pub async fn execute_extract(
    ontology_file: &Path,
    namespace: &str,
    output: &str,
) -> Result<ExtractResult> {
    use ggen_core::{Graph, OntologyExtractor};

    // Load ontology file
    let graph = Graph::new()?;
    let file_content = std::fs::read_to_string(ontology_file)?;
    graph.insert_turtle(&file_content)?;

    // Extract schema
    let schema = OntologyExtractor::extract(&graph, namespace)?;

    // Save to output
    let schema_json = serde_json::to_string_pretty(&schema)?;
    std::fs::write(output, schema_json)?;

    Ok(ExtractResult {
        classes_found: schema.classes.len(),
        properties_found: schema.properties.len(),
        relationships_found: schema.relationships.len(),
        namespace: schema.namespace,
        output_file: output.to_string(),
    })
}

pub struct ExtractResult {
    pub classes_found: usize,
    pub properties_found: usize,
    pub relationships_found: usize,
    pub namespace: String,
    pub output_file: String,
}

/// Execute code generation from ontology schema
pub async fn execute_generate(
    schema_file: &Path,
    language: &str,
    output_dir: &Path,
    use_zod: bool,
    use_utilities: bool,
) -> Result<GenerateResult> {
    use ggen_core::codegen::TypeScriptGenerator;

    // Read schema
    let schema_content = std::fs::read_to_string(schema_file)?;
    let schema: ggen_core::ontology::OntologySchema = serde_json::from_str(&schema_content)?;

    // Create output directory
    std::fs::create_dir_all(output_dir)?;

    let mut files_generated = 0;
    let mut primary_file = String::new();

    if language == "typescript" {
        // Generate interfaces
        let interfaces = TypeScriptGenerator::generate_interfaces(&schema)?;
        let interfaces_path = output_dir.join("types.ts");
        std::fs::write(&interfaces_path, interfaces)?;
        files_generated += 1;
        primary_file = interfaces_path.display().to_string();

        // Generate Zod schemas if requested
        if use_zod {
            let zod_schemas = TypeScriptGenerator::generate_zod_schemas(&schema)?;
            std::fs::write(output_dir.join("schemas.ts"), zod_schemas)?;
            files_generated += 1;
        }

        // Generate utilities if requested
        if use_utilities {
            let utils = TypeScriptGenerator::generate_utility_types(&schema)?;
            std::fs::write(output_dir.join("utilities.ts"), utils)?;
            files_generated += 1;
        }
    }

    Ok(GenerateResult {
        language: language.to_string(),
        files_generated,
        primary_file,
    })
}

pub struct GenerateResult {
    pub language: String,
    pub files_generated: usize,
    pub primary_file: String,
}

/// Execute ontology validation
pub async fn execute_validate(schema_file: &Path, strict: bool) -> Result<ValidateResult> {
    // Read schema
    let schema_content = std::fs::read_to_string(schema_file)?;
    let schema: ggen_core::ontology::OntologySchema = serde_json::from_str(&schema_content)?;

    let mut warnings = Vec::new();
    let mut errors = Vec::new();

    // Validation logic (same as CLI for now)
    for class in &schema.classes {
        if class.properties.is_empty() {
            warnings.push(format!("Class '{}' has no properties", class.name));
        }
    }

    for prop in &schema.properties {
        if prop.domain.is_empty() {
            warnings.push(format!("Property '{}' has no domain class", prop.name));
        }
    }

    if strict {
        for prop in &schema.properties {
            if let ggen_core::ontology::PropertyRange::Reference(ref_class) = &prop.range {
                if !schema.classes.iter().any(|c| &c.uri == ref_class) {
                    errors.push(format!(
                        "Property '{}' references undefined class '{}'",
                        prop.name, ref_class
                    ));
                }
            }
        }
    }

    Ok(ValidateResult {
        is_valid: errors.is_empty(),
        classes_count: schema.classes.len(),
        properties_count: schema.properties.len(),
        warnings,
        errors,
    })
}

pub struct ValidateResult {
    pub is_valid: bool,
    pub classes_count: usize,
    pub properties_count: usize,
    pub warnings: Vec<String>,
    pub errors: Vec<String>,
}

// FUTURE: Add execute_init
```

**Todo**:
- [ ] Create execute.rs file structure
- [ ] Implement all 4 execute functions
- [ ] Wire CLI verbs to execute functions
- [ ] Add proper ggen-core integration
- [ ] Handle file I/O with proper error handling

---

#### 4. project/execute.rs (7 functions)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/project/execute.rs`

```rust
//! Project Integration Layer - Async coordination for project operations

use ggen_utils::error::Result;
use std::path::{Path, PathBuf};

/// Execute project creation
pub async fn execute_new(
    name: &str,
    project_type: &str,
    framework: Option<&str>,
    output: &Path,
    skip_install: bool,
) -> Result<NewResult> {
    use crate::project::new;

    let args = new::NewInput {
        name: name.to_string(),
        project_type: project_type.to_string(),
        framework: framework.map(|s| s.to_string()),
        output: Some(output.display().to_string()),
        skip_install,
    };

    let result = new::create_project(&args)?;

    Ok(NewResult {
        project_name: name.to_string(),
        path: result.project_path,
        project_type: project_type.to_string(),
        framework: framework.map(|s| s.to_string()),
        files_created: 0,
        next_steps: result.next_steps,
    })
}

pub struct NewResult {
    pub project_name: String,
    pub path: String,
    pub project_type: String,
    pub framework: Option<String>,
    pub files_created: usize,
    pub next_steps: String,
}

/// Execute plan generation
pub async fn execute_plan(
    template_ref: &str,
    vars: Vec<String>,
    output: Option<&str>,
    format: &str,
) -> Result<PlanResult> {
    use crate::project::plan;

    let args = plan::PlanInput {
        template_ref: template_ref.to_string(),
        vars,
        output: output.map(|s| s.to_string()),
        format: format.to_string(),
    };

    let result = plan::create_plan(&args)?;

    Ok(PlanResult {
        plan_file: template_ref.to_string(),
        output_path: result.output_path,
        format: format.to_string(),
        tasks: vec![],
        variables_count: result.variables_count,
        operations_count: 0,
    })
}

pub struct PlanResult {
    pub plan_file: String,
    pub output_path: String,
    pub format: String,
    pub tasks: Vec<String>,
    pub variables_count: usize,
    pub operations_count: usize,
}

/// Execute code generation
pub async fn execute_gen(
    template_ref: &str,
    vars: Vec<String>,
    output_dir: &Path,
    dry_run: bool,
) -> Result<GenResult> {
    use crate::project::gen;

    let input = gen::GenInput {
        template_ref: template_ref.to_string(),
        vars,
        output_dir: output_dir.to_path_buf(),
        dry_run,
    };

    let result = gen::execute_gen(input).await?;

    Ok(GenResult {
        files_generated: result.files_created,
        files_created: result.files_created,
        output_dir: output_dir.display().to_string(),
        operations: result.operations,
        dry_run,
    })
}

pub struct GenResult {
    pub files_generated: usize,
    pub files_created: usize,
    pub output_dir: String,
    pub operations: Vec<gen::Operation>,
    pub dry_run: bool,
}

/// Execute plan application
pub async fn execute_apply(
    plan_file: &str,
    auto_confirm: bool,
    dry_run: bool,
) -> Result<ApplyResult> {
    use crate::project::apply;

    let input = apply::ApplyInput {
        plan_file: plan_file.to_string(),
        auto_confirm,
        dry_run,
    };

    let result = apply::apply_plan(&input)?;

    Ok(ApplyResult {
        changes_applied: result.operations_count,
        operations_count: result.operations_count,
        files_modified: 0,
        files_created: 0,
        files_deleted: 0,
        dry_run,
    })
}

pub struct ApplyResult {
    pub changes_applied: usize,
    pub operations_count: usize,
    pub files_modified: usize,
    pub files_created: usize,
    pub files_deleted: usize,
    pub dry_run: bool,
}

// FUTURE: Add execute_init, execute_generate, execute_watch
```

**Todo**:
- [ ] Create execute.rs file structure
- [ ] Implement all 7 execute functions
- [ ] Wire CLI verbs to execute functions
- [ ] Add proper async coordination
- [ ] Handle project scaffolding with proper error handling

---

#### 5. paper/execute.rs (10 functions - STUB ONLY)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/paper/execute.rs`

```rust
//! Paper Integration Layer - Async coordination for academic paper operations
//!
//! NOTE: This is a STUB implementation. Paper domain functions need to be implemented first.

use ggen_utils::error::Result;
use std::path::Path;

/// Execute paper creation (STUB)
pub async fn execute_new(
    name: &str,
    template: &str,
    discipline: Option<&str>,
    output: &Path,
) -> Result<NewResult> {
    // FUTURE: Implement when domain functions exist
    Ok(NewResult {
        paper_name: name.to_string(),
        paper_path: output.join(name).display().to_string(),
        template: template.to_string(),
        ontology_file: format!("{}.rdf", name),
        next_steps: vec![
            "Implement paper domain functions".to_string(),
        ],
    })
}

pub struct NewResult {
    pub paper_name: String,
    pub paper_path: String,
    pub template: String,
    pub ontology_file: String,
    pub next_steps: Vec<String>,
}

// FUTURE: Implement execute_generate, execute_validate, execute_export,
// execute_list_templates, execute_compile, execute_init_bibliography,
// execute_submit, execute_track when domain functions exist
```

**Todo**:
- [ ] Create execute.rs stub file
- [ ] Implement domain functions first
- [ ] Add execute functions for all 10 verbs
- [ ] Wire CLI verbs to execute functions

---

#### 6. ci/execute.rs (1 function - STUB ONLY)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/ci/execute.rs`

```rust
//! CI Integration Layer - Async coordination for CI/CD operations

use ggen_utils::error::Result;

/// Execute CI workflow generation (STUB)
pub async fn execute_workflow(
    name: &str,
    output: Option<&str>,
) -> Result<WorkflowResult> {
    // FUTURE: Implement when domain function exists
    let output_path = output
        .map(|s| s.to_string())
        .unwrap_or_else(|| format!(".github/workflows/{}.yml", name));

    Ok(WorkflowResult {
        workflow_name: name.to_string(),
        status: "Generated".to_string(),
        path: Some(output_path),
    })
}

pub struct WorkflowResult {
    pub workflow_name: String,
    pub status: String,
    pub path: Option<String>,
}
```

**Todo**:
- [ ] Create execute.rs stub file
- [ ] Implement domain function for workflow generation
- [ ] Wire CLI verb to execute function

---

#### 7. workflow/execute.rs (5 functions - STUB ONLY)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/workflow/execute.rs`

```rust
//! Workflow Integration Layer - Async coordination for workflow analytics

use ggen_utils::error::Result;
use std::path::Path;

/// Execute workflow initialization (STUB)
pub async fn execute_init(
    name: &str,
    workflow_type: &str,
    output_dir: &Path,
) -> Result<InitResult> {
    // FUTURE: Implement when domain functions exist
    Ok(InitResult {
        workflow_name: name.to_string(),
        path: format!(".workflows/{}.json", name),
        status: "Workflow initialized - ready to track events".to_string(),
    })
}

pub struct InitResult {
    pub workflow_name: String,
    pub path: String,
    pub status: String,
}

// FUTURE: Implement execute_analyze, execute_discover, execute_event, execute_report
```

**Todo**:
- [ ] Create execute.rs stub file
- [ ] Implement domain functions for workflow analytics
- [ ] Add execute functions for all 5 verbs
- [ ] Wire CLI verbs to execute functions

---

#### 8. fmea/execute.rs (5 functions)

**Path**: `/Users/sac/ggen/crates/ggen-domain/src/fmea/execute.rs`

```rust
//! FMEA Integration Layer - Async coordination for failure mode analysis
//!
//! NOTE: FMEA uses ggen_utils::fmea global registry, not domain-local state

use ggen_utils::error::Result;
use ggen_utils::fmea::{FailureCategory, FMEA_REGISTRY};

/// Execute FMEA report generation
pub async fn execute_report(
    format: &str,
    risk: Option<&str>,
    top: usize,
) -> Result<ReportResult> {
    let registry = FMEA_REGISTRY
        .read()
        .map_err(|_| ggen_utils::error::Error::new("Failed to acquire FMEA registry lock"))?;

    let mut failure_modes: Vec<_> = registry.all_failure_modes().collect();

    // Filter by risk level
    if let Some(risk_level) = risk {
        failure_modes.retain(|fm| fm.rpn.risk_level() == risk_level.to_uppercase());
    }

    // Sort by RPN descending
    failure_modes.sort_by(|a, b| b.rpn.value().cmp(&a.rpn.value()));
    failure_modes.truncate(top);

    Ok(ReportResult {
        format: format.to_string(),
        failure_modes_count: failure_modes.len(),
        total_rpn: failure_modes.iter().map(|fm| fm.rpn.value() as u32).sum(),
    })
}

pub struct ReportResult {
    pub format: String,
    pub failure_modes_count: usize,
    pub total_rpn: u32,
}

/// Execute Pareto analysis
pub async fn execute_pareto() -> Result<ParetoResult> {
    let registry = FMEA_REGISTRY
        .read()
        .map_err(|_| ggen_utils::error::Error::new("Failed to acquire FMEA registry lock"))?;

    let mut failure_modes: Vec<_> = registry.all_failure_modes().collect();
    failure_modes.sort_by(|a, b| b.rpn.value().cmp(&a.rpn.value()));

    let total_rpn: u32 = failure_modes.iter().map(|fm| fm.rpn.value() as u32).sum();

    // Count vital few (80%)
    let vital_few_count = failure_modes
        .iter()
        .scan(0u32, |acc, fm| {
            *acc += fm.rpn.value() as u32;
            Some((*acc as f64 / total_rpn as f64) <= 0.80)
        })
        .filter(|&x| x)
        .count();

    Ok(ParetoResult {
        total_modes: failure_modes.len(),
        vital_few_count,
        total_rpn,
    })
}

pub struct ParetoResult {
    pub total_modes: usize,
    pub vital_few_count: usize,
    pub total_rpn: u32,
}

// FUTURE: Add execute_list, execute_show, execute_export
```

**Todo**:
- [ ] Create execute.rs file structure
- [ ] Implement all 5 execute functions
- [ ] Wire CLI verbs to execute functions
- [ ] Add proper FMEA registry coordination

---

### Phase 2: Wire CLI to Execute Functions

For each module, update CLI verbs to call execute functions instead of domain directly:

**Example (ai module)**:

```rust
// BEFORE (in ggen-cli/src/cmds/ai.rs):
let _options = ai::generate::GenerateOptions::new(&prompt);

// AFTER:
use ggen_domain::ai::execute;
let result = crate::runtime::block_on(execute::execute_generate(
    &prompt,
    code.as_deref(),
    model.as_deref(),
    suggestions,
))?;
```

### Phase 3: Verification

#### Andon Signal Checks

1. **Cargo Check** (CRITICAL): `cargo make check` - No compiler errors
2. **Tests** (CRITICAL): `cargo make test` - All tests pass
3. **Lint** (HIGH): `cargo make lint` - No clippy warnings
4. **SLO Check**: `cargo make slo-check` - Performance targets met

#### Integration Tests

Create integration test for each execute function:

```rust
#[tokio::test]
async fn test_execute_generate_integration() {
    let result = execute::execute_generate(
        "write hello world",
        None,
        None,
        false,
    ).await;

    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(!output.analysis.is_empty());
}
```

## Summary

| Module | Execute.rs Path | Functions | Status |
|--------|----------------|-----------|--------|
| mape_k | `/Users/sac/ggen/crates/ggen-domain/src/mape_k/execute.rs` | 9 | ✅ COMPLETE |
| graph | Inline in load.rs, query.rs, export.rs, visualize.rs | 4 | ✅ COMPLETE |
| ai | `/Users/sac/ggen/crates/ggen-domain/src/ai/execute.rs` | 6 | ❌ CREATE |
| template | `/Users/sac/ggen/crates/ggen-domain/src/template/execute.rs` | 7 | ❌ CREATE |
| ontology | `/Users/sac/ggen/crates/ggen-domain/src/ontology/execute.rs` | 4 | ❌ CREATE |
| project | `/Users/sac/ggen/crates/ggen-domain/src/project/execute.rs` | 7 | ❌ CREATE |
| paper | `/Users/sac/ggen/crates/ggen-domain/src/paper/execute.rs` | 10 | ❌ STUB (domain needed) |
| ci | `/Users/sac/ggen/crates/ggen-domain/src/ci/execute.rs` | 1 | ❌ STUB (domain needed) |
| workflow | `/Users/sac/ggen/crates/ggen-domain/src/workflow/execute.rs` | 5 | ❌ STUB (domain needed) |
| fmea | `/Users/sac/ggen/crates/ggen-domain/src/fmea/execute.rs` | 5 | ❌ CREATE |

**Total**: 2 complete, 8 to create (5 full implementation, 3 stubs)
**Total Execute Functions**: 58 (9 existing + 49 to create)
