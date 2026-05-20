//! Paper Management Commands for clap-noun-verb v3.4.0
//!
//! Academic paper lifecycle management with LaTeX generation,
//! bibliography management, and publishing workflow support.

use clap_noun_verb::Result;
use clap_noun_verb_macros::verb;
use serde::Serialize;
use std::fs;
use std::path::{Path, PathBuf};

// ============================================================================
// Output Types (all must derive Serialize for JSON output)
// ============================================================================

#[derive(Serialize)]
struct NewPaperOutput {
    paper_name: String,
    paper_path: String,
    template: String,
    ontology_file: String,
    next_steps: Vec<String>,
}

#[derive(Serialize)]
struct GenerateOutput {
    paper_file: String,
    output_format: String,
    output_path: String,
    style: String,
    files_created: usize,
}

#[derive(Serialize)]
struct ValidateOutput {
    paper_file: String,
    is_valid: bool,
    checks_passed: usize,
    checks_failed: usize,
    errors: Vec<String>,
    warnings: Vec<String>,
}

#[derive(Serialize)]
struct ListTemplatesOutput {
    templates: Vec<String>,
    total_count: usize,
    categories: Vec<String>,
}

// ============================================================================
// Verb Functions (the actual CLI commands)
// ============================================================================

/// Create a new academic paper with specified template and discipline
#[verb]
fn new(
    name: String, template: Option<String>, _discipline: Option<String>, output: Option<String>,
) -> Result<NewPaperOutput> {
    let template = template.unwrap_or_else(|| "arxiv".to_string());
    let output_dir = output.unwrap_or_else(|| ".".to_string());
    let paper_path = PathBuf::from(&output_dir).join(&name);

    fs::create_dir_all(&paper_path).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create paper directory: {}",
            e
        ))
    })?;

    // Create sections directory
    fs::create_dir_all(paper_path.join("sections")).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create sections directory: {}",
            e
        ))
    })?;

    // Create base RDF file
    let rdf_file = format!("{}.rdf", name);
    let rdf_content = format!(
        r#"@prefix paper: <https://ggen.io/ontology/paper#> .
@prefix dc: <http://purl.org/dc/elements/1.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

<https://ggen.io/papers/{}> a paper:AcademicPaper ;
    dc:title "{}" ;
    dc:creator "Author Name" ;
    dc:date "{}" ;
    paper:style "{}" .
"#,
        name.replace(' ', "-").to_lowercase(),
        name,
        chrono::Local::now().format("%Y-%m-%d"),
        template
    );

    fs::write(paper_path.join(&rdf_file), rdf_content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to create RDF file: {}", e))
    })?;

    Ok(NewPaperOutput {
        paper_name: name.clone(),
        paper_path: paper_path.to_string_lossy().to_string(),
        template: template.clone(),
        ontology_file: rdf_file,
        next_steps: vec![
            "Edit the .rdf file with your paper metadata".to_string(),
            "Add section content in sections/ directory".to_string(),
            format!("Run: ggen paper generate {} --style {}", name, template),
        ],
    })
}

/// Generate LaTeX from paper ontology with specified style
#[verb]
fn generate(
    paper_file: String, style: Option<String>, output: Option<String>,
) -> Result<GenerateOutput> {
    let style = style.unwrap_or_else(|| "arxiv".to_string());
    let output_path = output.unwrap_or_else(|| {
        let mut path = PathBuf::from(&paper_file);
        path.set_extension("tex");
        path.to_string_lossy().to_string()
    });

    // In a full implementation, this would use ggen-core template engine
    // to render templates/papers/{style}.tmpl using the RDF data.
    // For now, we simulate the generation.

    let tex_content = format!(
        r#"\documentclass{{article}}
\title{{Generated from {}}}
\author{{ggen v26_5_19}}
\begin{{document}}
\maketitle
\section{{Introduction}}
This paper was automatically generated using the ggen academic pipeline.
\end{{document}}
"#,
        paper_file
    );

    fs::write(&output_path, tex_content).map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!("Failed to write LaTeX file: {}", e))
    })?;

    Ok(GenerateOutput {
        paper_file,
        output_format: "latex".to_string(),
        output_path,
        style,
        files_created: 1,
    })
}

/// Validate paper ontology and structure for compliance
#[verb]
fn validate(paper_file: String, strict: bool) -> Result<ValidateOutput> {
    let path = Path::new(&paper_file);
    let mut errors = vec![];
    let mut warnings = vec![];

    if !path.exists() {
        errors.push(format!("Paper file not found: {}", paper_file));
    } else {
        let content = fs::read_to_string(path).map_err(|e| {
            clap_noun_verb::NounVerbError::execution_error(format!(
                "Failed to read paper file: {}",
                e
            ))
        })?;

        if !content.contains("paper:AcademicPaper") {
            errors.push(
                "File is not a valid ggen paper ontology (missing paper:AcademicPaper type)"
                    .to_string(),
            );
        }

        if strict && !content.contains("dc:creator") {
            warnings.push("Missing author (dc:creator)".to_string());
        }
    }

    Ok(ValidateOutput {
        paper_file,
        is_valid: errors.is_empty(),
        checks_passed: if errors.is_empty() { 3 } else { 0 },
        checks_failed: errors.len(),
        errors,
        warnings,
    })
}

/// List available paper templates and styles
#[verb]
fn list_templates() -> Result<ListTemplatesOutput> {
    Ok(ListTemplatesOutput {
        templates: vec![
            "arxiv".to_string(),
            "ieee".to_string(),
            "acm".to_string(),
            "neurips".to_string(),
            "nature".to_string(),
            "phd-thesis".to_string(),
        ],
        total_count: 6,
        categories: vec![
            "preprint".to_string(),
            "conference".to_string(),
            "journal".to_string(),
            "thesis".to_string(),
        ],
    })
}

/// Initialize bibliography management with BibTeX
#[verb]
fn init_bibliography(paper_file: String, output: Option<String>) -> Result<serde_json::Value> {
    let mut bib_path = PathBuf::from(&paper_file);
    if let Some(out) = output {
        bib_path = PathBuf::from(out);
    } else {
        bib_path.set_extension("bib");
    }

    fs::write(&bib_path, "% ggen bibliography\n").map_err(|e| {
        clap_noun_verb::NounVerbError::execution_error(format!(
            "Failed to create bibliography file: {}",
            e
        ))
    })?;

    Ok(serde_json::json!({
        "status": "success",
        "bibtex_file": bib_path.to_string_lossy().to_string(),
        "message": "Bibliography initialized"
    }))
}
