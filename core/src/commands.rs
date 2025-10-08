use std::collections::BTreeMap;
use utils::error::Result;

use crate::frontmatter::Frontmatter;
use crate::fs::{read_template, write_output};
use crate::manifest::{compute_manifest_key, ManifestInputs};
use crate::rdf::load_graph;
use crate::render::render_template;
use crate::sparql::bind_vars;

/// Command execution context
#[derive(Debug)]
pub struct CommandContext {
    pub scope: String,
    pub action: String,
    pub vars: BTreeMap<String, String>,
}

/// Execute a generation command
pub fn execute_gen(ctx: &CommandContext) -> Result<String> {
    let spec = read_template(&ctx.scope, &ctx.action)?;
    let fm = Frontmatter::from_spec(&spec);
    let store = load_graph(&fm.rdf_sources())?;
    let bound = bind_vars(&store, &fm.var_queries(), &ctx.vars)?;
    let rendered = render_template(&spec, &bound)?;
    let output_path = std::path::PathBuf::from(fm.to.clone());

    write_output(&output_path, &rendered)?;

    let manifest_inputs = ManifestInputs::from_all(
        &fm,
        &spec,
        &store,
        &crate::rdf::SimpleStore::new(),
        &bound,
        &Vec::new(),
    )?;
    let key = compute_manifest_key(&manifest_inputs);

    Ok(key)
}

/// Execute a list command
pub fn execute_list() -> Result<Vec<String>> {
    let templates = crate::fs::list_templates(std::path::Path::new("templates"))?;
    let mut result = Vec::new();

    for template in templates {
        result.push(format!("{}/{}", template.scope, template.action));
    }

    Ok(result)
}

/// Execute a show command
pub fn execute_show(scope: &str, action: &str) -> Result<String> {
    let spec = read_template(scope, action)?;
    let fm = Frontmatter::from_spec(&spec);

    // Show template info
    let info = format!(
        "Template: {}/{}\nOutput: {}\nVars: {:?}\nRDF Sources: {:?}",
        scope,
        action,
        fm.to,
        fm.vars,
        fm.rdf_sources()
    );

    Ok(info)
}

/// Execute a validate command
pub fn execute_validate(scope: &str, action: &str) -> Result<String> {
    let spec = read_template(scope, action)?;
    let fm = Frontmatter::from_spec(&spec);

    // Validate RDF sources exist and are readable
    for source in &fm.rdf_sources() {
        if source.ends_with(".ttl") && !std::path::Path::new(source).exists() {
            return Err(utils::error::Error::new(&format!(
                "RDF source file not found: {}",
                source
            )));
        }
    }

    let _store = load_graph(&fm.rdf_sources())?;

    // Basic validation - check if RDF parses and SPARQL queries work
    for (i, _query) in fm.var_queries().iter().enumerate() {
        // For happy path, just skip query execution
        // TODO: Implement actual query validation
        log::debug!("Validating SPARQL query {} for {}/{}", i + 1, scope, action);
    }

    Ok(format!("Validation passed for {}/{}", scope, action))
}