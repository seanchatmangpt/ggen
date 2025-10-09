use clap::Args;
use rgen_utils::error::Result;
use std::collections::BTreeMap;
use std::path::PathBuf;
use tera::Context;

use rgen_core::graph::Graph;
use rgen_core::template::Template;

#[derive(Args, Debug)]
pub struct LintArgs {
    /// Path to the template file to lint
    #[arg(value_name = "TEMPLATE")]
    pub template: PathBuf,

    /// Variables (key=value pairs) for frontmatter rendering
    #[arg(short = 'v', long = "var", value_parser = parse_key_val::<String, String>)]
    pub vars: Vec<(String, String)>,

    /// Show detailed linting output
    #[arg(long)]
    pub verbose: bool,

    /// Perform SHACL validation (requires RDF data)
    #[arg(long)]
    pub shacl: bool,
}

fn parse_key_val<K, V>(s: &str) -> std::result::Result<(K, V), String>
where
    K: std::str::FromStr,
    K::Err: ToString,
    V: std::str::FromStr,
    V::Err: ToString,
{
    let pos = s
        .find('=')
        .ok_or_else(|| format!("invalid KEY=value: no `=` found in `{s}`"))?;
    let key = s[..pos].parse().map_err(|e: K::Err| e.to_string())?;
    let val = s[pos + 1..].parse().map_err(|e: V::Err| e.to_string())?;
    Ok((key, val))
}

pub fn run(args: &LintArgs) -> Result<()> {
    let mut issues = Vec::new();

    // Read template file
    let template_content = std::fs::read_to_string(&args.template)?;

    // Parse template
    let mut template = Template::parse(&template_content)?;

    // Create Tera context from vars
    let mut vars = BTreeMap::new();
    for (k, v) in &args.vars {
        vars.insert(k.clone(), v.clone());
    }
    let mut ctx = Context::from_serialize(&vars)?;

    // Insert environment variables
    insert_env(&mut ctx);

    // Create Tera instance
    let mut tera = tera::Tera::default();
    tera.autoescape_on(vec![]);

    // Register text transformation filters
    rgen_core::register::register_all(&mut tera);

    // Render frontmatter
    template.render_frontmatter(&mut tera, &ctx)?;

    // Schema validation
    validate_frontmatter_schema(&template.front, &mut issues);

    // SPARQL syntax validation
    validate_sparql_queries(&template.front, &mut issues);

    // RDF syntax validation
    validate_rdf_content(&template.front, &mut issues);

    // SHACL validation if requested
    if args.shacl {
        validate_shacl(&template.front, &mut issues);
    }

    // Report issues
    if issues.is_empty() {
        println!("✓ No linting issues found");
    } else {
        println!("Found {} linting issue(s):", issues.len());
        for (i, issue) in issues.iter().enumerate() {
            println!("{}. {}", i + 1, issue);
        }
        return Err(rgen_utils::error::Error::new("Linting failed"));
    }

    Ok(())
}

fn validate_frontmatter_schema(
    frontmatter: &rgen_core::template::Frontmatter, issues: &mut Vec<String>,
) {
    // Check for unknown or deprecated keys
    // This is a basic validation - in a real implementation, you'd load the JSON schema

    // Validate injection configuration
    if frontmatter.inject {
        if frontmatter.to.is_none() {
            issues.push("Injection mode requires 'to:' field to specify target file".to_string());
        }

        // Check for conflicting injection modes
        let _injection_modes = [
            frontmatter.before.is_some(),
            frontmatter.after.is_some(),
            frontmatter.prepend,
            frontmatter.append,
            frontmatter.at_line.is_some(),
        ];

        let active_modes: Vec<&str> = [
            (frontmatter.before.is_some(), "before"),
            (frontmatter.after.is_some(), "after"),
            (frontmatter.prepend, "prepend"),
            (frontmatter.append, "append"),
            (frontmatter.at_line.is_some(), "at_line"),
        ]
        .iter()
        .filter_map(|(active, name)| if *active { Some(*name) } else { None })
        .collect();

        if active_modes.len() > 1 {
            issues.push(format!(
                "Multiple injection modes specified: {}. Only one should be used",
                active_modes.join(", ")
            ));
        }

        if active_modes.is_empty() {
            issues.push("Injection mode enabled but no injection method specified (before, after, prepend, append, at_line)".to_string());
        }
    }

    // Validate shell hooks
    if frontmatter.sh_before.is_some() && !frontmatter.inject {
        issues.push("sh_before specified but injection mode is not enabled".to_string());
    }

    if frontmatter.sh_after.is_some() && !frontmatter.inject {
        issues.push("sh_after specified but injection mode is not enabled".to_string());
    }

    // Validate RDF configuration
    if !frontmatter.rdf.is_empty() && !frontmatter.rdf_inline.is_empty() {
        // This is actually fine, but we could warn about it
    }

    // Validate SPARQL configuration
    for (name, query) in &frontmatter.sparql {
        if name.trim().is_empty() {
            issues.push("SPARQL query name cannot be empty".to_string());
        }
        if query.trim().is_empty() {
            issues.push(format!("SPARQL query '{}' is empty", name));
        }
    }
}

fn validate_sparql_queries(
    frontmatter: &rgen_core::template::Frontmatter, issues: &mut Vec<String>,
) {
    for (name, query) in &frontmatter.sparql {
        // Basic SPARQL syntax validation
        if !query.to_uppercase().contains("SELECT")
            && !query.to_uppercase().contains("ASK")
            && !query.to_uppercase().contains("CONSTRUCT")
            && !query.to_uppercase().contains("DESCRIBE")
        {
            issues.push(format!("SPARQL query '{}' does not appear to be a valid query type (SELECT, ASK, CONSTRUCT, DESCRIBE)", name));
        }

        // Check for common syntax issues
        if query.contains("{{") && !query.contains("}}") {
            issues.push(format!(
                "SPARQL query '{}' has unclosed template variable",
                name
            ));
        }

        if query.contains("}}") && !query.contains("{{") {
            issues.push(format!(
                "SPARQL query '{}' has template closing without opening",
                name
            ));
        }
    }
}

fn validate_rdf_content(frontmatter: &rgen_core::template::Frontmatter, issues: &mut Vec<String>) {
    // Validate inline RDF content
    for (i, rdf_content) in frontmatter.rdf_inline.iter().enumerate() {
        if rdf_content.trim().is_empty() {
            issues.push(format!("Inline RDF block {} is empty", i + 1));
        }

        // Basic Turtle syntax validation
        if rdf_content.contains("@prefix") && !rdf_content.contains(" .") {
            issues.push(format!(
                "Inline RDF block {} has @prefix without proper termination",
                i + 1
            ));
        }
    }

    // Validate RDF file references
    for (i, rdf_file) in frontmatter.rdf.iter().enumerate() {
        if rdf_file.trim().is_empty() {
            issues.push(format!("RDF file reference {} is empty", i + 1));
        }
    }
}

fn validate_shacl(frontmatter: &rgen_core::template::Frontmatter, issues: &mut Vec<String>) {
    // Early validation checks
    if frontmatter.rdf.is_empty() && frontmatter.rdf_inline.is_empty() {
        issues.push("SHACL validation requested but no RDF data is available".to_string());
        return;
    }

    if frontmatter.shape.is_empty() {
        issues.push("SHACL validation requested but no shape files specified".to_string());
        return;
    }

    // Use a single graph for both data and shapes to avoid duplication
    let mut combined_graph = match Graph::new() {
        Ok(g) => g,
        Err(e) => {
            issues.push(format!(
                "Failed to initialize graph for SHACL validation: {}",
                e
            ));
            return;
        }
    };

    // Load RDF data and shapes into the same graph
    if let Err(e) = load_rdf_data_into_graph(frontmatter, &mut combined_graph) {
        issues.push(format!(
            "Failed to load RDF data for SHACL validation: {}",
            e
        ));
        return;
    }

    if let Err(e) = load_shacl_shapes_into_graph(frontmatter, &mut combined_graph) {
        issues.push(format!("Failed to load SHACL shapes: {}", e));
        return;
    }

    // Perform optimized SHACL validation
    match perform_optimized_shacl_validation(&combined_graph) {
        Ok(validation_results) => {
            if !validation_results.is_empty() {
                for result in validation_results {
                    issues.push(format!("SHACL validation error: {}", result));
                }
            }
        }
        Err(e) => {
            issues.push(format!("SHACL validation failed: {}", e));
        }
    }
}

fn load_rdf_data_into_graph(
    frontmatter: &rgen_core::template::Frontmatter, graph: &mut Graph,
) -> Result<()> {
    // Load inline RDF data first (usually smaller and faster)
    for rdf_content in &frontmatter.rdf_inline {
        if !rdf_content.trim().is_empty() {
            graph.insert_turtle(rdf_content)?;
        }
    }

    // Load RDF files
    for rdf_file in &frontmatter.rdf {
        if !rdf_file.trim().is_empty() {
            graph.load_path(rdf_file)?;
        }
    }

    Ok(())
}

fn load_shacl_shapes_into_graph(
    frontmatter: &rgen_core::template::Frontmatter, graph: &mut Graph,
) -> Result<()> {
    // Load shape files
    for shape_file in &frontmatter.shape {
        if !shape_file.trim().is_empty() {
            graph.load_path(shape_file)?;
        }
    }

    Ok(())
}

fn perform_optimized_shacl_validation(combined_graph: &Graph) -> Result<Vec<String>> {
    let mut validation_errors = Vec::new();

    // Early exit if graph is empty
    if combined_graph.is_empty() {
        validation_errors
            .push("Combined graph is empty - no data or shapes to validate".to_string());
        return Ok(validation_errors);
    }

    // Use cached queries for better performance
    let shapes_query = "SELECT ?shape WHERE { ?shape <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://www.w3.org/ns/shacl#NodeShape> }";
    let shapes_result = combined_graph.query_cached(shapes_query)?;

    match shapes_result {
        rgen_core::graph::CachedResult::Solutions(solutions) => {
            if solutions.is_empty() {
                validation_errors.push("No SHACL NodeShapes found in shapes graph".to_string());
            } else {
                // Validate each shape against the data
                for shape_solution in solutions {
                    if let Some(shape_iri) = shape_solution.get("shape") {
                        if let Err(e) = validate_single_shape(combined_graph, shape_iri) {
                            validation_errors
                                .push(format!("Shape validation error for {}: {}", shape_iri, e));
                        }
                    }
                }
            }
        }
        _ => {
            validation_errors.push("Failed to query for SHACL shapes".to_string());
        }
    }

    Ok(validation_errors)
}

fn validate_single_shape(graph: &Graph, shape_iri: &str) -> Result<()> {
    // Basic shape validation - check if shape has required properties
    let properties_query = format!(
        "SELECT ?property WHERE {{ <{}> <http://www.w3.org/ns/shacl#property> ?property }}",
        shape_iri
    );

    let properties_result = graph.query_cached(&properties_query)?;
    match properties_result {
        rgen_core::graph::CachedResult::Solutions(properties) => {
            // Validate each property constraint
            for property_solution in properties {
                if let Some(property_iri) = property_solution.get("property") {
                    validate_property_constraint(graph, property_iri)?;
                }
            }
        }
        _ => {
            // No properties defined for this shape - this might be valid
        }
    }

    Ok(())
}

fn validate_property_constraint(graph: &Graph, property_iri: &str) -> Result<()> {
    // Check for common SHACL property constraints
    let min_count_query = format!(
        "ASK WHERE {{ <{}> <http://www.w3.org/ns/shacl#minCount> ?minCount }}",
        property_iri
    );

    let min_count_result = graph.query_cached(&min_count_query)?;
    match min_count_result {
        rgen_core::graph::CachedResult::Boolean(true) => {
            // Property has minCount constraint - would need to validate against data
            // For now, just note that the constraint exists
        }
        _ => {
            // No minCount constraint
        }
    }

    Ok(())
}

fn insert_env(ctx: &mut Context) {
    let mut env_map: BTreeMap<String, String> = BTreeMap::new();
    for (k, v) in std::env::vars() {
        env_map.insert(k, v);
    }
    ctx.insert("env", &env_map);

    if let Ok(cwd) = std::env::current_dir() {
        ctx.insert("cwd", &cwd.display().to_string());
    }
}
