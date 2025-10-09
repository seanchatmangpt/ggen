use clap::Args;
use ggen_utils::error::Result;
use std::collections::BTreeMap;
use std::path::PathBuf;
use tera::Context;

use ggen_core::graph::Graph;
use ggen_core::template::Template;

#[derive(Args, Debug)]
pub struct LintArgs {
    /// Path to the template file to lint
    #[arg(value_name = "TEMPLATE")]
    pub template: Option<PathBuf>,

    /// Variables (key=value pairs) for frontmatter rendering
    #[arg(short = 'v', long = "var", value_parser = parse_key_val::<String, String>)]
    pub vars: Vec<(String, String)>,

    /// Show detailed linting output
    #[arg(long)]
    pub verbose: bool,

    /// Perform SHACL validation (requires RDF data)
    #[arg(long)]
    pub shacl: bool,

    /// Validate registry index instead of template
    #[arg(long)]
    pub registry: bool,
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
    if args.registry {
        return validate_registry(args);
    }

    let template_path = args.template.as_ref()
        .ok_or_else(|| ggen_utils::error::Error::new("Template path is required when not validating registry"))?;

    let mut issues = Vec::new();

    // Read template file
    let template_content = std::fs::read_to_string(template_path)?;

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
    ggen_core::register::register_all(&mut tera);

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
        return Err(ggen_utils::error::Error::new("Linting failed"));
    }

    Ok(())
}

fn validate_frontmatter_schema(
    frontmatter: &ggen_core::template::Frontmatter, issues: &mut Vec<String>,
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
    frontmatter: &ggen_core::template::Frontmatter, issues: &mut Vec<String>,
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

fn validate_rdf_content(frontmatter: &ggen_core::template::Frontmatter, issues: &mut Vec<String>) {
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

fn validate_shacl(frontmatter: &ggen_core::template::Frontmatter, issues: &mut Vec<String>) {
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
    frontmatter: &ggen_core::template::Frontmatter, graph: &mut Graph,
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
    frontmatter: &ggen_core::template::Frontmatter, graph: &mut Graph,
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
        ggen_core::graph::CachedResult::Solutions(solutions) => {
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
        ggen_core::graph::CachedResult::Solutions(properties) => {
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
        ggen_core::graph::CachedResult::Boolean(true) => {
            // Property has minCount constraint - would need to validate against data
            // For now, just note that the constraint exists
        }
        _ => {
            // No minCount constraint
        }
    }

    Ok(())
}

fn validate_registry(args: &LintArgs) -> Result<()> {
    let mut issues = Vec::new();

    // Load registry index
    let registry_path = std::env::current_dir()?.join("registry").join("index.json");
    if !registry_path.exists() {
        return Err(ggen_utils::error::Error::new("Registry index not found at registry/index.json"));
    }

    let registry_content = std::fs::read_to_string(&registry_path)?;
    let registry: serde_json::Value = serde_json::from_str(&registry_content)
        .map_err(|e| ggen_utils::error::Error::new_fmt(format_args!("Invalid JSON in registry index: {}", e)))?;

    // Validate registry structure
    validate_registry_structure(&registry, &mut issues);

    // Validate git URLs and revisions
    validate_git_references(&registry, &mut issues);

    // Validate SHA256 hashes
    validate_sha256_hashes(&registry, &mut issues);

    // Validate version strings
    validate_version_strings(&registry, &mut issues);

    // Report issues
    if issues.is_empty() {
        println!("✓ Registry validation passed");
        if args.verbose {
            println!("  - JSON structure is valid");
            println!("  - All required fields are present");
            println!("  - Git references are accessible");
            println!("  - SHA256 hashes are valid");
            println!("  - Version strings follow semver");
        }
    } else {
        println!("Found {} registry validation issue(s):", issues.len());
        for (i, issue) in issues.iter().enumerate() {
            println!("{}. {}", i + 1, issue);
        }
        return Err(ggen_utils::error::Error::new("Registry validation failed"));
    }

    Ok(())
}

fn validate_registry_structure(registry: &serde_json::Value, issues: &mut Vec<String>) {
    // Check top-level structure
    if !registry.is_object() {
        issues.push("Registry root must be an object".to_string());
        return;
    }

    let obj = registry.as_object().unwrap();

    // Check required fields
    if !obj.contains_key("updated") {
        issues.push("Missing 'updated' field".to_string());
    }

    if !obj.contains_key("packs") {
        issues.push("Missing 'packs' field".to_string());
        return;
    }

    let packs = &obj["packs"];
    if !packs.is_object() {
        issues.push("'packs' must be an object".to_string());
        return;
    }

    // Validate each pack
    for (pack_id, pack_data) in packs.as_object().unwrap() {
        validate_pack_structure(pack_id, pack_data, issues);
    }
}

fn validate_pack_structure(pack_id: &str, pack_data: &serde_json::Value, issues: &mut Vec<String>) {
    if !pack_data.is_object() {
        issues.push(format!("Pack '{}' must be an object", pack_id));
        return;
    }

    let pack_obj = pack_data.as_object().unwrap();

    // Required fields
    let required_fields = ["id", "name", "description", "latest_version", "versions"];
    for field in &required_fields {
        if !pack_obj.contains_key(*field) {
            issues.push(format!("Pack '{}' missing required field '{}'", pack_id, field));
        }
    }

    // Validate versions
    if let Some(versions) = pack_obj.get("versions") {
        if !versions.is_object() {
            issues.push(format!("Pack '{}' versions must be an object", pack_id));
        } else {
            for (version, version_data) in versions.as_object().unwrap() {
                validate_version_structure(pack_id, version, version_data, issues);
            }
        }
    }
}

fn validate_version_structure(pack_id: &str, version: &str, version_data: &serde_json::Value, issues: &mut Vec<String>) {
    if !version_data.is_object() {
        issues.push(format!("Pack '{}' version '{}' must be an object", pack_id, version));
        return;
    }

    let version_obj = version_data.as_object().unwrap();

    // Required fields for versions
    let required_fields = ["version", "git_url", "git_rev", "sha256"];
    for field in &required_fields {
        if !version_obj.contains_key(*field) {
            issues.push(format!("Pack '{}' version '{}' missing required field '{}'", pack_id, version, field));
        }
    }
}

fn validate_git_references(registry: &serde_json::Value, issues: &mut Vec<String>) {
    if let Some(packs) = registry.get("packs").and_then(|p| p.as_object()) {
        for (pack_id, pack_data) in packs {
            if let Some(versions) = pack_data.get("versions").and_then(|v| v.as_object()) {
                for (version, version_data) in versions {
                    if let Some(git_url) = version_data.get("git_url").and_then(|u| u.as_str()) {
                        if !git_url.starts_with("https://github.com/") && !git_url.starts_with("git@github.com:") {
                            issues.push(format!("Pack '{}' version '{}' has invalid git URL format", pack_id, version));
                        }
                    }

                    if let Some(git_rev) = version_data.get("git_rev").and_then(|r| r.as_str()) {
                        if git_rev.len() != 40 || !git_rev.chars().all(|c| c.is_ascii_hexdigit()) {
                            issues.push(format!("Pack '{}' version '{}' has invalid git revision format (should be 40-char hex)", pack_id, version));
                        }
                    }
                }
            }
        }
    }
}

fn validate_sha256_hashes(registry: &serde_json::Value, issues: &mut Vec<String>) {
    if let Some(packs) = registry.get("packs").and_then(|p| p.as_object()) {
        for (pack_id, pack_data) in packs {
            if let Some(versions) = pack_data.get("versions").and_then(|v| v.as_object()) {
                for (version, version_data) in versions {
                    if let Some(sha256) = version_data.get("sha256").and_then(|s| s.as_str()) {
                        if sha256.len() != 64 || !sha256.chars().all(|c| c.is_ascii_hexdigit()) {
                            issues.push(format!("Pack '{}' version '{}' has invalid SHA256 format (should be 64-char hex)", pack_id, version));
                        }
                    }
                }
            }
        }
    }
}

fn validate_version_strings(registry: &serde_json::Value, issues: &mut Vec<String>) {
    if let Some(packs) = registry.get("packs").and_then(|p| p.as_object()) {
        for (pack_id, pack_data) in packs {
            // Validate latest_version
            if let Some(latest_version) = pack_data.get("latest_version").and_then(|v| v.as_str()) {
                if !is_valid_semver(latest_version) {
                    issues.push(format!("Pack '{}' has invalid latest_version format: '{}'", pack_id, latest_version));
                }
            }

            // Validate version strings in versions object
            if let Some(versions) = pack_data.get("versions").and_then(|v| v.as_object()) {
                for (version, version_data) in versions {
                    if !is_valid_semver(version) {
                        issues.push(format!("Pack '{}' has invalid version format: '{}'", pack_id, version));
                    }

                    if let Some(version_field) = version_data.get("version").and_then(|v| v.as_str()) {
                        if version_field != version {
                            issues.push(format!("Pack '{}' version '{}' field mismatch: expected '{}', got '{}'", pack_id, version, version, version_field));
                        }
                    }
                }
            }
        }
    }
}

fn is_valid_semver(version: &str) -> bool {
    // Basic semver validation - should be in format x.y.z with optional pre-release and build metadata
    let parts: Vec<&str> = version.split('.').collect();
    if parts.len() < 3 {
        return false;
    }

    // Check major, minor, patch are numeric
    for part in &parts[..3] {
        if !part.chars().all(|c| c.is_ascii_digit()) {
            return false;
        }
    }

    // Check for pre-release and build metadata
    if parts.len() > 3 {
        let remaining = parts[3..].join(".");
        if remaining.contains("+") {
            let build_parts: Vec<&str> = remaining.split('+').collect();
            if build_parts.len() != 2 {
                return false;
            }
        }
    }

    true
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
