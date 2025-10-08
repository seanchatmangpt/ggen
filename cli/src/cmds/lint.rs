use clap::Args;
use std::collections::BTreeMap;
use std::path::PathBuf;
use tera::Context;
use utils::error::Result;

use core::template::Template;

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
    let pos = s.find('=').ok_or_else(|| format!("invalid KEY=value: no `=` found in `{s}`"))?;
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
    core::register::register_all(&mut tera);
    
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
        return Err(utils::error::Error::new("Linting failed"));
    }
    
    Ok(())
}

fn validate_frontmatter_schema(frontmatter: &core::template::Frontmatter, issues: &mut Vec<String>) {
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

fn validate_sparql_queries(frontmatter: &core::template::Frontmatter, issues: &mut Vec<String>) {
    for (name, query) in &frontmatter.sparql {
        // Basic SPARQL syntax validation
        if !query.to_uppercase().contains("SELECT") 
            && !query.to_uppercase().contains("ASK") 
            && !query.to_uppercase().contains("CONSTRUCT") 
            && !query.to_uppercase().contains("DESCRIBE") {
            issues.push(format!("SPARQL query '{}' does not appear to be a valid query type (SELECT, ASK, CONSTRUCT, DESCRIBE)", name));
        }
        
        // Check for common syntax issues
        if query.contains("{{") && !query.contains("}}") {
            issues.push(format!("SPARQL query '{}' has unclosed template variable", name));
        }
        
        if query.contains("}}") && !query.contains("{{") {
            issues.push(format!("SPARQL query '{}' has template closing without opening", name));
        }
    }
}

fn validate_rdf_content(frontmatter: &core::template::Frontmatter, issues: &mut Vec<String>) {
    // Validate inline RDF content
    for (i, rdf_content) in frontmatter.rdf_inline.iter().enumerate() {
        if rdf_content.trim().is_empty() {
            issues.push(format!("Inline RDF block {} is empty", i + 1));
        }
        
        // Basic Turtle syntax validation
        if rdf_content.contains("@prefix") && !rdf_content.contains(" .") {
            issues.push(format!("Inline RDF block {} has @prefix without proper termination", i + 1));
        }
    }
    
    // Validate RDF file references
    for (i, rdf_file) in frontmatter.rdf.iter().enumerate() {
        if rdf_file.trim().is_empty() {
            issues.push(format!("RDF file reference {} is empty", i + 1));
        }
    }
}

fn validate_shacl(frontmatter: &core::template::Frontmatter, issues: &mut Vec<String>) {
    // This is a placeholder for SHACL validation
    // In a real implementation, you would:
    // 1. Load the RDF data from the template
    // 2. Load SHACL shapes
    // 3. Perform validation
    // 4. Report any constraint violations
    
    if frontmatter.rdf.is_empty() && frontmatter.rdf_inline.is_empty() {
        issues.push("SHACL validation requested but no RDF data is available".to_string());
    }
    
    // For now, just note that SHACL validation is not fully implemented
    issues.push("SHACL validation is not yet fully implemented".to_string());
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
