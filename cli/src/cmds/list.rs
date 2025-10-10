use ggen_utils::error::Result;
use rayon::prelude::*;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use walkdir::WalkDir;

// Cache for template metadata to avoid re-parsing
static mut TEMPLATE_CACHE: Option<Arc<std::collections::HashMap<PathBuf, TemplateInfo>>> = None;
static CACHE_INIT: std::sync::Once = std::sync::Once::new();

#[derive(Debug, Clone)]
struct TemplateInfo {
    relative_path: String,
    output_path: Option<String>,
    variables: Vec<(String, String)>,
    rdf_files: usize,
    sparql_queries: usize,
}

pub fn run() -> Result<()> {
    let templates_dir = find_templates_directory()?;

    println!("Available templates:");
    println!("===================");

    // Use cached template info if available
    let template_infos = get_cached_template_infos(&templates_dir)?;

    if template_infos.is_empty() {
        println!("No templates found in {}", templates_dir.display());
    } else {
        // Display templates in parallel for better performance
        template_infos.par_iter().for_each(|(_, info)| {
            display_template_info_cached(info);
        });

        println!("\nTotal: {} template(s)", template_infos.len());
    }

    Ok(())
}

fn find_templates_directory() -> Result<PathBuf> {
    // Look for templates directory in common locations
    let possible_paths = [
        PathBuf::from("templates"),
        PathBuf::from("examples"),
        PathBuf::from("../templates"),
        PathBuf::from("../examples"),
    ];

    for path in &possible_paths {
        if path.exists() && path.is_dir() {
            return Ok(path.clone());
        }
    }

    Err(ggen_utils::error::Error::new(
        "No templates directory found. Please ensure you're in a project with templates.",
    ))
}

fn get_cached_template_infos(
    templates_dir: &Path,
) -> Result<Arc<std::collections::HashMap<PathBuf, TemplateInfo>>> {
    CACHE_INIT.call_once(|| unsafe {
        TEMPLATE_CACHE = Some(Arc::new(std::collections::HashMap::new()));
    });

    unsafe {
        #[allow(static_mut_refs)]
        if TEMPLATE_CACHE.is_some() {
            #[allow(static_mut_refs)]
            let cache = TEMPLATE_CACHE.as_ref().unwrap();
            if !cache.is_empty() {
                return Ok(cache.clone());
            }
        }
    }

    // Build cache by scanning templates directory
    let mut template_infos = std::collections::HashMap::new();

    // Collect all .tmpl files first
    let template_paths: Vec<_> = WalkDir::new(templates_dir)
        .follow_links(true)
        .into_iter()
        .filter_map(|e| e.ok())
        .filter(|entry| entry.path().extension().is_some_and(|ext| ext == "tmpl"))
        .map(|entry| entry.path().to_path_buf())
        .collect();

    // Process templates in parallel
    let parsed_infos: Vec<_> = template_paths
        .par_iter()
        .filter_map(|template_path| parse_template_info(template_path, templates_dir).ok())
        .collect();

    // Build the cache
    for (path, info) in parsed_infos {
        template_infos.insert(path, info);
    }

    let cache = Arc::new(template_infos);
    unsafe {
        TEMPLATE_CACHE = Some(cache.clone());
    }

    Ok(cache)
}

fn parse_template_info(template_path: &Path, base_dir: &Path) -> Result<(PathBuf, TemplateInfo)> {
    let relative_path = template_path
        .strip_prefix(base_dir)
        .unwrap_or(template_path)
        .to_string_lossy()
        .to_string();

    let content = std::fs::read_to_string(template_path)?;

    if let Some(frontmatter) = extract_frontmatter(&content) {
        let output_path = frontmatter
            .get("to")
            .and_then(|v| v.as_str())
            .map(|s| s.to_string());

        let variables: Vec<(String, String)> = frontmatter
            .get("vars")
            .and_then(|v| v.as_object())
            .map(|vars_map| {
                vars_map
                    .iter()
                    .map(|(k, v)| (k.clone(), v.as_str().unwrap_or("").to_string()))
                    .collect()
            })
            .unwrap_or_default();

        let rdf_files = frontmatter
            .get("rdf")
            .and_then(|v| v.as_array())
            .map(|arr| arr.len())
            .unwrap_or(0);

        let sparql_queries = frontmatter
            .get("sparql")
            .and_then(|v| v.as_array())
            .map(|arr| arr.len())
            .unwrap_or(0);

        Ok((
            template_path.to_path_buf(),
            TemplateInfo {
                relative_path,
                output_path,
                variables,
                rdf_files,
                sparql_queries,
            },
        ))
    } else {
        Ok((
            template_path.to_path_buf(),
            TemplateInfo {
                relative_path,
                output_path: None,
                variables: Vec::new(),
                rdf_files: 0,
                sparql_queries: 0,
            },
        ))
    }
}

fn display_template_info_cached(info: &TemplateInfo) {
    println!("\n📄 {}", info.relative_path);

    if let Some(output) = &info.output_path {
        println!("   Output: {}", output);
    }

    if !info.variables.is_empty() {
        println!("   Variables:");
        for (key, value) in &info.variables {
            println!("     {}: {}", key, value);
        }
    }

    if info.rdf_files > 0 {
        println!("   RDF files: {}", info.rdf_files);
    }

    if info.sparql_queries > 0 {
        println!("   SPARQL queries: {}", info.sparql_queries);
    }
}

fn extract_frontmatter(content: &str) -> Option<serde_json::Value> {
    // Look for YAML frontmatter between --- markers
    if let Some(stripped) = content.strip_prefix("---\n") {
        if let Some(end_marker) = stripped.find("\n---\n") {
            let yaml_content = &content[4..4 + end_marker];
            serde_yaml::from_str::<serde_json::Value>(yaml_content).ok()
        } else {
            None
        }
    } else {
        None
    }
}
