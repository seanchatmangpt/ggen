use std::path::{Path, PathBuf};
use tera::{Context, Tera};
use serde::Deserialize;
use crate::{catalog::RepoCatalogEntry, error::{DaemonError, Result}};

/// Minimal ggen.toml rule representation for direct generation.
#[derive(Debug, Deserialize)]
struct SpecManifest {
    #[serde(default)]
    generation: GenerationSection,
}

#[derive(Debug, Default, Deserialize)]
struct GenerationSection {
    #[serde(default)]
    rules: Vec<GenerationRule>,
}

#[derive(Debug, Deserialize)]
struct GenerationRule {
    template: String,
    output_file: String,
    #[serde(default)]
    mode: String,
}

/// Render all generation rules from a spec manifest for a specific repo.
/// Writes output files to `target_dir`. Returns list of files written.
///
/// `ggen_root` — the ggen repository root (where .specify/ lives)
/// `spec_manifest` — path to ggen.toml, relative to ggen_root
/// `entry` — repo catalog metadata (name, language, description, URL)
/// `target_dir` — local clone of the target repo; files are written here
pub fn generate_bundle(
    ggen_root: &Path,
    spec_manifest: &str,
    entry: &RepoCatalogEntry,
    target_dir: &Path,
) -> Result<Vec<PathBuf>> {
    let manifest_path = ggen_root.join(spec_manifest);
    let manifest_content = std::fs::read_to_string(&manifest_path)?;
    let manifest: SpecManifest = toml::from_str(&manifest_content)
        .map_err(|e| DaemonError::Scheduler(format!("manifest parse error: {e}")))?;

    let ctx = build_context(entry);
    let mut written = Vec::new();

    for rule in &manifest.generation.rules {
        // Skip Update-mode rules that can't overwrite existing files
        if rule.mode == "Update" && !rule.output_file.contains("{{") {
            let dest = target_dir.join(&rule.output_file);
            if dest.exists() {
                continue; // respect Update mode: don't overwrite
            }
        }

        // Resolve template path relative to ggen_root
        let template_path = ggen_root.join(&rule.template);
        let template_src = match std::fs::read_to_string(&template_path) {
            Ok(s) => s,
            Err(e) => {
                tracing::warn!("skipping template {}: {}", rule.template, e);
                continue;
            }
        };

        // Render template
        let rendered = render_template(&template_src, &ctx, &rule.template)
            .unwrap_or_else(|e| {
                tracing::warn!("template render error {}: {}", rule.template, e);
                String::new()
            });
        if rendered.is_empty() { continue; }

        // Resolve output filename (Tera-expand if needed)
        let output_name = if rule.output_file.contains("{{") {
            render_template(&rule.output_file, &ctx, "output_file")
                .unwrap_or_else(|_| entry.name.clone())
        } else {
            rule.output_file.clone()
        };

        let dest = target_dir.join(&output_name);
        if let Some(parent) = dest.parent() {
            std::fs::create_dir_all(parent)?;
        }
        std::fs::write(&dest, rendered.as_bytes())?;
        written.push(dest);
    }

    Ok(written)
}

fn build_context(entry: &RepoCatalogEntry) -> Context {
    let github_repo_url = format!("https://github.com/seanchatmangpt/{}", entry.name);
    let homepage = if entry.github_url.is_empty() { github_repo_url.clone() } else { entry.github_url.clone() };
    let mut ctx = Context::new();
    ctx.insert("repoName", &entry.name);
    ctx.insert("shortdesc", &entry.short_desc);
    ctx.insert("homepage", &homepage);
    ctx.insert("primaryLanguage", &entry.primary_language.clone().unwrap_or_default());
    ctx.insert("authorName", "seanchatmangpt");
    ctx.insert("maintainerEmail", "xpointsh@gmail.com");
    ctx.insert("licenseId", "MIT");
    ctx.insert("spdxId", "MIT");
    ctx.insert("year", "2026");
    ctx.insert("created", "2026-06-24");
    // Always use the GitHub Issues URL (not the homepage) for bug tracking
    ctx.insert("bugTracker", &format!("{}/issues", github_repo_url));
    ctx.insert("projectName", &entry.name);
    // Empty array for templates that iterate SPARQL query results;
    // they will render but produce no rows (graceful no-op).
    let empty: Vec<serde_json::Value> = vec![];
    ctx.insert("results", &empty);
    ctx
}

fn render_template(src: &str, ctx: &Context, name: &str) -> std::result::Result<String, String> {
    let mut tera = Tera::default();
    tera.add_raw_template(name, src)
        .map_err(|e| e.to_string())?;
    tera.render(name, ctx).map_err(|e| e.to_string())
}
