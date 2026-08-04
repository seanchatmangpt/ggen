//! `ggen_capability_status` — report which declared-but-inert `ggen.toml`
//! fields a project is relying on, BEFORE the pipeline refuses them.
//!
//! Closes a verified friction point: `TemplateSource::Pack` / `Git` /
//! `Package` deserialize fine (the TOML schema accepts them), but the
//! declarative-rules generator refuses each with `[FM-GEN-007] ... is not
//! implemented yet` — and only at USE time, after an author may have
//! written many rules against a field that was never going to work.
//!
//! This tool does not merely print a static list: it reads the project's
//! own manifest and reports which inert fields are ACTUALLY in use, and by
//! which rule.

use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;

#[derive(Debug, Deserialize, JsonSchema)]
pub struct CapabilityStatusParams {
    /// Project root directory (containing `ggen.toml`).
    pub root: String,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct InertField {
    /// The `ggen.toml` field/variant that is accepted structurally but not
    /// implemented.
    pub field: String,
    /// The typed diagnostic code the pipeline raises when it is used.
    pub code: String,
    /// A human-readable summary of the pipeline's refusal -- NOT verbatim.
    /// The real per-rule message
    /// (`ggen_engine::generation_rules::resolve_template_source`)
    /// additionally names the offending rule and the pack/git/package
    /// identifier (e.g. "rule `{name}`: TemplateSource::Pack (pack
    /// `{pack}`) is not implemented yet. Remediation: ..."); this field is
    /// a fixed summary shared across every rule that triggers the same
    /// variant.
    pub reason: String,
    /// Where the follow-up work is tracked.
    pub tracked_at: String,
    /// Rules in THIS project that use it. Empty means the project is not
    /// currently affected -- the field is still inert, but nothing here
    /// depends on it yet.
    pub used_by_rules: Vec<String>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct CapabilityStatusResult {
    pub ok: bool,
    /// `true` when at least one inert field is actually used by this
    /// project -- i.e. `ggen sync run` WILL refuse.
    pub project_is_affected: bool,
    pub inert_fields: Vec<InertField>,
}

const TRACKED_AT: &str = "specs/014-ggen-core-replacement/tasks.md";

/// Report inert-capability status for `root`.
///
/// # Errors
/// `ErrorCategory::PathTraversal` for an unresolvable `root`;
/// `ErrorCategory::NotFound` when `ggen.toml` is unreadable;
/// `ErrorCategory::ConfigError` when it cannot be parsed as a manifest.
pub fn capability_status(
    params: &CapabilityStatusParams,
) -> Result<CapabilityStatusResult, McpError> {
    let root = resolve_root(&params.root)?;
    let manifest_path = root.join("ggen.toml");
    let raw = std::fs::read_to_string(&manifest_path).map_err(|e| {
        McpError::new(
            ErrorCategory::NotFound,
            format!("{} unreadable: {e}", manifest_path.display()),
        )
    })?;

    // Parse as generic TOML rather than a typed manifest: this tool must
    // work even on a project whose ggen.toml the typed parser would reject,
    // since "which inert field am I depending on" is exactly the question
    // an author asks while the file is still being written.
    let table: toml::Table = raw.parse().map_err(|e| {
        McpError::new(
            ErrorCategory::ConfigError,
            format!("invalid ggen.toml: {e}"),
        )
    })?;

    let mut pack_rules = Vec::new();
    let mut git_rules = Vec::new();
    let mut package_rules = Vec::new();

    if let Some(rules) = table
        .get("generation")
        .and_then(|g| g.get("rules"))
        .and_then(|r| r.as_array())
    {
        for rule in rules {
            let name = rule
                .get("name")
                .and_then(|n| n.as_str())
                .unwrap_or("<unnamed>")
                .to_string();
            let Some(template) = rule.get("template") else {
                continue;
            };
            if template.get("pack").is_some() {
                pack_rules.push(name.clone());
            }
            if template.get("git").is_some() {
                git_rules.push(name.clone());
            }
            if template.get("package").is_some() {
                package_rules.push(name);
            }
        }
    }

    let inert_fields = vec![
        InertField {
            field: "generation.rules[].template.pack (TemplateSource::Pack)".to_string(),
            code: "FM-GEN-007".to_string(),
            reason: "TemplateSource::Pack is not implemented yet. Use \
                     TemplateSource::File or TemplateSource::Inline."
                .to_string(),
            tracked_at: TRACKED_AT.to_string(),
            used_by_rules: pack_rules,
        },
        InertField {
            field: "generation.rules[].template.git (TemplateSource::Git)".to_string(),
            code: "FM-GEN-007".to_string(),
            reason: "TemplateSource::Git is not implemented yet. Vendor the template \
                     locally and use TemplateSource::File."
                .to_string(),
            tracked_at: TRACKED_AT.to_string(),
            used_by_rules: git_rules,
        },
        InertField {
            field: "generation.rules[].template.package (TemplateSource::Package)".to_string(),
            code: "FM-GEN-007".to_string(),
            reason: "TemplateSource::Package is not implemented yet. Vendor the \
                     template locally and use TemplateSource::File."
                .to_string(),
            tracked_at: TRACKED_AT.to_string(),
            used_by_rules: package_rules,
        },
    ];

    let project_is_affected = inert_fields.iter().any(|f| !f.used_by_rules.is_empty());
    Ok(CapabilityStatusResult {
        ok: true,
        project_is_affected,
        inert_fields,
    })
}
