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
    /// Declared `[capabilities]` aggregated over the packs THIS project
    /// references (via `generation.rules[].template.pack`), resolved
    /// project-locally (`<root>/packs/<name>/pack.toml`) with fallback to
    /// the shared pack corpora. Absent -- not null -- when no referenced
    /// pack carries `[capabilities]`, so pre-capability consumers see a
    /// byte-stable object.
    #[serde(skip_serializing_if = "Option::is_none")]
    pub capabilities: Option<CapabilitiesStatus>,
}

/// Aggregate declared capability surface of the project's annotated packs.
/// `unsatisfied` lists `requires` URNs not covered by the union of
/// `provides` across those same referenced packs.
#[derive(Debug, Serialize, JsonSchema)]
pub struct CapabilitiesStatus {
    pub provides: Vec<String>,
    pub requires: Vec<String>,
    pub unsatisfied: Vec<String>,
    /// Referenced packs whose `pack.toml` carried `[capabilities]`.
    pub annotated_packs: Vec<String>,
}

const TRACKED_AT: &str = "specs/014-ggen-core-replacement/tasks.md";

/// Same corpora the `ggen pack capabilities` CLI verb scans (searched in
/// the same order, project-local `packs/` wins first).
const CAPABILITY_CORPUS_ROOTS: [&str; 2] =
    ["/Users/sac/ggen-marketplace/packs", "/Users/sac/ggen/packs"];

/// `[capabilities]` of one pack.toml: `provides`/`requires` URN lists.
/// `None` = the key is honestly absent (pack predates annotation).
#[derive(Debug, Default, serde::Deserialize)]
struct DeclaredCapabilities {
    #[serde(default)]
    provides: Vec<String>,
    #[serde(default)]
    requires: Vec<String>,
}

/// Locate `<base>/<name>/pack.toml` -- project-local `packs/` first, then
/// the shared corpora in CLI order.
fn find_pack_toml(root: &std::path::Path, name: &str) -> Option<std::path::PathBuf> {
    let mut bases = vec![root.join("packs")];
    bases.extend(CAPABILITY_CORPUS_ROOTS.iter().map(std::path::PathBuf::from));
    bases.iter().find_map(|base| {
        let candidate = base.join(name).join("pack.toml");
        candidate.is_file().then_some(candidate)
    })
}

/// Parse `[capabilities]` from a pack.toml, tolerating its absence and any
/// other top-level tables. Read errors are treated as absent -- capability
/// surfacing is additive and must never turn a status query into an error.
fn parse_capabilities(path: &std::path::Path) -> Option<DeclaredCapabilities> {
    let content = std::fs::read_to_string(path).ok()?;
    let value: toml::Value = star_toml::from_str(&content).ok()?;
    value
        .get("capabilities")
        .and_then(|c| c.clone().try_into().ok())
}

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
    let value: toml::Value = star_toml::from_str(&raw).map_err(|e| {
        McpError::new(
            ErrorCategory::ConfigError,
            format!("invalid ggen.toml: {e}"),
        )
    })?;
    let table = value.as_table().cloned().unwrap_or_default();

    let mut pack_rules = Vec::new();
    let mut git_rules = Vec::new();
    let mut package_rules = Vec::new();
    let mut referenced_packs: Vec<String> = Vec::new();

    if let Some(rules_value) = table.get("generation").and_then(|g| g.get("rules")) {
        // Fail-closed shape check: `[generation.rules]` (dotted table) is
        // valid TOML but not the array-of-tables form the pipeline
        // consumes. Accepting it silently would report zero rules while
        // the author believes rules are declared -- refuse with a typed
        // error naming the key instead.
        let Some(rules) = rules_value.as_array() else {
            return Err(McpError::new(
                ErrorCategory::ConfigError,
                "generation.rules must be an array of tables ([[generation.rules]]); \
                 found a table. Did you mean [[generation.rules]] instead of \
                 [generation.rules]?"
                    .to_string(),
            ));
        };
        for rule in rules {
            let name = rule
                .get("name")
                .and_then(|n| n.as_str())
                .unwrap_or("<unnamed>")
                .to_string();
            let Some(template) = rule.get("template") else {
                continue;
            };
            if let Some(pack) = template.get("pack").and_then(|p| p.as_str()) {
                pack_rules.push(name.clone());
                if !referenced_packs.contains(&pack.to_string()) {
                    referenced_packs.push(pack.to_string());
                }
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

    // Capability surface: aggregate `[capabilities]` over the referenced
    // packs only. No annotated pack => the key is omitted entirely
    // (backward compat: not null).
    let mut capabilities = None;
    let mut annotated = Vec::new();
    let mut provides: Vec<String> = Vec::new();
    let mut requires: Vec<String> = Vec::new();
    for pack in &referenced_packs {
        let Some(path) = find_pack_toml(&root, pack) else {
            continue;
        };
        let Some(caps) = parse_capabilities(&path) else {
            continue;
        };
        annotated.push(pack.clone());
        for p in caps.provides {
            if !provides.contains(&p) {
                provides.push(p);
            }
        }
        for r in caps.requires {
            if !requires.contains(&r) {
                requires.push(r);
            }
        }
    }
    if !annotated.is_empty() {
        provides.sort();
        requires.sort();
        let unsatisfied: Vec<String> = requires
            .iter()
            .filter(|r| !provides.contains(r))
            .cloned()
            .collect();
        annotated.sort();
        capabilities = Some(CapabilitiesStatus {
            provides,
            requires,
            unsatisfied,
            annotated_packs: annotated,
        });
    }

    Ok(CapabilityStatusResult {
        ok: true,
        project_is_affected,
        inert_fields,
        capabilities,
    })
}
