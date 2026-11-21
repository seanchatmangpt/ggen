use std::collections::BTreeSet;
use std::path::{Path, PathBuf};

use ggen_marketplace::packs_registry::composer::{compose, PackCompositionPlan};
use ggen_marketplace::packs_registry::metadata::pack_file_from_dir;
use ggen_marketplace::packs_registry::types::PackFile;
use schemars::JsonSchema;
use serde::{Deserialize, Serialize};

use crate::error::{ErrorCategory, McpError};
use crate::project_root::resolve_root;

const CAPABILITY_CORPUS_ROOTS: [&str; 2] =
    ["/Users/sac/ggen-marketplace/packs", "/Users/sac/ggen/packs"];

#[derive(Debug, Deserialize, JsonSchema)]
pub struct PackComposeParams {
    #[serde(default)]
    pub root: Option<String>,
    #[serde(default)]
    pub packs: Option<Vec<String>>,
    #[serde(default)]
    pub pack_dirs: Option<Vec<String>>,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct PackComposeResult {
    pub ok: bool,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub plan: Option<PackCompositionPlanJson>,
    #[serde(skip_serializing_if = "Option::is_none")]
    pub refusal: Option<String>,
    pub unresolved_packs: Vec<String>,
    pub packs_loaded: usize,
}

#[derive(Debug, Serialize, JsonSchema)]
pub struct PackCompositionPlanJson {
    pub pack_ids: Vec<String>,
    pub provides: serde_json::Value,
    pub order: Vec<String>,
    pub artifact_paths: serde_json::Value,
    pub transitive_deps: serde_json::Value,
    pub self_satisfied: Vec<String>,
}

pub fn pack_compose(params: &PackComposeParams) -> Result<PackComposeResult, McpError> {
    let project_root = match &params.root {
        Some(r) => Some(resolve_root(r)?),
        None if params.packs.is_some() || params.pack_dirs.is_some() => None,
        None => {
            return Err(McpError::new(
                ErrorCategory::SyntaxError,
                "root required when packs omitted".to_string(),
            ));
        }
    };
    let mut pack_names = params.packs.clone().unwrap_or_default();
    if pack_names.is_empty() {
        if let Some(root) = &project_root {
            pack_names = discover_packs(root)?;
        }
    }
    if pack_names.is_empty() && params.pack_dirs.as_ref().is_none_or(Vec::is_empty) {
        return Err(McpError::new(
            ErrorCategory::SyntaxError,
            "no packs to compose".to_string(),
        ));
    }
    let mut seen = BTreeSet::new();
    for n in &pack_names {
        if !seen.insert(n.as_str()) {
            return Err(McpError::new(
                ErrorCategory::SyntaxError,
                format!("duplicate pack '{n}'"),
            ));
        }
    }
    let mut pack_files = Vec::new();
    let mut unresolved = Vec::new();
    for name in &pack_names {
        let Some(toml) = find_pack_toml(project_root.as_deref(), name) else {
            unresolved.push(name.clone());
            continue;
        };
        let dir = toml.parent().ok_or_else(|| {
            McpError::new(ErrorCategory::NotFound, format!("pack '{name}' has no parent"))
        })?;
        pack_files.push(load_pack_dir(dir, name)?);
    }
    if let Some(dirs) = &params.pack_dirs {
        for rel in dirs {
            let dir = if let Some(r) = &project_root {
                crate::project_root::resolve_relative(r, rel)?
            } else {
                PathBuf::from(rel)
            };
            let id = dir.file_name().and_then(|s| s.to_str()).unwrap_or("x").to_string();
            if !seen.insert(id.clone()) {
                return Err(McpError::new(
                    ErrorCategory::SyntaxError,
                    format!("duplicate pack dir '{id}'"),
                ));
            }
            pack_files.push(load_pack_dir(&dir, &id)?);
        }
    }
    if !unresolved.is_empty() && pack_files.is_empty() {
        return Ok(PackComposeResult {
            ok: false,
            plan: None,
            refusal: Some(format!("missing packs: {}", unresolved.join(", "))),
            unresolved_packs: unresolved,
            packs_loaded: 0,
        });
    }
    match compose(&pack_files) {
        Ok(plan) => Ok(PackComposeResult {
            ok: true,
            plan: Some(to_json(&plan)),
            refusal: None,
            unresolved_packs: unresolved,
            packs_loaded: pack_files.len(),
        }),
        Err(r) => Ok(PackComposeResult {
            ok: false,
            plan: None,
            refusal: Some(r.to_string()),
            unresolved_packs: unresolved,
            packs_loaded: pack_files.len(),
        }),
    }
}

fn to_json(plan: &PackCompositionPlan) -> PackCompositionPlanJson {
    PackCompositionPlanJson {
        pack_ids: plan.pack_ids.iter().cloned().collect(),
        provides: serde_json::to_value(&plan.provides).unwrap_or_default(),
        order: plan.order.clone(),
        artifact_paths: serde_json::to_value(&plan.artifact_paths).unwrap_or_default(),
        transitive_deps: serde_json::to_value(&plan.transitive_deps).unwrap_or_default(),
        self_satisfied: plan.self_satisfied.clone(),
    }
}

fn corpus_roots() -> Vec<PathBuf> {
    match std::env::var("GGEN_CAPABILITY_CORPUS_ROOTS") {
        Ok(s) if !s.is_empty() => s.split(':').filter(|p| !p.is_empty()).map(PathBuf::from).collect(),
        _ => CAPABILITY_CORPUS_ROOTS.iter().map(PathBuf::from).collect(),
    }
}

fn workspace_packs() -> Option<PathBuf> {
    let m = std::env::var("CARGO_MANIFEST_DIR").ok()?;
    let ws = PathBuf::from(m).parent()?.parent()?;
    let p = ws.join("packs");
    p.is_dir().then_some(p)
}

fn find_pack_toml(project_root: Option<&Path>, name: &str) -> Option<PathBuf> {
    let mut bases = Vec::new();
    if let Some(r) = project_root {
        bases.push(r.join("packs"));
    }
    if let Some(ws) = workspace_packs() {
        bases.push(ws);
    }
    bases.extend(corpus_roots());
    bases.iter().find_map(|b| {
        let c = b.join(name).join("pack.toml");
        c.is_file().then_some(c)
    })
}

fn load_pack_dir(dir: &Path, ctx: &str) -> Result<PackFile, McpError> {
    pack_file_from_dir(dir).map_err(|e| {
        McpError::new(ErrorCategory::GraphLoadError, format!("load pack '{ctx}': {e}"))
    })
}

fn discover_packs(root: &Path) -> Result<Vec<String>, McpError> {
    let raw = std::fs::read_to_string(root.join("ggen.toml")).map_err(|e| {
        McpError::new(ErrorCategory::NotFound, format!("ggen.toml: {e}"))
    })?;
    let v: toml::Value = star_toml::from_str(&raw).map_err(|e| {
        McpError::new(ErrorCategory::ConfigError, format!("ggen.toml parse: {e}"))
    })?;
    let t = v.as_table().cloned().unwrap_or_default();
    let mut out = Vec::new();
    let mut push = |n: &str| {
        if !out.iter().any(|x| x == n) {
            out.push(n.to_string());
        }
    };
    if let Some(packs) = t.get("packs").and_then(|p| p.as_array()) {
        for e in packs {
            if let Some(n) = e.get("name").and_then(|x| x.as_str()) {
                push(n);
            }
        }
    }
    if let Some(rules) = t.get("generation").and_then(|g| g.get("rules")).and_then(|r| r.as_array())
    {
        for rule in rules {
            if let Some(p) = rule
                .get("template")
                .and_then(|t| t.get("pack"))
                .and_then(|x| x.as_str())
            {
                push(p);
            }
        }
    }
    Ok(out)
}
