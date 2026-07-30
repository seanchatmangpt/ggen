//! Fortune 5 deployment building blocks (`ggen bblock <verb>`).
//!
//! The command is a generic compiler over the retained ontology-derived catalog
//! at `packs/fortune5-deployment-blocks-pack/catalog/fortune5-bblocks.json`.
//! Provider aliases, group dependencies, package identities, and output
//! directories come from that catalog; this module contains no provider-specific
//! deployment branches and performs no cloud, Kubernetes, Terraform, or network
//! actuation.

use std::{
    collections::{BTreeMap, BTreeSet},
    fs,
    path::{Component, Path, PathBuf},
};

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use ggen_marketplace::{
    marketplace::models::PackageId,
    packs::lockfile::{LockedPack, PackLockfile, PackSource},
};
use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

const CATALOG_BYTES: &str = include_str!(concat!(
    env!("CARGO_MANIFEST_DIR"),
    "/../../packs/fortune5-deployment-blocks-pack/catalog/fortune5-bblocks.json"
));
const CATALOG_SCHEMA: &str = "ggen.bblock.catalog.v1";
const PLAN_SCHEMA: &str = "ggen.bblock.plan.v1";
const RECEIPT_SCHEMA: &str = "ggen.bblock.receipt.v1";
const REGISTRY_URL: &str = "https://registry.ggen.io";

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Catalog {
    schema: String,
    version: String,
    providers: Vec<Provider>,
    groups: Vec<BlockGroup>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Provider {
    id: String,
    title: String,
    #[serde(default)]
    aliases: Vec<String>,
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct BlockGroup {
    id: String,
    title: String,
    description: String,
    directory: String,
    #[serde(default)]
    dependencies: Vec<String>,
    #[serde(default)]
    common_packs: Vec<String>,
    provider_packs: BTreeMap<String, Vec<String>>,
}

#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
struct BlockPlan {
    schema: &'static str,
    catalog_version: String,
    catalog_digest: String,
    provider: String,
    requested_group: String,
    resolved_groups: Vec<String>,
    directories: Vec<String>,
    packs: Vec<String>,
    plan_digest: String,
}

#[derive(Debug, Clone, Serialize)]
struct ReceiptBody<'a> {
    schema: &'static str,
    operation: &'a str,
    provider: &'a str,
    group: &'a str,
    catalog_digest: &'a str,
    plan_digest: &'a str,
    previous_digest: &'a str,
    artifacts: &'a [String],
}

#[derive(Debug, Clone, Deserialize, Serialize)]
struct Receipt {
    schema: String,
    operation: String,
    provider: String,
    group: String,
    catalog_digest: String,
    plan_digest: String,
    previous_digest: String,
    artifacts: Vec<String>,
    digest_algorithm: String,
    digest: String,
}

impl Receipt {
    fn issue(
        operation: &str, plan: &BlockPlan, previous_digest: &str, artifacts: Vec<String>,
    ) -> Result<Self> {
        let body = ReceiptBody {
            schema: RECEIPT_SCHEMA,
            operation,
            provider: &plan.provider,
            group: &plan.requested_group,
            catalog_digest: &plan.catalog_digest,
            plan_digest: &plan.plan_digest,
            previous_digest,
            artifacts: &artifacts,
        };
        let digest = digest(&body)?;
        Ok(Self {
            schema: RECEIPT_SCHEMA.to_string(),
            operation: operation.to_string(),
            provider: plan.provider.clone(),
            group: plan.requested_group.clone(),
            catalog_digest: plan.catalog_digest.clone(),
            plan_digest: plan.plan_digest.clone(),
            previous_digest: previous_digest.to_string(),
            artifacts,
            digest_algorithm: "blake3".to_string(),
            digest,
        })
    }
}

fn catalog() -> Result<Catalog> {
    let catalog: Catalog = serde_json::from_str(CATALOG_BYTES).map_err(|error| {
        NounVerbError::execution_error(format!("embedded bblock catalog is invalid: {error}"))
    })?;
    validate_catalog(&catalog)?;
    Ok(catalog)
}

fn catalog_digest() -> String {
    blake3::hash(CATALOG_BYTES.as_bytes()).to_hex().to_string()
}

fn validate_catalog(catalog: &Catalog) -> Result<()> {
    if catalog.schema != CATALOG_SCHEMA || catalog.version.trim().is_empty() {
        return Err(NounVerbError::execution_error(
            "bblock catalog schema or version is invalid".to_string(),
        ));
    }

    let provider_ids: BTreeSet<_> = catalog
        .providers
        .iter()
        .map(|item| item.id.as_str())
        .collect();
    let expected = BTreeSet::from(["aws", "azure", "gcp"]);
    if provider_ids != expected || provider_ids.len() != catalog.providers.len() {
        return Err(NounVerbError::execution_error(
            "bblock provider universe must be exactly aws, azure, and gcp".to_string(),
        ));
    }

    let mut provider_names = BTreeSet::new();
    for provider in &catalog.providers {
        if provider.title.trim().is_empty() || !provider_names.insert(provider.id.as_str()) {
            return Err(NounVerbError::execution_error(format!(
                "provider {} has invalid identity or title",
                provider.id
            )));
        }
        for alias in &provider.aliases {
            if alias.trim().is_empty() || !provider_names.insert(alias.as_str()) {
                return Err(NounVerbError::execution_error(format!(
                    "provider alias {alias} is empty or ambiguous"
                )));
            }
        }
    }

    let group_ids: BTreeSet<_> = catalog.groups.iter().map(|item| item.id.as_str()).collect();
    if group_ids.len() != catalog.groups.len() || group_ids.is_empty() {
        return Err(NounVerbError::execution_error(
            "bblock group identities are empty or duplicated".to_string(),
        ));
    }

    let mut directories = BTreeSet::new();
    for group in &catalog.groups {
        validate_pack_id(&group.id)?;
        validate_relative_path(&group.directory)?;
        if group.title.trim().is_empty()
            || group.description.trim().is_empty()
            || !directories.insert(group.directory.as_str())
        {
            return Err(NounVerbError::execution_error(format!(
                "group {} has incomplete metadata or a duplicate directory",
                group.id
            )));
        }
        for dependency in &group.dependencies {
            if !group_ids.contains(dependency.as_str()) {
                return Err(NounVerbError::execution_error(format!(
                    "group {} references unknown dependency {dependency}",
                    group.id
                )));
            }
        }
        for pack in &group.common_packs {
            validate_pack_id(pack)?;
        }
        let keys: BTreeSet<_> = group.provider_packs.keys().map(String::as_str).collect();
        if keys != expected {
            return Err(NounVerbError::execution_error(format!(
                "group {} must declare aws, azure, and gcp pack projections",
                group.id
            )));
        }
        for (provider, packs) in &group.provider_packs {
            if packs.is_empty() {
                return Err(NounVerbError::execution_error(format!(
                    "group {} has zero packs for provider {provider}",
                    group.id
                )));
            }
            for pack in packs {
                validate_pack_id(pack)?;
            }
        }
    }

    for group in &catalog.groups {
        let mut visiting = BTreeSet::new();
        let mut visited = BTreeSet::new();
        visit_group(
            catalog,
            &group.id,
            &mut visiting,
            &mut visited,
            &mut Vec::new(),
        )?;
    }
    Ok(())
}

fn validate_pack_id(pack_id: &str) -> Result<()> {
    PackageId::new(pack_id).map_err(|error| {
        NounVerbError::argument_error(format!("invalid bblock pack id {pack_id}: {error}"))
    })?;
    Ok(())
}

fn validate_relative_path(raw: &str) -> Result<()> {
    let path = Path::new(raw);
    if raw.trim().is_empty()
        || path.is_absolute()
        || path
            .components()
            .any(|component| matches!(component, Component::ParentDir | Component::RootDir))
    {
        return Err(NounVerbError::argument_error(format!(
            "unsafe bblock output directory: {raw}"
        )));
    }
    Ok(())
}

fn normalize_provider<'a>(catalog: &'a Catalog, raw: &str) -> Result<&'a Provider> {
    let normalized = raw.trim().to_ascii_lowercase();
    catalog
        .providers
        .iter()
        .find(|provider| {
            provider.id == normalized || provider.aliases.iter().any(|alias| alias == &normalized)
        })
        .ok_or_else(|| {
            NounVerbError::argument_error(format!(
                "unsupported provider {raw}; expected aws, azure, gcp, or a declared alias"
            ))
        })
}

fn find_group<'a>(catalog: &'a Catalog, id: &str) -> Result<&'a BlockGroup> {
    catalog
        .groups
        .iter()
        .find(|group| group.id == id)
        .ok_or_else(|| NounVerbError::argument_error(format!("unknown bblock group {id}")))
}

fn visit_group(
    catalog: &Catalog, id: &str, visiting: &mut BTreeSet<String>, visited: &mut BTreeSet<String>,
    ordered: &mut Vec<String>,
) -> Result<()> {
    if visited.contains(id) {
        return Ok(());
    }
    if !visiting.insert(id.to_string()) {
        return Err(NounVerbError::execution_error(format!(
            "bblock dependency cycle detected at {id}"
        )));
    }
    let current = find_group(catalog, id)?;
    for dependency in &current.dependencies {
        visit_group(catalog, dependency, visiting, visited, ordered)?;
    }
    visiting.remove(id);
    visited.insert(id.to_string());
    ordered.push(id.to_string());
    Ok(())
}

fn resolve(catalog: &Catalog, group_id: &str, provider_raw: &str) -> Result<BlockPlan> {
    let provider = normalize_provider(catalog, provider_raw)?;
    let mut ordered = Vec::new();
    visit_group(
        catalog,
        group_id,
        &mut BTreeSet::new(),
        &mut BTreeSet::new(),
        &mut ordered,
    )?;

    let mut packs = BTreeSet::new();
    let mut directories = BTreeSet::new();
    for id in &ordered {
        let group = find_group(catalog, id)?;
        directories.insert(group.directory.clone());
        packs.extend(group.common_packs.iter().cloned());
        packs.extend(
            group
                .provider_packs
                .get(&provider.id)
                .into_iter()
                .flatten()
                .cloned(),
        );
    }

    let catalog_digest = catalog_digest();
    #[derive(Serialize)]
    struct DigestInput<'a> {
        schema: &'static str,
        catalog_version: &'a str,
        catalog_digest: &'a str,
        provider: &'a str,
        requested_group: &'a str,
        resolved_groups: &'a [String],
        directories: &'a [String],
        packs: &'a [String],
    }
    let directories: Vec<_> = directories.into_iter().collect();
    let packs: Vec<_> = packs.into_iter().collect();
    let plan_digest = digest(&DigestInput {
        schema: PLAN_SCHEMA,
        catalog_version: &catalog.version,
        catalog_digest: &catalog_digest,
        provider: &provider.id,
        requested_group: group_id,
        resolved_groups: &ordered,
        directories: &directories,
        packs: &packs,
    })?;
    Ok(BlockPlan {
        schema: PLAN_SCHEMA,
        catalog_version: catalog.version.clone(),
        catalog_digest,
        provider: provider.id.clone(),
        requested_group: group_id.to_string(),
        resolved_groups: ordered,
        directories,
        packs,
        plan_digest,
    })
}

fn digest<T: Serialize>(value: &T) -> Result<String> {
    let bytes = serde_json::to_vec(value).map_err(|error| {
        NounVerbError::execution_error(format!("cannot serialize bblock receipt: {error}"))
    })?;
    Ok(blake3::hash(&bytes).to_hex().to_string())
}

fn project_root() -> Result<PathBuf> {
    std::env::current_dir().map_err(|error| {
        NounVerbError::execution_error(format!("cannot resolve project directory: {error}"))
    })
}

fn runtime_root(root: &Path) -> PathBuf {
    root.join(".ggen").join("bblocks")
}

fn write_json<T: Serialize>(path: &Path, value: &T) -> Result<()> {
    if let Some(parent) = path.parent() {
        fs::create_dir_all(parent).map_err(|error| {
            NounVerbError::execution_error(format!("cannot create {}: {error}", parent.display()))
        })?;
    }
    let bytes = serde_json::to_vec_pretty(value).map_err(|error| {
        NounVerbError::execution_error(format!("cannot serialize {}: {error}", path.display()))
    })?;
    let temporary = path.with_extension("tmp");
    fs::write(&temporary, bytes).map_err(|error| {
        NounVerbError::execution_error(format!("cannot write {}: {error}", temporary.display()))
    })?;
    fs::rename(&temporary, path).map_err(|error| {
        NounVerbError::execution_error(format!(
            "cannot atomically replace {}: {error}",
            path.display()
        ))
    })?;
    Ok(())
}

fn previous_receipt_digest(path: &Path) -> Result<String> {
    if !path.is_file() {
        return Ok("GENESIS".to_string());
    }
    let bytes = fs::read(path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot read {}: {error}", path.display()))
    })?;
    let receipt: Receipt = serde_json::from_slice(&bytes).map_err(|error| {
        NounVerbError::execution_error(format!("cannot parse {}: {error}", path.display()))
    })?;
    Ok(receipt.digest)
}

fn plan_paths(root: &Path, plan: &BlockPlan, operation: &str) -> (PathBuf, PathBuf, PathBuf) {
    let runtime = runtime_root(root);
    let plan_path = runtime
        .join("plans")
        .join(&plan.provider)
        .join(format!("{}.json", plan.requested_group));
    let receipt_dir = runtime.join("receipts").join(&plan.provider);
    let intent_path = receipt_dir.join(format!("{}-{operation}-intent.json", plan.requested_group));
    let result_path = receipt_dir.join(format!("{}-{operation}-result.json", plan.requested_group));
    (plan_path, intent_path, result_path)
}

fn write_plan_receipts(root: &Path, plan: &BlockPlan, operation: &str) -> Result<Value> {
    let (plan_path, intent_path, result_path) = plan_paths(root, plan, operation);
    let previous = previous_receipt_digest(&result_path)?;
    let plan_artifact = relative(root, &plan_path);
    let intent = Receipt::issue(
        &format!("{operation}-intent"),
        plan,
        &previous,
        vec![plan_artifact.clone()],
    )?;
    write_json(&intent_path, &intent)?;
    write_json(&plan_path, plan)?;
    let result = Receipt::issue(
        &format!("{operation}-result"),
        plan,
        &intent.digest,
        vec![plan_artifact, relative(root, &intent_path)],
    )?;
    write_json(&result_path, &result)?;
    Ok(json!({
        "plan": plan,
        "plan_path": relative(root, &plan_path),
        "intent_receipt": relative(root, &intent_path),
        "result_receipt": relative(root, &result_path),
        "receipt_digest": result.digest,
    }))
}

fn relative(root: &Path, path: &Path) -> String {
    path.strip_prefix(root)
        .unwrap_or(path)
        .to_string_lossy()
        .replace('\\', "/")
}

fn enable_plan(root: &Path, plan: &BlockPlan) -> Result<Value> {
    let (plan_path, intent_path, result_path) = plan_paths(root, plan, "enable");
    let previous = previous_receipt_digest(&result_path)?;
    let lock_path = root.join(".ggen").join("packs.lock");
    let group_path = runtime_root(root)
        .join("groups")
        .join(format!("{}.json", plan.requested_group));
    let artifacts = vec![
        relative(root, &plan_path),
        relative(root, &group_path),
        relative(root, &lock_path),
    ];
    let intent = Receipt::issue("enable-intent", plan, &previous, artifacts.clone())?;
    write_json(&intent_path, &intent)?;

    write_json(&plan_path, plan)?;
    write_json(
        &group_path,
        &json!({
            "schema": "ggen.bblock.group.v1",
            "provider": plan.provider,
            "requested_group": plan.requested_group,
            "resolved_groups": plan.resolved_groups,
            "directories": plan.directories,
            "packs": plan.packs,
            "plan_digest": plan.plan_digest,
        }),
    )?;
    for directory in &plan.directories {
        fs::create_dir_all(root.join(directory)).map_err(|error| {
            NounVerbError::execution_error(format!("cannot create {directory}: {error}"))
        })?;
    }

    let mut lockfile = if lock_path.is_file() {
        PackLockfile::from_file(&lock_path).map_err(|error| {
            NounVerbError::execution_error(format!("cannot read lockfile: {error}"))
        })?
    } else {
        PackLockfile::new(env!("CARGO_PKG_VERSION"))
    };
    for pack in &plan.packs {
        let integrity = digest(&(pack, &plan.catalog_digest))?;
        lockfile.add_pack(
            pack,
            LockedPack {
                version: plan.catalog_version.clone(),
                source: PackSource::Registry {
                    url: REGISTRY_URL.to_string(),
                },
                integrity: Some(format!("blake3-{integrity}")),
                installed_at: chrono::Utc::now(),
                dependencies: Vec::new(),
            },
        );
    }
    lockfile.save(&lock_path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot write lockfile: {error}"))
    })?;

    let result = Receipt::issue(
        "enable-result",
        plan,
        &intent.digest,
        artifacts
            .iter()
            .cloned()
            .chain([relative(root, &intent_path)])
            .collect(),
    )?;
    write_json(&result_path, &result)?;
    Ok(json!({
        "status": "enabled",
        "provider": plan.provider,
        "group": plan.requested_group,
        "groups": plan.resolved_groups,
        "packs": plan.packs,
        "directories": plan.directories,
        "lockfile": relative(root, &lock_path),
        "plan": relative(root, &plan_path),
        "intent_receipt": relative(root, &intent_path),
        "result_receipt": relative(root, &result_path),
        "receipt_digest": result.digest,
    }))
}

/// List supported globally available cloud providers and aliases.
#[verb]
pub fn providers() -> Result<Value> {
    let catalog = catalog()?;
    Ok(json!({
        "schema": catalog.schema,
        "version": catalog.version,
        "providers": catalog.providers,
        "catalog_digest": catalog_digest(),
    }))
}

/// List every atomic building block and composite pack group.
#[verb]
pub fn list() -> Result<Value> {
    let catalog = catalog()?;
    Ok(json!({
        "schema": catalog.schema,
        "version": catalog.version,
        "groups": catalog.groups.iter().map(|group| json!({
            "id": group.id,
            "title": group.title,
            "description": group.description,
            "directory": group.directory,
            "dependencies": group.dependencies,
        })).collect::<Vec<_>>(),
        "catalog_digest": catalog_digest(),
    }))
}

/// Inspect one group after provider normalization and dependency expansion.
#[verb]
pub fn inspect(group_id: String, provider: String) -> Result<Value> {
    let catalog = catalog()?;
    let plan = resolve(&catalog, &group_id, &provider)?;
    Ok(serde_json::to_value(plan).map_err(|error| {
        NounVerbError::execution_error(format!("cannot encode bblock plan: {error}"))
    })?)
}

/// Resolve a group-of-packs without writing project state.
#[verb]
pub fn group(group_id: String, provider: String) -> Result<Value> {
    inspect(group_id, provider)
}

/// Manufacture a deterministic local deployment plan and chained receipts.
#[verb]
pub fn plan(group_id: String, provider: String) -> Result<Value> {
    let catalog = catalog()?;
    let plan = resolve(&catalog, &group_id, &provider)?;
    let root = project_root()?;
    write_plan_receipts(&root, &plan, "plan")
}

/// Enable a group by materializing directories, a plan, receipts, and lockfile entries.
#[verb]
pub fn enable(group_id: String, provider: String) -> Result<Value> {
    let catalog = catalog()?;
    let plan = resolve(&catalog, &group_id, &provider)?;
    let root = project_root()?;
    enable_plan(&root, &plan)
}

/// Validate provider closure, pack identifiers, paths, and dependency acyclicity.
#[verb]
pub fn validate() -> Result<Value> {
    let catalog = catalog()?;
    let mut pack_count = BTreeSet::new();
    for group in &catalog.groups {
        pack_count.extend(group.common_packs.iter().cloned());
        for packs in group.provider_packs.values() {
            pack_count.extend(packs.iter().cloned());
        }
    }
    Ok(json!({
        "status": "ALIVE",
        "schema": catalog.schema,
        "version": catalog.version,
        "providers": catalog.providers.len(),
        "groups": catalog.groups.len(),
        "unique_packs": pack_count.len(),
        "catalog_digest": catalog_digest(),
    }))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn catalog_is_closed_and_provider_complete() {
        let catalog = catalog().expect("catalog");
        assert_eq!(catalog.providers.len(), 3);
        assert!(catalog.groups.len() >= 18);
        assert!(catalog.groups.iter().all(|group| {
            group
                .provider_packs
                .get("aws")
                .is_some_and(|packs| !packs.is_empty())
                && group
                    .provider_packs
                    .get("azure")
                    .is_some_and(|packs| !packs.is_empty())
                && group
                    .provider_packs
                    .get("gcp")
                    .is_some_and(|packs| !packs.is_empty())
        }));
    }

    #[test]
    fn gpc_alias_resolves_to_gcp() {
        let catalog = catalog().expect("catalog");
        let plan = resolve(&catalog, "fortune5-complete", "gpc").expect("plan");
        assert_eq!(plan.provider, "gcp");
        assert!(plan.packs.iter().any(|pack| pack.starts_with("gcp-")));
    }

    #[test]
    fn complete_plan_is_deterministic_and_duplicate_free() {
        let catalog = catalog().expect("catalog");
        let first = resolve(&catalog, "fortune5-complete", "aws").expect("first");
        let second = resolve(&catalog, "fortune5-complete", "aws").expect("second");
        assert_eq!(first, second);
        let unique: BTreeSet<_> = first.packs.iter().collect();
        assert_eq!(unique.len(), first.packs.len());
        assert_eq!(
            first.resolved_groups.last(),
            Some(&"fortune5-complete".to_string())
        );
    }

    #[test]
    fn unknown_provider_and_group_are_typed_refusals() {
        let catalog = catalog().expect("catalog");
        assert!(resolve(&catalog, "fortune5-complete", "oracle").is_err());
        assert!(resolve(&catalog, "missing", "aws").is_err());
    }

    #[test]
    fn dependency_cycle_is_refused() {
        let mut catalog = catalog().expect("catalog");
        catalog
            .groups
            .iter_mut()
            .find(|group| group.id == "global-network")
            .expect("network")
            .dependencies
            .push("fortune5-complete".to_string());
        let result = visit_group(
            &catalog,
            "fortune5-complete",
            &mut BTreeSet::new(),
            &mut BTreeSet::new(),
            &mut Vec::new(),
        );
        assert!(result.is_err());
    }
}
