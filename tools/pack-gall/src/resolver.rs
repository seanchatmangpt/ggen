use crate::io::digest_json;
use crate::model::{Catalog, ResolutionEvidence};
use std::collections::BTreeSet;
use std::path::Path;

pub fn validate_catalog(catalog: &Catalog) -> Result<(), String> {
    if catalog.schema != "ggen.bblock.catalog.v1" || catalog.version.trim().is_empty() {
        return Err("invalid catalog schema or version".to_string());
    }
    let providers: BTreeSet<_> = catalog.providers.iter().map(|p| p.id.as_str()).collect();
    if providers != BTreeSet::from(["aws", "azure", "gcp"]) {
        return Err("provider universe must be exactly aws, azure, and gcp".to_string());
    }
    let groups: BTreeSet<_> = catalog.groups.iter().map(|g| g.id.as_str()).collect();
    if groups.len() != catalog.groups.len() || groups.is_empty() {
        return Err("group identities are empty or duplicated".to_string());
    }
    for group in &catalog.groups {
        if group.directory.is_empty()
            || Path::new(&group.directory).is_absolute()
            || group.directory.split('/').any(|part| part == "..")
        {
            return Err(format!("unsafe directory for {}", group.id));
        }
        let keys: BTreeSet<_> = group.provider_packs.keys().map(String::as_str).collect();
        if keys != providers {
            return Err(format!("group {} is not provider complete", group.id));
        }
        for dependency in &group.dependencies {
            if !groups.contains(dependency.as_str()) {
                return Err(format!(
                    "group {} references unknown dependency {dependency}",
                    group.id
                ));
            }
        }
    }
    for group in &catalog.groups {
        visit_group(
            catalog,
            &group.id,
            &mut BTreeSet::new(),
            &mut BTreeSet::new(),
            &mut Vec::new(),
        )?;
    }
    Ok(())
}

pub fn resolve(
    catalog: &Catalog,
    group_id: &str,
    provider: &str,
) -> Result<ResolutionEvidence, String> {
    let provider = normalize_provider(catalog, provider)?;
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
        let group = catalog
            .groups
            .iter()
            .find(|group| &group.id == id)
            .ok_or_else(|| format!("unknown group {id}"))?;
        directories.insert(group.directory.clone());
        packs.extend(group.common_packs.iter().cloned());
        packs.extend(
            group
                .provider_packs
                .get(provider)
                .into_iter()
                .flatten()
                .cloned(),
        );
    }
    let directories: Vec<_> = directories.into_iter().collect();
    let packs: Vec<_> = packs.into_iter().collect();
    let plan_digest = digest_json(&serde_json::json!({
        "catalog_schema": &catalog.schema,
        "catalog_version": &catalog.version,
        "provider": provider,
        "requested_group": group_id,
        "resolved_groups": &ordered,
        "directories": &directories,
        "packs": &packs,
    }))?;
    Ok(ResolutionEvidence {
        provider: provider.to_string(),
        requested_group: group_id.to_string(),
        resolved_groups: ordered,
        directories,
        packs,
        plan_digest,
    })
}

fn normalize_provider<'a>(catalog: &'a Catalog, raw: &str) -> Result<&'a str, String> {
    let normalized = raw.trim().to_ascii_lowercase();
    catalog
        .providers
        .iter()
        .find(|provider| {
            provider.id == normalized
                || provider.aliases.iter().any(|alias| alias == &normalized)
        })
        .map(|provider| provider.id.as_str())
        .ok_or_else(|| format!("unsupported provider {raw}"))
}

fn visit_group(
    catalog: &Catalog,
    id: &str,
    visiting: &mut BTreeSet<String>,
    visited: &mut BTreeSet<String>,
    ordered: &mut Vec<String>,
) -> Result<(), String> {
    if visited.contains(id) {
        return Ok(());
    }
    if !visiting.insert(id.to_string()) {
        return Err(format!("dependency cycle at {id}"));
    }
    let group = catalog
        .groups
        .iter()
        .find(|group| group.id == id)
        .ok_or_else(|| format!("unknown group {id}"))?;
    for dependency in &group.dependencies {
        visit_group(catalog, dependency, visiting, visited, ordered)?;
    }
    visiting.remove(id);
    visited.insert(id.to_string());
    ordered.push(id.to_string());
    Ok(())
}
