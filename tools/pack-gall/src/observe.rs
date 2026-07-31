use crate::io::{digest_bytes, digest_json, digest_path};
use crate::model::{
    Catalog, CommandEvidence, Contract, CorpusEvidence, Observation, SchemaEvidence,
    SurfaceEvidence, CONTRACT_SCHEMA, OBSERVATION_SCHEMA,
};
use crate::resolver::{resolve, validate_catalog};
use std::collections::{BTreeMap, BTreeSet};
use std::fs;
use std::path::Path;

pub fn load_contract(root: &Path) -> Result<Contract, String> {
    let path = root.join("tools/pack-gall/contracts/pack-gall.contract.json");
    let bytes = fs::read(&path).map_err(|e| format!("cannot read {}: {e}", path.display()))?;
    let contract: Contract = serde_json::from_slice(&bytes)
        .map_err(|e| format!("cannot parse {}: {e}", path.display()))?;
    if contract.schema != CONTRACT_SCHEMA {
        return Err(format!("unsupported contract schema {}", contract.schema));
    }
    Ok(contract)
}

pub fn observe(root: &Path) -> Result<Observation, String> {
    let contract = load_contract(root)?;
    let contract_path = root.join("tools/pack-gall/contracts/pack-gall.contract.json");
    let contract_digest = digest_path(&contract_path)?;
    let (surfaces, ownership) = observe_surfaces(root, &contract)?;
    let command_matrix = observe_commands(root, &contract)?;
    let schema_matrix = observe_schemas(root, &contract)?;
    let corpus = observe_corpus(root, &contract)?;
    let canonical_schema_digest = digest_path(&root.join(&contract.canonical_schema_path))?;
    let verifier_schema_digest = digest_path(&root.join(&contract.verifier_schema_path))?;
    let source_digest = digest_json(&serde_json::json!({
        "surfaces": &surfaces,
        "catalog": &corpus.catalog_digest,
        "canonical_schema": &canonical_schema_digest,
        "verifier_schema": &verifier_schema_digest
    }))?;
    Ok(Observation {
        schema: OBSERVATION_SCHEMA.to_string(),
        contract_digest,
        source_digest,
        surfaces,
        command_matrix,
        schema_matrix,
        ownership,
        corpus,
        canonical_schema_digest,
        verifier_schema_digest,
    })
}

fn observe_surfaces(
    root: &Path,
    contract: &Contract,
) -> Result<(Vec<SurfaceEvidence>, BTreeMap<String, String>), String> {
    let mut surfaces = Vec::new();
    let mut ownership = BTreeMap::new();
    for required in &contract.required_surfaces {
        let path = root.join(&required.path);
        let bytes = fs::read(&path)
            .map_err(|e| format!("required surface {} unavailable: {e}", required.path))?;
        if ownership
            .insert(required.path.clone(), required.owner.clone())
            .is_some()
        {
            return Err(format!("duplicate exclusive ownership for {}", required.path));
        }
        surfaces.push(SurfaceEvidence {
            path: required.path.clone(),
            owner: required.owner.clone(),
            class: required.class.clone(),
            bytes: bytes.len() as u64,
            blake3: digest_bytes(&bytes),
        });
    }
    surfaces.sort_by(|left, right| left.path.cmp(&right.path));
    Ok((surfaces, ownership))
}

fn observe_commands(root: &Path, contract: &Contract) -> Result<Vec<CommandEvidence>, String> {
    let mut matrix = Vec::new();
    for command in &contract.command_surfaces {
        let source = read_text(root, &command.path)?;
        let observed_verbs = extract_public_functions(&source);
        let observed_set: BTreeSet<_> = observed_verbs.iter().cloned().collect();
        let missing_verbs = command
            .required_verbs
            .iter()
            .filter(|verb| !observed_set.contains(*verb))
            .cloned()
            .collect();
        matrix.push(CommandEvidence {
            noun: command.noun.clone(),
            path: command.path.clone(),
            observed_verbs,
            required_verbs: command.required_verbs.clone(),
            missing_verbs,
        });
    }
    matrix.sort_by(|left, right| left.noun.cmp(&right.noun));
    Ok(matrix)
}

fn observe_schemas(root: &Path, contract: &Contract) -> Result<Vec<SchemaEvidence>, String> {
    let mut matrix = Vec::new();
    for schema in &contract.schema_tokens {
        let source = read_text(root, &schema.path)?;
        let missing_tokens = schema
            .tokens
            .iter()
            .filter(|token| !source.contains(token.as_str()))
            .cloned()
            .collect();
        matrix.push(SchemaEvidence {
            path: schema.path.clone(),
            required_tokens: schema.tokens.clone(),
            missing_tokens,
        });
    }
    matrix.sort_by(|left, right| left.path.cmp(&right.path));
    Ok(matrix)
}

fn observe_corpus(root: &Path, contract: &Contract) -> Result<CorpusEvidence, String> {
    let catalog_path = root.join(&contract.catalog_path);
    let catalog_bytes = fs::read(&catalog_path)
        .map_err(|e| format!("cannot read {}: {e}", catalog_path.display()))?;
    let catalog: Catalog = serde_json::from_slice(&catalog_bytes)
        .map_err(|e| format!("cannot parse {}: {e}", catalog_path.display()))?;
    validate_catalog(&catalog)?;
    let representative_group = if catalog.groups.iter().any(|group| group.id == "fortune5-complete")
    {
        "fortune5-complete".to_string()
    } else {
        catalog
            .groups
            .last()
            .map(|group| group.id.clone())
            .ok_or("catalog has no groups")?
    };
    let mut representative_resolutions = Vec::new();
    for provider in &catalog.providers {
        representative_resolutions.push(resolve(
            &catalog,
            &representative_group,
            &provider.id,
        )?);
    }
    let mut unique_packs = BTreeSet::new();
    for group in &catalog.groups {
        unique_packs.extend(group.common_packs.iter().cloned());
        for packs in group.provider_packs.values() {
            unique_packs.extend(packs.iter().cloned());
        }
    }
    let mut provider_ids: Vec<_> = catalog
        .providers
        .iter()
        .map(|provider| provider.id.clone())
        .collect();
    provider_ids.sort();
    Ok(CorpusEvidence {
        catalog_schema: catalog.schema,
        catalog_version: catalog.version,
        provider_ids,
        group_count: catalog.groups.len(),
        unique_pack_count: unique_packs.len(),
        catalog_digest: digest_bytes(&catalog_bytes),
        representative_resolutions,
    })
}

fn extract_public_functions(source: &str) -> Vec<String> {
    let mut functions = BTreeSet::new();
    for line in source.lines() {
        let trimmed = line.trim_start();
        for marker in ["pub fn ", "pub async fn "] {
            if let Some(rest) = trimmed.strip_prefix(marker) {
                let name: String = rest
                    .chars()
                    .take_while(|character| character.is_ascii_alphanumeric() || *character == '_')
                    .collect();
                if !name.is_empty() {
                    functions.insert(name);
                }
            }
        }
    }
    functions.into_iter().collect()
}

fn read_text(root: &Path, relative: &str) -> Result<String, String> {
    let path = root.join(relative);
    fs::read_to_string(&path).map_err(|e| format!("cannot read {}: {e}", path.display()))
}
