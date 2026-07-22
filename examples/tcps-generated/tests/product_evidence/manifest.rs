fn project_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
}

fn parse_manifest(text: &str) -> Result<BTreeMap<String, String>, EvidenceError> {
    let mut entries = BTreeMap::new();
    for (line_number, line) in text.lines().enumerate() {
        if line.trim().is_empty() {
            continue;
        }
        let Some((digest, path)) = line.split_once("  ") else {
            return Err(EvidenceError::InvalidData {
                identity: MANIFEST_PATH.to_owned(),
                message: format!("line {} has no two-space separator", line_number + 1),
            });
        };
        if digest.len() != 64 || !digest.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err(EvidenceError::InvalidData {
                identity: MANIFEST_PATH.to_owned(),
                message: format!("line {} has invalid SHA-256", line_number + 1),
            });
        }
        if path.is_empty() || path.starts_with('/') || path.contains("..") {
            return Err(EvidenceError::InvalidData {
                identity: MANIFEST_PATH.to_owned(),
                message: format!("line {} has unsafe path", line_number + 1),
            });
        }
        if entries
            .insert(path.to_owned(), digest.to_ascii_lowercase())
            .is_some()
        {
            return Err(EvidenceError::InvalidData {
                identity: MANIFEST_PATH.to_owned(),
                message: format!("duplicate path {path}"),
            });
        }
    }
    Ok(entries)
}

fn load_manifest(root: &Path) -> Result<BTreeMap<String, String>, EvidenceError> {
    let bytes = read_nonempty(root, MANIFEST_PATH)?;
    parse_manifest(utf8(MANIFEST_PATH, &bytes)?)
}

fn public_item_count(file: &syn::File) -> usize {
    file.items
        .iter()
        .filter(|item| match item {
            syn::Item::Const(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Enum(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::ExternCrate(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Fn(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Mod(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Static(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Struct(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Trait(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::TraitAlias(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Type(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Union(item) => matches!(item.vis, syn::Visibility::Public(_)),
            syn::Item::Use(item) => matches!(item.vis, syn::Visibility::Public(_)),
            _ => false,
        })
        .count()
}

fn require_fields(identity: &str, text: &str, fields: &[&str]) -> Result<(), EvidenceError> {
    for field in fields {
        if !text.contains(field) {
            return Err(EvidenceError::InvalidData {
                identity: identity.to_owned(),
                message: format!("missing required field {field}"),
            });
        }
    }
    Ok(())
}

fn validate_ggen_lock(text: &str) -> Result<usize, EvidenceError> {
    let value: toml::Value = toml::from_str(text).map_err(|error| EvidenceError::InvalidData {
        identity: "ggen.lock".to_owned(),
        message: error.to_string(),
    })?;
    let packs = value
        .get("packs")
        .and_then(toml::Value::as_table)
        .ok_or_else(|| EvidenceError::InvalidData {
            identity: "ggen.lock".to_owned(),
            message: "missing packs table".to_owned(),
        })?;

    let actual: BTreeSet<_> = packs.keys().map(String::as_str).collect();
    let expected: BTreeSet<_> = EXPECTED_PACKS.into_iter().collect();
    if actual != expected {
        return Err(EvidenceError::InvalidData {
            identity: "ggen.lock".to_owned(),
            message: format!("pack set mismatch: {actual:?}"),
        });
    }

    for (name, pack) in packs {
        let table = pack
            .as_table()
            .ok_or_else(|| EvidenceError::InvalidData {
                identity: name.clone(),
                message: "pack entry is not a table".to_owned(),
            })?;
        let source = table
            .get("source")
            .and_then(toml::Value::as_str)
            .unwrap_or_default();
        let digest = table
            .get("content_hash")
            .and_then(toml::Value::as_str)
            .unwrap_or_default();
        if !source.starts_with("path:") {
            return Err(EvidenceError::InvalidData {
                identity: name.clone(),
                message: "source is not an admitted path source".to_owned(),
            });
        }
        let Some(hex_digest) = digest.strip_prefix("blake3:") else {
            return Err(EvidenceError::InvalidData {
                identity: name.clone(),
                message: "content_hash is not BLAKE3".to_owned(),
            });
        };
        if hex_digest.len() != 64 || !hex_digest.bytes().all(|byte| byte.is_ascii_hexdigit()) {
            return Err(EvidenceError::InvalidData {
                identity: name.clone(),
                message: "invalid BLAKE3 digest".to_owned(),
            });
        }
    }
    Ok(packs.len())
}
