fn validate_capability(
    root: &Path,
    manifest: &BTreeMap<String, String>,
    capability: &Capability,
) -> Result<CapabilityEvidence, EvidenceError> {
    let bytes = read_nonempty(root, capability.path)?;
    let mut facts = validate_format(root, capability, &bytes)?;
    let actual_sha256 = sha256_hex(&bytes);
    let manifest_bound = if let Some(manifest_path) = capability.manifest_path {
        let expected = manifest
            .get(manifest_path)
            .ok_or_else(|| EvidenceError::InvalidData {
                identity: capability.id.to_owned(),
                message: format!("manifest has no entry for {manifest_path}"),
            })?;
        if expected != &actual_sha256 {
            return Err(EvidenceError::InvalidData {
                identity: capability.id.to_owned(),
                message: format!("SHA-256 mismatch: expected {expected}, found {actual_sha256}"),
            });
        }
        facts.push("manifest-sha256-match");
        true
    } else {
        false
    };

    let facts_text = facts.join("\n");
    let digest = domain_digest(
        "tcps/capability-evidence/v2",
        &[
            capability.id.as_bytes(),
            capability.path.as_bytes(),
            actual_sha256.as_bytes(),
            facts_text.as_bytes(),
        ],
    );

    Ok(CapabilityEvidence {
        id: capability.id,
        kind: capability.kind,
        path: capability.path,
        byte_len: bytes.len(),
        sha256: actual_sha256,
        manifest_bound,
        facts,
        evidence_digest: digest.to_hex(),
    })
}

fn validate_all(root: &Path) -> Result<Vec<CapabilityEvidence>, EvidenceError> {
    let manifest = load_manifest(root)?;
    CAPABILITIES
        .iter()
        .map(|capability| validate_capability(root, &manifest, capability))
        .collect()
}

fn receipt(
    evidence: &[CapabilityEvidence],
    manifest_entry_count: usize,
) -> Result<EvidenceReceipt, EvidenceError> {
    let mut sorted = evidence.to_vec();
    sorted.sort_by_key(|item| item.id);
    let leaves: Result<Vec<_>, _> = sorted
        .iter()
        .map(|item| {
            let decoded = hex::decode(&item.evidence_digest).map_err(|error| {
                EvidenceError::InvalidData {
                    identity: item.id.to_owned(),
                    message: error.to_string(),
                }
            })?;
            let bytes: [u8; 32] =
                decoded
                    .try_into()
                    .map_err(|_| EvidenceError::InvalidData {
                        identity: item.id.to_owned(),
                        message: "evidence digest is not 32 bytes".to_owned(),
                    })?;
            Ok(EvidenceDigest::from_bytes(bytes))
        })
        .collect();
    let root = merkle_root("tcps/evidence-root/v2", &leaves?)?;

    Ok(EvidenceReceipt {
        schema: "tcps-product-evidence/v2",
        capability_count: evidence.len(),
        manifest_entry_count,
        manifest_bound_capabilities: evidence.iter().filter(|item| item.manifest_bound).count(),
        pack_count: EXPECTED_PACKS.len(),
        evidence_root: root.to_hex(),
    })
}
