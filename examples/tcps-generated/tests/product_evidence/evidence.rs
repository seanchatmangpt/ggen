fn validate_capability(
    root: &Path,
    manifest: &BTreeMap<String, String>,
    capability: &Capability,
) -> Result<CapabilityEvidence, EvidenceError> {
    let bytes = read_nonempty(root, capability.path)?;
    let mut facts = validate_format(root, capability, &bytes)?;
    let actual_sha256 = sha256_hex(&bytes);

    let reference_sha256 = match capability.reference_standing {
        ReferenceStanding::ExactManifest => {
            let manifest_path = capability.manifest_path.ok_or_else(|| EvidenceError::InvalidData {
                identity: capability.id.to_owned(),
                message: "exact-manifest capability has no reference path".to_owned(),
            })?;
            let expected = manifest
                .get(manifest_path)
                .ok_or_else(|| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: format!("manifest has no entry for {manifest_path}"),
                })?;
            if expected != &actual_sha256 {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: format!(
                        "undeclared SHA-256 drift: expected {expected}, found {actual_sha256}"
                    ),
                });
            }
            facts.push("exact-manifest-sha256-match");
            Some(expected.clone())
        }
        ReferenceStanding::DeclaredDerivative => {
            let manifest_path = capability.manifest_path.ok_or_else(|| EvidenceError::InvalidData {
                identity: capability.id.to_owned(),
                message: "declared derivative has no source reference path".to_owned(),
            })?;
            let expected = manifest
                .get(manifest_path)
                .ok_or_else(|| EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: format!("manifest has no source entry for derivative {manifest_path}"),
                })?;
            if expected == &actual_sha256 {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "declared derivative is now byte-identical and must be reclassified"
                        .to_owned(),
                });
            }
            facts.push("declared-generated-derivative");
            Some(expected.clone())
        }
        ReferenceStanding::Independent => {
            if capability.manifest_path.is_some() {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "independent capability unexpectedly names a manifest path".to_owned(),
                });
            }
            facts.push("independently-validated");
            None
        }
    };

    let facts_text = facts.join("\n");
    let reference_text = reference_sha256.as_deref().unwrap_or("none");
    let standing_text = match capability.reference_standing {
        ReferenceStanding::ExactManifest => "exact-manifest",
        ReferenceStanding::DeclaredDerivative => "declared-derivative",
        ReferenceStanding::Independent => "independent",
    };
    let digest = domain_digest(
        "tcps/capability-evidence/v3",
        &[
            capability.id.as_bytes(),
            capability.path.as_bytes(),
            standing_text.as_bytes(),
            actual_sha256.as_bytes(),
            reference_text.as_bytes(),
            facts_text.as_bytes(),
        ],
    );

    Ok(CapabilityEvidence {
        id: capability.id,
        kind: capability.kind,
        path: capability.path,
        byte_len: bytes.len(),
        sha256: actual_sha256,
        reference_standing: capability.reference_standing,
        reference_sha256,
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

static PRODUCT_EVIDENCE: OnceLock<Result<Vec<CapabilityEvidence>, EvidenceError>> = OnceLock::new();

fn product_evidence() -> Result<&'static [CapabilityEvidence], EvidenceError> {
    match PRODUCT_EVIDENCE.get_or_init(|| validate_all(&project_root())) {
        Ok(evidence) => Ok(evidence.as_slice()),
        Err(error) => Err(error.clone()),
    }
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
            let bytes: [u8; 32] = decoded.try_into().map_err(|_| EvidenceError::InvalidData {
                identity: item.id.to_owned(),
                message: "evidence digest is not 32 bytes".to_owned(),
            })?;
            Ok(EvidenceDigest::from_bytes(bytes))
        })
        .collect();
    let root = merkle_root("tcps/evidence-root/v3", &leaves?)?;

    Ok(EvidenceReceipt {
        schema: "tcps-product-evidence/v3",
        capability_count: evidence.len(),
        manifest_entry_count,
        exact_manifest_capabilities: evidence
            .iter()
            .filter(|item| item.reference_standing == ReferenceStanding::ExactManifest)
            .count(),
        declared_derivative_capabilities: evidence
            .iter()
            .filter(|item| item.reference_standing == ReferenceStanding::DeclaredDerivative)
            .count(),
        independent_capabilities: evidence
            .iter()
            .filter(|item| item.reference_standing == ReferenceStanding::Independent)
            .count(),
        pack_count: EXPECTED_PACKS.len(),
        evidence_root: root.to_hex(),
    })
}
