const AUTO_SELECT_OVERRIDE_PREFIX: &str =
    "tcps:自動選択Module tcps:sourceTextOverride \"\"\"";
const AUTO_SELECT_OVERRIDE_SUFFIX: &str = "\"\"\" .";

fn expected_derivative_blob_oid(capability_id: &str) -> Option<&'static str> {
    match capability_id {
        "core.kaizen" => Some("fe1e1444106941e9e165f5f23114b563945fd68d"),
        "core.crypto-digest" => Some("60ee7f07a2ac2961bd334fe5787a61fd03ca297f"),
        "core.auto-select" => Some("9b4c5232b912ca02ab540a36b0ea137ecabbe58a"),
        "core.blue-river-dam" => Some("33f5ba218628f4d314ca97caee99ae4054157935"),
        "runtime.std" => Some("594596dd47784f58b2bb17d7857531846ca027c1"),
        _ => None,
    }
}

fn extract_single_auto_select_override(document: &str) -> Result<&str, EvidenceError> {
    let matches: Vec<_> = document.match_indices(AUTO_SELECT_OVERRIDE_PREFIX).collect();
    if matches.len() != 1 {
        return Err(EvidenceError::InvalidData {
            identity: "tcps-auto-select-rdf-override".to_owned(),
            message: format!(
                "expected exactly one Auto Select sourceTextOverride, found {}",
                matches.len()
            ),
        });
    }

    let body_start = matches[0].0 + AUTO_SELECT_OVERRIDE_PREFIX.len();
    let tail = &document[body_start..];
    let body_end = tail.find(AUTO_SELECT_OVERRIDE_SUFFIX).ok_or_else(|| {
        EvidenceError::InvalidData {
            identity: "tcps-auto-select-rdf-override".to_owned(),
            message: "Auto Select sourceTextOverride has no closing Turtle literal".to_owned(),
        }
    })?;
    let body = &tail[..body_end];

    if tail[body_end + AUTO_SELECT_OVERRIDE_SUFFIX.len()..]
        .contains(AUTO_SELECT_OVERRIDE_PREFIX)
    {
        return Err(EvidenceError::InvalidData {
            identity: "tcps-auto-select-rdf-override".to_owned(),
            message: "multiple Auto Select sourceTextOverride literals".to_owned(),
        });
    }
    Ok(body)
}

fn validate_auto_select_override(root: &Path, generated: &[u8]) -> Result<(), EvidenceError> {
    let ontology = read_nonempty(root, "schema/domain.ttl")?;
    let text = utf8("schema/domain.ttl", &ontology)?;
    let admitted = extract_single_auto_select_override(text)?;
    if admitted.as_bytes() != generated {
        return Err(EvidenceError::InvalidData {
            identity: "core.auto-select".to_owned(),
            message: format!(
                "RDF override/generated projection mismatch: admitted {} bytes, generated {} bytes",
                admitted.len(),
                generated.len()
            ),
        });
    }
    Ok(())
}

fn validate_capability(
    root: &Path, manifest: &BTreeMap<String, String>, capability: &Capability,
) -> Result<CapabilityEvidence, EvidenceError> {
    let bytes = read_nonempty(root, capability.path)?;
    let mut facts = validate_format(root, capability, &bytes)?;
    let actual_sha256 = sha256_hex(&bytes);
    let actual_blob_oid = git_blob_oid(&bytes);

    if capability.id == "core.auto-select" {
        validate_auto_select_override(root, &bytes)?;
        facts.push("rdf-source-override-byte-match");
    }

    let reference_sha256 = match capability.reference_standing {
        ReferenceStanding::ExactManifest => {
            let manifest_path =
                capability
                    .manifest_path
                    .ok_or_else(|| EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: "exact-manifest capability has no reference path".to_owned(),
                    })?;
            let expected =
                manifest
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
            let manifest_path =
                capability
                    .manifest_path
                    .ok_or_else(|| EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: "declared derivative has no source reference path".to_owned(),
                    })?;
            let expected_source =
                manifest
                    .get(manifest_path)
                    .ok_or_else(|| EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: format!(
                            "manifest has no source entry for derivative {manifest_path}"
                        ),
                    })?;
            if expected_source == &actual_sha256 {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "declared derivative is now byte-identical and must be reclassified"
                        .to_owned(),
                });
            }
            let expected_blob_oid =
                expected_derivative_blob_oid(capability.id).ok_or_else(|| {
                    EvidenceError::InvalidData {
                        identity: capability.id.to_owned(),
                        message: "declared derivative has no pinned generated object identity"
                            .to_owned(),
                    }
                })?;
            if actual_blob_oid != expected_blob_oid {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: format!(
                        "undeclared derivative drift: expected Git blob {expected_blob_oid}, found {actual_blob_oid}"
                    ),
                });
            }
            facts.push("declared-generated-derivative");
            facts.push("pinned-generated-git-object");
            Some(expected_source.clone())
        }
        ReferenceStanding::Independent => {
            if capability.manifest_path.is_some() {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "independent capability unexpectedly names a manifest path".to_owned(),
                });
            }
            if expected_derivative_blob_oid(capability.id).is_some() {
                return Err(EvidenceError::InvalidData {
                    identity: capability.id.to_owned(),
                    message: "independent capability unexpectedly has a derivative identity"
                        .to_owned(),
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
        "tcps/capability-evidence/v5",
        &[
            capability.id.as_bytes(),
            capability.path.as_bytes(),
            standing_text.as_bytes(),
            actual_sha256.as_bytes(),
            actual_blob_oid.as_bytes(),
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
    let mut evidence = Vec::with_capacity(CAPABILITIES.len());
    let mut refusals = Vec::new();

    for capability in CAPABILITIES {
        match validate_capability(root, &manifest, capability) {
            Ok(item) => evidence.push(item),
            Err(error) => refusals.push(format!("{} => {error}", capability.id)),
        }
    }

    if refusals.is_empty() {
        Ok(evidence)
    } else {
        Err(EvidenceError::InvalidData {
            identity: "tcps-capability-census".to_owned(),
            message: format!(
                "{} of {} capability contracts refused:\n{}",
                refusals.len(),
                CAPABILITIES.len(),
                refusals.join("\n")
            ),
        })
    }
}

static PRODUCT_EVIDENCE: OnceLock<Result<Vec<CapabilityEvidence>, EvidenceError>> = OnceLock::new();

fn product_evidence() -> Result<&'static [CapabilityEvidence], EvidenceError> {
    match PRODUCT_EVIDENCE.get_or_init(|| validate_all(&project_root())) {
        Ok(evidence) => Ok(evidence.as_slice()),
        Err(error) => Err(error.clone()),
    }
}

fn receipt(
    evidence: &[CapabilityEvidence], manifest_entry_count: usize,
) -> Result<EvidenceReceipt, EvidenceError> {
    let mut sorted = evidence.to_vec();
    sorted.sort_by_key(|item| item.id);
    let leaves: Result<Vec<_>, _> = sorted
        .iter()
        .map(|item| {
            let decoded =
                hex::decode(&item.evidence_digest).map_err(|error| EvidenceError::InvalidData {
                    identity: item.id.to_owned(),
                    message: error.to_string(),
                })?;
            let bytes: [u8; 32] = decoded.try_into().map_err(|_| EvidenceError::InvalidData {
                identity: item.id.to_owned(),
                message: "evidence digest is not 32 bytes".to_owned(),
            })?;
            Ok(EvidenceDigest::from_bytes(bytes))
        })
        .collect();
    let root = merkle_root("tcps/evidence-root/v5", &leaves?)?;

    Ok(EvidenceReceipt {
        schema: "tcps-product-evidence/v5",
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
