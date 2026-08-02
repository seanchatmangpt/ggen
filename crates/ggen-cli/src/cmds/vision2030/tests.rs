use super::evaluation::evaluate;
use super::*;

fn write(path: &Path, bytes: &[u8]) -> Evidence {
    fs::write(path, bytes).expect("write evidence");
    Evidence {
        locator: path
            .file_name()
            .expect("name")
            .to_string_lossy()
            .to_string(),
        digest: format!("blake3:{}", digest_bytes(bytes)),
    }
}

fn json_evidence<T: Serialize>(path: &Path, value: &T) -> Evidence {
    let bytes = serde_json::to_vec_pretty(value).expect("json");
    write(path, &bytes)
}

fn report_digest_value(index: usize) -> String {
    format!("{:064x}", index + 1)
}

fn capability_evidence(
    root: &Path, index: usize, id: &str, iri: &str, witness: &Evidence,
) -> BTreeMap<String, Evidence> {
    let report_digest = report_digest_value(index);
    let report = json_evidence(
        &root.join(format!("sbb-report-{index}.json")),
        &json!({
            "schema": SBB_REPORT_SCHEMA,
            "sbb": {
                "id": id,
                "version": "1.0.0",
                "architecture_contract": iri,
                "minimum_commit_equivalent_units": 1
            },
            "claim_ceiling": "PARTIAL_ALIVE",
            "eligible_for_external_admission": true,
            "commit_equivalent_units": 1,
            "distribution_contexts": "1000",
            "delivered_capability_instances": "1000",
            "report_digest": report_digest
        }),
    );
    let receipt = json_evidence(
        &root.join(format!("sbb-result-{index}.json")),
        &json!({
            "schema": SBB_RECEIPT_SCHEMA,
            "operation": "density-evaluate-result",
            "report_digest": report_digest,
            "digest_algorithm": "blake3",
            "digest": "b".repeat(64)
        }),
    );
    let replay = json_evidence(
        &root.join(format!("sbb-replay-{index}.json")),
        &json!({
            "schema": REPLAY_SCHEMA,
            "status": "REPLAY_MATCH",
            "matches": true,
            "report_digest": report_digest
        }),
    );
    let acceptance = json_evidence(
        &root.join(format!("acceptance-{index}.json")),
        &json!({
            "schema": EXTERNAL_ACCEPTANCE_SCHEMA,
            "subject": iri,
            "decision": "ACCEPTED",
            "issuer": "independent-test-authority",
            "report_digest": report_digest
        }),
    );
    [
        ("sbb_report".to_string(), report),
        ("positive".to_string(), witness.clone()),
        ("negative".to_string(), witness.clone()),
        ("verifier".to_string(), witness.clone()),
        ("receipt".to_string(), receipt),
        ("replay".to_string(), replay),
        ("external_acceptance".to_string(), acceptance),
    ]
    .into_iter()
    .collect()
}

fn fixture(root: &Path) -> PathBuf {
    let witness = write(
        &root.join("witness.txt"),
        b"positive negative verifier evidence",
    );
    let capabilities = REQUIRED_DOMAINS
        .iter()
        .enumerate()
        .map(|(index, domain)| {
            let id = format!("cap-{domain}");
            let iri = format!("urn:ggen:test:{domain}");
            let evidence = capability_evidence(root, index, &id, &iri, &witness);
            Capability {
                id,
                iri,
                domain: (*domain).to_string(),
                horizon: HORIZONS[index % HORIZONS.len()],
                blue_ocean_move: BLUE_OCEAN_MOVES[index % BLUE_OCEAN_MOVES.len()].to_string(),
                authority: match *domain {
                    "healthcare" => "observe".to_string(),
                    "doctor" => "recommend".to_string(),
                    _ => "construct".to_string(),
                },
                summary: format!("Accepted {domain} capability"),
                dependencies: Vec::new(),
                evidence,
            }
        })
        .collect();
    let manifest = Manifest {
        schema: MANIFEST_SCHEMA.to_string(),
        program: Program {
            id: "vision-2030-test".to_string(),
            version: "1.0.0".to_string(),
            target_year: 2030,
            phase_change_target: 1000,
            trusted_issuers: BTreeMap::new(),
            trusted_brokers: BTreeMap::new(),
        },
        required_domains: REQUIRED_DOMAINS
            .iter()
            .map(|domain| (*domain).to_string())
            .collect(),
        horizons: HORIZONS
            .iter()
            .map(|year| Horizon {
                year: *year,
                minimum_alive_capabilities: 1,
            })
            .collect(),
        capabilities,
    };
    let path = root.join("manifest.json");
    write_manifest(&path, &manifest);
    path
}

fn read_manifest(path: &Path) -> Manifest {
    serde_json::from_slice(&fs::read(path).expect("manifest")).expect("parse manifest")
}

fn write_manifest(path: &Path, manifest: &Manifest) {
    fs::write(path, serde_json::to_vec_pretty(manifest).expect("manifest")).expect("write");
}

fn replace_json_evidence(
    manifest: &mut Manifest, capability_index: usize, role: &str, path: &Path, value: &Value,
) {
    manifest.capabilities[capability_index]
        .evidence
        .insert(role.to_string(), json_evidence(path, value));
}

#[test]
fn complete_program_reaches_measured_thousand_x() {
    let directory = tempfile::tempdir().expect("tempdir");
    let report = evaluate(&fixture(directory.path())).expect("report");
    assert!(report.achieved);
    assert_eq!(report.standing, "ALIVE");
    assert_eq!(report.phase_change_multiplier, "1000.000");
    assert_eq!(report.canonical_units, REQUIRED_DOMAINS.len());
    assert!(report.domains.values().all(|domain| domain.covered));
    assert!(report.horizons.values().all(|horizon| horizon.met));
    assert!(report.blue_ocean.values().all(|count| *count > 0));
}

#[test]
fn shared_sbb_report_digest_cannot_inflate_capabilities() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    let capability = manifest.capabilities[1].clone();
    let shared_digest = report_digest_value(0);
    replace_json_evidence(
        &mut manifest,
        1,
        "sbb_report",
        &directory.path().join("sbb-report-1.json"),
        &json!({
            "schema": SBB_REPORT_SCHEMA,
            "sbb": {
                "id": capability.id,
                "version": "1.0.0",
                "architecture_contract": capability.iri,
                "minimum_commit_equivalent_units": 1
            },
            "claim_ceiling": "PARTIAL_ALIVE",
            "eligible_for_external_admission": true,
            "commit_equivalent_units": 1,
            "distribution_contexts": "1000",
            "delivered_capability_instances": "1000",
            "report_digest": shared_digest
        }),
    );
    replace_json_evidence(
        &mut manifest,
        1,
        "receipt",
        &directory.path().join("sbb-result-1.json"),
        &json!({
            "schema": SBB_RECEIPT_SCHEMA,
            "operation": "density-evaluate-result",
            "report_digest": shared_digest,
            "digest_algorithm": "blake3",
            "digest": "b".repeat(64)
        }),
    );
    replace_json_evidence(
        &mut manifest,
        1,
        "replay",
        &directory.path().join("sbb-replay-1.json"),
        &json!({
            "schema": REPLAY_SCHEMA,
            "status": "REPLAY_MATCH",
            "matches": true,
            "report_digest": shared_digest
        }),
    );
    replace_json_evidence(
        &mut manifest,
        1,
        "external_acceptance",
        &directory.path().join("acceptance-1.json"),
        &json!({
            "schema": EXTERNAL_ACCEPTANCE_SCHEMA,
            "subject": capability.iri,
            "decision": "ACCEPTED",
            "issuer": "independent-test-authority",
            "report_digest": shared_digest
        }),
    );
    write_manifest(&path, &manifest);
    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("multiple capabilities")));
}

#[test]
fn divergent_acceptance_refuses_achievement() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    fs::write(directory.path().join("acceptance-0.json"), b"tampered").expect("tamper");
    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert_ne!(report.standing, "ALIVE");
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("external_acceptance")));
}

#[test]
fn healthcare_actuation_is_refused() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    let healthcare = manifest
        .capabilities
        .iter_mut()
        .find(|capability| capability.domain == "healthcare")
        .expect("healthcare");
    healthcare.authority = "actuate".to_string();
    write_manifest(&path, &manifest);
    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("healthcare authority")));
}

#[test]
fn dependency_cycle_is_refused() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    let first = manifest.capabilities[0].id.clone();
    let second = manifest.capabilities[1].id.clone();
    manifest.capabilities[0].dependencies = vec![second];
    manifest.capabilities[1].dependencies = vec![first];
    write_manifest(&path, &manifest);
    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("dependency cycle")));
}

#[test]
fn program_receipts_replay_and_detect_tampering() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let output = directory.path().join("receipts");
    receipt(path.display().to_string(), output.display().to_string()).expect("receipt");
    let replayed =
        replay(path.display().to_string(), output.display().to_string()).expect("replay");
    assert_eq!(replayed["status"], "REPLAY_MATCH");
    let report_path = output.join("vision-2030-report.json");
    let mut stored: Value =
        serde_json::from_slice(&fs::read(&report_path).expect("report")).expect("json");
    stored["phase_change_multiplier"] = json!("999999.000");
    fs::write(
        &report_path,
        serde_json::to_vec_pretty(&stored).expect("json"),
    )
    .expect("write");
    let replayed =
        replay(path.display().to_string(), output.display().to_string()).expect("replay");
    assert_eq!(replayed["status"], "REPLAY_DIVERGED");
}
