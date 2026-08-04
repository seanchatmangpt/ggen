use ed25519_dalek::{Signer, SigningKey};

use super::evaluation::evaluate;
use super::*;

/// Deterministic ed25519 keypair standing in for the trust registry's registered
/// external-acceptance issuer across the whole test module.
fn issuer_signing_key() -> SigningKey {
    SigningKey::from_bytes(&[0x42_u8; 32])
}

/// Deterministic ed25519 keypair standing in for a registered execution-grant broker.
fn broker_signing_key() -> SigningKey {
    SigningKey::from_bytes(&[0x99_u8; 32])
}

fn public_key_hex(signing_key: &SigningKey) -> String {
    hex::encode(signing_key.verifying_key().to_bytes())
}

/// Sign exactly the bytes `trusted_issuer_signature_valid` recomputes and verifies
/// against (`ExternalAcceptanceBody`), so tests exercise the real signature check
/// rather than a placeholder the evaluator never inspects.
fn sign_acceptance(
    signing_key: &SigningKey, subject: &str, decision: &str, issuer: &str, report_digest: &str,
) -> String {
    let body = serde_json::to_vec(&ExternalAcceptanceBody {
        schema: EXTERNAL_ACCEPTANCE_SCHEMA,
        subject,
        decision,
        issuer,
        report_digest,
    })
    .expect("acceptance body bytes");
    hex::encode(signing_key.sign(&body).to_bytes())
}

/// Sign exactly the bytes `trusted_broker_signature_valid` recomputes and verifies
/// against (`ExecutionGrantBody`).
fn sign_grant(
    signing_key: &SigningKey, subject: &str, broker: &str, grant: &str, report_digest: &str,
) -> String {
    let body = serde_json::to_vec(&ExecutionGrantBody {
        schema: EXECUTION_GRANT_SCHEMA,
        subject,
        broker,
        grant,
        report_digest,
    })
    .expect("grant body bytes");
    hex::encode(signing_key.sign(&body).to_bytes())
}

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
            "manifest_digest": "0".repeat(64),
            "sbb": {
                "id": id,
                "version": "1.0.0",
                "architecture_contract": iri,
                "minimum_commit_equivalent_units": 1
            },
            "standing": "PARTIAL_ALIVE",
            "claim_ceiling": "PARTIAL_ALIVE",
            "target_met": true,
            "eligible_for_external_admission": true,
            "declared_deltas": 1,
            "commit_equivalent_units": 1,
            "duplicate_commit_collisions": 0,
            "axes": {},
            "distribution_contexts": "1000",
            "delivered_capability_instances": "1000",
            "deltas": [],
            "violations": [],
            "report_digest": report_digest
        }),
    );
    let receipt = json_evidence(
        &root.join(format!("sbb-result-{index}.json")),
        &json!({
            "schema": SBB_RECEIPT_SCHEMA,
            "operation": "density-evaluate-result",
            "manifest_digest": "0".repeat(64),
            "report_digest": report_digest,
            "previous_digest": "0".repeat(64),
            "artifacts": [],
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
    let issuer = "independent-test-authority";
    let signing_key = issuer_signing_key();
    let signature = sign_acceptance(&signing_key, iri, "ACCEPTED", issuer, &report_digest);
    let acceptance = json_evidence(
        &root.join(format!("acceptance-{index}.json")),
        &json!({
            "schema": EXTERNAL_ACCEPTANCE_SCHEMA,
            "subject": iri,
            "decision": "ACCEPTED",
            "issuer": issuer,
            "report_digest": report_digest,
            "issuer_public_key": public_key_hex(&signing_key),
            "signature": signature
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
            trusted_issuers: BTreeMap::from([(
                "independent-test-authority".to_string(),
                public_key_hex(&issuer_signing_key()),
            )]),
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

fn runtime_capability_index() -> usize {
    REQUIRED_DOMAINS
        .iter()
        .position(|domain| *domain == "runtime")
        .expect("runtime domain present in REQUIRED_DOMAINS")
}

fn runtime_capability_iri() -> String {
    "urn:ggen:test:runtime".to_string()
}

/// Promotes the fixture's "runtime" capability (an authority that is not restricted
/// the way healthcare/doctor are) to `actuate` and attaches an execution-grant
/// evidence binding built from the given (possibly untrusted or forged) fields.
fn promote_runtime_to_actuate_with_grant(
    root: &Path, path: &Path, broker: &str, broker_public_key: &str, signature: &str,
) {
    let mut manifest = read_manifest(path);
    let index = runtime_capability_index();
    manifest.capabilities[index].authority = "actuate".to_string();
    let report_digest = report_digest_value(index);
    let grant = json_evidence(
        &root.join("execution-grant.json"),
        &json!({
            "schema": EXECUTION_GRANT_SCHEMA,
            "subject": runtime_capability_iri(),
            "broker": broker,
            "grant": "GRANTED",
            "report_digest": report_digest,
            "broker_public_key": broker_public_key,
            "signature": signature
        }),
    );
    manifest.capabilities[index]
        .evidence
        .insert("execution_grant".to_string(), grant);
    write_manifest(path, &manifest);
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
            "manifest_digest": "0".repeat(64),
            "sbb": {
                "id": capability.id,
                "version": "1.0.0",
                "architecture_contract": capability.iri,
                "minimum_commit_equivalent_units": 1
            },
            "standing": "PARTIAL_ALIVE",
            "claim_ceiling": "PARTIAL_ALIVE",
            "target_met": true,
            "eligible_for_external_admission": true,
            "declared_deltas": 1,
            "commit_equivalent_units": 1,
            "duplicate_commit_collisions": 0,
            "axes": {},
            "distribution_contexts": "1000",
            "delivered_capability_instances": "1000",
            "deltas": [],
            "violations": [],
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
            "manifest_digest": "0".repeat(64),
            "report_digest": shared_digest,
            "previous_digest": "0".repeat(64),
            "artifacts": [],
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
            "report_digest": shared_digest,
            "issuer_public_key": "0".repeat(64),
            "signature": "0".repeat(128)
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

/// Regression for the dead-code trust-registry bug: an acceptance whose issuer was
/// never registered in `program.trusted_issuers` must be refused even though every
/// other field (schema, subject, decision, issuer non-empty and not self-issued,
/// report digest) is otherwise perfectly valid and the signature itself is real.
#[test]
fn untrusted_external_acceptance_issuer_is_refused() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    assert!(
        !manifest.program.trusted_issuers.is_empty(),
        "fixture must start with a populated trust registry for this test to be meaningful"
    );
    manifest.program.trusted_issuers.clear();
    write_manifest(&path, &manifest);

    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert_ne!(report.standing, "ALIVE");
    assert!(report
        .capabilities
        .iter()
        .all(|capability| capability.standing != "ALIVE"));
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("untrusted")));
}

/// Regression for the dead-code signature-verification bug: an acceptance issued
/// under a registered, correct public key but carrying an all-zero, non-cryptographic
/// garbage signature must be refused. Before the fix, `verify_signature` was defined
/// but never called, so this exact shape passed unchecked.
#[test]
fn garbage_external_acceptance_signature_is_refused() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    let capability = manifest.capabilities[0].clone();
    let report_digest = report_digest_value(0);
    replace_json_evidence(
        &mut manifest,
        0,
        "external_acceptance",
        &directory.path().join("acceptance-0.json"),
        &json!({
            "schema": EXTERNAL_ACCEPTANCE_SCHEMA,
            "subject": capability.iri,
            "decision": "ACCEPTED",
            "issuer": "independent-test-authority",
            "report_digest": report_digest,
            "issuer_public_key": public_key_hex(&issuer_signing_key()),
            "signature": "0".repeat(128)
        }),
    );
    write_manifest(&path, &manifest);

    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    let evaluated = report
        .capabilities
        .iter()
        .find(|report_entry| report_entry.id == capability.id)
        .expect("capability present in report");
    assert_ne!(evaluated.standing, "ALIVE");
    assert!(evaluated
        .violations
        .iter()
        .any(|violation| violation.contains("untrusted")));
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

/// Positive control for the execution-grant trust check: a broker registered in
/// `program.trusted_brokers` under the exact public key it signs with, granting a
/// genuine ed25519 signature over the canonical grant body, must be accepted and let
/// the actuating capability reach ALIVE.
#[test]
fn trusted_execution_grant_permits_actuation() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    manifest.program.trusted_brokers.insert(
        "trusted-runtime-broker".to_string(),
        public_key_hex(&broker_signing_key()),
    );
    write_manifest(&path, &manifest);

    let report_digest = report_digest_value(runtime_capability_index());
    let signature = sign_grant(
        &broker_signing_key(),
        &runtime_capability_iri(),
        "trusted-runtime-broker",
        "GRANTED",
        &report_digest,
    );
    promote_runtime_to_actuate_with_grant(
        directory.path(),
        &path,
        "trusted-runtime-broker",
        &public_key_hex(&broker_signing_key()),
        &signature,
    );

    let report = evaluate(&path).expect("report");
    let runtime = report
        .capabilities
        .iter()
        .find(|capability| capability.id == "cap-runtime")
        .expect("runtime capability present");
    assert_eq!(
        runtime.standing, "ALIVE",
        "violations: {:?}",
        runtime.violations
    );
    assert!(report.achieved);
}

/// Regression: a broker never registered in `program.trusted_brokers` must be
/// refused even though it holds a real ed25519 keypair and signs the grant body
/// correctly -- signature validity alone must not substitute for trust-registry
/// membership.
#[test]
fn untrusted_execution_grant_broker_blocks_actuation() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    // program.trusted_brokers is left empty -- "rogue-broker" is never registered.
    let report_digest = report_digest_value(runtime_capability_index());
    let signature = sign_grant(
        &broker_signing_key(),
        &runtime_capability_iri(),
        "rogue-broker",
        "GRANTED",
        &report_digest,
    );
    promote_runtime_to_actuate_with_grant(
        directory.path(),
        &path,
        "rogue-broker",
        &public_key_hex(&broker_signing_key()),
        &signature,
    );

    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    let runtime = report
        .capabilities
        .iter()
        .find(|capability| capability.id == "cap-runtime")
        .expect("runtime capability present");
    assert_ne!(runtime.standing, "ALIVE");
    assert!(runtime
        .violations
        .iter()
        .any(|violation| violation.contains("trusted, signed execution grant")));
}

/// Regression for the dead-code signature-verification bug on the execution-grant
/// side: a registered broker's genuine public key paired with an all-zero garbage
/// signature must be refused, not silently accepted.
#[test]
fn garbage_execution_grant_signature_blocks_actuation() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    manifest.program.trusted_brokers.insert(
        "trusted-runtime-broker".to_string(),
        public_key_hex(&broker_signing_key()),
    );
    write_manifest(&path, &manifest);

    promote_runtime_to_actuate_with_grant(
        directory.path(),
        &path,
        "trusted-runtime-broker",
        &public_key_hex(&broker_signing_key()),
        &"0".repeat(128),
    );

    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    let runtime = report
        .capabilities
        .iter()
        .find(|capability| capability.id == "cap-runtime")
        .expect("runtime capability present");
    assert_ne!(runtime.standing, "ALIVE");
    assert!(runtime
        .violations
        .iter()
        .any(|violation| violation.contains("trusted, signed execution grant")));
}
