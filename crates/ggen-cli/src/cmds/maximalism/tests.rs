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

fn sbb_receipt(report_digest_value: &str) -> SbbReceipt {
    let operation = "density-evaluate-result".to_string();
    let manifest_digest = "1".repeat(64);
    let previous_digest = "2".repeat(64);
    let artifacts = vec![
        "density-report.json".to_string(),
        "density-intent.json".to_string(),
    ];
    let digest = digest_json(&SbbReceiptBody {
        schema: SBB_RECEIPT_SCHEMA,
        operation: &operation,
        manifest_digest: &manifest_digest,
        report_digest: report_digest_value,
        previous_digest: &previous_digest,
        artifacts: &artifacts,
    })
    .expect("digest");
    SbbReceipt {
        schema: SBB_RECEIPT_SCHEMA.to_string(),
        operation,
        manifest_digest,
        report_digest: report_digest_value.to_string(),
        previous_digest,
        artifacts,
        digest_algorithm: "blake3".to_string(),
        digest,
    }
}

fn capability_id(domain: &str) -> String {
    match domain {
        "doctor" => "cap-doctor".to_string(),
        "wizard" => "cap-wizard".to_string(),
        "telco" => "telco-office".to_string(),
        _ => format!("cap-{domain}"),
    }
}

fn capability(
    root: &Path, index: usize, id: String, domain: &str, surface: &str, outcome: &str,
    report: &SbbReport, report_evidence: &Evidence, receipt_evidence: &Evidence,
    replay_evidence: &Evidence,
) -> Capability {
    let iri = format!("urn:ggen:maximalism:test:{id}");
    let positive = json_evidence(
        &root.join(format!("positive-{index}.json")),
        &ProofWitness {
            schema: WITNESS_SCHEMA.to_string(),
            kind: "positive".to_string(),
            subject: iri.clone(),
            result: "PASS".to_string(),
            report_digest: report.report_digest.clone(),
        },
    );
    let negative = json_evidence(
        &root.join(format!("negative-{index}.json")),
        &ProofWitness {
            schema: WITNESS_SCHEMA.to_string(),
            kind: "negative".to_string(),
            subject: iri.clone(),
            result: "PASS".to_string(),
            report_digest: report.report_digest.clone(),
        },
    );
    let adversarial = json_evidence(
        &root.join(format!("adversarial-{index}.json")),
        &ProofWitness {
            schema: WITNESS_SCHEMA.to_string(),
            kind: "adversarial".to_string(),
            subject: iri.clone(),
            result: "PASS".to_string(),
            report_digest: report.report_digest.clone(),
        },
    );
    let verifier = json_evidence(
        &root.join(format!("verifier-{index}.json")),
        &VerifierWitness {
            schema: VERIFIER_SCHEMA.to_string(),
            subject: iri.clone(),
            verifier: "maximalism-test-verifier".to_string(),
            result: "PASS".to_string(),
            report_digest: report.report_digest.clone(),
        },
    );
    let passport = json_evidence(
        &root.join(format!("passport-{index}.json")),
        &Passport {
            schema: PASSPORT_SCHEMA.to_string(),
            subject: iri.clone(),
            report_digest: report.report_digest.clone(),
            manifest: "manifest.json".to_string(),
            architecture_contract: "urn:ggen:test:architecture-contract".to_string(),
            route_model: "route.powl.json".to_string(),
            interface_contract: "interface.wit".to_string(),
            schemas: vec!["schema.json".to_string()],
            positive_fixtures: vec!["positive.json".to_string()],
            negative_fixtures: vec!["negative.json".to_string()],
            adversary_results: vec!["adversary.json".to_string()],
            provenance: vec!["provenance.ttl".to_string()],
            shacl_result: "CONFORMS".to_string(),
            runtime_verdict: "PASS".to_string(),
            telemetry_verdict: "PASS".to_string(),
            deployment_hash: "3".repeat(64),
            signature: "test-signature".to_string(),
            bundle_digest: "4".repeat(64),
        },
    );
    let acceptance = json_evidence(
        &root.join(format!("acceptance-{index}.json")),
        &ExternalAcceptance {
            schema: ACCEPTANCE_SCHEMA.to_string(),
            subject: iri.clone(),
            decision: "ACCEPTED".to_string(),
            issuer: "independent-test-authority".to_string(),
            report_digest: report.report_digest.clone(),
        },
    );
    let evidence = [
        ("sbb_report".to_string(), report_evidence.clone()),
        ("positive".to_string(), positive),
        ("negative".to_string(), negative),
        ("adversarial".to_string(), adversarial),
        ("verifier".to_string(), verifier),
        ("passport".to_string(), passport),
        ("receipt".to_string(), receipt_evidence.clone()),
        ("replay".to_string(), replay_evidence.clone()),
        ("external_acceptance".to_string(), acceptance),
    ]
    .into_iter()
    .collect();
    Capability {
        id,
        iri,
        domain: domain.to_string(),
        horizon: HORIZONS[index % HORIZONS.len()],
        authority: match domain {
            "healthcare" => "observe".to_string(),
            "doctor" | "truthforge" => "recommend".to_string(),
            _ => "construct".to_string(),
        },
        surface: surface.to_string(),
        summary: format!("Accepted {domain} capability"),
        outcomes: vec![outcome.to_string()],
        dependencies: Vec::new(),
        evidence,
    }
}

fn fixture(root: &Path) -> PathBuf {
    let report = SbbReport {
        schema: SBB_REPORT_SCHEMA.to_string(),
        claim_ceiling: "PARTIAL_ALIVE".to_string(),
        eligible_for_external_admission: true,
        commit_equivalent_units: 2,
        axes: [
            ("ontology_modules".to_string(), 4),
            ("textual_forms".to_string(), 8),
            ("audiences".to_string(), 2),
            ("languages".to_string(), 1),
            ("jurisdictions".to_string(), 2),
            ("organization_profiles".to_string(), 2),
            ("runtimes".to_string(), 4),
        ]
        .into_iter()
        .collect(),
        distribution_contexts: "1000".to_string(),
        delivered_capability_instances: "2000".to_string(),
        report_digest: "a".repeat(64),
    };
    let report_evidence = json_evidence(&root.join("sbb-report.json"), &report);
    let receipt_evidence = json_evidence(
        &root.join("sbb-result.json"),
        &sbb_receipt(&report.report_digest),
    );
    let replay_evidence = json_evidence(
        &root.join("sbb-replay.json"),
        &ReplayWitness {
            schema: SBB_REPLAY_SCHEMA.to_string(),
            status: "REPLAY_MATCH".to_string(),
            matches: true,
            report_digest: report.report_digest.clone(),
        },
    );

    let mut capabilities = REQUIRED_DOMAINS
        .iter()
        .enumerate()
        .map(|(index, domain)| {
            capability(
                root,
                index,
                capability_id(domain),
                domain,
                if *domain == "telco" { "office" } else { domain },
                OUTCOMES[index % OUTCOMES.len()],
                &report,
                &report_evidence,
                &receipt_evidence,
                &replay_evidence,
            )
        })
        .collect::<Vec<_>>();
    for (offset, (id, surface)) in [
        ("telco-register", "register"),
        ("telco-line", "line"),
        ("telco-bridge", "bridge"),
        ("telco-record", "record"),
    ]
    .iter()
    .enumerate()
    {
        let index = capabilities.len();
        capabilities.push(capability(
            root,
            index,
            (*id).to_string(),
            "telco",
            surface,
            OUTCOMES[(index + offset) % OUTCOMES.len()],
            &report,
            &report_evidence,
            &receipt_evidence,
            &replay_evidence,
        ));
    }

    let manifest = Manifest {
        schema: MANIFEST_SCHEMA.to_string(),
        program: Program {
            id: "maximalism-test".to_string(),
            version: "1.0.0".to_string(),
            target_year: 2030,
            minimum_multiplier: 1000,
        },
        required_domains: REQUIRED_DOMAINS
            .iter()
            .map(|domain| (*domain).to_string())
            .collect(),
        required_outcomes: OUTCOMES
            .iter()
            .map(|outcome| (*outcome).to_string())
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
    fs::write(
        &path,
        serde_json::to_vec_pretty(&manifest).expect("manifest"),
    )
    .expect("write");
    path
}

fn read_manifest(path: &Path) -> Manifest {
    serde_json::from_slice(&fs::read(path).expect("manifest")).expect("parse")
}

fn write_manifest(path: &Path, manifest: &Manifest) {
    fs::write(path, serde_json::to_vec_pretty(manifest).expect("manifest")).expect("write");
}

fn rewrite_evidence<T: Serialize>(root: &Path, evidence: &mut Evidence, value: &T) {
    let path = root.join(&evidence.locator);
    *evidence = json_evidence(&path, value);
}

#[test]
fn complete_program_closes_domains_outcomes_and_thousand_x() {
    let directory = tempfile::tempdir().expect("tempdir");
    let report = evaluate(&fixture(directory.path())).expect("report");
    assert!(report.achieved, "{:?}", report.violations);
    assert_eq!(report.standing, "ALIVE");
    assert_eq!(report.measured_multiplier, "1000.000");
    assert!(report.semantic_cells.parse::<u128>().expect("cells") > 0);
    assert_eq!(report.alive_domain_count, REQUIRED_DOMAINS.len());
    assert_eq!(
        report.domain_combination_space,
        ((1_u128 << REQUIRED_DOMAINS.len()) - 1).to_string()
    );
    assert!(report.domains.values().all(|coverage| coverage.closed));
    assert!(report.outcomes.values().all(|coverage| coverage.closed));
    assert!(report.horizons.values().all(|horizon| horizon.closed));
}

#[test]
fn incomplete_passport_is_refused() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    let capability = &mut manifest.capabilities[0];
    let passport = Passport {
        schema: PASSPORT_SCHEMA.to_string(),
        subject: capability.iri.clone(),
        report_digest: "a".repeat(64),
        manifest: String::new(),
        architecture_contract: String::new(),
        route_model: String::new(),
        interface_contract: String::new(),
        schemas: Vec::new(),
        positive_fixtures: Vec::new(),
        negative_fixtures: Vec::new(),
        adversary_results: Vec::new(),
        provenance: Vec::new(),
        shacl_result: String::new(),
        runtime_verdict: String::new(),
        telemetry_verdict: String::new(),
        deployment_hash: String::new(),
        signature: String::new(),
        bundle_digest: String::new(),
    };
    rewrite_evidence(
        directory.path(),
        capability.evidence.get_mut("passport").expect("passport"),
        &passport,
    );
    write_manifest(&path, &manifest);
    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("passport")));
}

#[test]
fn wrong_witness_kind_is_refused() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    let capability = &mut manifest.capabilities[0];
    let witness = ProofWitness {
        schema: WITNESS_SCHEMA.to_string(),
        kind: "negative".to_string(),
        subject: capability.iri.clone(),
        result: "PASS".to_string(),
        report_digest: "a".repeat(64),
    };
    rewrite_evidence(
        directory.path(),
        capability.evidence.get_mut("positive").expect("positive"),
        &witness,
    );
    write_manifest(&path, &manifest);
    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("positive witness")));
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
fn doctor_and_healthcare_actuation_are_refused() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let mut manifest = read_manifest(&path);
    for capability in &mut manifest.capabilities {
        if matches!(capability.domain.as_str(), "doctor" | "healthcare") {
            capability.authority = "actuate".to_string();
        }
    }
    write_manifest(&path, &manifest);
    let report = evaluate(&path).expect("report");
    assert!(!report.achieved);
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("Doctor capability")));
    assert!(report
        .violations
        .iter()
        .any(|violation| violation.contains("healthcare authority")));
}

#[test]
fn receipts_replay_and_refuse_tampering() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let output = directory.path().join("receipts");
    receipt(path.display().to_string(), output.display().to_string()).expect("receipt");
    let replayed =
        replay(path.display().to_string(), output.display().to_string()).expect("replay");
    assert_eq!(replayed["status"], "REPLAY_MATCH");
    let report_path = output.join("maximalism-report.json");
    let mut stored: Value =
        serde_json::from_slice(&fs::read(&report_path).expect("report")).expect("json");
    stored["measured_multiplier"] = json!("999999.000");
    fs::write(
        &report_path,
        serde_json::to_vec_pretty(&stored).expect("json"),
    )
    .expect("write");
    let replayed =
        replay(path.display().to_string(), output.display().to_string()).expect("replay");
    assert_eq!(replayed["status"], "REPLAY_DIVERGED");
}

#[test]
fn wizard_is_topological_and_non_actuating() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    let value = wizard_plan(&path, "cap-wizard").expect("wizard");
    assert_eq!(value["never_generate_hot_law"], true);
    assert_eq!(value["actuated"], false);
    assert_eq!(value["dependency_order"][0], "cap-wizard");
}

#[test]
fn telco_covers_office_register_line_bridge_and_record() {
    let directory = tempfile::tempdir().expect("tempdir");
    let path = fixture(directory.path());
    for surface in ["office", "register", "line", "bridge", "record"] {
        let value = telco_surface(&path, surface).expect("telco");
        assert_eq!(value["closed"], true, "{surface}");
        assert_eq!(value["surfaces"][0]["surface"], surface);
    }
}
