chicago_tdd_tools::test!(source_manifest_has_129_unique_safe_entries, {
    let manifest = load_manifest(&project_root()).expect("source manifest must parse");
    assert_eq!(manifest.len(), EXPECTED_MANIFEST_ENTRIES);
    assert!(manifest
        .keys()
        .all(|path| !path.starts_with('/') && !path.contains("..")));
});

chicago_tdd_tools::test!(all_43_capabilities_have_typed_reference_standing, {
    let evidence = product_evidence().expect("all capability evidence must stand");
    assert_eq!(evidence.len(), 43);
    assert_eq!(
        evidence
            .iter()
            .map(|item| item.id)
            .collect::<BTreeSet<_>>()
            .len(),
        43
    );
    assert_eq!(
        evidence
            .iter()
            .filter(|item| item.reference_standing == ReferenceStanding::ExactManifest)
            .count(),
        36
    );
    assert_eq!(
        evidence
            .iter()
            .filter(|item| item.reference_standing == ReferenceStanding::DeclaredDerivative)
            .count(),
        5
    );
    assert_eq!(
        evidence
            .iter()
            .filter(|item| item.reference_standing == ReferenceStanding::Independent)
            .count(),
        2
    );
    assert!(evidence
        .iter()
        .all(|item| item.byte_len > 0 && !item.facts.is_empty()));
});

chicago_tdd_tools::test!(declared_derivative_population_is_exact_and_named, {
    let evidence = product_evidence().expect("evidence");
    let derivatives: BTreeSet<_> = evidence
        .iter()
        .filter(|item| item.reference_standing == ReferenceStanding::DeclaredDerivative)
        .map(|item| item.id)
        .collect();
    assert_eq!(
        derivatives,
        BTreeSet::from([
            "core.auto-select",
            "core.blue-river-dam",
            "core.crypto-digest",
            "core.kaizen",
            "runtime.std",
        ])
    );
    assert!(evidence
        .iter()
        .filter(|item| item.reference_standing == ReferenceStanding::DeclaredDerivative)
        .all(|item| item
            .reference_sha256
            .as_ref()
            .is_some_and(|digest| digest != &item.sha256)));
});

chicago_tdd_tools::test!(auto_select_derivative_is_the_exact_rdf_projection, {
    let root = project_root();
    let generated = read_nonempty(&root, "src/自動選択.rs").expect("generated Auto Select");
    validate_auto_select_override(&root, &generated).expect("RDF override must equal projection");

    let evidence = product_evidence().expect("evidence");
    let auto_select = evidence
        .iter()
        .find(|item| item.id == "core.auto-select")
        .expect("Auto Select evidence");
    assert!(auto_select
        .facts
        .contains(&"rdf-source-override-byte-match"));
});

chicago_tdd_tools::test!(duplicate_auto_select_rdf_override_is_refused, {
    let root = project_root();
    let bytes = read_nonempty(&root, "schema/domain.ttl").expect("domain ontology");
    let text = utf8("schema/domain.ttl", &bytes).expect("UTF-8");
    let duplicate = format!(
        "{text}\n{AUTO_SELECT_OVERRIDE_PREFIX}duplicate{AUTO_SELECT_OVERRIDE_SUFFIX}\n"
    );
    assert!(matches!(
        extract_single_auto_select_override(&duplicate),
        Err(EvidenceError::InvalidData { message, .. })
            if message.contains("exactly one") || message.contains("multiple")
    ));
});

chicago_tdd_tools::test!(auto_select_rdf_projection_mutation_is_refused, {
    let root = project_root();
    let ontology = read_nonempty(&root, "schema/domain.ttl").expect("domain ontology");
    let mut generated = read_nonempty(&root, "src/自動選択.rs").expect("generated Auto Select");
    generated[0] ^= 1;

    let temp = tempfile::TempDir::new().expect("tempdir");
    std::fs::create_dir_all(temp.path().join("schema")).expect("schema directory");
    std::fs::write(temp.path().join("schema/domain.ttl"), ontology).expect("write ontology");

    assert!(matches!(
        validate_auto_select_override(temp.path(), &generated),
        Err(EvidenceError::InvalidData { message, .. })
            if message.contains("projection mismatch")
    ));
});

chicago_tdd_tools::test!(capability_kinds_cover_the_complete_product_boundary, {
    let evidence = product_evidence().expect("evidence");
    let mut counts = BTreeMap::new();
    for item in evidence {
        *counts.entry(item.kind).or_insert(0_usize) += 1;
    }
    assert_eq!(counts.get(&CapabilityKind::Core), Some(&26));
    assert_eq!(counts.get(&CapabilityKind::Runtime), Some(&4));
    assert_eq!(counts.get(&CapabilityKind::Release), Some(&6));
    assert_eq!(counts.get(&CapabilityKind::Evidence), Some(&4));
    assert_eq!(counts.get(&CapabilityKind::Manufacture), Some(&3));
});

chicago_tdd_tools::test!(ggen_lock_names_exactly_the_eight_admitted_packs, {
    let bytes = read_nonempty(&project_root(), "ggen.lock").expect("ggen.lock");
    let count =
        validate_ggen_lock(utf8("ggen.lock", &bytes).expect("UTF-8")).expect("valid ggen lock");
    assert_eq!(count, EXPECTED_PACKS.len());
});

chicago_tdd_tools::test!(exact_manifest_digest_mutation_is_refused, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let capability = CAPABILITIES
        .iter()
        .find(|item| item.id == "core.origin")
        .expect("exact-manifest capability");
    let temp = tempfile::TempDir::new().expect("tempdir");
    let target = temp.path().join(capability.path);
    std::fs::create_dir_all(target.parent().expect("parent")).expect("mkdir");
    let mut bytes = read_nonempty(&root, capability.path).expect("source");
    bytes.push(b'\n');
    std::fs::write(&target, bytes).expect("write mutation");

    let result = validate_capability(temp.path(), &manifest, capability);
    assert!(matches!(result, Err(EvidenceError::InvalidData { message, .. }) if message.contains("undeclared SHA-256 drift")));
});

chicago_tdd_tools::test!(declared_derivative_unpinned_drift_is_refused, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let capability = CAPABILITIES
        .iter()
        .find(|item| item.id == "core.kaizen")
        .expect("declared derivative");
    let temp = tempfile::TempDir::new().expect("tempdir");
    let target = temp.path().join(capability.path);
    std::fs::create_dir_all(target.parent().expect("parent")).expect("mkdir");
    let mut bytes = read_nonempty(&root, capability.path).expect("source");
    bytes.push(b'\n');
    std::fs::write(&target, bytes).expect("write derivative mutation");

    let result = validate_capability(temp.path(), &manifest, capability);
    assert!(matches!(result, Err(EvidenceError::InvalidData { message, .. }) if message.contains("undeclared derivative drift")));
});

chicago_tdd_tools::test!(declared_derivative_becoming_exact_requires_reclassification, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let capability = CAPABILITIES
        .iter()
        .find(|item| item.id == "core.kaizen")
        .expect("declared derivative");
    let manifest_path = capability.manifest_path.expect("source reference path");
    let reference = root
        .join("../../packs/tcps-core-pack/reference/製品版")
        .join(manifest_path);
    let temp = tempfile::TempDir::new().expect("tempdir");
    let target = temp.path().join(capability.path);
    std::fs::create_dir_all(target.parent().expect("parent")).expect("mkdir");
    std::fs::copy(reference, target).expect("copy exact reference into derivative slot");

    let result = validate_capability(temp.path(), &manifest, capability);
    assert!(matches!(result, Err(EvidenceError::InvalidData { message, .. }) if message.contains("must be reclassified")));
});

chicago_tdd_tools::test!(missing_capability_is_a_loud_refusal, {
    let manifest = load_manifest(&project_root()).expect("manifest");
    let capability = CAPABILITIES[0];
    let empty = tempfile::TempDir::new().expect("tempdir");
    assert!(matches!(
        validate_capability(empty.path(), &manifest, &capability),
        Err(EvidenceError::InvalidData { message, .. }) if message.starts_with("MISSING_ARTIFACT:")
    ));
});

chicago_tdd_tools::test!(receipt_is_replay_stable_and_cryptographically_bound, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let evidence = product_evidence().expect("evidence");
    let first = receipt(evidence, manifest.len()).expect("receipt");
    let second = receipt(evidence, manifest.len()).expect("receipt replay");
    assert_eq!(first, second);
    assert_eq!(first.schema, "tcps-product-evidence/v5");
    assert_eq!(first.capability_count, 43);
    assert_eq!(first.manifest_entry_count, 129);
    assert_eq!(first.exact_manifest_capabilities, 36);
    assert_eq!(first.declared_derivative_capabilities, 5);
    assert_eq!(first.independent_capabilities, 2);
    assert_eq!(first.evidence_root.len(), 64);
});

chicago_tdd_tools::test!(receipt_json_is_valid_and_contains_the_evidence_index, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let evidence = product_evidence().expect("evidence");
    let receipt = receipt(evidence, manifest.len()).expect("receipt");
    let bundle = serde_json::json!({
        "receipt": receipt,
        "capabilities": evidence,
    });
    let bytes = serialize_json(&bundle).expect("serialize bundle");
    let decoded: JsonValue = serde_json::from_slice(&bytes).expect("parse bundle");
    assert_eq!(decoded["receipt"]["capability_count"].as_u64(), Some(43));
    assert_eq!(
        decoded["receipt"]["exact_manifest_capabilities"].as_u64(),
        Some(36)
    );
    assert_eq!(
        decoded["receipt"]["declared_derivative_capabilities"].as_u64(),
        Some(5)
    );
    assert_eq!(decoded["capabilities"].as_array().map(Vec::len), Some(43));
});

chicago_tdd_tools::test!(receipt_root_changes_when_one_evidence_record_changes, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let evidence = product_evidence().expect("evidence");
    let original = receipt(evidence, manifest.len()).expect("original receipt");

    let mut mutated = evidence.to_vec();
    mutated[0].evidence_digest = domain_digest(
        "tcps/adversarial-evidence-mutation/v1",
        &[mutated[0].evidence_digest.as_bytes()],
    )
    .to_hex();
    let changed = receipt(&mutated, manifest.len()).expect("changed receipt");
    assert_ne!(changed.evidence_root, original.evidence_root);
});

chicago_tdd_tools::test!(malformed_pack_hash_is_refused, {
    let bytes = read_nonempty(&project_root(), "ggen.lock").expect("ggen.lock");
    let text = utf8("ggen.lock", &bytes).expect("UTF-8");
    let mutated = text.replacen("blake3:", "sha256:", 1);
    assert!(matches!(
        validate_ggen_lock(&mutated),
        Err(EvidenceError::InvalidData { message, .. }) if message.contains("not BLAKE3")
    ));
});
