chicago_tdd_tools::test!(contract_registry_declares_only_observed_boundaries, {
    let registry = TestContractRegistry::new(CONTRACTS);
    assert_eq!(registry.len(), 4);
    assert!(registry
        .uncovered_modules(&[
            "tcps.source-manifest",
            "tcps.capability-catalog",
            "tcps.ggen-lock",
            "tcps.evidence-receipt",
        ])
        .is_empty());
});

chicago_tdd_tools::test!(source_manifest_has_129_unique_safe_entries, {
    let manifest = load_manifest(&project_root()).expect("source manifest must parse");
    assert_eq!(manifest.len(), EXPECTED_MANIFEST_ENTRIES);
    assert!(manifest
        .keys()
        .all(|path| !path.starts_with('/') && !path.contains("..")));
});

chicago_tdd_tools::test!(all_43_capabilities_have_real_format_and_identity_evidence, {
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
        evidence.iter().filter(|item| item.manifest_bound).count(),
        41
    );
    assert!(evidence
        .iter()
        .all(|item| item.byte_len > 0 && !item.facts.is_empty()));
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

chicago_tdd_tools::test!(ggen_lock_binds_exactly_the_eight_admitted_packs, {
    let bytes = read_nonempty(&project_root(), "ggen.lock").expect("ggen.lock");
    let count =
        validate_ggen_lock(utf8("ggen.lock", &bytes).expect("UTF-8")).expect("valid ggen lock");
    assert_eq!(count, EXPECTED_PACKS.len());
});

chicago_tdd_tools::test!(manifest_digest_mutation_is_refused, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let capability = CAPABILITIES
        .iter()
        .find(|item| item.id == "core.auto-select")
        .expect("auto-select capability");
    let temp = tempfile::TempDir::new().expect("tempdir");
    let target = temp.path().join(capability.path);
    std::fs::create_dir_all(target.parent().expect("parent")).expect("mkdir");
    let mut bytes = read_nonempty(&root, capability.path).expect("source");
    bytes.push(b'\n');
    std::fs::write(&target, bytes).expect("write mutation");

    let result = validate_capability(temp.path(), &manifest, capability);
    assert!(matches!(result, Err(EvidenceError::InvalidData { message, .. }) if message.contains("SHA-256 mismatch")));
});

chicago_tdd_tools::test!(missing_capability_is_a_loud_refusal, {
    let manifest = load_manifest(&project_root()).expect("manifest");
    let capability = CAPABILITIES[0];
    let empty = tempfile::TempDir::new().expect("tempdir");
    assert!(matches!(
        validate_capability(empty.path(), &manifest, &capability),
        Err(EvidenceError::Missing { .. })
    ));
});

chicago_tdd_tools::test!(receipt_is_replay_stable_and_cryptographically_bound, {
    let root = project_root();
    let manifest = load_manifest(&root).expect("manifest");
    let evidence = product_evidence().expect("evidence");
    let first = receipt(evidence, manifest.len()).expect("receipt");
    let second = receipt(evidence, manifest.len()).expect("receipt replay");
    assert_eq!(first, second);
    assert_eq!(first.schema, "tcps-product-evidence/v2");
    assert_eq!(first.capability_count, 43);
    assert_eq!(first.manifest_entry_count, 129);
    assert_eq!(first.manifest_bound_capabilities, 41);
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
