use super::*;

#[derive(Debug)]
struct Candidate {
    report: CapabilityReport,
    intrinsic_alive: bool,
    observed_any: bool,
    dependencies: Vec<String>,
}

fn expected_digest(raw: &str) -> Option<&str> {
    raw.strip_prefix("blake3:")
        .or_else(|| raw.strip_prefix("blake3-"))
        .filter(|value| value.len() == 64 && value.bytes().all(|byte| byte.is_ascii_hexdigit()))
}

fn is_hex_digest(raw: &str) -> bool {
    raw.len() == 64 && raw.bytes().all(|byte| byte.is_ascii_hexdigit())
}

fn safe_locator(locator: &str) -> bool {
    let path = Path::new(locator);
    !locator.trim().is_empty()
        && !locator.contains("://")
        && !path.is_absolute()
        && !path.components().any(|part| {
            matches!(
                part,
                Component::ParentDir | Component::RootDir | Component::Prefix(_)
            )
        })
}

fn evidence_path(manifest: &Path, evidence: &Evidence) -> Option<PathBuf> {
    safe_locator(&evidence.locator).then(|| {
        manifest
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join(&evidence.locator)
    })
}

fn evidence_bytes(manifest: &Path, evidence: &Evidence) -> Option<Vec<u8>> {
    let expected = expected_digest(&evidence.digest)?;
    let bytes = fs::read(evidence_path(manifest, evidence)?).ok()?;
    expected
        .eq_ignore_ascii_case(&digest_bytes(&bytes))
        .then_some(bytes)
}

fn parse_evidence<T>(manifest: &Path, evidence: &Evidence) -> Option<T>
where
    T: for<'de> Deserialize<'de>,
{
    serde_json::from_slice(&evidence_bytes(manifest, evidence)?).ok()
}

fn unique_nonempty(values: &[String]) -> bool {
    !values.is_empty()
        && values.iter().all(|value| !value.trim().is_empty())
        && values.iter().collect::<BTreeSet<_>>().len() == values.len()
}

fn exact_evidence_keys(evidence: &BTreeMap<String, Evidence>) -> bool {
    let mut allowed = REQUIRED_EVIDENCE.iter().copied().collect::<BTreeSet<_>>();
    allowed.insert("execution_grant");
    let actual = evidence.keys().map(String::as_str).collect::<BTreeSet<_>>();
    REQUIRED_EVIDENCE.iter().all(|role| actual.contains(role))
        && actual.iter().all(|role| allowed.contains(role))
}

fn multiplier(delivered: u128, canonical: usize) -> String {
    if canonical == 0 {
        return "0.000".to_string();
    }
    let canonical = canonical as u128;
    let whole = delivered / canonical;
    let fractional = delivered.saturating_mul(1000) / canonical % 1000;
    format!("{whole}.{fractional:03}")
}

fn load(path: &Path) -> Result<(Manifest, Vec<u8>)> {
    let bytes = fs::read(path).map_err(|error| {
        NounVerbError::execution_error(format!("cannot read {}: {error}", path.display()))
    })?;
    let manifest = serde_json::from_slice(&bytes).map_err(|error| {
        NounVerbError::argument_error(format!("invalid {}: {error}", path.display()))
    })?;
    Ok((manifest, bytes))
}

fn validate_authority(capability: &Capability, violations: &mut Vec<String>) {
    if !AUTHORITIES.contains(&capability.authority.as_str()) {
        violations.push("authority is not recognized".to_string());
    }
    if capability.domain == "healthcare"
        && !matches!(capability.authority.as_str(), "observe" | "recommend")
    {
        violations.push("healthcare authority may observe or recommend only".to_string());
    }
    if capability.domain == "doctor" && capability.authority == "actuate" {
        violations.push("Doctor capability may not actuate".to_string());
    }
    if capability.domain == "wizard" && capability.authority == "actuate" {
        violations
            .push("Wizard capability may construct candidates but may not actuate".to_string());
    }
    if capability.domain == "truthforge"
        && !matches!(capability.authority.as_str(), "observe" | "recommend")
    {
        violations.push("Truthforge may observe or recommend admission only".to_string());
    }
}

fn validate_sbb_receipt(receipt: &SbbReceipt, report_digest: &str) -> bool {
    if receipt.schema != SBB_RECEIPT_SCHEMA
        || receipt.operation != "density-evaluate-result"
        || receipt.report_digest != report_digest
        || receipt.digest_algorithm != "blake3"
        || !is_hex_digest(&receipt.digest)
    {
        return false;
    }
    digest_json(&SbbReceiptBody {
        schema: SBB_RECEIPT_SCHEMA,
        operation: &receipt.operation,
        manifest_digest: &receipt.manifest_digest,
        report_digest: &receipt.report_digest,
        previous_digest: &receipt.previous_digest,
        artifacts: &receipt.artifacts,
    })
    .is_ok_and(|digest| digest == receipt.digest)
}

fn validate_sbb(
    report: &SbbReport, violations: &mut Vec<String>,
) -> Option<(usize, u128, usize, usize, u128)> {
    if report.schema != SBB_REPORT_SCHEMA {
        violations.push("sbb_report has unsupported schema".to_string());
    }
    if report.claim_ceiling != "PARTIAL_ALIVE" {
        violations.push("sbb_report claim ceiling must be PARTIAL_ALIVE".to_string());
    }
    if !report.eligible_for_external_admission {
        violations.push("sbb_report is not eligible for external admission".to_string());
    }
    if !is_hex_digest(&report.report_digest) {
        violations.push("sbb_report report_digest is malformed".to_string());
    }
    let contexts = report.distribution_contexts.parse::<u128>().ok();
    let delivered = report.delivered_capability_instances.parse::<u128>().ok();
    let ontology_modules = report
        .axes
        .get("ontology_modules")
        .copied()
        .unwrap_or_default();
    let textual_forms = report
        .axes
        .get("textual_forms")
        .copied()
        .unwrap_or_default();
    if report.commit_equivalent_units == 0
        || contexts == Some(0)
        || delivered == Some(0)
        || ontology_modules == 0
        || textual_forms == 0
    {
        violations.push(
            "sbb_report has zero canonical units, semantic axes, or distribution".to_string(),
        );
    }
    let expected =
        contexts.and_then(|contexts| contexts.checked_mul(report.commit_equivalent_units as u128));
    if expected != delivered {
        violations.push("sbb_report delivered instances are inconsistent".to_string());
    }
    let semantic_cells = (report.commit_equivalent_units as u128)
        .checked_mul(ontology_modules as u128)
        .and_then(|value| value.checked_mul(textual_forms as u128));
    if semantic_cells.is_none() {
        violations.push("sbb_report semantic-cell product overflowed u128".to_string());
    }
    if violations.is_empty() {
        Some((
            report.commit_equivalent_units,
            delivered?,
            ontology_modules,
            textual_forms,
            semantic_cells?,
        ))
    } else {
        None
    }
}

fn validate_proof(
    manifest: &Path, capability: &Capability, role: &str, kind: &str, report_digest: &str,
    violations: &mut Vec<String>,
) {
    let witness = capability
        .evidence
        .get(role)
        .and_then(|binding| parse_evidence::<ProofWitness>(manifest, binding));
    match witness {
        Some(witness)
            if witness.schema == WITNESS_SCHEMA
                && witness.kind == kind
                && witness.subject == capability.iri
                && witness.result == "PASS"
                && witness.report_digest == report_digest => {}
        _ => violations.push(format!(
            "{role} witness is absent or does not prove {kind} for the exact report"
        )),
    }
}

fn validate_verifier(
    manifest: &Path, capability: &Capability, report_digest: &str, violations: &mut Vec<String>,
) {
    let verifier = capability
        .evidence
        .get("verifier")
        .and_then(|binding| parse_evidence::<VerifierWitness>(manifest, binding));
    match verifier {
        Some(verifier)
            if verifier.schema == VERIFIER_SCHEMA
                && verifier.subject == capability.iri
                && !verifier.verifier.trim().is_empty()
                && verifier.result == "PASS"
                && verifier.report_digest == report_digest => {}
        _ => violations.push("verifier does not prove the exact capability report".to_string()),
    }
}

fn passport_complete(passport: &Passport) -> bool {
    !passport.manifest.trim().is_empty()
        && !passport.architecture_contract.trim().is_empty()
        && !passport.route_model.trim().is_empty()
        && !passport.interface_contract.trim().is_empty()
        && unique_nonempty(&passport.schemas)
        && unique_nonempty(&passport.positive_fixtures)
        && unique_nonempty(&passport.negative_fixtures)
        && unique_nonempty(&passport.adversary_results)
        && unique_nonempty(&passport.provenance)
        && !passport.shacl_result.trim().is_empty()
        && !passport.runtime_verdict.trim().is_empty()
        && !passport.telemetry_verdict.trim().is_empty()
        && is_hex_digest(&passport.deployment_hash)
        && !passport.signature.trim().is_empty()
        && is_hex_digest(&passport.bundle_digest)
}

fn validate_passport(
    manifest: &Path, capability: &Capability, report_digest: &str, violations: &mut Vec<String>,
) {
    let passport = capability
        .evidence
        .get("passport")
        .and_then(|binding| parse_evidence::<Passport>(manifest, binding));
    match passport {
        Some(passport)
            if passport.schema == PASSPORT_SCHEMA
                && passport.subject == capability.iri
                && passport.report_digest == report_digest
                && passport_complete(&passport) => {}
        _ => violations.push("capability passport is incomplete or divergent".to_string()),
    }
}

fn validate_external_acceptance(
    manifest: &Path, program: &Program, capability: &Capability, report_digest: &str,
    violations: &mut Vec<String>,
) {
    let acceptance = capability
        .evidence
        .get("external_acceptance")
        .and_then(|binding| parse_evidence::<ExternalAcceptance>(manifest, binding));
    match acceptance {
        Some(acceptance)
            if acceptance.schema == ACCEPTANCE_SCHEMA
                && acceptance.subject == capability.iri
                && acceptance.decision == "ACCEPTED"
                && !acceptance.issuer.trim().is_empty()
                && acceptance.issuer != program.id
                && acceptance.report_digest == report_digest => {}
        _ => {
            violations.push("external acceptance is absent, self-issued, or divergent".to_string())
        }
    }
}

fn validate_execution_grant(
    manifest: &Path, capability: &Capability, report_digest: &str, violations: &mut Vec<String>,
) {
    if capability.authority != "actuate" {
        return;
    }
    let grant = capability
        .evidence
        .get("execution_grant")
        .and_then(|binding| parse_evidence::<ExecutionGrant>(manifest, binding));
    match grant {
        Some(grant)
            if grant.schema == EXECUTION_GRANT_SCHEMA
                && grant.subject == capability.iri
                && grant.grant == "GRANTED"
                && !grant.broker.trim().is_empty()
                && grant.report_digest == report_digest => {}
        _ => violations.push("actuating capability lacks a valid broker-scoped grant".to_string()),
    }
}

fn evaluate_capability(
    manifest: &Path, program: &Program, capability: &Capability, known_ids: &BTreeSet<String>,
) -> Candidate {
    let mut violations = Vec::new();
    if capability.id.trim().is_empty()
        || !capability.iri.contains(':')
        || capability.surface.trim().is_empty()
        || capability.summary.trim().is_empty()
    {
        violations.push("capability identity, IRI, surface, and summary are required".to_string());
    }
    if !REQUIRED_DOMAINS.contains(&capability.domain.as_str()) {
        violations.push("capability domain is outside the maximalist profile".to_string());
    }
    if !HORIZONS.contains(&capability.horizon) {
        violations.push("capability horizon must be between 2026 and 2030".to_string());
    }
    validate_authority(capability, &mut violations);
    if !unique_nonempty(&capability.outcomes)
        || capability
            .outcomes
            .iter()
            .any(|outcome| !OUTCOMES.contains(&outcome.as_str()))
    {
        violations
            .push("outcomes must be unique members of the lawful outcome lattice".to_string());
    }
    if !capability.dependencies.is_empty() && !unique_nonempty(&capability.dependencies) {
        violations.push("dependencies must be unique and non-empty".to_string());
    }
    for dependency in &capability.dependencies {
        if dependency == &capability.id {
            violations.push("capability cannot depend on itself".to_string());
        } else if !known_ids.contains(dependency) {
            violations.push(format!("unknown dependency {dependency}"));
        }
    }
    if !exact_evidence_keys(&capability.evidence) {
        violations.push(
            "evidence must contain every required role plus optional execution_grant".to_string(),
        );
    }

    let observed_any = capability
        .evidence
        .values()
        .any(|binding| evidence_path(manifest, binding).is_some_and(|path| path.is_file()));
    for role in REQUIRED_EVIDENCE {
        match capability.evidence.get(role) {
            Some(binding) if evidence_bytes(manifest, binding).is_some() => {}
            _ => violations.push(format!(
                "{role} evidence is absent, unsafe, or digest-divergent"
            )),
        }
    }

    let mut canonical_units = 0;
    let mut delivered_instances = 0;
    let mut ontology_modules = 0;
    let mut textual_forms = 0;
    let mut semantic_cells = 0;
    let mut report_digest_value = String::new();
    if let Some(binding) = capability.evidence.get("sbb_report") {
        match parse_evidence::<SbbReport>(manifest, binding) {
            Some(report) => {
                report_digest_value = report.report_digest.clone();
                if let Some((units, delivered, ontologies, forms, cells)) =
                    validate_sbb(&report, &mut violations)
                {
                    canonical_units = units;
                    delivered_instances = delivered;
                    ontology_modules = ontologies;
                    textual_forms = forms;
                    semantic_cells = cells;
                }
            }
            None => violations.push("sbb_report is not valid JSON evidence".to_string()),
        }
    }

    validate_proof(
        manifest,
        capability,
        "positive",
        "positive",
        &report_digest_value,
        &mut violations,
    );
    validate_proof(
        manifest,
        capability,
        "negative",
        "negative",
        &report_digest_value,
        &mut violations,
    );
    validate_proof(
        manifest,
        capability,
        "adversarial",
        "adversarial",
        &report_digest_value,
        &mut violations,
    );
    validate_verifier(manifest, capability, &report_digest_value, &mut violations);
    validate_passport(manifest, capability, &report_digest_value, &mut violations);

    if let Some(binding) = capability.evidence.get("receipt") {
        match parse_evidence::<SbbReceipt>(manifest, binding) {
            Some(receipt) if validate_sbb_receipt(&receipt, &report_digest_value) => {}
            _ => violations
                .push("receipt does not cryptographically bind the SBB report".to_string()),
        }
    }

    if let Some(binding) = capability.evidence.get("replay") {
        match parse_evidence::<ReplayWitness>(manifest, binding) {
            Some(replay)
                if replay.schema == SBB_REPLAY_SCHEMA
                    && replay.status == "REPLAY_MATCH"
                    && replay.matches
                    && replay.report_digest == report_digest_value => {}
            _ => violations.push("replay does not prove REPLAY_MATCH for the report".to_string()),
        }
    }

    validate_external_acceptance(
        manifest,
        program,
        capability,
        &report_digest_value,
        &mut violations,
    );
    validate_execution_grant(manifest, capability, &report_digest_value, &mut violations);

    let intrinsic_alive = violations.is_empty();
    let standing = if intrinsic_alive {
        "PARTIAL_ALIVE"
    } else if observed_any {
        "REFUSED"
    } else {
        "DESIGNED"
    };
    Candidate {
        report: CapabilityReport {
            id: capability.id.clone(),
            iri: capability.iri.clone(),
            domain: capability.domain.clone(),
            horizon: capability.horizon,
            authority: capability.authority.clone(),
            surface: capability.surface.clone(),
            outcomes: capability.outcomes.clone(),
            standing: standing.to_string(),
            canonical_units,
            delivered_instances,
            ontology_modules,
            textual_forms,
            semantic_cells,
            multiplier: multiplier(delivered_instances, canonical_units),
            dependencies_satisfied: capability.dependencies.is_empty(),
            violations,
        },
        intrinsic_alive,
        observed_any,
        dependencies: capability.dependencies.clone(),
    }
}

fn cycle_nodes(graph: &BTreeMap<String, Vec<String>>) -> BTreeSet<String> {
    fn visit(
        node: &str, graph: &BTreeMap<String, Vec<String>>, state: &mut BTreeMap<String, u8>,
        stack: &mut Vec<String>, cycles: &mut BTreeSet<String>,
    ) {
        match state.get(node).copied().unwrap_or_default() {
            1 => {
                if let Some(index) = stack.iter().position(|item| item == node) {
                    cycles.extend(stack[index..].iter().cloned());
                }
                return;
            }
            2 => return,
            _ => {}
        }
        state.insert(node.to_string(), 1);
        stack.push(node.to_string());
        if let Some(dependencies) = graph.get(node) {
            for dependency in dependencies {
                if graph.contains_key(dependency) {
                    visit(dependency, graph, state, stack, cycles);
                }
            }
        }
        stack.pop();
        state.insert(node.to_string(), 2);
    }

    let mut state = BTreeMap::new();
    let mut stack = Vec::new();
    let mut cycles = BTreeSet::new();
    for node in graph.keys() {
        visit(node, graph, &mut state, &mut stack, &mut cycles);
    }
    cycles
}

fn domain_space(alive_domains: usize, violations: &mut Vec<String>) -> u128 {
    let Some(power) = 1_u128.checked_shl(alive_domains as u32) else {
        violations.push("domain combination space overflowed u128".to_string());
        return 0;
    };
    power.saturating_sub(1)
}

pub(super) fn evaluate(path: &Path) -> Result<Report> {
    let (manifest, bytes) = load(path)?;
    let mut violations = Vec::new();
    if manifest.schema != MANIFEST_SCHEMA {
        violations.push(format!(
            "unsupported schema {}; expected {MANIFEST_SCHEMA}",
            manifest.schema
        ));
    }
    if manifest.program.id.trim().is_empty()
        || manifest.program.version.trim().is_empty()
        || manifest.program.target_year != 2030
        || manifest.program.minimum_multiplier < 1000
    {
        violations.push("program identity, target year, or multiplier is invalid".to_string());
    }
    if !unique_nonempty(&manifest.required_domains) {
        violations.push("required_domains must be unique and non-empty".to_string());
    }
    for domain in REQUIRED_DOMAINS {
        if !manifest
            .required_domains
            .iter()
            .any(|candidate| candidate == domain)
        {
            violations.push(format!("required domain {domain} is missing"));
        }
    }
    if !unique_nonempty(&manifest.required_outcomes) {
        violations.push("required_outcomes must be unique and non-empty".to_string());
    }
    for outcome in OUTCOMES {
        if !manifest
            .required_outcomes
            .iter()
            .any(|candidate| candidate == outcome)
        {
            violations.push(format!("lawful outcome {outcome} is missing"));
        }
    }

    let mut horizon_targets = BTreeMap::new();
    for horizon in &manifest.horizons {
        if !HORIZONS.contains(&horizon.year) || horizon.minimum_alive_capabilities == 0 {
            violations.push(format!("invalid horizon {}", horizon.year));
        }
        if horizon_targets
            .insert(horizon.year, horizon.minimum_alive_capabilities)
            .is_some()
        {
            violations.push(format!("duplicate horizon {}", horizon.year));
        }
    }
    for year in HORIZONS {
        if !horizon_targets.contains_key(&year) {
            violations.push(format!("horizon {year} is missing"));
        }
    }
    if manifest.capabilities.is_empty() {
        violations.push("at least one capability realization is required".to_string());
    }

    let mut ids = BTreeSet::new();
    let mut iris = BTreeSet::new();
    for capability in &manifest.capabilities {
        if !ids.insert(capability.id.clone()) {
            violations.push(format!("duplicate capability id {}", capability.id));
        }
        if !iris.insert(capability.iri.clone()) {
            violations.push(format!("duplicate capability IRI {}", capability.iri));
        }
    }
    let graph = manifest
        .capabilities
        .iter()
        .map(|capability| (capability.id.clone(), capability.dependencies.clone()))
        .collect::<BTreeMap<_, _>>();
    let cycles = cycle_nodes(&graph);
    for node in &cycles {
        violations.push(format!("dependency cycle contains {node}"));
    }

    let mut candidates = manifest
        .capabilities
        .iter()
        .map(|capability| evaluate_capability(path, &manifest.program, capability, &ids))
        .collect::<Vec<_>>();
    for candidate in &mut candidates {
        if cycles.contains(&candidate.report.id) {
            candidate.intrinsic_alive = false;
            candidate
                .report
                .violations
                .push("dependency cycle".to_string());
            candidate.report.standing = if candidate.observed_any {
                "REFUSED".to_string()
            } else {
                "DESIGNED".to_string()
            };
        }
    }

    let mut alive = BTreeSet::new();
    for _ in 0..=candidates.len() {
        let before = alive.len();
        for candidate in &candidates {
            if candidate.intrinsic_alive
                && candidate
                    .dependencies
                    .iter()
                    .all(|dependency| alive.contains(dependency))
            {
                alive.insert(candidate.report.id.clone());
            }
        }
        if alive.len() == before {
            break;
        }
    }

    for candidate in &mut candidates {
        if alive.contains(&candidate.report.id) {
            candidate.report.standing = "ALIVE".to_string();
            candidate.report.dependencies_satisfied = true;
        } else if candidate.intrinsic_alive {
            candidate.report.standing = "PARTIAL_ALIVE".to_string();
            candidate.report.dependencies_satisfied = false;
            for dependency in &candidate.dependencies {
                if !alive.contains(dependency) {
                    candidate
                        .report
                        .violations
                        .push(format!("dependency {dependency} is not ALIVE"));
                }
            }
        }
    }

    let capability_reports = candidates
        .into_iter()
        .map(|candidate| candidate.report)
        .collect::<Vec<_>>();
    violations.extend(capability_reports.iter().flat_map(|capability| {
        capability
            .violations
            .iter()
            .map(move |violation| format!("{}: {violation}", capability.id))
    }));

    let mut domains = BTreeMap::new();
    for domain in &manifest.required_domains {
        let declared = capability_reports
            .iter()
            .filter(|capability| &capability.domain == domain)
            .count();
        let alive_count = capability_reports
            .iter()
            .filter(|capability| &capability.domain == domain && capability.standing == "ALIVE")
            .count();
        domains.insert(
            domain.clone(),
            Coverage {
                declared,
                alive: alive_count,
                closed: alive_count > 0,
            },
        );
    }

    let mut outcomes = BTreeMap::new();
    for outcome in &manifest.required_outcomes {
        let declared = capability_reports
            .iter()
            .filter(|capability| capability.outcomes.contains(outcome))
            .count();
        let alive_count = capability_reports
            .iter()
            .filter(|capability| {
                capability.standing == "ALIVE" && capability.outcomes.contains(outcome)
            })
            .count();
        outcomes.insert(
            outcome.clone(),
            Coverage {
                declared,
                alive: alive_count,
                closed: alive_count > 0,
            },
        );
    }

    let mut horizons = BTreeMap::new();
    for year in HORIZONS {
        let minimum = horizon_targets.get(&year).copied().unwrap_or_default();
        let alive_count = capability_reports
            .iter()
            .filter(|capability| capability.horizon == year && capability.standing == "ALIVE")
            .count();
        horizons.insert(
            year.to_string(),
            HorizonReport {
                minimum_alive_capabilities: minimum,
                alive: alive_count,
                closed: alive_count >= minimum,
            },
        );
    }

    let alive_reports = capability_reports
        .iter()
        .filter(|capability| capability.standing == "ALIVE")
        .collect::<Vec<_>>();
    let canonical_units = alive_reports
        .iter()
        .map(|capability| capability.canonical_units)
        .sum::<usize>();
    let delivered_instances = alive_reports.iter().try_fold(0_u128, |sum, capability| {
        sum.checked_add(capability.delivered_instances)
    });
    if delivered_instances.is_none() {
        violations.push("delivered instance sum overflowed u128".to_string());
    }
    let delivered_instances = delivered_instances.unwrap_or_default();
    let semantic_cells = alive_reports.iter().try_fold(0_u128, |sum, capability| {
        sum.checked_add(capability.semantic_cells)
    });
    if semantic_cells.is_none() {
        violations.push("semantic-cell sum overflowed u128".to_string());
    }
    let semantic_cells = semantic_cells.unwrap_or_default();
    let alive_domain_count = domains.values().filter(|coverage| coverage.closed).count();
    let domain_combination_space = domain_space(alive_domain_count, &mut violations);
    let target_product =
        (canonical_units as u128).checked_mul(manifest.program.minimum_multiplier as u128);
    if target_product.is_none() {
        violations.push("multiplier target product overflowed u128".to_string());
    }
    let multiplier_closed =
        canonical_units > 0 && target_product.is_some_and(|target| delivered_instances >= target);
    let all_capabilities_alive = !capability_reports.is_empty()
        && capability_reports
            .iter()
            .all(|capability| capability.standing == "ALIVE");
    let domain_closed = domains.values().all(|coverage| coverage.closed);
    let outcome_closed = outcomes.values().all(|coverage| coverage.closed);
    let horizon_closed = horizons.values().all(|horizon| horizon.closed);
    let achieved = all_capabilities_alive
        && domain_closed
        && outcome_closed
        && horizon_closed
        && multiplier_closed
        && semantic_cells > 0
        && violations.is_empty();
    let standing = if achieved {
        "ALIVE"
    } else if !alive.is_empty() {
        "PARTIAL_ALIVE"
    } else {
        "DESIGNED"
    };

    let mut report = Report {
        schema: REPORT_SCHEMA.to_string(),
        manifest_digest: digest_bytes(&bytes),
        program: manifest.program,
        standing: standing.to_string(),
        achieved,
        measured_multiplier: multiplier(delivered_instances, canonical_units),
        canonical_units,
        delivered_instances: delivered_instances.to_string(),
        semantic_cells: semantic_cells.to_string(),
        alive_domain_count,
        domain_combination_space: domain_combination_space.to_string(),
        all_capabilities_alive,
        domains,
        outcomes,
        horizons,
        capabilities: capability_reports,
        violations,
        report_digest: String::new(),
    };
    report.report_digest = report_digest(&report)?;
    Ok(report)
}

pub(super) fn as_value(path: &Path) -> Result<Value> {
    serde_json::to_value(evaluate(path)?).map_err(|error| {
        NounVerbError::execution_error(format!("cannot encode maximalism report: {error}"))
    })
}

pub(super) fn validation(path: &Path) -> Result<Value> {
    let report = evaluate(path)?;
    Ok(json!({
        "standing": report.standing,
        "achieved": report.achieved,
        "minimum_multiplier": report.program.minimum_multiplier,
        "measured_multiplier": report.measured_multiplier,
        "canonical_units": report.canonical_units,
        "delivered_instances": report.delivered_instances,
        "semantic_cells": report.semantic_cells,
        "alive_domain_count": report.alive_domain_count,
        "domain_combination_space": report.domain_combination_space,
        "domains_closed": report.domains.values().all(|coverage| coverage.closed),
        "outcomes_closed": report.outcomes.values().all(|coverage| coverage.closed),
        "horizons_closed": report.horizons.values().all(|horizon| horizon.closed),
        "all_capabilities_alive": report.all_capabilities_alive,
        "violations": report.violations,
        "manifest_digest": report.manifest_digest,
        "report_digest": report.report_digest
    }))
}

pub(super) fn combinations(path: &Path) -> Result<Value> {
    let report = evaluate(path)?;
    Ok(json!({
        "standing": report.standing,
        "canonical_units": report.canonical_units,
        "delivered_instances": report.delivered_instances,
        "measured_multiplier": report.measured_multiplier,
        "semantic_cells": report.semantic_cells,
        "alive_domain_count": report.alive_domain_count,
        "domain_combination_space": report.domain_combination_space,
        "capabilities": report.capabilities.iter().map(|capability| json!({
            "id": capability.id,
            "standing": capability.standing,
            "canonical_units": capability.canonical_units,
            "ontology_modules": capability.ontology_modules,
            "textual_forms": capability.textual_forms,
            "semantic_cells": capability.semantic_cells,
            "delivered_instances": capability.delivered_instances,
            "multiplier": capability.multiplier
        })).collect::<Vec<_>>(),
        "report_digest": report.report_digest
    }))
}

pub(super) fn outcome_report(path: &Path) -> Result<Value> {
    let report = evaluate(path)?;
    let missing = report
        .outcomes
        .iter()
        .filter_map(|(outcome, coverage)| (!coverage.closed).then_some(outcome.clone()))
        .collect::<Vec<_>>();
    Ok(json!({
        "standing": report.standing,
        "lawful_outcomes": report.outcomes,
        "missing": missing,
        "closed": missing.is_empty(),
        "report_digest": report.report_digest
    }))
}

pub(super) fn domain_lens(path: &Path, domain: &str) -> Result<Value> {
    let report = evaluate(path)?;
    let capabilities = report
        .capabilities
        .iter()
        .filter(|capability| capability.domain == domain)
        .map(|capability| {
            json!({
                "id": capability.id,
                "surface": capability.surface,
                "standing": capability.standing,
                "authority": capability.authority,
                "outcomes": capability.outcomes,
                "violations": capability.violations
            })
        })
        .collect::<Vec<_>>();
    let closed = !capabilities.is_empty()
        && report
            .domains
            .get(domain)
            .is_some_and(|coverage| coverage.closed);
    Ok(json!({
        "domain": domain,
        "standing": report.standing,
        "closed": closed,
        "capabilities": capabilities,
        "report_digest": report.report_digest
    }))
}

fn remediation(violation: &str) -> &'static str {
    if violation.contains("digest") || violation.contains("evidence") {
        "Bind the exact evidence bytes through a safe relative locator and matching BLAKE3 digest."
    } else if violation.contains("passport") {
        "Complete the passport with contracts, routes, interfaces, fixtures, adversary results, provenance, runtime and telemetry verdicts, hashes, signature, and bundle digest."
    } else if violation.contains("dependency") {
        "Admit the dependency first or remove the dependency cycle, then replay the dependent capability."
    } else if violation.contains("acceptance") {
        "Obtain independent ACCEPTED evidence bound to the capability IRI and exact SBB report digest."
    } else if violation.contains("authority") || violation.contains("actuat") {
        "Reduce authority to the permitted boundary or provide a broker-scoped execution grant."
    } else if violation.contains("outcome") {
        "Add an accepted capability that implements the missing lawful outcome without collapsing it into success or failure."
    } else if violation.contains("domain") || violation.contains("horizon") {
        "Add externally accepted capability realizations until the required coverage gate closes."
    } else {
        "Correct the admitted input, rerun the executable verifier, and replay the aggregate receipt."
    }
}

pub(super) fn doctor(path: &Path) -> Result<Value> {
    let report = evaluate(path)?;
    let findings = report
        .violations
        .iter()
        .map(|violation| {
            json!({
                "severity": "blocking",
                "violation": violation,
                "remediation": remediation(violation)
            })
        })
        .collect::<Vec<_>>();
    Ok(json!({
        "schema": "ggen.maximalism.doctor.v1",
        "standing": report.standing,
        "healthy": findings.is_empty(),
        "findings": findings,
        "actuated": false,
        "report_digest": report.report_digest
    }))
}

fn dependency_order(manifest: &Manifest, target: &str) -> Vec<String> {
    fn visit(
        id: &str, graph: &BTreeMap<String, Vec<String>>, seen: &mut BTreeSet<String>,
        order: &mut Vec<String>,
    ) {
        if !seen.insert(id.to_string()) {
            return;
        }
        if let Some(dependencies) = graph.get(id) {
            for dependency in dependencies {
                visit(dependency, graph, seen, order);
            }
        }
        order.push(id.to_string());
    }
    let graph = manifest
        .capabilities
        .iter()
        .map(|capability| (capability.id.clone(), capability.dependencies.clone()))
        .collect::<BTreeMap<_, _>>();
    let mut seen = BTreeSet::new();
    let mut order = Vec::new();
    visit(target, &graph, &mut seen, &mut order);
    order
}

pub(super) fn wizard(path: &Path, capability_id: &str) -> Result<Value> {
    let (manifest, _) = load(path)?;
    let report = evaluate(path)?;
    let capability = manifest
        .capabilities
        .iter()
        .find(|capability| capability.id == capability_id)
        .ok_or_else(|| {
            NounVerbError::argument_error(format!("unknown capability {capability_id}"))
        })?;
    let current = report
        .capabilities
        .iter()
        .find(|candidate| candidate.id == capability_id)
        .ok_or_else(|| NounVerbError::execution_error("capability report missing"))?;
    Ok(json!({
        "schema": "ggen.maximalism.wizard.v1",
        "capability": capability.id,
        "iri": capability.iri,
        "standing": current.standing,
        "dependency_order": dependency_order(&manifest, capability_id),
        "bounded_questions": [
            "Which admitted observation changes this architecture contract?",
            "Which authority class is required and what must remain prohibited?",
            "Which ontology modules and textual forms are authoritative projections?",
            "Which positive, negative, and adversarial witnesses prove the exact claim?",
            "Which independent authority may accept or release this cell?"
        ],
        "production_cell": [
            "observation_contract",
            "architecture_contract",
            "ontology",
            "shacl",
            "sparql",
            "typestate",
            "templates",
            "runtime_surface",
            "positive_witness",
            "negative_fixture",
            "adversarial_falsifier",
            "executable_verifier",
            "passport",
            "receipt",
            "replay",
            "independent_acceptance_packet"
        ],
        "allowed_outcomes": capability.outcomes,
        "never_generate_hot_law": true,
        "actuated": false,
        "violations": current.violations,
        "report_digest": report.report_digest
    }))
}

fn telco_id(surface: &str) -> Option<&'static str> {
    match surface {
        "office" => Some("telco-office"),
        "register" => Some("telco-register"),
        "line" => Some("telco-line"),
        "bridge" => Some("telco-bridge"),
        "record" => Some("telco-record"),
        _ => None,
    }
}

pub(super) fn telco(path: &Path, surface: Option<&str>) -> Result<Value> {
    let report = evaluate(path)?;
    let selected = match surface {
        Some(surface) => {
            let id = telco_id(surface).ok_or_else(|| {
                NounVerbError::argument_error(format!("unknown Telco surface {surface}"))
            })?;
            report
                .capabilities
                .iter()
                .filter(|capability| capability.id == id)
                .collect::<Vec<_>>()
        }
        None => report
            .capabilities
            .iter()
            .filter(|capability| capability.domain == "telco")
            .collect::<Vec<_>>(),
    };
    let surfaces = selected
        .iter()
        .map(|capability| {
            json!({
                "id": capability.id,
                "surface": capability.surface,
                "standing": capability.standing,
                "authority": capability.authority,
                "outcomes": capability.outcomes,
                "violations": capability.violations
            })
        })
        .collect::<Vec<_>>();
    let closed = !surfaces.is_empty()
        && selected
            .iter()
            .all(|capability| capability.standing == "ALIVE");
    Ok(json!({
        "schema": "ggen.maximalism.telco.v1",
        "standing": report.standing,
        "surface_filter": surface,
        "surfaces": surfaces,
        "closed": closed,
        "mcp_and_a2a_are_telco_surfaces": true,
        "powl_coordinates_work": true,
        "actuated": false,
        "report_digest": report.report_digest
    }))
}
