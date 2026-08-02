use super::*;

#[derive(Debug)]
struct Candidate {
    report: CapabilityReport,
    intrinsically_alive: bool,
    observed_any: bool,
    dependencies: Vec<String>,
}

fn expected_digest(raw: &str) -> Option<&str> {
    raw.strip_prefix("blake3:")
        .or_else(|| raw.strip_prefix("blake3-"))
        .filter(|value| value.len() == 64 && value.bytes().all(|byte| byte.is_ascii_hexdigit()))
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
    if !safe_locator(&evidence.locator) {
        return None;
    }
    Some(
        manifest
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join(&evidence.locator),
    )
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

fn validate_sbb(report: &SbbReport, violations: &mut Vec<String>) -> Option<(usize, u128)> {
    if report.schema != SBB_REPORT_SCHEMA {
        violations.push("sbb_report has unsupported schema".to_string());
    }
    if report.claim_ceiling != "PARTIAL_ALIVE" {
        violations.push("sbb_report claim ceiling must be PARTIAL_ALIVE".to_string());
    }
    if !report.eligible_for_external_admission {
        violations.push("sbb_report is not eligible for external admission".to_string());
    }
    let contexts = report.distribution_contexts.parse::<u128>().ok();
    let delivered = report.delivered_capability_instances.parse::<u128>().ok();
    if report.commit_equivalent_units == 0 || contexts == Some(0) || delivered == Some(0) {
        violations.push("sbb_report has zero canonical units or distribution".to_string());
    }
    let expected = contexts.and_then(|contexts| {
        contexts.checked_mul(report.commit_equivalent_units as u128)
    });
    if expected != delivered {
        violations.push("sbb_report delivered instances are inconsistent".to_string());
    }
    if violations.is_empty() {
        Some((report.commit_equivalent_units, delivered?))
    } else {
        None
    }
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
}

fn evaluate_capability(
    manifest_path: &Path,
    program: &Program,
    capability: &Capability,
    known_ids: &BTreeSet<String>,
) -> Candidate {
    let mut violations = Vec::new();
    if capability.id.trim().is_empty()
        || capability.summary.trim().is_empty()
        || !capability.iri.contains(':')
    {
        violations.push("capability identity, IRI, and summary are required".to_string());
    }
    if !REQUIRED_DOMAINS.contains(&capability.domain.as_str()) {
        violations.push("capability domain is outside the required Vision 2030 profile".to_string());
    }
    if !HORIZONS.contains(&capability.horizon) {
        violations.push("capability horizon must be between 2026 and 2030".to_string());
    }
    if !BLUE_OCEAN_MOVES.contains(&capability.blue_ocean_move.as_str()) {
        violations.push("blue_ocean_move is not recognized".to_string());
    }
    validate_authority(capability, &mut violations);
    if !unique_nonempty(&capability.dependencies) && !capability.dependencies.is_empty() {
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
        violations.push("evidence must contain exactly the required roles plus optional execution_grant".to_string());
    }

    let observed_any = capability
        .evidence
        .values()
        .any(|binding| evidence_path(manifest_path, binding).is_some_and(|path| path.is_file()));
    for role in REQUIRED_EVIDENCE {
        match capability.evidence.get(role) {
            Some(binding) if evidence_bytes(manifest_path, binding).is_some() => {}
            _ => violations.push(format!("{role} evidence is absent, unsafe, or digest-divergent")),
        }
    }

    let mut canonical_units = 0;
    let mut delivered_instances = 0;
    let mut report_digest_value = String::new();
    if let Some(binding) = capability.evidence.get("sbb_report") {
        match parse_evidence::<SbbReport>(manifest_path, binding) {
            Some(report) => {
                report_digest_value = report.report_digest.clone();
                if let Some((units, delivered)) = validate_sbb(&report, &mut violations) {
                    canonical_units = units;
                    delivered_instances = delivered;
                }
            }
            None => violations.push("sbb_report is not valid JSON evidence".to_string()),
        }
    }

    if let Some(binding) = capability.evidence.get("receipt") {
        match parse_evidence::<SbbReceipt>(manifest_path, binding) {
            Some(receipt)
                if receipt.schema == SBB_RECEIPT_SCHEMA
                    && receipt.operation == "density-evaluate-result"
                    && receipt.report_digest == report_digest_value
                    && receipt.digest_algorithm == "blake3"
                    && receipt.digest.len() == 64 => {}
            _ => violations.push("receipt does not bind the admitted SBB report".to_string()),
        }
    }

    if let Some(binding) = capability.evidence.get("replay") {
        match parse_evidence::<ReplayWitness>(manifest_path, binding) {
            Some(replay)
                if replay.schema == REPLAY_SCHEMA
                    && replay.status == "REPLAY_MATCH"
                    && replay.matches
                    && replay.report_digest == report_digest_value => {}
            _ => violations.push("replay does not prove REPLAY_MATCH for the SBB report".to_string()),
        }
    }

    if let Some(binding) = capability.evidence.get("external_acceptance") {
        match parse_evidence::<ExternalAcceptance>(manifest_path, binding) {
            Some(acceptance)
                if acceptance.schema == EXTERNAL_ACCEPTANCE_SCHEMA
                    && acceptance.subject == capability.iri
                    && acceptance.decision == "ACCEPTED"
                    && !acceptance.issuer.trim().is_empty()
                    && acceptance.issuer != program.id
                    && acceptance.report_digest == report_digest_value => {}
            _ => violations.push("external acceptance is absent, self-issued, or divergent".to_string()),
        }
    }

    if capability.authority == "actuate" {
        match capability
            .evidence
            .get("execution_grant")
            .and_then(|binding| parse_evidence::<ExecutionGrant>(manifest_path, binding))
        {
            Some(grant)
                if grant.schema == EXECUTION_GRANT_SCHEMA
                    && grant.subject == capability.iri
                    && grant.grant == "GRANTED"
                    && !grant.broker.trim().is_empty()
                    && grant.report_digest == report_digest_value => {}
            _ => violations.push("actuating capability lacks a valid execution grant".to_string()),
        }
    }

    let intrinsically_alive = violations.is_empty();
    let standing = if intrinsically_alive {
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
            blue_ocean_move: capability.blue_ocean_move.clone(),
            authority: capability.authority.clone(),
            standing: standing.to_string(),
            canonical_units,
            delivered_instances,
            multiplier: multiplier(delivered_instances, canonical_units),
            dependencies_satisfied: capability.dependencies.is_empty(),
            violations,
        },
        intrinsically_alive,
        observed_any,
        dependencies: capability.dependencies.clone(),
    }
}

fn cycle_nodes(graph: &BTreeMap<String, Vec<String>>) -> BTreeSet<String> {
    fn visit(
        node: &str,
        graph: &BTreeMap<String, Vec<String>>,
        state: &mut BTreeMap<String, u8>,
        stack: &mut Vec<String>,
        cycles: &mut BTreeSet<String>,
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

pub(super) fn evaluate(path: &Path) -> Result<Report> {
    let (manifest, bytes) = load(path)?;
    let mut violations = Vec::new();
    if manifest.schema != MANIFEST_SCHEMA {
        violations.push(format!("unsupported schema {}; expected {MANIFEST_SCHEMA}", manifest.schema));
    }
    if manifest.program.id.trim().is_empty()
        || manifest.program.version.trim().is_empty()
        || manifest.program.target_year != 2030
        || manifest.program.phase_change_target < 1000
    {
        violations.push("program identity, target year, or phase target is invalid".to_string());
    }
    if !unique_nonempty(&manifest.required_domains) {
        violations.push("required_domains must be unique and non-empty".to_string());
    }
    for domain in REQUIRED_DOMAINS {
        if !manifest.required_domains.iter().any(|candidate| candidate == domain) {
            violations.push(format!("required domain {domain} is missing"));
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
            candidate.intrinsically_alive = false;
            candidate.report.violations.push("dependency cycle".to_string());
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
            if candidate.intrinsically_alive
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
        } else if candidate.intrinsically_alive {
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
            DomainReport {
                declared,
                alive: alive_count,
                covered: alive_count > 0,
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
                met: alive_count >= minimum,
            },
        );
    }

    let mut blue_ocean = BLUE_OCEAN_MOVES
        .iter()
        .map(|movement| ((*movement).to_string(), 0_usize))
        .collect::<BTreeMap<_, _>>();
    for capability in &capability_reports {
        if capability.standing == "ALIVE" {
            *blue_ocean
                .entry(capability.blue_ocean_move.clone())
                .or_default() += 1;
        }
    }

    let alive_reports = capability_reports
        .iter()
        .filter(|capability| capability.standing == "ALIVE")
        .collect::<Vec<_>>();
    let canonical_units = alive_reports
        .iter()
        .map(|capability| capability.canonical_units)
        .sum::<usize>();
    let delivered_instances = alive_reports
        .iter()
        .map(|capability| capability.delivered_instances)
        .try_fold(0_u128, u128::checked_add);
    if delivered_instances.is_none() {
        violations.push("delivered instance sum overflowed u128".to_string());
    }
    let delivered_instances = delivered_instances.unwrap_or_default();
    let all_capabilities_alive = !capability_reports.is_empty()
        && capability_reports
            .iter()
            .all(|capability| capability.standing == "ALIVE");
    let domain_closure = domains.values().all(|domain| domain.covered);
    let horizon_closure = horizons.values().all(|horizon| horizon.met);
    let blue_ocean_closure = blue_ocean.values().all(|count| *count > 0);
    let target_product = (canonical_units as u128)
        .checked_mul(manifest.program.phase_change_target as u128);
    if target_product.is_none() {
        violations.push("phase-change target product overflowed u128".to_string());
    }
    let target_met = canonical_units > 0
        && target_product.is_some_and(|target| delivered_instances >= target);
    let achieved = all_capabilities_alive
        && domain_closure
        && horizon_closure
        && blue_ocean_closure
        && target_met
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
        program: manifest.program.clone(),
        standing: standing.to_string(),
        achieved,
        phase_change_target: manifest.program.phase_change_target,
        phase_change_multiplier: multiplier(delivered_instances, canonical_units),
        canonical_units,
        delivered_instances: delivered_instances.to_string(),
        all_capabilities_alive,
        domains,
        horizons,
        blue_ocean,
        capabilities: capability_reports,
        violations,
        report_digest: String::new(),
    };
    report.report_digest = report_digest(&report)?;
    Ok(report)
}

pub(super) fn as_value(path: &Path) -> Result<Value> {
    serde_json::to_value(evaluate(path)?).map_err(|error| {
        NounVerbError::execution_error(format!("cannot encode Vision 2030 report: {error}"))
    })
}

pub(super) fn validation(path: &Path) -> Result<Value> {
    let report = evaluate(path)?;
    Ok(json!({
        "standing": report.standing,
        "achieved": report.achieved,
        "phase_change_target": report.phase_change_target,
        "phase_change_multiplier": report.phase_change_multiplier,
        "canonical_units": report.canonical_units,
        "delivered_instances": report.delivered_instances,
        "all_capabilities_alive": report.all_capabilities_alive,
        "domains_closed": report.domains.values().all(|domain| domain.covered),
        "horizons_closed": report.horizons.values().all(|horizon| horizon.met),
        "blue_ocean_closed": report.blue_ocean.values().all(|count| *count > 0),
        "violations": report.violations,
        "manifest_digest": report.manifest_digest,
        "report_digest": report.report_digest
    }))
}

pub(super) fn roadmap(path: &Path) -> Result<Value> {
    let report = evaluate(path)?;
    let missing = report
        .horizons
        .iter()
        .map(|(year, horizon)| {
            (
                year.clone(),
                horizon
                    .minimum_alive_capabilities
                    .saturating_sub(horizon.alive),
            )
        })
        .collect::<BTreeMap<_, _>>();
    Ok(json!({
        "standing": report.standing,
        "target_year": report.program.target_year,
        "horizons": report.horizons,
        "missing_alive_capabilities": missing,
        "achieved": report.achieved,
        "report_digest": report.report_digest
    }))
}

pub(super) fn blue_ocean(path: &Path) -> Result<Value> {
    let report = evaluate(path)?;
    let uncovered = report
        .blue_ocean
        .iter()
        .filter_map(|(movement, count)| (*count == 0).then_some(movement.clone()))
        .collect::<Vec<_>>();
    Ok(json!({
        "standing": report.standing,
        "accepted_capabilities_by_move": report.blue_ocean,
        "uncovered_moves": uncovered,
        "closed": uncovered.is_empty(),
        "report_digest": report.report_digest
    }))
}

fn lens_requirements(domain: &str) -> &'static [&'static str] {
    match domain {
        "dx" => &[
            "dx-local-first-control-plane",
            "dx-explainable-refusal",
            "dx-one-command-projection",
            "dx-environment-passport",
        ],
        "qol" => &[
            "qol-zero-setup-workspace",
            "qol-reversible-operations",
            "qol-accessible-control-surface",
            "qol-offline-degraded-mode",
        ],
        _ => &[],
    }
}

pub(super) fn lens(path: &Path, domain: &str) -> Result<Value> {
    let report = evaluate(path)?;
    let status = lens_requirements(domain)
        .iter()
        .map(|required| {
            let standing = report
                .capabilities
                .iter()
                .find(|capability| capability.id == *required)
                .map(|capability| capability.standing.as_str())
                .unwrap_or("MISSING");
            ((*required).to_string(), standing.to_string())
        })
        .collect::<BTreeMap<_, _>>();
    let closed = !status.is_empty() && status.values().all(|standing| standing == "ALIVE");
    Ok(json!({
        "lens": domain,
        "standing": report.standing,
        "requirements": status,
        "closed": closed,
        "non_actuating": true,
        "report_digest": report.report_digest
    }))
}

fn remediation(violation: &str) -> &'static str {
    if violation.contains("digest") || violation.contains("evidence") {
        "Recompute the BLAKE3 digest from the exact evidence bytes and bind a safe relative locator."
    } else if violation.contains("dependency") {
        "Admit the named dependency first, remove the cycle, then replay the dependent capability."
    } else if violation.contains("external acceptance") {
        "Obtain an independent ACCEPTED receipt bound to the capability IRI and exact SBB report digest."
    } else if violation.contains("execution grant") {
        "Provide a broker-scoped execution grant or reduce the capability authority below actuate."
    } else if violation.contains("healthcare") || violation.contains("Doctor") {
        "Reduce authority to the permitted diagnostic or recommendation boundary."
    } else if violation.contains("horizon") {
        "Add externally accepted capability realizations until the horizon minimum is met."
    } else {
        "Inspect the referenced capability evidence, correct the admitted input, and rerun validation."
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
        "schema": "ggen.vision2030.doctor.v1",
        "standing": report.standing,
        "healthy": findings.is_empty(),
        "findings": findings,
        "actuated": false,
        "report_digest": report.report_digest
    }))
}
