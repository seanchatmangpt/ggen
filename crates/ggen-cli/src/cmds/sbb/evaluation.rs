use super::*;

fn expected_digest(raw: &str) -> Option<&str> {
    raw.strip_prefix("blake3:")
        .or_else(|| raw.strip_prefix("blake3-"))
        .filter(|value| value.len() == 64 && value.bytes().all(|byte| byte.is_ascii_hexdigit()))
}

fn repository_root(manifest: &Path, raw: &str) -> PathBuf {
    let root = Path::new(raw);
    if root.is_absolute() {
        root.to_path_buf()
    } else {
        manifest
            .parent()
            .unwrap_or_else(|| Path::new("."))
            .join(root)
    }
}

fn git(repository: &Path, args: &[String]) -> Option<Vec<u8>> {
    let output = Command::new("git")
        .arg("-C")
        .arg(repository)
        .args(args)
        .output()
        .ok()?;
    output.status.success().then_some(output.stdout)
}

fn repository_observed(repository: &Path) -> bool {
    git(
        repository,
        &["rev-parse".into(), "--is-inside-work-tree".into()],
    )
    .is_some_and(|bytes| bytes.starts_with(b"true"))
}

fn resolve_commit(repository: &Path, commit: &str) -> Option<String> {
    if !(7..=64).contains(&commit.len()) || !commit.bytes().all(|byte| byte.is_ascii_hexdigit()) {
        return None;
    }
    let bytes = git(
        repository,
        &[
            "rev-parse".into(),
            "--verify".into(),
            format!("{commit}^{{commit}}"),
        ],
    )?;
    String::from_utf8(bytes).ok().map(|sha| sha.trim().to_ascii_lowercase())
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

fn evidence_observed(repository: &Path, commit: &str, evidence: &Evidence) -> bool {
    let Some(expected) = expected_digest(&evidence.digest) else {
        return false;
    };
    if !safe_locator(&evidence.locator) {
        return false;
    }
    let Some(bytes) = git(
        repository,
        &["show".into(), format!("{commit}:{}", evidence.locator)],
    ) else {
        return false;
    };
    expected.eq_ignore_ascii_case(&digest_bytes(&bytes))
}

fn unique_nonempty(values: &[String]) -> bool {
    !values.is_empty()
        && values.iter().all(|value| !value.trim().is_empty())
        && values.iter().collect::<BTreeSet<_>>().len() == values.len()
}

fn exact_keys<T>(map: &BTreeMap<String, T>, required: &[&str]) -> bool {
    map.keys().map(String::as_str).collect::<BTreeSet<_>>()
        == required.iter().copied().collect::<BTreeSet<_>>()
}

fn evaluate_delta(repository: &Path, delta: &Delta) -> DeltaReport {
    let mut violations = Vec::new();
    if delta.id.trim().is_empty()
        || delta.family.trim().is_empty()
        || delta.summary.trim().is_empty()
    {
        violations.push("id, family, and summary are required".to_string());
    }
    if !delta.capability_iri.contains(':') {
        violations.push("capability_iri must be absolute".to_string());
    }
    if !unique_nonempty(&delta.ontology_modules) || !unique_nonempty(&delta.textual_forms) {
        violations
            .push("ontology_modules and textual_forms must be non-empty and unique".to_string());
    }
    if !exact_keys(&delta.chain, &CHAIN) {
        violations.push("manufacturing chain must contain exactly the ten required stages".to_string());
    }
    let canonical_commit = resolve_commit(repository, &delta.commit);
    if canonical_commit.is_none() {
        violations.push("commit is not present in the admitted repository".to_string());
    }
    let commit = canonical_commit.unwrap_or_else(|| delta.commit.to_ascii_lowercase());
    let mut evidence: Vec<(&str, &Evidence)> = delta
        .chain
        .iter()
        .map(|(role, binding)| (role.as_str(), binding))
        .collect();
    evidence.extend([
        ("positive_witness", &delta.positive_witness),
        ("negative_fixture", &delta.negative_fixture),
        ("adversarial_falsifier", &delta.adversarial_falsifier),
        ("verifier", &delta.verifier),
    ]);
    for (role, binding) in evidence {
        if !evidence_observed(repository, &commit, binding) {
            violations.push(format!("{role} is absent, unsafe, or digest-divergent"));
        }
    }
    DeltaReport {
        id: delta.id.clone(),
        commit,
        observed: violations.is_empty(),
        violations,
    }
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

pub(super) fn evaluate(path: &Path) -> Result<Report> {
    let (manifest, bytes) = load(path)?;
    let repository = repository_root(path, &manifest.repository.root);
    let mut violations = Vec::new();
    if manifest.schema != MANIFEST_SCHEMA {
        violations.push(format!(
            "unsupported schema {}; expected {MANIFEST_SCHEMA}",
            manifest.schema
        ));
    }
    if manifest.sbb.id.trim().is_empty()
        || manifest.sbb.version.trim().is_empty()
        || !manifest.sbb.architecture_contract.contains(':')
        || manifest.sbb.minimum_commit_equivalent_units == 0
    {
        violations.push("incomplete SBB identity, contract, or density target".to_string());
    }
    if !repository_observed(&repository) {
        violations.push(format!(
            "{} is not an observed Git work tree",
            repository.display()
        ));
    }
    if manifest.deltas.is_empty() {
        violations.push("at least one capability delta is required".to_string());
    }
    if !exact_keys(&manifest.distribution, &AXES) {
        violations.push("distribution must contain exactly the seven required axes".to_string());
    }

    let mut axes = BTreeMap::new();
    for axis in AXES {
        let values = manifest.distribution.get(axis).cloned().unwrap_or_default();
        if !unique_nonempty(&values) {
            violations.push(format!("distribution axis {axis} is empty or duplicated"));
        }
        axes.insert(axis.to_string(), values.iter().collect::<BTreeSet<_>>().len());
    }

    let mut deltas: Vec<_> = manifest
        .deltas
        .iter()
        .map(|delta| evaluate_delta(&repository, delta))
        .collect();
    let mut ids = BTreeSet::new();
    let mut iris = BTreeSet::new();
    let mut commit_counts = BTreeMap::<String, usize>::new();
    for (source, report) in manifest.deltas.iter().zip(&mut deltas) {
        if !ids.insert(source.id.clone()) {
            report.violations.push("duplicate delta id".to_string());
        }
        if !iris.insert(source.capability_iri.clone()) {
            report.violations.push("duplicate capability IRI".to_string());
        }
        *commit_counts.entry(report.commit.clone()).or_default() += 1;
    }
    let duplicate_commit_collisions = commit_counts.values().filter(|count| **count > 1).count();
    for report in &mut deltas {
        if commit_counts.get(&report.commit).copied().unwrap_or_default() > 1 {
            report.violations.push("duplicate commit claim".to_string());
        }
        report.observed = report.violations.is_empty();
    }
    violations.extend(deltas.iter().flat_map(|delta| {
        delta
            .violations
            .iter()
            .map(move |message| format!("{}: {message}", delta.id))
    }));

    let units = deltas.iter().filter(|delta| delta.observed).count();
    let contexts = axes
        .values()
        .try_fold(1_u128, |product, value| product.checked_mul(*value as u128));
    if contexts.is_none() {
        violations.push("distribution context product overflowed u128".to_string());
    }
    let contexts = contexts.unwrap_or_default();
    let delivered = contexts.checked_mul(units as u128);
    if delivered.is_none() {
        violations.push("delivered capability product overflowed u128".to_string());
    }
    let target_met = units as u64 >= manifest.sbb.minimum_commit_equivalent_units;
    let eligible = target_met && violations.is_empty() && units == manifest.deltas.len();
    let standing = if units == 0 { "UNKNOWN" } else { "PARTIAL_ALIVE" };
    let mut report = Report {
        schema: REPORT_SCHEMA.to_string(),
        manifest_digest: digest_bytes(&bytes),
        sbb: manifest.sbb,
        standing: standing.to_string(),
        claim_ceiling: "PARTIAL_ALIVE".to_string(),
        target_met,
        eligible_for_external_admission: eligible,
        declared_deltas: manifest.deltas.len(),
        commit_equivalent_units: units,
        duplicate_commit_collisions,
        axes,
        distribution_contexts: contexts.to_string(),
        delivered_capability_instances: delivered.unwrap_or_default().to_string(),
        deltas,
        violations,
        report_digest: String::new(),
    };
    report.report_digest = report_digest(&report)?;
    Ok(report)
}
