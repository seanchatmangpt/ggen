//! Typed admission of WorkOrders — port of `admit_work_order/1`
//! (semantic_jira.ex lines 100-151) aligned to the Elixir SoT:
//! B3 non-empty `path_scope`, 19-field definition take + defaults,
//! closed standing vocabulary at admit (not UNKNOWN-only), opaque
//! `replay_identity`, and strip-unknowns for Elixir optional extras.

use serde_json::Value;
use std::fmt;

use crate::work_order::{definition_digest, digest_of, WorkOrder};

/// The nine typed dependency classes (`@dependency_types`, line 20).
/// Kept here (not imported from `frontier`) to avoid an admit↔frontier cycle.
const DEPENDENCY_TYPES: [&str; 9] = [
    "requiresSemanticIdentity",
    "requiresProjection",
    "requiresCapability",
    "requiresReceipt",
    "requiresRuntime",
    "requiresVerifier",
    "requiresObservation",
    "requiresPostcondition",
    "requiresPublication",
];

/// The fifteen deterministic projection classes (`@projection_types`,
/// semantic_jira.ex line 24).
pub const PROJECTION_TYPES: [&str; 15] = [
    "jira",
    "wbpr",
    "prd",
    "ard",
    "vision",
    "fond",
    "hddl",
    "sa2a",
    "a2a_agent_card",
    "worker",
    "verification",
    "executive",
    "machine",
    "receipt",
    "replay",
];

/// Independent receipt classes (`@receipt_classes`, line 22).
pub const RECEIPT_CLASSES: [&str; 9] = [
    "manufacture",
    "projection",
    "authority_preparation",
    "actuation",
    "verification",
    "postcondition",
    "replay",
    "publication",
    "deployment",
];

/// Typed refusal vocabulary (`{:error, {:refused_work_order, reason}}`
/// shape, rendered as typed variants with remediation text per house law).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum Refusal {
    #[error(
        "MalformedWorkOrder: {0} — remediation: supply all required fields \
         (identity, title, description, subject, repository, base_sha, standing, \
         evidence_ceiling, promotion_rule, replay_identity, origin_authority, \
         projections, required_courts, required_evidence, acceptance, falsifiers, \
         path_scope) with well-formed values"
    )]
    MalformedWorkOrder(String),

    #[error(
        "InvalidStanding: {value} — remediation: standing must be one of UNKNOWN, \
         PARTIAL_ALIVE, ALIVE, BLOCKED, BUILD_BROKEN, UNSUPPORTED, or REFUSED(<reason>)"
    )]
    InvalidStanding { value: String },

    #[error(
        "InvalidIdentity: {value} — remediation: identity must match \
         ^[A-Z][A-Z0-9-]{{1,63}}$ (SHACL dcterms:identifier / B1)"
    )]
    InvalidIdentity { value: String },

    #[error(
        "InvalidPathScope: {detail} — remediation: path_scope must be a non-empty \
         list of non-empty relative paths (no leading '/'); B3 sj:pathScope minCount 1"
    )]
    InvalidPathScope { detail: String },

    #[error(
        "InvalidOriginAuthority: {value} — remediation: origin_authority must be an \
         absolute IRI (RFC 3986 scheme plus non-empty whitespace-free rest, e.g. \
         https://graph.test/auth/...), never a bare local name"
    )]
    InvalidOriginAuthority { value: String },

    #[error(
        "InvalidOriginObservation: {value} — remediation: origin_observation when \
         present must be an absolute IRI"
    )]
    InvalidOriginObservation { value: String },

    #[error(
        "InvalidTargetPack: {value} — remediation: target_pack when present must match \
         ^[a-z0-9][a-z0-9_-]*$ (FORMAT only; pack existence is sync-time WORLD)"
    )]
    InvalidTargetPack { value: String },

    #[error(
        "DigestMismatch: field {field} carries {found} — remediation: digests must be \
         `sha256:<64 hex>`"
    )]
    DigestMismatch { field: String, found: String },

    #[error(
        "InvalidRepository: {value} — remediation: repository must be owner/repo \
         (alphanumeric, dot, underscore, hyphen on each side)"
    )]
    InvalidRepository { value: String },

    #[error(
        "InvalidSha: field {field} carries {found} — remediation: git SHAs must be \
         exactly 40 lowercase hex characters"
    )]
    InvalidSha { field: String, found: String },

    #[error(
        "InvalidList: field {field} — remediation: list fields must be non-empty \
         arrays of non-empty strings"
    )]
    InvalidList { field: String },

    #[error(
        "UnsupportedProjectionType: {value} — remediation: projections must come from \
         the fifteen-class vocabulary (jira, wbpr, prd, ard, vision, fond, hddl, \
         sa2a, a2a_agent_card, worker, verification, executive, machine, receipt, replay)"
    )]
    UnsupportedProjectionType { value: String },

    #[error(
        "UnsupportedReceiptClass: {value} — remediation: required_receipt_classes must \
         come from the closed receipt-class vocabulary"
    )]
    UnsupportedReceiptClass { value: String },

    #[error(
        "InvalidDependency: {detail} — remediation: each dependency needs upstream + \
         type in the closed dependency vocabulary; optional required_receipt_digest \
         must be sha256:<64 hex>"
    )]
    InvalidDependency { detail: String },

    #[error(
        "Deserialization: {0} — remediation: the payload is not a valid WorkOrder \
         JSON object (unknown keys are stripped; required fields must be present)"
    )]
    Deserialization(String),
}

/// An admitted WorkOrder: the order plus its stamped authority and derived
/// digests. Mirrors the igniter's normalized-map output (lines 126-143).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdmittedWorkOrder {
    pub work_order: WorkOrder,
    /// Always `NONE` — admission never creates authority (line 138).
    pub authority: String,
    /// Snapshot digest: over the full order including standing.
    pub work_order_digest: String,
    /// Definition digest: over the closed 19-field subset, standing excluded.
    pub definition_digest: String,
}

/// Admits and normalizes one WorkOrder-shaped JSON value without creating
/// authority. Port of `admit_work_order/1`.
pub fn admit_work_order(value: &Value) -> Result<AdmittedWorkOrder, Refusal> {
    let mut order: WorkOrder = serde_json::from_value(value.clone())
        .map_err(|e| Refusal::Deserialization(e.to_string()))?;

    // Apply Elixir put_new defaults explicitly (serde defaults already cover
    // most; re-stamp authority_requirement if somehow empty).
    if order.authority_requirement.trim().is_empty() {
        order.authority_requirement = "NONE".to_string();
    }

    validate(&order)?;

    // Snapshot digest over the raw payload (caller key order independent via
    // canonical/); definition digest over the closed 19-field normalized take.
    let work_order_digest = digest_of(value);
    let definition_digest = definition_digest(&order);

    Ok(AdmittedWorkOrder {
        authority: "NONE".to_string(),
        work_order: order,
        work_order_digest,
        definition_digest,
    })
}

/// Field-level validation shared with the digest path.
fn validate(order: &WorkOrder) -> Result<(), Refusal> {
    for (field, v) in [
        ("identity", order.identity.as_str()),
        ("title", order.title.as_str()),
        ("description", order.description.as_str()),
        ("subject", order.subject.as_str()),
        ("evidence_ceiling", order.evidence_ceiling.as_str()),
        ("promotion_rule", order.promotion_rule.as_str()),
        ("replay_identity", order.replay_identity.as_str()),
    ] {
        if v.trim().is_empty() {
            return Err(Refusal::MalformedWorkOrder(format!(
                "missing or blank required field {field:?}"
            )));
        }
    }

    // B1: dcterms:identifier pattern.
    if !valid_identity(&order.identity) {
        return Err(Refusal::InvalidIdentity {
            value: order.identity.clone(),
        });
    }

    if !valid_repo(&order.repository) {
        return Err(Refusal::InvalidRepository {
            value: order.repository.clone(),
        });
    }

    if !valid_sha(&order.base_sha) {
        return Err(Refusal::InvalidSha {
            field: "base_sha".to_string(),
            found: order.base_sha.clone(),
        });
    }

    if let Some(ref cand) = order.candidate_sha {
        if !valid_sha(cand) {
            return Err(Refusal::InvalidSha {
                field: "candidate_sha".to_string(),
                found: cand.clone(),
            });
        }
    }

    if !valid_iri(&order.origin_authority) {
        return Err(Refusal::InvalidOriginAuthority {
            value: order.origin_authority.clone(),
        });
    }

    if let Some(ref obs) = order.origin_observation {
        if !valid_iri(obs) {
            return Err(Refusal::InvalidOriginObservation { value: obs.clone() });
        }
    }

    if let Some(ref pack) = order.target_pack {
        if !valid_pack_name(pack) {
            return Err(Refusal::InvalidTargetPack {
                value: pack.clone(),
            });
        }
    }

    // Standing: any closed vocabulary value (or REFUSED(...)) — matches
    // Elixir `standing/1`. Deserialize already parsed; nothing further.

    for (field, list) in [
        ("projections", &order.projections),
        ("required_courts", &order.required_courts),
        ("required_evidence", &order.required_evidence),
        ("acceptance", &order.acceptance),
        ("falsifiers", &order.falsifiers),
    ] {
        if list.is_empty() || list.iter().any(|s| s.trim().is_empty()) {
            return Err(Refusal::InvalidList {
                field: field.to_string(),
            });
        }
    }

    for p in &order.projections {
        if !PROJECTION_TYPES.contains(&p.as_str()) {
            return Err(Refusal::UnsupportedProjectionType { value: p.clone() });
        }
    }

    // B3 path_scope.
    validate_path_scope(&order.path_scope)?;

    // B4 receipt classes (empty OK; unsupported refused).
    for c in &order.required_receipt_classes {
        if !RECEIPT_CLASSES.contains(&c.as_str()) {
            return Err(Refusal::UnsupportedReceiptClass { value: c.clone() });
        }
    }

    for dep in &order.dependencies {
        if dep.upstream.trim().is_empty() {
            return Err(Refusal::InvalidDependency {
                detail: "blank upstream".to_string(),
            });
        }
        if !DEPENDENCY_TYPES.contains(&dep.dep_type.as_str()) {
            return Err(Refusal::InvalidDependency {
                detail: format!("unsupported type {:?}", dep.dep_type),
            });
        }
        if let Some(ref dig) = dep.required_receipt_digest {
            if !valid_digest(dig) {
                return Err(Refusal::DigestMismatch {
                    field: "required_receipt_digest".to_string(),
                    found: dig.clone(),
                });
            }
        }
    }

    // replay_identity: Elixir requires non-blank only (already checked).
    Ok(())
}

fn validate_path_scope(values: &[String]) -> Result<(), Refusal> {
    if values.is_empty() {
        return Err(Refusal::InvalidPathScope {
            detail: "empty or omitted".to_string(),
        });
    }
    if values.iter().any(|s| s.is_empty() || s.starts_with('/')) {
        return Err(Refusal::InvalidPathScope {
            detail: format!("{values:?}"),
        });
    }
    Ok(())
}

impl fmt::Display for AdmittedWorkOrder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.work_order.identity, self.authority)
    }
}

/// B1 identity pattern: `\\A[A-Z][A-Z0-9-]{1,63}\\z`.
pub fn valid_identity(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_uppercase() => {}
        _ => return false,
    }
    let rest: String = chars.collect();
    if rest.is_empty() || rest.len() > 63 {
        return false;
    }
    rest.bytes()
        .all(|b| matches!(b, b'A'..=b'Z' | b'0'..=b'9' | b'-'))
}

/// `@sha` regex (line 43): `\\A[0-9a-f]{40}\\z`.
pub fn valid_sha(s: &str) -> bool {
    s.len() == 40 && s.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
}

/// `@digest` regex (line 44): `\\Asha256:[0-9a-f]{64}\\z`.
pub fn valid_digest(s: &str) -> bool {
    match s.strip_prefix("sha256:") {
        Some(rest) => {
            rest.len() == 64 && rest.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
        }
        None => false,
    }
}

/// `@repo` regex (line 45): `\\A[A-Za-z0-9_.-]+\\/[A-Za-z0-9_.-]+\\z`.
pub fn valid_repo(s: &str) -> bool {
    match s.split_once('/') {
        Some((owner, repo)) => {
            !owner.is_empty()
                && !repo.is_empty()
                && !repo.contains('/')
                && owner.bytes().all(repo_char)
                && repo.bytes().all(repo_char)
        }
        None => false,
    }
}

fn repo_char(b: u8) -> bool {
    b.is_ascii_alphanumeric() || matches!(b, b'.' | b'_' | b'-')
}

/// `@iri` regex (line 54): `\\A[A-Za-z][A-Za-z0-9+.\\-]*:\\S+\\z`.
pub fn valid_iri(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_alphabetic() => {}
        _ => return false,
    }
    let mut scheme_end = None;
    for (i, c) in s.char_indices() {
        if i == 0 {
            continue;
        }
        if c == ':' {
            scheme_end = Some(i);
            break;
        }
        if !(c.is_ascii_alphanumeric() || c == '+' || c == '.' || c == '-') {
            return false;
        }
    }
    match scheme_end {
        Some(i) => !s[i + 1..].is_empty() && !s[i + 1..].chars().any(|c| c.is_whitespace()),
        None => false,
    }
}

/// `@pack_name` regex (line 50): `\\A[a-z0-9][a-z0-9_-]*\\z`.
pub fn valid_pack_name(s: &str) -> bool {
    let mut chars = s.chars();
    match chars.next() {
        Some(c) if c.is_ascii_lowercase() || c.is_ascii_digit() => {}
        _ => return false,
    }
    chars.all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_' || c == '-')
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn validators_match_igniter_regexes() {
        assert!(valid_sha("aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa"));
        assert!(!valid_sha("AAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAAA"));
        assert!(!valid_sha("aaaa"));
        assert!(valid_digest(
            "sha256:1111111111111111111111111111111111111111111111111111111111111111"
        ));
        assert!(!valid_digest(
            "1111111111111111111111111111111111111111111111111111111111111111"
        ));
        assert!(valid_repo("owner/repo"));
        assert!(valid_repo("own.er_x-y/r.e-p_o1"));
        assert!(!valid_repo("owner"));
        assert!(!valid_repo("a/b/c"));
        assert!(!valid_repo("/leadingslash"));
        assert!(valid_iri("https://graph.test/auth/x1"));
        assert!(valid_iri("urn:gall:002"));
        assert!(!valid_iri("local_name"));
        assert!(!valid_iri("has space:https://x"));
        assert!(valid_identity("SJ-ALIGN-001"));
        assert!(valid_identity("X1"));
        assert!(!valid_identity("A"));
        assert!(!valid_identity("lowercase"));
        assert!(valid_pack_name("semantic-jira"));
        assert!(!valid_pack_name("BadPack"));
    }
}
