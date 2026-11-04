//! Typed admission of WorkOrders — port of `admit_work_order/1`
//! (semantic_jira.ex lines 100-145) restricted to the core-slice field set.
//!
//! Validates required fields, regexes (40-hex sha, sha256 digest,
//! owner/repo, absolute-IRI origin authority), non-empty string lists,
//! the projection vocabulary, and the standing-at-UNKNOWN law. Authority is
//! stamped `NONE`; admission never creates authority.

use serde_json::Value;
use std::fmt;

use crate::work_order::{definition_digest, digest_of, Standing, WorkOrder};

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

/// Typed refusal vocabulary (`{:error, {:refused_work_order, reason}}`
/// shape, rendered as typed variants with remediation text per house law).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum Refusal {
    #[error(
        "MalformedWorkOrder: {0} — remediation: supply all fifteen required fields \
         (identity, title, description, subject, repository, base_sha, standing, \
         evidence_ceiling, promotion_rule, replay_identity, origin_authority, \
         projections, required_courts, required_evidence, acceptance, falsifiers) \
         with well-formed values"
    )]
    MalformedWorkOrder(String),

    #[error(
        "StandingNotUnknown: order {identity} carries standing {found} — remediation: \
         admission admits only UNKNOWN orders; standing transitions belong to the \
         promotion/transition kernels, never to the admission payload"
    )]
    StandingNotUnknown { identity: String, found: String },

    #[error(
        "InvalidOriginAuthority: {value} — remediation: origin_authority must be an \
         absolute IRI (RFC 3986 scheme plus non-empty whitespace-free rest, e.g. \
         https://graph.test/auth/...), never a bare local name"
    )]
    InvalidOriginAuthority { value: String },

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
        "Deserialization: {0} — remediation: the payload is not a valid WorkOrder \
         JSON object (unknown fields are refused: deny_unknown_fields)"
    )]
    Deserialization(String),
}

/// An admitted WorkOrder: the order plus its stamped authority and derived
/// digests. Mirrors the igniter's normalized-map output (lines 120-137).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct AdmittedWorkOrder {
    pub work_order: WorkOrder,
    /// Always `NONE` — admission never creates authority (line 132).
    pub authority: String,
    /// Snapshot digest: over the full order including standing.
    pub work_order_digest: String,
    /// Definition digest: over the closed subset, standing excluded.
    pub definition_digest: String,
}

/// Admits and normalizes one WorkOrder-shaped JSON value without creating
/// authority. Port of `admit_work_order/1`.
pub fn admit_work_order(value: &Value) -> Result<AdmittedWorkOrder, Refusal> {
    let order: WorkOrder = serde_json::from_value(value.clone())
        .map_err(|e| Refusal::Deserialization(e.to_string()))?;

    // required/2 (line 1361): deny_unknown_fields + Deserialize covers
    // presence; the blank-string checks in validate/1 cover the igniter's
    // blank? law.
    validate(&order)?;

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

    if !valid_iri(&order.origin_authority) {
        return Err(Refusal::InvalidOriginAuthority {
            value: order.origin_authority.clone(),
        });
    }

    // Standing-at-UNKNOWN law: admission admits only UNKNOWN orders.
    // REFUSED(...) standing is legal vocabulary but never UNKNOWN.
    if order.standing != Standing::Unknown {
        return Err(Refusal::StandingNotUnknown {
            identity: order.identity.clone(),
            found: order.standing.as_str(),
        });
    }

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
            return Err(Refusal::UnsupportedProjectionType {
                value: p.clone(),
            });
        }
    }

    // replay_identity carries the sha256 digest shape (`@digest` regex,
    // line 44).
    if !valid_digest(&order.replay_identity) {
        return Err(Refusal::DigestMismatch {
            field: "replay_identity".to_string(),
            found: order.replay_identity.clone(),
        });
    }

    Ok(())
}

impl fmt::Display for AdmittedWorkOrder {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{} ({})", self.work_order.identity, self.authority)
    }
}

/// `@sha` regex (line 43): `\\A[0-9a-f]{40}\\z`.
pub fn valid_sha(s: &str) -> bool {
    s.len() == 40 && s.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f'))
}

/// `@digest` regex (line 44): `\\Asha256:[0-9a-f]{64}\\z`.
pub fn valid_digest(s: &str) -> bool {
    match s.strip_prefix("sha256:") {
        Some(rest) => rest.len() == 64 && rest.bytes().all(|b| matches!(b, b'0'..=b'9' | b'a'..=b'f')),
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

/// `@iri` regex (line 54): `\\A[A-Za-z][A-Za-z0-9+.\\-]*:\\S+\\z` — scheme
/// prefix plus a non-empty, whitespace-free rest.
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
        assert!(!valid_digest("1111111111111111111111111111111111111111111111111111111111111111"));
        assert!(valid_repo("owner/repo"));
        assert!(valid_repo("own.er_x-y/r.e-p_o1"));
        assert!(!valid_repo("owner"));
        assert!(!valid_repo("a/b/c"));
        assert!(!valid_repo("/leadingslash"));
        assert!(valid_iri("https://graph.test/auth/x1"));
        assert!(valid_iri("urn:gall:002"));
        assert!(!valid_iri("local_name"));
        assert!(!valid_iri("has space:https://x"));
    }
}
