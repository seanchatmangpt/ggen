//! WorkOrder representation and content-identity (digest) machinery.
//!
//! Port of `GgenIgniter.SemanticJira`'s representation + digest half:
//! the `@required` field set, the standing vocabulary, and the canonical
//! sorted-key sha256 digest over the closed definition-field subset.

use serde::de::Error as DeError;
use serde::{Deserialize, Deserializer, Serialize};
use serde_json::Value;
use sha2::{Digest as _, Sha256};
use std::collections::BTreeMap;

use crate::admit::Refusal;

/// Canonical semantic standing values (`@standings`, semantic_jira.ex line 17)
/// plus the `REFUSED(<reason>)` string form (lines 1431-1439).
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Standing {
    Unknown,
    PartialAlive,
    Alive,
    Blocked,
    BuildBroken,
    Unsupported,
    Refused(String),
}

impl Standing {
    /// The plain vocabulary names accepted by the igniter's `standing/1`.
    pub fn parse(s: &str) -> Result<Standing, Refusal> {
        match s {
            "UNKNOWN" => Ok(Standing::Unknown),
            "PARTIAL_ALIVE" => Ok(Standing::PartialAlive),
            "ALIVE" => Ok(Standing::Alive),
            "BLOCKED" => Ok(Standing::Blocked),
            "BUILD_BROKEN" => Ok(Standing::BuildBroken),
            "UNSUPPORTED" => Ok(Standing::Unsupported),
            other => {
                if let Some(reason) = refused_reason(other) {
                    Ok(Standing::Refused(reason.to_string()))
                } else {
                    Err(Refusal::MalformedWorkOrder(format!(
                        "invalid standing {other:?}: expected one of UNKNOWN, PARTIAL_ALIVE, \
                         ALIVE, BLOCKED, BUILD_BROKEN, UNSUPPORTED, or REFUSED(<reason>)"
                    )))
                }
            }
        }
    }

    /// Canonical wire form. `REFUSED(<reason>)` round-trips.
    pub fn as_str(&self) -> String {
        match self {
            Standing::Unknown => "UNKNOWN".to_string(),
            Standing::PartialAlive => "PARTIAL_ALIVE".to_string(),
            Standing::Alive => "ALIVE".to_string(),
            Standing::Blocked => "BLOCKED".to_string(),
            Standing::BuildBroken => "BUILD_BROKEN".to_string(),
            Standing::Unsupported => "UNSUPPORTED".to_string(),
            Standing::Refused(reason) => format!("REFUSED({reason})"),
        }
    }
}

impl Serialize for Standing {
    fn serialize<S>(&self, serializer: S) -> Result<S::Ok, S::Error>
    where
        S: serde::Serializer,
    {
        serializer.serialize_str(&self.as_str())
    }
}

impl<'de> Deserialize<'de> for Standing {
    fn deserialize<D>(deserializer: D) -> Result<Standing, D::Error>
    where
        D: Deserializer<'de>,
    {
        let s = String::deserialize(deserializer)?;
        Standing::parse(&s).map_err(DeError::custom)
    }
}

fn refused_reason(s: &str) -> Option<&str> {    let rest = s.strip_prefix("REFUSED(")?;
    if rest.ends_with(')') && rest.len() > 1 {
        Some(&rest[..rest.len() - 1])
    } else {
        None
    }
}

/// The WorkOrder, restricted to the igniter's fifteen required fields
/// (`@required`, semantic_jira.ex line 29) — the core-slice scope.
///
/// `deny_unknown_fields` refuses any input carrying fields outside this
/// closed set; authority is stamped `NONE` by admission, never carried in
/// the wire form (see `crate::admit`).
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct WorkOrder {
    pub identity: String,
    pub title: String,
    pub description: String,
    /// The work subject (file/path/subject identity the order binds to).
    pub subject: String,
    /// owner/repo form (`@repo` regex, line 45).
    pub repository: String,
    /// 40-hex git SHA (`@sha` regex, line 43).
    pub base_sha: String,
    pub standing: Standing,
    pub evidence_ceiling: String,
    pub promotion_rule: String,
    pub replay_identity: String,
    pub origin_authority: String,
    /// The fifteen deterministic projection classes (`@projection_types`,
    /// line 24).
    pub projections: Vec<String>,
    /// Non-empty list of non-empty strings (`nonempty_lists/2`, lines
    /// 1368-1376).
    pub required_courts: Vec<String>,
    pub required_evidence: Vec<String>,
    pub acceptance: Vec<String>,
    pub falsifiers: Vec<String>,
}

impl WorkOrder {
    /// The closed definition-field subset (`@definition_fields`, lines
    /// 38-41, restricted to the fields this struct carries). Standing is
    /// excluded by the same law: standing changes never move the
    /// definition digest. Derived digests are excluded by construction
    /// (they are not fields of this struct).
    fn definition_subset(&self) -> BTreeMap<&'static str, Value> {
        let mut m = BTreeMap::new();
        m.insert("identity", json_str(&self.identity));
        m.insert("title", json_str(&self.title));
        m.insert("description", json_str(&self.description));
        m.insert("subject", json_str(&self.subject));
        m.insert("repository", json_str(&self.repository));
        m.insert("base_sha", json_str(&self.base_sha));
        m.insert("evidence_ceiling", json_str(&self.evidence_ceiling));
        m.insert("promotion_rule", json_str(&self.promotion_rule));
        m.insert("replay_identity", json_str(&self.replay_identity));
        m.insert("origin_authority", json_str(&self.origin_authority));
        m.insert("projections", json_vec(&self.projections));
        m.insert("required_courts", json_vec(&self.required_courts));
        m.insert("required_evidence", json_vec(&self.required_evidence));
        m.insert("acceptance", json_vec(&self.acceptance));
        m.insert("falsifiers", json_vec(&self.falsifiers));
        m
    }
}

fn json_str(s: &str) -> Value {
    Value::String(s.to_string())
}

fn json_vec(v: &[String]) -> Value {
    Value::Array(v.iter().map(|s| Value::String(s.to_string())).collect())
}

/// Canonical sorted-key JSON encoding (`canonical/1` + `strings/1`,
/// semantic_jira.ex lines 1498-1524): object keys sorted, each key-value
/// pair a two-element array, arrays recursed in order, scalars unchanged.
pub fn canonical(value: &Value) -> Value {
    match value {
        Value::Object(map) => {
            let mut keys: Vec<&String> = map.keys().collect();
            keys.sort();
            Value::Array(
                keys.into_iter()
                    .map(|k| {
                        Value::Array(vec![
                            Value::String((*k).clone()),
                            canonical(&map[k]),
                        ])
                    })
                    .collect(),
            )
        }
        Value::Array(items) => Value::Array(items.iter().map(canonical).collect()),
        other => other.clone(),
    }
}

/// Stable digest of JSON-safe semantic data, independent of key order
/// (`digest/1`, lines 80-87). Digest fields are excluded by construction.
pub fn digest_of(value: &Value) -> String {
    let mut hasher = Sha256::new();
    hasher.update(serde_json::to_string(&canonical(value)).expect("canonical is serializable"));
    format!("sha256:{}", hex(&hasher.finalize()))
}

/// Digest over the CLOSED definition-field subset. Port of
/// `definition_digest_of/1` restricted to the struct's fields; standing is
/// excluded, so standing changes never move it.
pub fn definition_digest(order: &WorkOrder) -> String {
    let mut map = serde_json::Map::new();
    for (k, v) in order.definition_subset() {
        map.insert(k.to_string(), v);
    }
    digest_of(&Value::Object(map))
}

/// Hex-encode bytes (no `hex` crate — dependency floor is
/// serde/serde_json/sha2/thiserror/blake3).
pub(crate) fn hex(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for b in bytes {
        s.push_str(&format!("{:02x}", b));
    }
    s
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn standing_parses_vocabulary() {
        assert_eq!(Standing::parse("UNKNOWN").unwrap(), Standing::Unknown);
        assert_eq!(
            Standing::parse("PARTIAL_ALIVE").unwrap(),
            Standing::PartialAlive
        );
        assert!(Standing::parse("unknown").is_err());
    }

    #[test]
    fn standing_parses_refused_form() {
        let s = Standing::parse("REFUSED(bad evidence)").unwrap();
        assert_eq!(s, Standing::Refused("bad evidence".to_string()));
        assert!(Standing::parse("REFUSED()").is_err());
        assert!(Standing::parse("REFUSED(bad").is_err());
    }

    #[test]
    fn digest_is_order_independent() {
        let a = serde_json::json!({"b": 1, "a": 2});
        let b = serde_json::json!({"a": 2, "b": 1});
        assert_eq!(digest_of(&a), digest_of(&b));
    }

    #[test]
    fn standing_change_does_not_move_definition_digest() {
        let mut raw = serde_json::json!({
            "identity": "X1",
            "title": "t",
            "description": "d",
            "subject": "src/x1.rs",
            "repository": "owner/repo",
            "base_sha": "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa",
            "standing": "UNKNOWN",
            "evidence_ceiling": "UNVERIFIED",
            "promotion_rule": "courts",
            "replay_identity": "sha256:1111111111111111111111111111111111111111111111111111111111111111",
            "origin_authority": "https://example.test/auth/X1",
            "projections": ["jira"],
            "required_courts": ["c1"],
            "required_evidence": ["e1"],
            "acceptance": ["a1"],
            "falsifiers": ["f1"]
        });
        let before = definition_digest(
            &serde_json::from_value::<WorkOrder>(raw.clone()).unwrap(),
        );
        raw["standing"] = Value::String("PARTIAL_ALIVE".into());
        let after =
            definition_digest(&serde_json::from_value::<WorkOrder>(raw).unwrap());
        assert_eq!(before, after);
    }
}
