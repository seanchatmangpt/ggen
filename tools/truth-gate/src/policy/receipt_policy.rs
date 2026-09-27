//! Receipt validation: evidence records must bind exact subjects and consequences.
use serde::{Deserialize, Serialize};
use super::Violation;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    pub subject: String,
    pub base: String,
    pub head: String,
    pub operation: String,
    pub outcome: String,
    #[serde(default)]
    pub consequence: String,
    #[serde(default)]
    pub replay: String,
}

pub fn validate(receipt: &Receipt, location: &str) -> Vec<Violation> {
    let mut v=Vec::new();
    for (name,value) in [
        ("subject",receipt.subject.as_str()),("base",receipt.base.as_str()),
        ("head",receipt.head.as_str()),("operation",receipt.operation.as_str()),
        ("outcome",receipt.outcome.as_str())
    ] {
        if value.trim().is_empty() {
            v.push(Violation{pattern:format!("missing receipt {}",name),location:location.into(),
                rule:"Receipt identity fields are mandatory; candidate evidence without exact identity is not admitted.".into()});
        }
    }
    if receipt.base == receipt.head && receipt.outcome.eq_ignore_ascii_case("success") {
        v.push(Violation{pattern:"success without subject delta".into(),location:location.into(),
            rule:"A success receipt must identify a changed head or explicitly record a non-mutating observation.".into()});
    }
    v
}

pub fn parse_and_validate(content:&str, location:&str)->Vec<Violation>{
    match serde_json::from_str::<Receipt>(content) {
        Ok(r)=>validate(&r,location),
        Err(e)=>vec![Violation{pattern:"invalid receipt json".into(),location:location.into(),rule:format!("Receipt must parse into the canonical schema: {}",e)}],
    }
}
