//! Composes path, content, and receipt policies into a single admission surface.
use serde_json::Value;
use super::{decision::Decision, evidence_policy, path_policy, receipt_policy, test_policy, tool_adapter};

pub fn decide_write(tool:&str,input:&Value,fallback_path:Option<&str>)->Option<Decision>{
    let intent=tool_adapter::normalize(tool,input,fallback_path)?;
    let mut violations=Vec::new();
    for fragment in &intent.fragments {
        violations.extend(path_policy::check_write(&intent.path,fragment));
        if intent.path.ends_with(".py") {
            violations.extend(test_policy::check(fragment,&intent.path));
            violations.extend(evidence_policy::check(fragment,&intent.path));
        }
        if matches!(path_policy::classify(&intent.path),path_policy::PathClass::Receipt) && intent.path.ends_with(".json") {
            violations.extend(receipt_policy::parse_and_validate(fragment,&intent.path));
        }
    }
    Some(Decision::from_violations(intent.path,violations))
}

#[cfg(test)]
mod tests {
 use super::*; use serde_json::json;
 #[test] fn generated_projection_is_refused(){
   let d=decide_write("Write",&json!({"file_path":"ontology/generated/runtime.ttl","content":"x"}),None).unwrap();
   assert!(d.refused());
 }
 #[test] fn ordinary_write_is_admitted(){
   let d=decide_write("Write",&json!({"file_path":"docs/x.md","content":"hello"}),None).unwrap();
   assert!(!d.refused());
 }
}
