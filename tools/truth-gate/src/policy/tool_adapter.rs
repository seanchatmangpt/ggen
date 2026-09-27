//! Normalizes heterogeneous agent write-tool payloads into one write intent.
use serde_json::Value;

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct WriteIntent {
    pub path: String,
    pub fragments: Vec<String>,
    pub tool: String,
}

const WRITE_TOOLS: &[&str] = &[
    "Edit", "Write", "replace_file_content", "write_to_file",
    "multi_replace_file_content", "replace", "write_file",
];

pub fn normalize(tool: &str, input: &Value, fallback_path: Option<&str>) -> Option<WriteIntent> {
    if !WRITE_TOOLS.contains(&tool) { return None; }
    let path = ["file_path", "TargetFile", "targetFile", "path"]
        .iter().find_map(|k| input.get(k).and_then(Value::as_str))
        .or(fallback_path).unwrap_or("").to_string();

    let mut fragments = Vec::new();
    for key in ["new_string", "content", "CodeContent", "codeContent", "ReplacementContent", "replacementContent"] {
        if let Some(s) = input.get(key).and_then(Value::as_str) { fragments.push(s.to_string()); }
    }
    for key in ["ReplacementChunks", "replacementChunks"] {
        if let Some(chunks) = input.get(key).and_then(Value::as_array) {
            for chunk in chunks {
                for ck in ["ReplacementContent", "replacementContent"] {
                    if let Some(s) = chunk.get(ck).and_then(Value::as_str) {
                        fragments.push(s.to_string());
                    }
                }
            }
        }
    }
    Some(WriteIntent { path, fragments, tool: tool.to_string() })
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;
    #[test] fn normalizes_multi_replace() {
        let v=json!({"TargetFile":"tests/x.py","ReplacementChunks":[{"ReplacementContent":"a"},{"replacementContent":"b"}]});
        let i=normalize("multi_replace_file_content",&v,None).unwrap();
        assert_eq!(i.path,"tests/x.py"); assert_eq!(i.fragments,vec!["a","b"]);
    }
    #[test] fn ignores_non_write_tool() { assert!(normalize("Read",&json!({}),None).is_none()); }
}
