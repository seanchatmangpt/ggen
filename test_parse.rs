fn main() {
    let json_str = r#"{"event_type": "ArtifactWritten"}"#;
    let v: serde_json::Value = serde_json::from_str(json_str).unwrap();
    let s = v.as_object().unwrap().get("event_type").and_then(|v| v.as_str()).unwrap_or("");
    println!("Parsed: '{}'", s);
}
