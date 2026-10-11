// NEGATIVE (historical FP class): "mockall" appears only inside a string
// literal and an identifier, never as a `use` path or #[automock].
// Expected verdict: [] (clean).
fn lint_message_mentions_mockall() -> String {
    "forbidden: use mockall / #[automock]".to_string()
}

#[test]
fn string_literals_are_not_mock_imports() {
    let msg = lint_message_mentions_mockall();
    assert!(msg.contains("forbidden"));
    assert_eq!(msg.len(), 37);
}
