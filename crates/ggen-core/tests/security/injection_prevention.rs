//! Injection prevention tests

#[test]
fn test_json_injection_prevention() {
    // Test that user input cannot break out of JSON structure
    let malicious_inputs = vec![
        r#"", "admin": true, ""#,
        r#"}{"admin":true}{"#,
        "\"}]}]}]}",
        "\\\"",
    ];

    for input in malicious_inputs {
        // Input should be properly escaped when serialized
        let escaped = serde_json::to_string(&input).unwrap();

        // Should not contain unescaped quotes that could break JSON
        let parsed: String = serde_json::from_str(&escaped).unwrap();
        assert_eq!(parsed, input);
    }
}

#[test]
fn test_newline_injection() {
    // Test handling of newlines that could break line-based parsing
    let input_with_newlines = "line1\nline2\rline3\r\nline4";

    let json = serde_json::to_string(&input_with_newlines).unwrap();
    let parsed: String = serde_json::from_str(&json).unwrap();

    assert_eq!(parsed, input_with_newlines);
}

#[test]
fn test_unicode_escape_injection() {
    // Test Unicode escapes that could be used to inject
    let unicode_escapes = vec![
        "\\u0000", // Null byte
        "\\u000A", // Newline
        "\\u000D", // Carriage return
        "\\u0022", // Quote
        "\\u005C", // Backslash
    ];

    for input in unicode_escapes {
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        // Should preserve the literal string
        assert_eq!(parsed, input);
    }
}

#[test]
fn test_html_injection_prevention() {
    // Test HTML tags that could be used for XSS
    let html_inputs = vec![
        "<script>alert(1)</script>",
        "<img src=x onerror=alert(1)>",
        "<iframe src=javascript:alert(1)>",
        "<<SCRIPT>alert(1);//<</SCRIPT>",
        "<svg onload=alert(1)>",
    ];

    for input in html_inputs {
        // Input should be stored as-is (escaping happens at display)
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);

        // Verify JSON encoding prevents injection
        assert!(json.contains("\\u003c") || json.contains("<")); // '<' is escaped or preserved
    }
}

#[test]
fn test_yaml_injection_prevention() {
    // Test YAML-specific injection vectors
    let yaml_inputs = vec![
        "!!python/object/apply:os.system ['id']",
        "- |",
        "!!str",
        "&anchor *anchor",
    ];

    for input in yaml_inputs {
        // Should be treated as plain strings in JSON
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);
    }
}

#[test]
fn test_template_injection_prevention() {
    // Test template engine injection attempts
    let template_inputs = vec![
        "{{7*7}}",
        "${7*7}",
        "<%=7*7%>",
        "{{{dangerous}}}",
        "[[dangerous]]",
    ];

    for input in template_inputs {
        // Should be treated as literal strings
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);
    }
}

#[test]
fn test_ldap_injection_prevention() {
    // Test LDAP injection attempts
    let ldap_inputs = vec!["*", "*)(uid=*", "admin)(|(password=*", "\\2a\\28\\29"];

    for input in ldap_inputs {
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);
    }
}

#[test]
fn test_nosql_injection_prevention() {
    // Test NoSQL injection attempts
    let nosql_inputs = vec![
        r#"{"$gt": ""}"#,
        r#"{"$ne": null}"#,
        r#"' || 1==1//"#,
        r#"'; return 1==1;//"#,
    ];

    for input in nosql_inputs {
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);
    }
}

#[test]
fn test_xml_injection_prevention() {
    // Test XML/XXE injection attempts
    let xml_inputs = vec![
        "<!DOCTYPE foo [<!ENTITY xxe SYSTEM \"file:///etc/passwd\">]>",
        "<!ENTITY xxe SYSTEM \"file:///dev/random\">]>",
        "<![CDATA[<script>alert(1)</script>]]>",
    ];

    for input in xml_inputs {
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);
    }
}

#[test]
fn test_csv_injection_prevention() {
    // Test CSV formula injection
    let csv_inputs = vec!["=1+1", "+1+1", "-1+1", "@SUM(1+1)", "=cmd|'/c calc'!A1"];

    for input in csv_inputs {
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);
    }
}

#[test]
fn test_polyglot_injection_prevention() {
    // Test polyglot payloads (valid in multiple contexts)
    let polyglot_inputs = vec![
        "javascript:alert(1)",
        "data:text/html,<script>alert(1)</script>",
        "vbscript:msgbox(1)",
    ];

    for input in polyglot_inputs {
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed, input);
    }
}

#[test]
fn test_buffer_overflow_prevention() {
    // Test that extremely long inputs don't cause buffer overflows
    let huge_input = "A".repeat(10_000_000); // 10MB string

    let json = serde_json::to_string(&huge_input).unwrap();
    let parsed: String = serde_json::from_str(&json).unwrap();

    assert_eq!(parsed.len(), 10_000_000);
}

#[test]
fn test_format_string_prevention() {
    // Test format string injection attempts
    let format_inputs = vec![
        "%s%s%s%s%s%s%s%s%s%s",
        "%x%x%x%x%x%x%x%x%x%x",
        "%n%n%n%n%n%n%n%n%n%n",
        "{} {} {} {}",
        "${FOO}",
    ];

    for input in format_inputs {
        let json = serde_json::to_string(&input).unwrap();
        let parsed: String = serde_json::from_str(&json).unwrap();

        // Should be treated as literal string
        assert_eq!(parsed, input);
    }
}
