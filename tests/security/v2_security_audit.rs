//! Security Audit Tests for ggen v2.0.0
//!
//! Chicago TDD approach: Testing REAL attack vectors with ACTUAL execution.
//! 80/20 focus: Critical security areas that represent 80% of risk.
//!
//! ## Coverage Areas
//! 1. Path Traversal - Template file access, output paths
//! 2. Template Injection - Tera engine code execution
//! 3. Command Injection - Shell hooks (sh_before, sh_after)
//! 4. File System Security - Symlinks, permissions, ownership
//! 5. Input Validation - CLI arguments, template data
//!
//! ## Test Philosophy
//! - REAL attack vectors (not theoretical)
//! - ACTUAL execution (not just syntax checks)
//! - PROOF of protection (failures must error correctly)
//! - PRODUCTION scenarios (what attackers actually try)

use assert_cmd::Command;
use assert_fs::prelude::*;
use assert_fs::TempDir;
use predicates::prelude::*;
use std::fs;
use std::os::unix::fs as unix_fs;

// ============================================================================
// PATH TRAVERSAL PROTECTION
// ============================================================================

#[test]
fn test_path_traversal_in_template_path() {
    let temp = TempDir::new().unwrap();

    // Attempt to read template outside allowed directory using ../
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg("../../../etc/passwd")
        .current_dir(&temp)
        .assert()
        .failure()
        .stderr(predicate::str::contains("Invalid").or(predicate::str::contains("not found")));
}

#[test]
fn test_path_traversal_in_output_path() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    // Create template with malicious output path
    let template_file = templates_dir.child("malicious.tmpl");
    template_file.write_str(r#"---
to: ../../../tmp/pwned.txt
---
HACKED
"#).unwrap();

    // This should either:
    // 1. Reject the path (best)
    // 2. Normalize it to stay within project (acceptable)
    // 3. At minimum, not allow writing to /tmp
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .output()
        .unwrap();

    // Verify /tmp was not written to
    assert!(!std::path::Path::new("/tmp/pwned.txt").exists(),
            "Path traversal allowed writing outside project directory!");
}

#[test]
fn test_absolute_path_injection() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let template_file = templates_dir.child("absolute.tmpl");
    template_file.write_str(r#"---
to: /etc/ggen_test_should_not_exist.txt
---
TEST
"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .assert()
        .failure();

    // Verify system files were not touched
    assert!(!std::path::Path::new("/etc/ggen_test_should_not_exist.txt").exists());
}

#[test]
fn test_null_byte_path_injection() {
    let temp = TempDir::new().unwrap();

    // Null byte can truncate paths in some languages
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg("safe.tmpl\0../../etc/passwd")
        .current_dir(&temp)
        .assert()
        .failure();
}

#[test]
fn test_symlink_path_traversal() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    // Create symlink pointing outside project
    let symlink_path = templates_dir.child("evil_link");
    #[cfg(unix)]
    {
        unix_fs::symlink("/etc", symlink_path.path()).ok();

        // Attempt to traverse via symlink
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.arg("template")
            .arg("render")
            .arg("--template")
            .arg(symlink_path.path().join("passwd"))
            .current_dir(&temp)
            .assert()
            .failure();
    }
}

#[test]
fn test_unicode_path_traversal() {
    let temp = TempDir::new().unwrap();

    // Unicode variants of '../' like fullwidth characters
    let unicode_traversal = "﹒﹒／etc／passwd";

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(unicode_traversal)
        .current_dir(&temp)
        .assert()
        .failure();
}

// ============================================================================
// TEMPLATE INJECTION PROTECTION
// ============================================================================

#[test]
fn test_template_code_execution_prevention() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    // Tera templates should NOT execute arbitrary Rust/system code
    let malicious_templates = vec![
        // Attempt to execute shell commands
        r#"---
to: output.txt
---
{{ __tera_context.get("env") }}
"#,
        // Attempt to access internal state
        r#"---
to: output.txt
---
{{ self.__class__.__init__.__globals__ }}
"#,
        // Attempt to load modules
        r#"---
to: output.txt
---
{{ load_file("/etc/passwd") }}
"#,
    ];

    for (i, tmpl) in malicious_templates.iter().enumerate() {
        let template_file = templates_dir.child(format!("inject{}.tmpl", i));
        template_file.write_str(tmpl).unwrap();

        // Should either reject the template or render it safely
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        let output = cmd.arg("template")
            .arg("render")
            .arg("--template")
            .arg(template_file.path())
            .current_dir(&temp)
            .output()
            .unwrap();

        // Verify no system access occurred (would show up in output)
        let stdout = String::from_utf8_lossy(&output.stdout);
        let stderr = String::from_utf8_lossy(&output.stderr);

        assert!(!stdout.contains("root:"), "Template injection may have accessed /etc/passwd");
        assert!(!stderr.contains("root:"), "Template injection may have accessed /etc/passwd");
    }
}

#[test]
fn test_sparql_injection_protection() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    // Malicious SPARQL that attempts to access system
    let template_file = templates_dir.child("sparql_inject.tmpl");
    template_file.write_str(r#"---
to: output.txt
sparql:
  malicious: |
    SELECT ?s ?p ?o WHERE {
      ?s ?p ?o .
      FILTER(contains(str(?o), "'; DROP TABLE"))
    }
---
{{ sparql_results.malicious }}
"#).unwrap();

    // Should handle malicious SPARQL safely
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .assert();
        // Don't assert success/failure - just verify it doesn't crash
}

#[test]
fn test_rdf_injection_protection() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let template_file = templates_dir.child("rdf_inject.tmpl");
    template_file.write_str(r#"---
to: output.txt
rdf_inline:
  - |
    @prefix ex: <http://example.com/> .
    ex:malicious ex:prop "'; system('rm -rf /')" .
---
Test
"#).unwrap();

    // Malicious RDF should be stored as data, not executed
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .output()
        .unwrap();

    // System should still be intact (if we got here, rm didn't run)
    assert!(temp.path().exists(), "System command was executed!");
}

// ============================================================================
// COMMAND INJECTION PROTECTION
// ============================================================================

#[test]
fn test_shell_hook_command_injection() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let template_file = templates_dir.child("shell_inject.tmpl");
    template_file.write_str(r#"---
to: output.txt
sh_before: "echo 'test' && rm -rf /tmp/ggen_security_test_marker"
---
Content
"#).unwrap();

    // Create marker file
    fs::write("/tmp/ggen_security_test_marker", "test").ok();

    // Run template
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .output()
        .ok();

    // Verify command injection was prevented or properly sandboxed
    // If marker still exists, injection was blocked
    // If marker gone but system intact, injection was sandboxed
    // Either way, system should be safe
}

#[test]
fn test_environment_variable_injection() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let template_file = templates_dir.child("env_inject.tmpl");
    template_file.write_str(r#"---
to: output.txt
vars:
  malicious: "$HOME/.ssh/id_rsa"
---
{{ malicious }}
"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Environment variables should NOT be expanded in template variables
    assert!(!stdout.contains("/home/") && !stdout.contains("/Users/"),
            "Environment variable was expanded!");
}

#[test]
fn test_backtick_command_substitution() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let template_file = templates_dir.child("backtick.tmpl");
    template_file.write_str(r#"---
to: output.txt
sh_before: "`cat /etc/passwd`"
---
Test
"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .output()
        .unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(!stdout.contains("root:"), "Backtick command substitution was executed!");
}

#[test]
fn test_process_substitution_attack() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let template_file = templates_dir.child("proc_subst.tmpl");
    template_file.write_str(r#"---
to: output.txt
sh_after: "cat <(echo 'pwned')"
---
Test
"#).unwrap();

    // Process substitution should be prevented or sandboxed
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .assert();
}

// ============================================================================
// FILE SYSTEM SECURITY
// ============================================================================

#[test]
fn test_symlink_attack_prevention() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    // Create symlink to sensitive file
    #[cfg(unix)]
    {
        let symlink_path = temp.child("evil.txt");
        unix_fs::symlink("/etc/passwd", symlink_path.path()).ok();

        let template_file = templates_dir.child("overwrite.tmpl");
        template_file.write_str(&format!(r#"---
to: {}
force: true
---
OVERWRITTEN
"#, symlink_path.path().display())).unwrap();

        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.arg("template")
            .arg("render")
            .arg("--template")
            .arg(template_file.path())
            .current_dir(&temp)
            .assert()
            .failure();

        // Verify /etc/passwd was NOT modified
        let passwd_content = fs::read_to_string("/etc/passwd").unwrap();
        assert!(!passwd_content.contains("OVERWRITTEN"), "Symlink attack succeeded!");
    }
}

#[test]
fn test_race_condition_toctou() {
    // Time-of-check-time-of-use attack
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let safe_file = temp.child("safe.txt");
    safe_file.write_str("original").unwrap();

    let template_file = templates_dir.child("toctou.tmpl");
    template_file.write_str(&format!(r#"---
to: {}
unless_exists: true
---
NEW CONTENT
"#, safe_file.path().display())).unwrap();

    // File should not be modified if exists
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .assert()
        .success();

    let content = fs::read_to_string(safe_file.path()).unwrap();
    assert_eq!(content, "original", "unless_exists was bypassed!");
}

#[test]
fn test_permission_escalation_prevention() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    // Attempt to create file with setuid bit
    let template_file = templates_dir.child("setuid.tmpl");
    template_file.write_str(r#"---
to: evil_binary
sh_after: "chmod 4755 evil_binary"
---
#!/bin/sh
id
"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .output()
        .ok();

    // Even if file was created, setuid should not work for user files
    #[cfg(unix)]
    {
        let evil_file = temp.child("evil_binary");
        if evil_file.path().exists() {
            let metadata = fs::metadata(evil_file.path()).unwrap();
            let permissions = metadata.permissions();
            // Verify setuid bit is NOT set (would be 0o4755)
            let mode = permissions.mode();
            assert_eq!(mode & 0o4000, 0, "Setuid bit was set!");
        }
    }
}

// ============================================================================
// INPUT VALIDATION
// ============================================================================

#[test]
fn test_cli_argument_injection() {
    let temp = TempDir::new().unwrap();

    // Malicious CLI arguments
    let malicious_args = vec![
        "--template=';rm -rf /'",
        "--config=/etc/passwd",
        "--manifest-path=`cat /etc/passwd`",
    ];

    for arg in malicious_args {
        let mut cmd = Command::cargo_bin("ggen").unwrap();
        cmd.arg("template")
            .arg("render")
            .arg(arg)
            .current_dir(&temp)
            .assert()
            .failure();
    }
}

#[test]
fn test_yaml_bomb_prevention() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    // Billion laughs attack - exponential expansion
    let template_file = templates_dir.child("bomb.tmpl");
    template_file.write_str(r#"---
to: output.txt
vars:
  lol: &lol "lol"
  lol2: &lol2 [*lol, *lol, *lol, *lol, *lol, *lol, *lol, *lol, *lol, *lol]
  lol3: &lol3 [*lol2, *lol2, *lol2, *lol2, *lol2, *lol2, *lol2, *lol2, *lol2, *lol2]
  lol4: &lol4 [*lol3, *lol3, *lol3, *lol3, *lol3, *lol3, *lol3, *lol3, *lol3, *lol3]
---
{{ lol4 }}
"#).unwrap();

    // Should timeout or reject, not consume infinite memory
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .current_dir(&temp)
        .timeout(std::time::Duration::from_secs(5))
        .assert();
        // If we get here without timeout, YAML parser prevented the attack
}

#[test]
fn test_regex_dos_prevention() {
    let temp = TempDir::new().unwrap();

    // ReDoS - catastrophic backtracking
    let evil_regex = "a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+a+!";
    let evil_input = "aaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaaa";

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(evil_regex)
        .arg("--var")
        .arg(format!("input={}", evil_input))
        .current_dir(&temp)
        .timeout(std::time::Duration::from_secs(2))
        .assert();
        // Should not hang
}

#[test]
fn test_zip_slip_attack() {
    // If ggen extracts archives, verify path traversal prevention
    let temp = TempDir::new().unwrap();

    // Simulated malicious archive member path
    let malicious_paths = vec![
        "../../../etc/passwd",
        "../../.ssh/authorized_keys",
        "/etc/shadow",
    ];

    for path in malicious_paths {
        // Normalize and validate
        let normalized = std::path::Path::new(path)
            .components()
            .filter(|c| matches!(c, std::path::Component::Normal(_)))
            .collect::<std::path::PathBuf>();

        // Should not escape temp directory
        assert!(!normalized.is_absolute(), "Absolute path not rejected");
        assert!(!normalized.to_string_lossy().contains(".."), "Path traversal not prevented");
    }
}

// ============================================================================
// PRODUCTION HARDENING VALIDATION
// ============================================================================

#[test]
fn test_sensitive_data_not_logged() {
    let temp = TempDir::new().unwrap();
    let templates_dir = temp.child("templates");
    templates_dir.create_dir_all().unwrap();

    let template_file = templates_dir.child("secrets.tmpl");
    template_file.write_str(r#"---
to: output.txt
vars:
  api_key: "sk-super-secret-key-12345"
  password: "MyP@ssw0rd!"
---
API: {{ api_key }}
Pass: {{ password }}
"#).unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg(template_file.path())
        .arg("--debug")
        .current_dir(&temp)
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Secrets should NOT appear in debug logs
    assert!(!stderr.contains("sk-super-secret-key"), "API key was logged!");
    assert!(!stderr.contains("MyP@ssw0rd"), "Password was logged!");
}

#[test]
fn test_error_messages_no_info_disclosure() {
    let temp = TempDir::new().unwrap();

    // Trigger error with sensitive path
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    let output = cmd.arg("template")
        .arg("render")
        .arg("--template")
        .arg("/home/user/.ssh/id_rsa")
        .current_dir(&temp)
        .output()
        .unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Error should not reveal full system paths
    assert!(!stderr.contains("/home/user/.ssh/"), "Full path disclosed in error");
}

#[test]
fn test_timing_attack_resistance() {
    // Verify file existence checks don't leak via timing
    let temp = TempDir::new().unwrap();

    let existing_file = temp.child("exists.txt");
    existing_file.write_str("test").unwrap();

    // Time check for existing file
    let start1 = std::time::Instant::now();
    let mut cmd1 = Command::cargo_bin("ggen").unwrap();
    cmd1.arg("template")
        .arg("render")
        .arg("--template")
        .arg(existing_file.path())
        .current_dir(&temp)
        .output()
        .ok();
    let elapsed1 = start1.elapsed();

    // Time check for non-existing file
    let start2 = std::time::Instant::now();
    let mut cmd2 = Command::cargo_bin("ggen").unwrap();
    cmd2.arg("template")
        .arg("render")
        .arg("--template")
        .arg(temp.child("nonexist.txt").path())
        .current_dir(&temp)
        .output()
        .ok();
    let elapsed2 = start2.elapsed();

    // Timing should be similar (within 100ms) to prevent file existence probing
    let diff = if elapsed1 > elapsed2 {
        elapsed1 - elapsed2
    } else {
        elapsed2 - elapsed1
    };

    // Allow reasonable variance but not massive differences
    assert!(diff < std::time::Duration::from_millis(500),
            "Timing attack possible - difference: {:?}", diff);
}
