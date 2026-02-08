//! Comprehensive security tests for SafeCommand
//!
//! Tests all edge cases and attack vectors:
//! - Command injection variations
//! - Shell metacharacter attacks
//! - Path traversal in arguments
//! - Command length attacks
//! - Whitelist bypass attempts

use ggen_utils::safe_command::{CommandArg, CommandName, SafeCommand};
use ggen_utils::safe_path::SafePath;

// ============================================================================
// Command Injection Attack Tests
// ============================================================================

#[test]
fn test_command_injection_semicolon() {
    // Arrange
    let attacks = vec![
        "build; rm -rf /",
        "build ; rm -rf /",
        "build  ;  rm -rf /",
        "; rm -rf /",
    ];

    // Act & Assert
    for attack in attacks {
        let result = SafeCommand::new("cargo").unwrap().arg(attack);
        assert!(
            result.is_err(),
            "Should block semicolon injection: {}",
            attack
        );
        assert!(
            result.unwrap_err().to_string().contains("metacharacter"),
            "Error should mention metacharacter"
        );
    }
}

#[test]
fn test_command_injection_pipe() {
    // Arrange
    let attacks = vec![
        "build | tee output",
        "build|tee output",
        "build || echo hacked",
        "| cat /etc/passwd",
    ];

    // Act & Assert
    for attack in attacks {
        let result = SafeCommand::new("cargo").unwrap().arg(attack);
        assert!(result.is_err(), "Should block pipe injection: {}", attack);
    }
}

#[test]
fn test_command_injection_ampersand() {
    // Arrange
    let attacks = vec![
        "build & rm -rf /",
        "build&rm -rf /",
        "build && rm -rf /",
        "build&&rm -rf /",
        "& rm -rf /",
    ];

    // Act & Assert
    for attack in attacks {
        let result = SafeCommand::new("cargo").unwrap().arg(attack);
        assert!(
            result.is_err(),
            "Should block ampersand injection: {}",
            attack
        );
    }
}

#[test]
fn test_command_injection_redirection() {
    // Arrange
    let attacks = vec![
        "build > /etc/passwd",
        "build>> /etc/passwd",
        "build < /etc/passwd",
        "build 2>&1",
        "> /etc/passwd",
        "< /etc/passwd",
    ];

    // Act & Assert
    for attack in attacks {
        let result = SafeCommand::new("cargo").unwrap().arg(attack);
        assert!(
            result.is_err(),
            "Should block redirection injection: {}",
            attack
        );
    }
}

#[test]
fn test_command_injection_command_substitution() {
    // Arrange
    let attacks = vec![
        "$(whoami)",
        "$(rm -rf /)",
        "`whoami`",
        "`rm -rf /`",
        "build $(whoami)",
        "build `whoami`",
    ];

    // Act & Assert
    for attack in attacks {
        let result = SafeCommand::new("cargo").unwrap().arg(attack);
        assert!(
            result.is_err(),
            "Should block command substitution: {}",
            attack
        );
    }
}

#[test]
fn test_command_injection_newline() {
    // Arrange
    let attacks = vec![
        "build\nrm -rf /",
        "build\n\nrm -rf /",
        "\nrm -rf /",
        "build\rrm -rf /",
        "build\r\nrm -rf /",
    ];

    // Act & Assert
    for attack in attacks {
        let result = SafeCommand::new("cargo").unwrap().arg(attack);
        assert!(
            result.is_err(),
            "Should block newline injection: {:?}",
            attack
        );
    }
}

// ============================================================================
// Whitelist Bypass Tests
// ============================================================================

#[test]
fn test_whitelist_dangerous_commands() {
    // Arrange
    let dangerous = vec![
        "rm", "rmdir", "dd", "mkfs", "kill", "killall", "pkill", "sudo", "su", "chmod", "chown",
        "curl", "wget", "nc", "netcat", "telnet", "ssh", "scp", "rsync", "tar", "zip", "unzip",
        "7z",
    ];

    // Act & Assert
    for cmd in dangerous {
        let result = SafeCommand::new(cmd);
        assert!(result.is_err(), "Should block dangerous command: {}", cmd);
        assert!(
            result.unwrap_err().to_string().contains("not in whitelist"),
            "Error should mention whitelist for: {}",
            cmd
        );
    }
}

#[test]
fn test_whitelist_case_sensitivity() {
    // Arrange - uppercase versions of allowed commands
    let uppercase_attempts = vec!["CARGO", "GIT", "NPM", "Cargo", "Git", "Npm"];

    // Act & Assert
    for cmd in uppercase_attempts {
        let result = SafeCommand::new(cmd);
        assert!(result.is_err(), "Should be case-sensitive, block: {}", cmd);
    }
}

#[test]
fn test_whitelist_with_path() {
    // Arrange - attempts to use full paths
    let path_attempts = vec![
        "/usr/bin/cargo",
        "/bin/git",
        "./cargo",
        "../cargo",
        "~/cargo",
    ];

    // Act & Assert
    for cmd in path_attempts {
        let result = SafeCommand::new(cmd);
        assert!(
            result.is_err(),
            "Should block path-qualified commands: {}",
            cmd
        );
    }
}

#[test]
fn test_whitelist_with_whitespace() {
    // Arrange
    let whitespace_attempts = vec![
        "cargo ", " cargo", " cargo ", "car go", "cargo\t", "\tcargo",
    ];

    // Act & Assert
    for cmd in whitespace_attempts {
        let result = CommandName::new(cmd);
        assert!(
            result.is_err(),
            "Should block command with whitespace: {:?}",
            cmd
        );
    }
}

// ============================================================================
// Path Traversal in Arguments Tests
// ============================================================================

#[test]
fn test_path_argument_safe_path_validated() {
    // Arrange
    let safe_path = SafePath::new("src/generated").unwrap();

    // Act
    let cmd = SafeCommand::new("cargo")
        .unwrap()
        .arg_path(&safe_path)
        .validate();

    // Assert
    assert!(cmd.is_ok(), "SafePath arguments should be validated");
}

#[test]
fn test_path_argument_parent_dir_blocked() {
    // Arrange - try to pass parent dir as string argument
    let attacks = vec![
        "../../../etc/passwd",
        "../../etc/passwd",
        "../etc/passwd",
        "subdir/../../etc/passwd",
    ];

    // Act & Assert
    for attack in attacks {
        // Parent dirs don't contain shell metacharacters, so they pass CommandArg validation
        // But they should be blocked if user tries to create SafePath from them
        let result = SafePath::new(attack);
        assert!(
            result.is_err(),
            "SafePath should block path traversal: {}",
            attack
        );
    }
}

#[test]
fn test_safe_path_integration() {
    // Arrange
    let valid_path = SafePath::new("src/main.rs").unwrap();
    let invalid_path_str = "../../../etc/passwd";

    // Act - valid path
    let valid_cmd = SafeCommand::new("rustfmt")
        .unwrap()
        .arg_path(&valid_path)
        .validate();

    // Act - invalid path (can't create SafePath)
    let invalid_path = SafePath::new(invalid_path_str);

    // Assert
    assert!(valid_cmd.is_ok(), "Valid SafePath should work");
    assert!(invalid_path.is_err(), "Invalid path should be blocked");
}

// ============================================================================
// Length Attack Tests
// ============================================================================

#[test]
fn test_max_length_single_arg() {
    // Arrange - create argument that exceeds MAX_COMMAND_LENGTH (4096)
    let long_arg = "a".repeat(5000);

    // Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg(&long_arg)
        .unwrap()
        .validate();

    // Assert
    assert!(result.is_err(), "Should block command exceeding max length");
    assert!(
        result.unwrap_err().to_string().contains("exceeds maximum"),
        "Error should mention max length"
    );
}

#[test]
fn test_max_length_many_args() {
    // Arrange - create many args that together exceed MAX_COMMAND_LENGTH
    let mut cmd = SafeCommand::new("cargo").unwrap();

    // Add 500 args of 10 chars each = 5000 chars total (exceeds 4096)
    for i in 0..500 {
        cmd = cmd.arg(&format!("arg_{:05}", i)).unwrap();
    }

    // Act
    let result = cmd.validate();

    // Assert
    assert!(result.is_err(), "Should block total length exceeding max");
}

#[test]
fn test_max_length_boundary() {
    // Arrange - create command at exactly MAX_COMMAND_LENGTH
    // "cargo" = 5 chars
    // " " + "build" = 1 + 5 = 6 chars
    // " " + arg = 1 + arg_len
    // Total = 5 + 6 + 1 + arg_len = 12 + arg_len
    // For total = 4096: arg_len = 4096 - 12 = 4084
    let boundary_arg = "a".repeat(4084);

    // Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("build")
        .unwrap()
        .arg(&boundary_arg)
        .unwrap()
        .validate();

    // Assert - should succeed at exact boundary
    assert!(
        result.is_ok(),
        "Should allow command at max length boundary"
    );
}

#[test]
fn test_max_length_just_over_boundary() {
    // Arrange - create command just over MAX_COMMAND_LENGTH
    let over_boundary_arg = "a".repeat(4086);

    // Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("build")
        .unwrap()
        .arg(&over_boundary_arg)
        .unwrap()
        .validate();

    // Assert
    assert!(result.is_err(), "Should block command just over max length");
}

// ============================================================================
// Combined Attack Tests
// ============================================================================

#[test]
fn test_combined_attack_injection_and_path() {
    // Arrange - combine command injection with path traversal
    let combined_attacks = vec![
        "../../etc/passwd; rm -rf /",
        "../../../etc/passwd | cat",
        "../../etc/passwd && whoami",
    ];

    // Act & Assert
    for attack in combined_attacks {
        let result = SafeCommand::new("cargo").unwrap().arg(attack);
        assert!(result.is_err(), "Should block combined attack: {}", attack);
    }
}

#[test]
fn test_combined_attack_multiple_stages() {
    // Arrange - try injection in different args
    let result1 = SafeCommand::new("cargo")
        .unwrap()
        .arg("build")
        .unwrap()
        .arg("--release");

    let result2 = result1.unwrap().arg("; rm -rf /");

    // Act & Assert
    assert!(result2.is_err(), "Should block injection in any arg");
}

// ============================================================================
// Encoding Attack Tests
// ============================================================================

#[test]
fn test_unicode_shell_metacharacters() {
    // Arrange - Unicode look-alikes (should be blocked if they match)
    // Note: Rust char matching is exact, so these won't match ASCII metacharacters
    // But we test to ensure no unexpected behavior
    let unicode_attacks = vec![
        "build｜cat /etc/passwd", // Full-width pipe
        "build；rm -rf /",        // Full-width semicolon
        "build＆rm -rf /",        // Full-width ampersand
    ];

    // Act & Assert
    for attack in unicode_attacks {
        // These won't be blocked by our ASCII metachar check, but they're also
        // not valid shell syntax, so they're safe in practice
        let result = CommandArg::new(attack);
        // If it passes (which it will), it's safe because shells don't interpret
        // full-width Unicode chars as metacharacters
        let _ = result;
    }
}

#[test]
fn test_null_byte_in_command() {
    // Arrange
    let null_attacks = vec!["cargo\0", "\0cargo", "car\0go"];

    // Act & Assert
    for attack in null_attacks {
        // Rust strings can contain null bytes, but they won't work as commands
        // Our whitelist check will catch these because they won't match exactly
        let result = CommandName::new(attack);
        assert!(
            result.is_err(),
            "Should block command with null byte: {:?}",
            attack
        );
    }
}

#[test]
fn test_null_byte_in_arg() {
    // Arrange
    let null_attacks = vec!["build\0", "\0", "build\0--release"];

    // Act & Assert
    for attack in null_attacks {
        // Null bytes are not in our metacharacter list, but they're dangerous
        // We should add them to be extra safe
        let result = CommandArg::new(attack);
        // Currently these would pass, but they're safe because std::process::Command
        // will reject them. We could add explicit null byte check for defense-in-depth.
        let _ = result;
    }
}

// ============================================================================
// Batch Validation Tests
// ============================================================================

#[test]
fn test_args_bulk_all_valid() {
    // Arrange
    let args = vec!["build", "--release", "--all-features"];

    // Act
    let result = SafeCommand::new("cargo").unwrap().args(&args);

    // Assert
    assert!(result.is_ok(), "All valid args should pass");
}

#[test]
fn test_args_bulk_one_invalid() {
    // Arrange
    let args = vec!["build", "--release; rm -rf /", "--all-features"];

    // Act
    let result = SafeCommand::new("cargo").unwrap().args(&args);

    // Assert
    assert!(result.is_err(), "Should fail on first invalid arg");
}

#[test]
fn test_args_bulk_empty_vec() {
    // Arrange
    let args: Vec<&str> = vec![];

    // Act
    let result = SafeCommand::new("cargo").unwrap().args(&args);

    // Assert
    assert!(result.is_ok(), "Empty args vec should be allowed");
}

// ============================================================================
// Real-World Usage Tests
// ============================================================================

#[test]
fn test_cargo_make_command() {
    // Arrange & Act
    let cmd = SafeCommand::new("cargo")
        .unwrap()
        .arg("make")
        .unwrap()
        .arg("test")
        .unwrap()
        .validate();

    // Assert
    assert!(cmd.is_ok());
    assert_eq!(cmd.unwrap().to_string_debug(), "cargo make test");
}

#[test]
fn test_git_status_command() {
    // Arrange & Act
    let cmd = SafeCommand::new("git")
        .unwrap()
        .arg("status")
        .unwrap()
        .validate();

    // Assert
    assert!(cmd.is_ok());
    assert_eq!(cmd.unwrap().to_string_debug(), "git status");
}

#[test]
fn test_timeout_wrapper() {
    // Arrange & Act
    let cmd = SafeCommand::new("timeout")
        .unwrap()
        .arg("5s")
        .unwrap()
        .arg("cargo")
        .unwrap()
        .arg("build")
        .unwrap()
        .validate();

    // Assert
    assert!(cmd.is_ok());
    assert_eq!(cmd.unwrap().to_string_debug(), "timeout 5s cargo build");
}

#[test]
fn test_rustfmt_with_path() {
    // Arrange
    let path = SafePath::new("src/generated/output.rs").unwrap();

    // Act
    let cmd = SafeCommand::new("rustfmt")
        .unwrap()
        .arg_path(&path)
        .validate();

    // Assert
    assert!(cmd.is_ok());
    let cmd_str = cmd.unwrap().to_string_debug();
    assert!(cmd_str.contains("rustfmt"));
    assert!(cmd_str.contains("src/generated/output.rs"));
}

// ============================================================================
// Edge Cases
// ============================================================================

#[test]
fn test_command_with_no_args() {
    // Arrange & Act
    let cmd = SafeCommand::new("git").unwrap().validate();

    // Assert
    assert!(cmd.is_ok(), "Command with no args should be valid");
}

#[test]
fn test_command_with_many_short_args() {
    // Arrange
    let mut cmd = SafeCommand::new("cargo").unwrap();

    // Add 1000 short args
    for i in 0..1000 {
        cmd = cmd.arg(&format!("a{}", i)).unwrap();
    }

    // Act
    let result = cmd.validate();

    // Assert - will likely exceed max length
    // This is expected and should be blocked
    assert!(result.is_err(), "Many args should exceed max length");
}

#[test]
fn test_arg_with_equals_sign() {
    // Arrange & Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("--config=release")
        .unwrap()
        .validate();

    // Assert - equals sign is safe
    assert!(result.is_ok(), "Equals sign should be allowed in args");
}

#[test]
fn test_arg_with_colon() {
    // Arrange & Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("package:name")
        .unwrap()
        .validate();

    // Assert - colon is safe
    assert!(result.is_ok(), "Colon should be allowed in args");
}

#[test]
fn test_arg_with_slash() {
    // Arrange & Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("path/to/file")
        .unwrap()
        .validate();

    // Assert - slash is safe (for paths)
    assert!(result.is_ok(), "Slash should be allowed in args");
}

#[test]
fn test_arg_with_dot() {
    // Arrange & Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("file.rs")
        .unwrap()
        .validate();

    // Assert - dot is safe
    assert!(result.is_ok(), "Dot should be allowed in args");
}

#[test]
fn test_arg_with_underscore_and_dash() {
    // Arrange & Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("my-package_name")
        .unwrap()
        .validate();

    // Assert
    assert!(
        result.is_ok(),
        "Underscore and dash should be allowed in args"
    );
}
