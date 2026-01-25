//! Property-based tests for SafeCommand using proptest
//!
//! These tests use property-based testing to fuzz the validation logic
//! and ensure it handles arbitrary inputs correctly.

use ggen_utils::safe_command::{CommandArg, CommandName, SafeCommand};
use proptest::prelude::*;

// ============================================================================
// Property-Based Tests for CommandArg
// ============================================================================

proptest! {
    /// Test that any string without shell metacharacters is accepted
    #[test]
    fn prop_command_arg_accepts_safe_strings(
        s in "[a-zA-Z0-9_\\-./]+",
    ) {
        let result = CommandArg::new(&s);
        prop_assert!(result.is_ok(), "Safe string should be accepted: {}", s);
    }

    /// Test that any string with shell metacharacters is rejected
    #[test]
    fn prop_command_arg_rejects_shell_metacharacters(
        prefix in "[a-zA-Z0-9]+",
        metachar in "[;|&><$`\n\r]",
        suffix in "[a-zA-Z0-9]*",
    ) {
        let s = format!("{}{}{}", prefix, metachar, suffix);
        let result = CommandArg::new(&s);
        prop_assert!(result.is_err(), "String with metachar should be rejected: {}", s);
    }

    /// Test that CommandArg roundtrips correctly (if valid)
    #[test]
    fn prop_command_arg_roundtrip(
        s in "[a-zA-Z0-9_\\-./]{1,100}",
    ) {
        let arg = CommandArg::new(&s)?;
        prop_assert_eq!(arg.as_str(), &s, "Roundtrip should preserve value");
    }

    /// Test that very long safe strings are handled correctly
    #[test]
    fn prop_command_arg_long_strings(
        len in 0usize..5000,
    ) {
        let s = "a".repeat(len);
        let result = CommandArg::new(&s);
        // Should always succeed for 'a' repeated (no metacharacters)
        prop_assert!(result.is_ok(), "Long safe string should be accepted");
    }
}

// ============================================================================
// Property-Based Tests for CommandName
// ============================================================================

proptest! {
    /// Test that whitelisted commands are always accepted
    fn prop_command_name_accepts_whitelisted(cmd_idx in 0usize..8) {
        let whitelisted = ["cargo", "git", "npm", "rustc", "rustfmt", "timeout", "make", "ggen"];
        let cmd = whitelisted[cmd_idx % whitelisted.len()];
        let result = CommandName::new(cmd);
        prop_assert!(result.is_ok(), "Whitelisted command should be accepted: {}", cmd);
    }

    /// Test that non-whitelisted commands are rejected
    #[test]
    fn prop_command_name_rejects_non_whitelisted(
        s in "[a-z]{1,10}",
    ) {
        // Filter out actual whitelisted commands
        let whitelisted = ["cargo", "git", "npm", "rustc", "rustfmt", "timeout", "make", "ggen", "sh", "bash", "cmake"];
        if !whitelisted.contains(&s.as_str()) {
            let result = CommandName::new(&s);
            prop_assert!(result.is_err(), "Non-whitelisted command should be rejected: {}", s);
        }
    }

    /// Test that commands with whitespace are rejected
    #[test]
    fn prop_command_name_rejects_whitespace(
        prefix in "[a-z]+",
        suffix in "[a-z]*",
    ) {
        let s = format!("{} {}", prefix, suffix);
        let result = CommandName::new(&s);
        prop_assert!(result.is_err(), "Command with whitespace should be rejected: {}", s);
    }

    /// Test that empty command is rejected
    #[test]
    fn prop_command_name_rejects_empty() {
        let result = CommandName::new("");
        prop_assert!(result.is_err(), "Empty command should be rejected");
    }
}

// ============================================================================
// Property-Based Tests for SafeCommand
// ============================================================================

proptest! {
    /// Test that SafeCommand with valid args can be validated
    #[test]
    fn prop_safe_command_validates_with_safe_args(
        args in prop::collection::vec("[a-zA-Z0-9_\\-]{1,20}", 0..10),
    ) {
        let mut cmd = SafeCommand::new("cargo")?;

        for arg in args {
            cmd = cmd.arg(&arg)?;
        }

        let result = cmd.validate();
        prop_assert!(result.is_ok(), "SafeCommand with safe args should validate");
    }

    /// Test that SafeCommand rejects args with metacharacters
    #[test]
    fn prop_safe_command_rejects_args_with_metacharacters(
        safe_prefix in "[a-zA-Z0-9]+",
        metachar in "[;|&><$`]",
    ) {
        let arg = format!("{}{}", safe_prefix, metachar);
        let result = SafeCommand::new("cargo")?.arg(&arg);
        prop_assert!(result.is_err(), "SafeCommand should reject arg with metachar: {}", arg);
    }

    /// Test that total command length is calculated correctly
    #[test]
    fn prop_safe_command_length_calculation(
        arg_count in 1usize..20,
        arg_len in 1usize..10,
    ) {
        let mut cmd = SafeCommand::new("cargo")?;
        let arg = "a".repeat(arg_len);

        for _ in 0..arg_count {
            cmd = cmd.arg(&arg)?;
        }

        // Expected length: "cargo" (5) + (arg_count * (arg_len + 1 for space))
        let expected_length = 5 + (arg_count * (arg_len + 1));

        // We can't directly access total_length(), but we can validate
        // and check if it respects MAX_COMMAND_LENGTH
        let result = cmd.validate();

        if expected_length <= 4096 {
            prop_assert!(result.is_ok(), "Command within length should validate");
        } else {
            prop_assert!(result.is_err(), "Command exceeding length should fail");
        }
    }

    /// Test that very long commands are rejected
    #[test]
    fn prop_safe_command_rejects_long_commands(
        len in 4100usize..5000,
    ) {
        let long_arg = "a".repeat(len);
        let result = SafeCommand::new("cargo")?.arg(&long_arg)?.validate();
        prop_assert!(result.is_err(), "Very long command should be rejected");
    }

    /// Test that commands at boundary are handled correctly
    #[test]
    fn prop_safe_command_boundary_handling(
        // Test lengths around the boundary: 4090-4100
        len in 4090usize..4100,
    ) {
        // "cargo" = 5 chars, " " = 1 char, so arg can be len - 6
        let arg_len = if len > 6 { len - 6 } else { 0 };
        let arg = "a".repeat(arg_len);

        let result = SafeCommand::new("cargo")?.arg(&arg)?.validate();

        // Total length = 5 (cargo) + 1 (space) + arg_len = len
        if len <= 4096 {
            prop_assert!(result.is_ok(), "Command at boundary {} should validate", len);
        } else {
            prop_assert!(result.is_err(), "Command over boundary {} should fail", len);
        }
    }
}

// ============================================================================
// Property-Based Tests for Edge Cases
// ============================================================================

proptest! {
    /// Test that cloning preserves command properties
    #[test]
    fn prop_safe_command_clone_preserves_properties(
        args in prop::collection::vec("[a-zA-Z0-9_]{1,10}", 1..5),
    ) {
        let mut cmd = SafeCommand::new("cargo")?;

        for arg in &args {
            cmd = cmd.arg(arg)?;
        }

        let validated = cmd.validate()?;
        let cloned = validated.clone();

        prop_assert_eq!(
            validated.to_string_debug(),
            cloned.to_string_debug(),
            "Cloned command should be identical"
        );
    }

    /// Test that args() method returns correct number of arguments
    #[test]
    fn prop_safe_command_args_count(
        arg_count in 0usize..20,
    ) {
        let mut cmd = SafeCommand::new("cargo")?;

        for i in 0..arg_count {
            cmd = cmd.arg(&format!("arg{}", i))?;
        }

        let validated = cmd.validate()?;
        let args = validated.args();

        prop_assert_eq!(args.len(), arg_count, "Args count should match");
    }

    /// Test that to_string_debug() produces valid format
    #[test]
    fn prop_safe_command_debug_format(
        args in prop::collection::vec("[a-zA-Z0-9]{1,10}", 1..5),
    ) {
        let mut cmd = SafeCommand::new("cargo")?;

        for arg in &args {
            cmd = cmd.arg(arg)?;
        }

        let validated = cmd.validate()?;
        let debug_str = validated.to_string_debug();

        // Should start with "cargo"
        prop_assert!(debug_str.starts_with("cargo"), "Debug string should start with command");

        // Should contain all args
        for arg in &args {
            prop_assert!(debug_str.contains(arg), "Debug string should contain arg: {}", arg);
        }
    }
}

// ============================================================================
// Regression Tests (converted to properties)
// ============================================================================

proptest! {
    /// Regression test: ensure semicolon is always blocked
    #[test]
    fn prop_regression_semicolon_blocked(
        prefix in "[a-zA-Z0-9]+",
        suffix in "[a-zA-Z0-9]*",
    ) {
        let arg = format!("{};{}", prefix, suffix);
        let result = CommandArg::new(&arg);
        prop_assert!(result.is_err(), "Semicolon should always be blocked");
    }

    /// Regression test: ensure pipe is always blocked
    #[test]
    fn prop_regression_pipe_blocked(
        prefix in "[a-zA-Z0-9]+",
        suffix in "[a-zA-Z0-9]*",
    ) {
        let arg = format!("{}|{}", prefix, suffix);
        let result = CommandArg::new(&arg);
        prop_assert!(result.is_err(), "Pipe should always be blocked");
    }

    /// Regression test: ensure command substitution is blocked
    #[test]
    fn prop_regression_command_substitution_blocked(
        inner in "[a-zA-Z0-9]+",
    ) {
        let arg = format!("$({})", inner);
        let result = CommandArg::new(&arg);
        prop_assert!(result.is_err(), "Command substitution should be blocked");
    }
}
