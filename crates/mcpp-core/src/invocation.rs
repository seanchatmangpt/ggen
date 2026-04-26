/// InvocationMode — how mcpp was invoked. Determines protocol obligations.
///
/// - SpecifyCli: `sr specify ...` or `sr portfolio ...` (speckit-ralph CLI sub-invocation)
/// - AgentSlash: `/command` syntax via agent slash interface
/// - ShellBinary: direct shell execution `./mcpp` or `mcpp` (no mcpp prefix in args)
/// - MCPPControl: `mcpp <noun> <verb>` canonical CLI (the primary mode)
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum InvocationMode {
    SpecifyCli,
    AgentSlash,
    ShellBinary,
    MCPPControl,
}

/// Classify invocation mode from program name + argv.
///
/// Rules (applied in order):
/// 1. If any arg starts with `/` (slash command) → AgentSlash
/// 2. If program name ends with `sr` or starts with `sr` → SpecifyCli
/// 3. If no args or program is just the binary name without mcpp prefix → ShellBinary
/// 4. Otherwise → MCPPControl
pub fn classify_invocation(program: &str, args: &[&str]) -> InvocationMode {
    if args.iter().any(|a| a.starts_with('/')) {
        return InvocationMode::AgentSlash;
    }
    let prog = program.trim_end_matches('/');
    let basename = prog.rsplit('/').next().unwrap_or(prog);
    if basename == "sr" || basename.starts_with("sr-") || basename.ends_with("-sr") {
        return InvocationMode::SpecifyCli;
    }
    if args.is_empty() || !basename.contains("mcpp") {
        return InvocationMode::ShellBinary;
    }
    InvocationMode::MCPPControl
}

/// Error envelope class for invocation defects.
pub const INVOCATION_DEFECT_CLASS: &str = "INVOCATION_DEFECT";

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn agent_slash_detected_on_slash_arg() {
        assert_eq!(
            classify_invocation("mcpp", &["/specify", "something"]),
            InvocationMode::AgentSlash
        );
    }

    #[test]
    fn agent_slash_detected_even_with_sr_program() {
        assert_eq!(
            classify_invocation("sr", &["/portfolio", "show"]),
            InvocationMode::AgentSlash
        );
    }

    #[test]
    fn specify_cli_detected_by_program_sr() {
        assert_eq!(
            classify_invocation("sr", &["specify", "contract.yaml"]),
            InvocationMode::SpecifyCli
        );
    }

    #[test]
    fn specify_cli_detected_by_full_path_sr() {
        assert_eq!(
            classify_invocation("/usr/local/bin/sr", &["portfolio", "show"]),
            InvocationMode::SpecifyCli
        );
    }

    #[test]
    fn shell_binary_when_no_args() {
        assert_eq!(
            classify_invocation("mcpp", &[]),
            InvocationMode::ShellBinary
        );
    }

    #[test]
    fn shell_binary_when_program_has_no_mcpp_in_name() {
        assert_eq!(
            classify_invocation("./speckit", &["run"]),
            InvocationMode::ShellBinary
        );
    }

    #[test]
    fn shell_binary_for_dotslash_mcpp_no_args() {
        assert_eq!(
            classify_invocation("./mcpp", &[]),
            InvocationMode::ShellBinary
        );
    }

    #[test]
    fn mcpp_control_canonical_invocation() {
        assert_eq!(
            classify_invocation("mcpp", &["receipt", "emit"]),
            InvocationMode::MCPPControl
        );
    }

    #[test]
    fn mcpp_control_with_full_path() {
        assert_eq!(
            classify_invocation("/usr/local/bin/mcpp", &["portfolio", "show"]),
            InvocationMode::MCPPControl
        );
    }

    #[test]
    fn invocation_defect_class_is_stable() {
        assert_eq!(INVOCATION_DEFECT_CLASS, "INVOCATION_DEFECT");
    }

    #[test]
    fn all_variants_are_distinct() {
        let modes = [
            InvocationMode::SpecifyCli,
            InvocationMode::AgentSlash,
            InvocationMode::ShellBinary,
            InvocationMode::MCPPControl,
        ];
        for (i, a) in modes.iter().enumerate() {
            for (j, b) in modes.iter().enumerate() {
                if i == j {
                    assert_eq!(a, b);
                } else {
                    assert_ne!(a, b);
                }
            }
        }
    }
}
