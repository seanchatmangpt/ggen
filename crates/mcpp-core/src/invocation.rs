//! Invocation classifier — promoted from
//! `chatmangpt-mcpp-v2-cell::speckit-ralph::classify`.
//!
//! Per `portfolio-obl-0002`, the canonical classifier is *extended* from the
//! v2 three-variant version (SpecifyCli/AgentSlash/Unknown) to four lawful
//! variants: `SpecifyCli`, `AgentSlash`, `ShellBinary`, `MCPPControl`.
//!
//! The contract: given a raw command string, exactly one variant is the
//! lawful caller. Mixing modes is the `INVOCATION_DEFECT` failure class.

use serde::Serialize;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize)]
pub enum InvocationMode {
    /// `specify ...` — the Specify CLI run from the developer environment.
    SpecifyCli,
    /// `/speckit.<verb>` — a slash command run inside an agent loop.
    AgentSlash,
    /// `mcpp <verb>`, `sr <verb>`, `git`, `cargo`, … — a plain shell binary.
    ShellBinary,
    /// `mcpp.<service>.<action>` style — MCPP semantic control surface
    /// (e.g. `mcpp.ralph.next`, `mcpp.receipt.verify`). These are A2A skill
    /// names and stdio JSON-RPC method names, not shell invocations.
    MCPPControl,
    /// Unrecognized.
    Unknown,
}

pub fn classify(s: &str) -> InvocationMode {
    let trimmed = s.trim();
    if trimmed.is_empty() {
        return InvocationMode::Unknown;
    }

    // /speckit.* — agent slash command.
    if let Some(rest) = trimmed.strip_prefix('/') {
        if rest.starts_with("speckit.") {
            return InvocationMode::AgentSlash;
        }
        return InvocationMode::Unknown;
    }

    // mcpp.<service>.<action> — MCPP semantic control (A2A skill / MCP tool).
    // Heuristic: bare token, no whitespace, contains at least two dots, and
    // first segment is "mcpp".
    if !trimmed.contains(' ') {
        let segs: Vec<&str> = trimmed.split('.').collect();
        if segs.len() >= 3 && segs[0] == "mcpp" {
            return InvocationMode::MCPPControl;
        }
    }

    // First token decides the rest.
    let head = trimmed.split_whitespace().next().unwrap_or("");
    match head {
        "specify" => InvocationMode::SpecifyCli,
        // Known shell binaries from the v2/canonical surface.
        "mcpp" | "sr" | "speckit-ralph" | "git" | "cargo" | "make" | "gh" | "python3" | "node"
        | "bash" => InvocationMode::ShellBinary,
        _ => InvocationMode::Unknown,
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn empty_is_unknown() {
        assert_eq!(classify(""), InvocationMode::Unknown);
        assert_eq!(classify("   "), InvocationMode::Unknown);
    }

    #[test]
    fn specify_init_is_specify_cli() {
        assert_eq!(classify("specify init"), InvocationMode::SpecifyCli);
        assert_eq!(classify("specify"), InvocationMode::SpecifyCli);
    }

    #[test]
    fn slash_speckit_is_agent_slash() {
        assert_eq!(classify("/speckit.implement"), InvocationMode::AgentSlash);
        assert_eq!(
            classify("/speckit.plan --target mcpp"),
            InvocationMode::AgentSlash
        );
    }

    #[test]
    fn unknown_slash_command_is_unknown() {
        assert_eq!(classify("/foo.bar"), InvocationMode::Unknown);
    }

    #[test]
    fn mcpp_dotted_skill_is_mcpp_control() {
        assert_eq!(classify("mcpp.ralph.next"), InvocationMode::MCPPControl);
        assert_eq!(classify("mcpp.receipt.verify"), InvocationMode::MCPPControl);
    }

    #[test]
    fn mcpp_shell_invocation_is_shell_binary() {
        assert_eq!(classify("mcpp ralph next"), InvocationMode::ShellBinary);
        assert_eq!(classify("sr verify"), InvocationMode::ShellBinary);
    }

    #[test]
    fn known_shell_binaries() {
        for cmd in ["git status", "cargo build", "make test", "gh pr create"] {
            assert_eq!(classify(cmd), InvocationMode::ShellBinary, "{cmd}");
        }
    }

    #[test]
    fn unknown_shell_token_is_unknown() {
        assert_eq!(classify("randomtool foo"), InvocationMode::Unknown);
    }

    #[test]
    fn four_lawful_variants_are_distinct() {
        // The canonical classifier promises four lawful modes plus Unknown.
        let lawful = [
            classify("specify init"),
            classify("/speckit.plan"),
            classify("mcpp ralph next"),
            classify("mcpp.ralph.next"),
        ];
        for (i, a) in lawful.iter().enumerate() {
            for (j, b) in lawful.iter().enumerate() {
                if i != j {
                    assert_ne!(a, b, "modes must be distinct: {a:?} vs {b:?}");
                }
            }
            assert_ne!(*a, InvocationMode::Unknown);
        }
    }
}
