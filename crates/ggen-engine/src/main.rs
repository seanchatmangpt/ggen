//! Binary entrypoint for ggen.

use anyhow::Result;
// Link the library crate so its `#[verb]` linkme registrations are included.
use ggen as _;

fn main() -> Result<()> {
    let raw: Vec<String> = std::env::args().collect();

    // Cargo external subcommand protocol: `cargo foo bar` → argv = ["cargo-foo", "foo", "bar"]
    // Re-exec self with the injected noun stripped so the rest of main sees clean argv.
    if raw.get(1).map(String::as_str) == Some(env!("CARGO_BIN_NAME").trim_start_matches("cargo-")) {
        let status = std::process::Command::new(&raw[0])
            .args(&raw[2..])
            .status()?;
        std::process::exit(status.code().unwrap_or(1));
    }

    let args = inject_default_verbs(raw);

    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::try_from_default_env()
                .unwrap_or_else(|_| tracing_subscriber::EnvFilter::new("info")),
        )
        .init();

    // Without this, --help/--version show clap-noun-verb's own hardcoded
    // "cli" name and its own compiled-in version, not ggen's.
    ::clap_noun_verb::cli::CommandRegistry::set_app_metadata(
        env!("CARGO_PKG_NAME"),
        env!("CARGO_PKG_VERSION"),
    );

    let registry_mutex = ::clap_noun_verb::cli::CommandRegistry::get();
    let registry = registry_mutex
        .lock()
        .map_err(|e| anyhow::anyhow!("Failed to lock registry: {}", e))?;
    registry.run(args).map_err(|e| anyhow::anyhow!("{}", e))
}

/// Map bare nouns (no verb given) to sensible default verbs.
///
/// Examples:
/// - `tool status`   → `tool status show`
/// - `tool receipt`  → `tool receipt verify`
/// - `tool evidence` → `tool evidence doctor`
///
/// Nouns that need to bypass CliBuilder entirely should use the `run_direct()`
/// escape hatch before this function is called (or before `registry.route()`).
fn inject_default_verbs(mut args: Vec<String>) -> Vec<String> {
    let noun = args.get(1).cloned().unwrap_or_default();
    // A bare `--help`/`-h` after the noun (e.g. `ggen receipt --help`) must list
    // the noun's own subcommands, not get rewritten into the default verb's
    // help. Only treat the arg slot as "missing a verb" when it isn't a help flag.
    let next = args.get(2).map(String::as_str);
    let is_help_flag = matches!(next, Some("--help") | Some("-h"));
    let has_verb = next.map(|a| !a.starts_with('-')).unwrap_or(false);
    if !has_verb && !is_help_flag {
        let default_verb = match noun.as_str() {
            "status" => Some("show"),
            "receipt" => Some("verify"),
            "evidence" => Some("doctor"),
            _ => None,
        };
        if let Some(verb) = default_verb {
            args.insert(2, verb.to_string());
        }
    }
    args
}

#[cfg(test)]
mod inject_default_verbs_tests {
    use super::inject_default_verbs;

    fn v(args: &[&str]) -> Vec<String> {
        args.iter().map(|s| s.to_string()).collect()
    }

    #[test]
    fn bare_noun_gets_default_verb() {
        assert_eq!(
            inject_default_verbs(v(&["ggen", "receipt"])),
            v(&["ggen", "receipt", "verify"])
        );
    }

    #[test]
    fn noun_with_explicit_verb_is_untouched() {
        assert_eq!(
            inject_default_verbs(v(&["ggen", "receipt", "history"])),
            v(&["ggen", "receipt", "history"])
        );
    }

    #[test]
    fn noun_help_flag_is_not_rewritten_into_default_verb_help() {
        // Regression: `ggen receipt --help` must list receipt's own
        // subcommands, not silently become `ggen receipt verify --help`.
        assert_eq!(
            inject_default_verbs(v(&["ggen", "receipt", "--help"])),
            v(&["ggen", "receipt", "--help"])
        );
    }

    #[test]
    fn noun_short_help_flag_is_not_rewritten_into_default_verb_help() {
        assert_eq!(
            inject_default_verbs(v(&["ggen", "receipt", "-h"])),
            v(&["ggen", "receipt", "-h"])
        );
    }

    #[test]
    fn noun_without_default_verb_mapping_is_untouched() {
        assert_eq!(
            inject_default_verbs(v(&["ggen", "graph"])),
            v(&["ggen", "graph"])
        );
    }
}
