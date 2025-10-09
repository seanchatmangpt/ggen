#[cfg(not(debug_assertions))]
use human_panic::setup_panic;

#[cfg(debug_assertions)]
extern crate better_panic;

use ggen_utils::app_config::AppConfig;
use ggen_utils::error::Result;

// Declare test modules
#[allow(dead_code)]
mod mock_registry;

/// The main entry point of the application.
#[tokio::main]
async fn main() -> Result<()> {
    // Human Panic. Only enabled when *not* debugging.
    #[cfg(not(debug_assertions))]
    {
        setup_panic!();
    }

    // Better Panic. Only enabled *when* debugging.
    #[cfg(debug_assertions)]
    {
        better_panic::Settings::debug()
            .most_recent_first(false)
            .lineno_suffix(true)
            .verbosity(better_panic::Verbosity::Full)
            .install();
    }

    let _guard = ggen_utils::logger::setup_logging()?;

    // Initialize tracing for pipeline debugging
    // ggen_core::tracing::init_tracing()?; // Temporarily disabled

    // Initialize Configuration
    let config_contents = include_str!("resources/ggen_config.toml");
    AppConfig::init(Some(config_contents))?;

    // Match Commands
    ggen_cli_lib::cli_match().await?;

    Ok(())
}
