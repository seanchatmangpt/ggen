//! WIP Integration Example
//!
//! This example demonstrates how to set up real-time autonomous graph evolution
//! from WIP (Work In Progress) events.
//!
//! # Usage
//!
//! ```bash
//! cargo run --example wip_integration_example
//! ```
//!
//! Make sure you have:
//! 1. A WIP WebSocket server running at ws://localhost:8080/events
//! 2. LLM API keys configured (ANTHROPIC_API_KEY or OPENAI_API_KEY)
//! 3. Configuration file at config/wip_integration.toml

use clap::Parser;
use ggen_ai::{
    autonomous::{
        DeploymentAutomation, EvolutionConfig, GraphChangeNotifier, GraphEvolutionEngine,
        OrchestratorConfig, RegenerationEngine, RegenerationOrchestrator, TelemetryCollector,
    },
    create_client_from_args,
    wip_integration::{load_wip_config, WipConnector},
    LlmProvider,
};
use std::sync::Arc;
use tokio::sync::RwLock;

#[derive(Parser, Debug)]
#[command(author, version, about, long_about = None)]
struct Args {
    /// LLM provider (openai, anthropic, ollama)
    #[arg(long, default_value = "anthropic")]
    provider: String,

    /// Model name
    #[arg(long, default_value = "claude-3-5-sonnet-20241022")]
    model: String,

    /// API key (or use environment variable)
    #[arg(long)]
    api_key: Option<String>,

    /// WIP configuration file
    #[arg(long, default_value = "config/wip_integration.toml")]
    config: String,
}

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize logging
    ggen_ai::init_logging();
    tracing::info!("Starting WIP Integration Example");

    let args = Args::parse();

    // Load WIP configuration
    let wip_config = load_wip_config(&args.config)?;
    tracing::info!("Loaded WIP config from {}", args.config);
    tracing::info!("WebSocket URL: {}", wip_config.ws_url);
    tracing::info!("Auto-evolve: {}", wip_config.auto_evolve);
    tracing::info!("Auto-regenerate: {}", wip_config.auto_regenerate);

    // Create LLM clients
    let provider = match args.provider.as_str() {
        "openai" => LlmProvider::OpenAI,
        "anthropic" => LlmProvider::Anthropic,
        "ollama" => LlmProvider::Ollama,
        _ => {
            eprintln!("Unsupported provider: {}", args.provider);
            std::process::exit(1);
        }
    };

    let parser_client = Arc::new(create_client_from_args(
        provider.clone(),
        &args.model,
        args.api_key.as_deref(),
    )?);

    let validator_client = Arc::new(create_client_from_args(
        provider.clone(),
        &args.model,
        args.api_key.as_deref(),
    )?);

    tracing::info!("Created LLM clients with provider: {:?}", provider);

    // Create graph evolution engine
    let evolution_config = EvolutionConfig {
        base_uri: "http://example.org/wip/".to_string(),
        confidence_threshold: wip_config.confidence_threshold,
        auto_validate: true,
        auto_rollback: true,
        regeneration_threshold: 5,
        history_path: Some(".swarm/evolution_history.json".into()),
    };

    let evolution_engine = Arc::new(RwLock::new(GraphEvolutionEngine::new(
        parser_client.clone(),
        validator_client.clone(),
        evolution_config,
    )?));

    tracing::info!("Created graph evolution engine");

    // Create regeneration orchestrator components
    let regeneration_engine = Arc::new(RegenerationEngine::new(
        Default::default(),
        parser_client.clone(),
    ));

    let deployment = Arc::new(RwLock::new(DeploymentAutomation::new(Default::default())));
    let telemetry = Arc::new(TelemetryCollector::new(Default::default()));
    let notifier = Arc::new(GraphChangeNotifier::new());

    // Create orchestrator
    let orchestrator_config = OrchestratorConfig {
        autonomous: true,
        max_concurrent: num_cpus::get(),
        target_cycle_ms: 30000,
        adaptive_optimization: true,
        health_check_interval_secs: 60,
    };

    let orchestrator = Arc::new(RegenerationOrchestrator::new(
        orchestrator_config,
        regeneration_engine,
        deployment,
        telemetry,
        notifier,
    ));

    tracing::info!("Created regeneration orchestrator");

    // Create WIP connector
    let mut connector = WipConnector::new(wip_config, evolution_engine, orchestrator);

    tracing::info!("Starting WIP event listener...");
    tracing::info!("Press Ctrl+C to stop");

    // Start listening (this will run indefinitely with auto-reconnect)
    connector.start_listening().await?;

    Ok(())
}
