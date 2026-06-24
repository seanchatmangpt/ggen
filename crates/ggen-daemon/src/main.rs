use std::sync::Arc;
use tracing::info;
use tracing_subscriber::{EnvFilter, fmt};

use ggen_daemon::{
    CampaignRunner, DaemonScheduler, DaemonState, GgenDaemonMcp, MetricsStore,
};
use rmcp::{ServiceExt, transport::stdio};

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_writer(std::io::stderr)
        .init();

    let working_dir = std::env::current_dir()?;
    let cron_ttl = working_dir.join(".specify/specs/cron/cron-schedule.ttl");
    let catalog_ttl = working_dir.join(".specify/specs/repos-catalog.ttl");
    let state_dir = working_dir.join(".ggen");
    let repos_dir = working_dir.join(".ggen/repos");

    if !state_dir.exists() { tokio::fs::create_dir_all(&state_dir).await?; }
    if !repos_dir.exists() { tokio::fs::create_dir_all(&repos_dir).await?; }

    let persist_path = Some(state_dir.join("daemon-runs.json"));
    let state = Arc::new(
        DaemonState::new(persist_path, cron_ttl.to_string_lossy().into_owned()).await?
    );

    let args: Vec<String> = std::env::args().collect();
    let cmd = args.get(1).map(|s| s.as_str()).unwrap_or("scheduler");

    match cmd {
        "--mcp" | "mcp" => {
            info!("starting MCP server over stdio");
            let mcp = GgenDaemonMcp::new(Arc::clone(&state), working_dir.clone());
            let service = mcp.serve(stdio()).await?;
            service.waiting().await?;
        }

        "run-day" => {
            let day: u8 = args.get(2)
                .and_then(|s| s.parse().ok())
                .ok_or_else(|| anyhow::anyhow!("usage: ggen-daemon run-day <1-7>"))?;
            let runner = CampaignRunner::new(
                catalog_ttl, cron_ttl, repos_dir, Arc::clone(&state),
            );
            let result = runner.run_day(day).await?;
            println!("{}", serde_json::to_string_pretty(&result)?);
        }

        "run-campaign" => {
            let runner = CampaignRunner::new(
                catalog_ttl, cron_ttl, repos_dir, Arc::clone(&state),
            );
            let results = runner.run_full().await?;
            println!("{}", serde_json::to_string_pretty(&results)?);
        }

        "status" => {
            let metrics = MetricsStore::new(Arc::clone(&state));
            let dash = metrics.dashboard().await?;
            println!("{}", serde_json::to_string_pretty(&dash)?);
        }

        "--watch" | "watch" => {
            let specify_dir = working_dir.join(".specify");
            let debounce_secs: u64 = args.get(2)
                .and_then(|s| s.parse().ok())
                .unwrap_or(5);
            info!("watch mode: monitoring {} ({}s debounce)", specify_dir.display(), debounce_secs);
            ggen_daemon::scheduler::watch_and_dispatch(
                specify_dir,
                Arc::clone(&state),
                working_dir,
                debounce_secs,
            ).await?;
        }

        _ => {
            if !cron_ttl.exists() {
                anyhow::bail!("cron TTL not found: {}", cron_ttl.display());
            }
            let daemon = DaemonScheduler::from_ontology(&cron_ttl, Arc::clone(&state), working_dir).await?;
            info!("scheduler running — Ctrl-C to stop");
            daemon.run_until_signal().await;
        }
    }

    Ok(())
}
