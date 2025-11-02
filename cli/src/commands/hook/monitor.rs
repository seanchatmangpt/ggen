use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;

#[derive(Args, Debug, Clone)]
pub struct HookMonitorArgs {
    /// Knowledge graph file to monitor (e.g., 'project.ttl')
    pub graph: String,

    /// Watch interval in seconds
    #[arg(long, default_value = "5")]
    pub interval: Option<u64>,
}

/// Monitor a knowledge graph for changes and trigger hooks
pub fn run(args: &HookMonitorArgs) -> Result<()> {
    crate::runtime::execute(async {
        let output = domain::hook::monitor::monitor(args.graph.clone(), args.interval)
            .await
            .map_err(|e| ggen_utils::error::Error::new(&e))?;

        println!("✓ Monitoring graph: {}", output.graph);
        println!("  Active hooks: {}", output.active_hooks);
        if !output.watching.is_empty() {
            println!("  Watching:");
            for path in output.watching {
                println!("    - {}", path);
            }
        }
        Ok(())
    })
}
