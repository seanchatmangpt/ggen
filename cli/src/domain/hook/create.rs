//! Hook creation - CLI wrapper
//!
//! Wraps async domain logic for creating hooks in a synchronous CLI context.

use clap::Args;
use ggen_utils::error::Result;

/// Arguments for creating a hook
#[derive(Debug, Args)]
pub struct CreateArgs {
    /// Hook trigger event (e.g., "file:change", "template:render")
    #[arg(short = 't', long)]
    pub trigger: String,

    /// Action to execute (shell command or script path)
    #[arg(short = 'a', long)]
    pub action: String,

    /// Optional hook name
    #[arg(short = 'n', long)]
    pub name: Option<String>,
}

/// Run hook create command (placeholder implementation)
pub fn run(args: &CreateArgs) -> Result<()> {
    let hook_id = args.name.clone().unwrap_or_else(|| {
        format!(
            "hook_{}_{}",
            args.trigger.replace([':', ' '], "_"),
            std::time::SystemTime::now()
                .duration_since(std::time::UNIX_EPOCH)
                .unwrap()
                .as_secs()
        )
    });

    println!("✅ Created hook: {}", hook_id);
    println!("   Trigger: {}", args.trigger);
    println!("   Action:  {}", args.action);
    Ok(())
}
