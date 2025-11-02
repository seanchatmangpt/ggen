use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;

#[derive(Args, Debug, Clone)]
pub struct HookCreateArgs {
    /// Trigger event (e.g., 'schema_change', 'file_change')
    pub trigger: String,

    /// Action to execute (e.g., 'regenerate_models', 'regenerate')
    pub action: String,

    /// Optional hook name
    #[arg(long)]
    pub name: Option<String>,
}

/// Create a new hook for autonomic regeneration
pub fn run(args: &HookCreateArgs) -> Result<()> {
    crate::runtime::execute(async {
        let output = domain::hook::create::create(
            args.trigger.clone(),
            args.action.clone(),
            args.name.clone(),
        )
        .await
        .map_err(|e| ggen_utils::error::Error::new(&e))?;

        println!("✓ Hook created: {}", output.hook_id);
        println!("  Trigger: {}", output.trigger);
        println!("  Action:  {}", output.action);
        Ok(())
    })
}
