use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;

#[derive(Args, Debug, Clone)]
pub struct HookRemoveArgs {
    /// Hook ID or name to remove
    pub hook_id: String,
}

/// Remove an installed hook
pub fn run(args: &HookRemoveArgs) -> Result<()> {
    crate::runtime::execute(async {
        let output = domain::hook::remove::remove(args.hook_id.clone())
            .await
            .map_err(|e| ggen_utils::error::Error::new(&e))?;

        if output.removed {
            println!("✓ Hook removed: {}", output.hook_id);
        } else {
            println!("✗ Failed to remove hook: {}", output.hook_id);
        }
        Ok(())
    })
}
