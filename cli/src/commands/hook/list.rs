use clap::Args;
use clap_noun_verb_macros::verb;
use ggen_utils::error::Result;

#[derive(Args, Debug, Clone)]
pub struct HookListArgs {}

/// List all installed hooks
pub fn run(_args: &HookListArgs) -> Result<()> {
    crate::runtime::execute(async {
        let output = domain::hook::list::list()
            .await
            .map_err(|e| ggen_utils::error::Error::new(&e))?;

        if output.hooks.is_empty() {
            println!("No hooks installed");
        } else {
            println!("Installed hooks ({}):", output.count);
            for hook in output.hooks {
                println!("  - {} [{}]", hook.id, if hook.enabled { "enabled" } else { "disabled" });
                println!("    Trigger: {}", hook.trigger);
                println!("    Action:  {}", hook.action);
            }
        }
        Ok(())
    })
}
