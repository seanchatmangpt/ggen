use clap::Args;
use utils::error::Result;

#[derive(Args, Debug)]
pub struct GraphArgs {
    #[arg(value_name = "SCOPE")] pub scope: String,
    #[arg(value_name = "ACTION")] pub action: String,
}

pub fn run(args: &GraphArgs) -> Result<()> {
    // TODO: Implement graph export
    println!("Graph export for {}/{} not yet implemented", args.scope, args.action);
    Ok(())
}
