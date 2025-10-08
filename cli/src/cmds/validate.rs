use clap::Args;
use utils::error::Result;

#[derive(Args, Debug)]
pub struct ValidateArgs {
    #[arg(value_name = "SCOPE")] pub scope: String,
    #[arg(value_name = "ACTION")] pub action: String,
    #[arg(short = 'v', long = "vars")] pub vars: Vec<String>,
}

pub fn run(args: &ValidateArgs) -> Result<()> {
    println!("Validating {}/{} with {} vars", args.scope, args.action, args.vars.len());
    Ok(())
}
