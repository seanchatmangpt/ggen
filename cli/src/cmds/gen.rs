use std::collections::BTreeMap;
use clap::Args;
use utils::error::Result;

#[derive(Args, Debug)]
pub struct GenArgs {
    #[arg(value_name = "SCOPE")] pub scope: String,
    #[arg(value_name = "ACTION")] pub action: String,
    /// key=value pairs, repeatable
    #[arg(short = 'v', long = "vars")] pub vars: Vec<String>,
    /// dry run, print manifest key without writing
    #[arg(long = "dry-run")] pub dry_run: bool,
}

pub fn run(args: &GenArgs) -> Result<()> {
    let mut cli_vars: BTreeMap<String, String> = BTreeMap::new();
    for kv in &args.vars {
        if let Some((k, v)) = kv.split_once('=') {
            cli_vars.insert(k.to_string(), v.to_string());
        }
    }

    let engine = core::Engine::new();
    engine.project(&args.scope, &args.action, &cli_vars, args.dry_run)?;
    Ok(())
}
