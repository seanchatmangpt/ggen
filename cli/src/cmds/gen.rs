use clap::Args;
use std::collections::BTreeMap;
use std::path::PathBuf;
use utils::error::Result;

use core::pipeline::Pipeline;

#[derive(Args, Debug)]
pub struct GenArgs {
    /// Path to the template file
    #[arg(short, long)]
    pub template: PathBuf,

    /// Output directory root
    #[arg(short, long, default_value = ".")]
    pub out: PathBuf,

    /// Variables (key=value pairs)
    #[arg(short = 'v', long = "var", value_parser = parse_key_val::<String, String>)]
    pub vars: Vec<(String, String)>,

    /// Dry run (no write)
    #[arg(long)]
    pub dry: bool,
}

fn parse_key_val<K, V>(s: &str) -> std::result::Result<(K, V), String>
where
    K: std::str::FromStr,
    K::Err: ToString,
    V: std::str::FromStr,
    V::Err: ToString,
{
    let pos = s.find('=').ok_or_else(|| format!("invalid KEY=value: no `=` found in `{s}`"))?;
    let key = s[..pos].parse().map_err(|e: K::Err| e.to_string())?;
    let val = s[pos + 1..].parse().map_err(|e: V::Err| e.to_string())?;
    Ok((key, val))
}

pub fn run(args: &GenArgs) -> Result<()> {
    let mut pipeline = Pipeline::new().map_err(utils::error::Error::from)?;
    let mut vars = BTreeMap::new();
    for (k, v) in &args.vars {
        vars.insert(k.clone(), v.clone());
    }

    let rendered_path = pipeline.run_from_path(&args.template, &args.out, &vars, args.dry).map_err(utils::error::Error::from)?;
    println!("{}", rendered_path.display());
    Ok(())
}