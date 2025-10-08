use utils::error::Result;

#[derive(clap::Args, Debug)]
pub struct ShowArgs {
    #[arg(value_name = "SCOPE")]
    pub scope: String,
    #[arg(value_name = "ACTION")]
    pub action: String,
}

pub fn run(args: &ShowArgs) -> Result<()> {
    let spec = core::fs::read_template(&args.scope, &args.action)?;
    let fm = core::frontmatter::Frontmatter::from_spec(&spec);
    println!("to: {}", spec.output_path.display());
    println!("vars: {:?}", fm.defaults());
    Ok(())
}
