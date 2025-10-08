use utils::error::Result;

#[derive(clap::Args, Debug)]
pub struct ValidateArgs {
    #[arg(value_name = "SCOPE")]
    pub scope: String,
    #[arg(value_name = "ACTION")]
    pub action: String,
}

pub fn run(args: &ValidateArgs) -> Result<()> {
    let spec = core::fs::read_template(&args.scope, &args.action)?;
    let fm = core::frontmatter::Frontmatter::from_spec(&spec);
    let data = core::rdf::load_graph(&fm.rdf_sources())?;
    let shapes = core::rdf::load_graph(&fm.shape_sources())?;
    core::shacl::validate(&data, &shapes)?;
    println!("OK");
    Ok(())
}
