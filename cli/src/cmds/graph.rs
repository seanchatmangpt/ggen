use clap::Args;
use utils::error::Result;

#[derive(Args, Debug)]
pub struct GraphArgs {
    #[arg(value_name = "SCOPE")] pub scope: String,
    #[arg(value_name = "ACTION")] pub action: String,
}

pub fn run(args: &GraphArgs) -> Result<()> {
    let spec = core::fs::read_template(&args.scope, &args.action)?;
    let fm = core::frontmatter::Frontmatter::from_spec(&spec);
    let store = core::rdf::load_graph(&fm.rdf)?;
    let nquads = core::rdf::canonical_nquads(&store)?;
    print!("{}", nquads);
    Ok(())
}
