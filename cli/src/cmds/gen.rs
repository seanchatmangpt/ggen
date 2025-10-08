use anyhow::Result;
use std::collections::BTreeMap;
use std::path::PathBuf;

use core::generator::{GenContext, Generator};
use core::pipeline::PipelineBuilder;
use utils::project_config::RgenConfig;

#[derive(clap::Args, Debug)]
pub struct GenArgs {
    /// Path to the template file
    #[arg(short, long)]
    pub template: PathBuf,
    
    /// Output directory root
    #[arg(short, long, default_value = ".")]
    pub out: PathBuf,

    /// Variables (key=value pairs)
    #[arg(short = 'v', long = "var", value_parser = parse_kv)]
    pub vars: Vec<(String, String)>,

    /// Dry run (no write)
    #[arg(long)]
    pub dry: bool,
}

fn parse_kv(s: &str) -> std::result::Result<(String,String), String> {
    let pos = s.find('=').ok_or_else(|| format!("invalid KEY=value: `{s}`"))?;
    Ok((s[..pos].to_string(), s[pos+1..].to_string()))
}

pub fn run(args: &GenArgs) -> Result<()> {
    run_with_config(args, None)
}

pub fn run_with_config(args: &GenArgs, rgen_config: Option<RgenConfig>) -> Result<()> {
    let vars: BTreeMap<_,_> = args.vars.iter().cloned().collect();

    // Merge variables: CLI vars override rgen.toml vars
    let merged_vars = if let Some(config) = &rgen_config {
        let mut merged = config.vars.clone();
        for (k, v) in vars {
            merged.insert(k, v);
        }
        merged
    } else {
        vars
    };

    // Determine output directory: CLI overrides rgen.toml default
    let out_dir = if let Some(config) = &rgen_config {
        if args.out == PathBuf::from(".") {
            // Use rgen.toml output_dir if CLI didn't specify
            config.project.output_dir.clone()
        } else {
            args.out.clone()
        }
    } else {
        args.out.clone()
    };

    // Build pipeline with rgen.toml config
    let mut builder = PipelineBuilder::new();

    if let Some(config) = &rgen_config {
        // Add global prefixes
        if !config.prefixes.is_empty() {
            builder = builder.with_prefixes(config.prefixes.clone(), None);
        }

        // Add RDF files
        if !config.rdf.files.is_empty() {
            let file_paths: Vec<String> = config.rdf.files.iter().map(|p| p.to_string_lossy().to_string()).collect();
            builder = builder.with_rdf_files(file_paths);
        }

        // Add inline RDF
        if !config.rdf.inline.is_empty() {
            builder = builder.with_inline_rdf(&config.rdf.inline);
        }
    }

    let pipeline = builder.build()?;

    let ctx = GenContext::new(args.template.clone(), out_dir)
        .with_vars(merged_vars)
        .dry(args.dry);

    let mut gen = Generator::new(pipeline, ctx);
    let path = gen.generate()?;
    println!("{}", path.display());
    Ok(())
}