use clap::{Args, Subcommand};
use ggen_utils::error::Result;

// Declare verb modules
pub mod apply;
pub mod diff;
pub mod freeze;
pub mod gen;
pub mod inject;
pub mod plan;
pub mod test;
pub mod validate;
pub mod watch;

#[derive(Args, Debug)]
pub struct ProjectCmd {
    #[command(subcommand)]
    pub verb: Verb,
}

#[derive(Subcommand, Debug)]
pub enum Verb {
    /// Generate artifacts from a template directly
    ///
    /// Examples:
    ///   ggen project gen "rust-cli-template" --var name=myapp
    ///   ggen project gen "web-api.tmpl" --dry-run
    ///   ggen project gen "template.tmpl" --seed 12345
    Gen(gen::GenArgs),

    /// Create a machine-readable plan of changes without applying them (dry-run)
    ///
    /// Examples:
    ///   ggen project plan "rust-cli-template" --var name=myapp
    ///   ggen project plan "template.tmpl" --format yaml
    Plan(plan::PlanArgs),

    /// Apply a previously generated plan to the filesystem
    ///
    /// Examples:
    ///   ggen project apply plan.json
    ///   ggen project apply plan.yaml --dry-run
    Apply(apply::ApplyArgs),

    /// Show a unified diff of what a generation would change
    ///
    /// Examples:
    ///   ggen project diff "rust-cli-template" --var name=myapp
    ///   ggen project diff "template.tmpl" --var version=1.0.0
    Diff(diff::DiffArgs),

    /// Run golden file snapshot tests for templates
    ///
    /// Examples:
    ///   ggen project test "template.tmpl" --golden expected/
    ///   ggen project test "rust-cli.tmpl" --golden golden/ --update
    ///   ggen project test "*.tmpl" --golden snapshots/ --json
    Test(test::TestArgs),

    /// Add freeze blocks to generated files for immutability
    ///
    /// Examples:
    ///   ggen project freeze "src/main.rs" --blocks "impl,business_logic"
    ///   ggen project freeze "src/" --blocks "custom" --recursive
    ///   ggen project freeze "app.ts" --blocks "config,routes" --dry-run
    Freeze(freeze::FreezeArgs),

    /// Inject code idempotently into existing files
    ///
    /// Examples:
    ///   ggen project inject "src/mod.rs" --anchor "// IMPORTS" --content "use foo;"
    ///   ggen project inject "main.rs" --anchor "fn main()" --content "println!(\"hello\");" --after
    ///   ggen project inject "config.toml" --anchor "[dependencies]" --content "serde = \"1.0\"" --dry-run
    Inject(inject::InjectArgs),

    /// Validate plan files or generated output
    ///
    /// Examples:
    ///   ggen project validate plan.json
    ///   ggen project validate generated/ --recursive
    ///   ggen project validate output.rs --schema rust-file.schema.json
    ///   ggen project validate plan.yaml --verbose --json
    Validate(validate::ValidateArgs),

    /// Watch templates and continuously regenerate on changes
    ///
    /// Examples:
    ///   ggen project watch "*.tmpl" --target src/
    ///   ggen project watch "templates/" --target output/ --recursive
    ///   ggen project watch "api.tmpl" --target api/ --debounce 1000 --var env=dev
    Watch(watch::WatchArgs),
}

impl ProjectCmd {
    pub async fn run(&self) -> Result<()> {
        match &self.verb {
            Verb::Gen(args) => gen::run(args).await,
            Verb::Plan(args) => plan::run(args).await,
            Verb::Apply(args) => apply::run(args).await,
            Verb::Diff(args) => diff::run(args).await,
            Verb::Test(args) => test::run(args).await,
            Verb::Freeze(args) => freeze::run(args).await,
            Verb::Inject(args) => inject::run(args).await,
            Verb::Validate(args) => validate::run(args).await,
            Verb::Watch(args) => watch::run(args).await,
        }
    }
}
