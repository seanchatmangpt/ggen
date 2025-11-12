use clap::Parser;
use reasoner_cli::{ClassifierCmd, OntologyCmd, InferenceCmd, ValidatorCmd, Result};

#[derive(Parser)]
#[command(name = "reasoner")]
#[command(about = "OWL inference engine and ontology classification", long_about = None)]
struct Cli {
    #[command(subcommand)]
    command: Commands,
}

#[derive(Parser)]
enum Commands {
    /// Taxonomy building and instance classification
    #[command(name = "classifier")]
    Classifier(ClassifierCmd),

    /// OWL ontology management and operations
    #[command(name = "ontology")]
    Ontology(OntologyCmd),

    /// Logical inference and entailment
    #[command(name = "inference")]
    Inference(InferenceCmd),

    /// Ontology validation and consistency checking
    #[command(name = "validator")]
    Validator(ValidatorCmd),
}

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    let cli = Cli::parse();

    match cli.command {
        Commands::Classifier(cmd) => cmd.execute().await,
        Commands::Ontology(cmd) => cmd.execute().await,
        Commands::Inference(cmd) => cmd.execute().await,
        Commands::Validator(cmd) => cmd.execute().await,
    }
}
