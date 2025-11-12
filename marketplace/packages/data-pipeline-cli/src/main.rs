use anyhow::Result;
use clap_noun_verb::ClapNounVerb;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing if enabled
    #[cfg(feature = "tracing")]
    {
        tracing_subscriber::fmt()
            .with_env_filter(
                tracing_subscriber::EnvFilter::from_default_env()
                    .add_directive(tracing::Level::INFO.into()),
            )
            .init();
    }

    // Parse command-line arguments using clap-noun-verb
    let app = ClapNounVerb::new()
        .name("data-pipeline")
        .version(env!("CARGO_PKG_VERSION"))
        .about("ETL data pipelines, transformation, and integration")
        .author("ggen Marketplace")
        .ontology_path("rdf/ontology.ttl");

    // Execute the command
    app.execute().await?;

    Ok(())
}
