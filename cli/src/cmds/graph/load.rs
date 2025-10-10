use clap::Args;
use ggen_utils::error::Result;

#[derive(Args, Debug)]
pub struct LoadArgs {
    /// RDF file to load
    pub file: String,

    /// RDF format (turtle, ntriples, rdfxml)
    #[arg(long)]
    pub format: Option<String>,

    /// Base IRI for relative URIs
    #[arg(long)]
    pub base: Option<String>,

    /// Merge with existing graph
    #[arg(long)]
    pub merge: bool,
}

#[cfg_attr(test, mockall::automock)]
pub trait RdfLoader {
    fn load(
        &self,
        file: String,
        format: Option<String>,
        base: Option<String>,
        merge: bool,
    ) -> Result<LoadStats>;
}

#[derive(Debug, Clone)]
pub struct LoadStats {
    pub triples_loaded: usize,
    pub total_triples: usize,
    pub format_detected: String,
}

pub async fn run(args: &LoadArgs) -> Result<()> {
    println!("🚧 Placeholder: graph load");
    println!("  File: {}", args.file);
    println!("  Format: {:?}", args.format);
    println!("  Base: {:?}", args.base);
    println!("  Merge: {}", args.merge);
    Ok(())
}

pub async fn run_with_deps(args: &LoadArgs, loader: &dyn RdfLoader) -> Result<()> {
    let stats = loader.load(
        args.file.clone(),
        args.format.clone(),
        args.base.clone(),
        args.merge,
    )?;

    if args.merge {
        println!(
            "✅ Loaded {} triples from {} ({})",
            stats.triples_loaded, args.file, stats.format_detected
        );
        println!("Total triples in graph: {}", stats.total_triples);
    } else {
        println!(
            "✅ Loaded {} triples from {} ({})",
            stats.triples_loaded, args.file, stats.format_detected
        );
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_load_rdf_file() {
        let mut mock_loader = MockRdfLoader::new();
        mock_loader
            .expect_load()
            .with(
                eq("data.ttl"),
                eq(Some("turtle")),
                eq(None::<&str>),
                eq(false),
            )
            .times(1)
            .returning(|_, _, _, _| {
                Ok(LoadStats {
                    triples_loaded: 100,
                    total_triples: 100,
                    format_detected: "Turtle".to_string(),
                })
            });

        let args = LoadArgs {
            file: "data.ttl".to_string(),
            format: Some("turtle".to_string()),
            base: None,
            merge: false,
        };

        let result = run_with_deps(&args, &mock_loader).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_load_with_merge() {
        let mut mock_loader = MockRdfLoader::new();
        mock_loader
            .expect_load()
            .with(eq("additional.ttl"), always(), always(), eq(true))
            .times(1)
            .returning(|_, _, _, _| {
                Ok(LoadStats {
                    triples_loaded: 50,
                    total_triples: 150,
                    format_detected: "Turtle".to_string(),
                })
            });

        let args = LoadArgs {
            file: "additional.ttl".to_string(),
            format: None,
            base: None,
            merge: true,
        };

        let result = run_with_deps(&args, &mock_loader).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_load_with_base_iri() {
        let mut mock_loader = MockRdfLoader::new();
        mock_loader
            .expect_load()
            .with(
                eq("relative.ttl"),
                always(),
                eq(Some("http://example.org/")),
                eq(false),
            )
            .times(1)
            .returning(|_, _, _, _| {
                Ok(LoadStats {
                    triples_loaded: 25,
                    total_triples: 25,
                    format_detected: "Turtle".to_string(),
                })
            });

        let args = LoadArgs {
            file: "relative.ttl".to_string(),
            format: None,
            base: Some("http://example.org/".to_string()),
            merge: false,
        };

        let result = run_with_deps(&args, &mock_loader).await;
        assert!(result.is_ok());
    }
}
