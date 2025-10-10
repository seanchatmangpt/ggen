use clap::Args;
use ggen_utils::error::Result;
use serde::Serialize;
use std::collections::HashMap;

#[derive(Args, Debug)]
pub struct QueryArgs {
    /// SPARQL query to execute
    pub query: String,

    /// Output format (json, csv, table)
    #[arg(long, default_value = "table")]
    pub format: String,

    /// RDF graph file to query
    #[arg(long)]
    pub graph: Option<String>,
}

#[cfg_attr(test, mockall::automock)]
pub trait SparqlExecutor {
    fn execute(&self, query: String, graph: Option<String>) -> Result<QueryResults>;
}

#[derive(Debug, Clone, Serialize)]
pub struct QueryResults {
    pub bindings: Vec<HashMap<String, String>>,
    pub variables: Vec<String>,
}

pub async fn run(args: &QueryArgs) -> Result<()> {
    println!("🚧 Placeholder: graph query");
    println!("  Query: {}", args.query);
    println!("  Format: {}", args.format);
    println!("  Graph: {:?}", args.graph);
    Ok(())
}

pub async fn run_with_deps(args: &QueryArgs, executor: &dyn SparqlExecutor) -> Result<()> {
    let results = executor.execute(args.query.clone(), args.graph.clone())?;

    match args.format.as_str() {
        "json" => {
            let json = serde_json::to_string_pretty(&results.bindings)
                .map_err(ggen_utils::error::Error::from)?;
            println!("{}", json);
        }
        "csv" => {
            // Print CSV header
            println!("{}", results.variables.join(","));
            // Print rows
            for binding in &results.bindings {
                let row: Vec<String> = results
                    .variables
                    .iter()
                    .map(|var| binding.get(var).cloned().unwrap_or_default())
                    .collect();
                println!("{}", row.join(","));
            }
        }
        "table" => {
            // Print table header
            println!("{}", results.variables.join(" | "));
            println!("{}", "-".repeat(results.variables.len() * 20));
            // Print rows
            for binding in &results.bindings {
                let row: Vec<String> = results
                    .variables
                    .iter()
                    .map(|var| binding.get(var).cloned().unwrap_or_default())
                    .collect();
                println!("{}", row.join(" | "));
            }
        }
        _ => {
            return Err(ggen_utils::error::Error::new(&format!(
                "Unsupported output format: {}. Supported formats: json, csv, table",
                args.format
            )));
        }
    }

    println!("\n{} results", results.bindings.len());
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use mockall::predicate::*;

    #[tokio::test]
    async fn test_query_executes_sparql() {
        let mut mock_executor = MockSparqlExecutor::new();
        mock_executor
            .expect_execute()
            .with(
                eq("SELECT ?s ?p ?o WHERE { ?s ?p ?o }"),
                eq(Some("data.ttl")),
            )
            .times(1)
            .returning(|_, _| {
                let mut binding = HashMap::new();
                binding.insert("s".to_string(), "ex:Subject".to_string());
                binding.insert("p".to_string(), "ex:predicate".to_string());
                binding.insert("o".to_string(), "ex:Object".to_string());

                Ok(QueryResults {
                    bindings: vec![binding],
                    variables: vec!["s".to_string(), "p".to_string(), "o".to_string()],
                })
            });

        let args = QueryArgs {
            query: "SELECT ?s ?p ?o WHERE { ?s ?p ?o }".to_string(),
            format: "table".to_string(),
            graph: Some("data.ttl".to_string()),
        };

        let result = run_with_deps(&args, &mock_executor).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_query_json_format() {
        let mut mock_executor = MockSparqlExecutor::new();
        mock_executor.expect_execute().times(1).returning(|_, _| {
            Ok(QueryResults {
                bindings: vec![],
                variables: vec![],
            })
        });

        let args = QueryArgs {
            query: "SELECT * WHERE { ?s ?p ?o }".to_string(),
            format: "json".to_string(),
            graph: None,
        };

        let result = run_with_deps(&args, &mock_executor).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_query_csv_format() {
        let mut mock_executor = MockSparqlExecutor::new();
        mock_executor.expect_execute().times(1).returning(|_, _| {
            let mut binding = HashMap::new();
            binding.insert("name".to_string(), "Alice".to_string());
            binding.insert("age".to_string(), "30".to_string());

            Ok(QueryResults {
                bindings: vec![binding],
                variables: vec!["name".to_string(), "age".to_string()],
            })
        });

        let args = QueryArgs {
            query: "SELECT ?name ?age WHERE { ?person :name ?name ; :age ?age }".to_string(),
            format: "csv".to_string(),
            graph: None,
        };

        let result = run_with_deps(&args, &mock_executor).await;
        assert!(result.is_ok());
    }
}
