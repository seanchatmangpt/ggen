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

/// Validate and sanitize SPARQL query input
fn validate_sparql_query(query: &str) -> Result<()> {
    // Validate query is not empty
    if query.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "SPARQL query cannot be empty",
        ));
    }

    // Validate query length
    if query.len() > 10000 {
        return Err(ggen_utils::error::Error::new(
            "SPARQL query too long (max 10000 characters)",
        ));
    }

    // Basic SPARQL syntax validation
    let query_upper = query.to_uppercase();
    if !query_upper.contains("SELECT")
        && !query_upper.contains("ASK")
        && !query_upper.contains("CONSTRUCT")
        && !query_upper.contains("DESCRIBE")
    {
        return Err(ggen_utils::error::Error::new(
            "Invalid SPARQL query: must contain SELECT, ASK, CONSTRUCT, or DESCRIBE",
        ));
    }

    // Check for potentially dangerous patterns
    if query.contains("DROP")
        || query.contains("DELETE")
        || query.contains("INSERT")
        || query.contains("CREATE")
    {
        return Err(ggen_utils::error::Error::new(
            "Write operations not allowed: only SELECT, ASK, CONSTRUCT, and DESCRIBE queries are permitted",
        ));
    }

    Ok(())
}

/// Validate and sanitize output format input
fn validate_output_format(format: &str) -> Result<()> {
    // Validate format is not empty
    if format.trim().is_empty() {
        return Err(ggen_utils::error::Error::new(
            "Output format cannot be empty",
        ));
    }

    // Validate format length
    if format.len() > 20 {
        return Err(ggen_utils::error::Error::new(
            "Output format too long (max 20 characters)",
        ));
    }

    // Validate against known formats
    let valid_formats = ["json", "csv", "table"];
    if !valid_formats.contains(&format.to_lowercase().as_str()) {
        return Err(ggen_utils::error::Error::new(
            "Unsupported output format: supported formats are json, csv, table",
        ));
    }

    Ok(())
}

/// Validate and sanitize graph file path input (if provided)
fn validate_graph_path(graph: &Option<String>) -> Result<()> {
    if let Some(graph) = graph {
        // Validate graph path is not empty
        if graph.trim().is_empty() {
            return Err(ggen_utils::error::Error::new(
                "Graph file path cannot be empty",
            ));
        }

        // Validate graph path length
        if graph.len() > 1000 {
            return Err(ggen_utils::error::Error::new(
                "Graph file path too long (max 1000 characters)",
            ));
        }

        // Basic path traversal protection
        if graph.contains("..") {
            return Err(ggen_utils::error::Error::new(
                "Path traversal detected: graph file path cannot contain '..'",
            ));
        }

        // Validate graph path format (basic pattern check)
        if !graph.chars().all(|c| {
            c.is_alphanumeric() || c == '.' || c == '/' || c == '-' || c == '_' || c == '\\'
        }) {
            return Err(ggen_utils::error::Error::new(
                "Invalid graph file path format: only alphanumeric characters, dots, slashes, dashes, underscores, and backslashes allowed",
            ));
        }
    }

    Ok(())
}

pub async fn run(args: &QueryArgs) -> Result<()> {
    // Validate inputs
    validate_sparql_query(&args.query)?;
    validate_output_format(&args.format)?;
    validate_graph_path(&args.graph)?;

    println!("🚧 Placeholder: graph query");
    println!("  Query: {}", args.query.trim());
    println!("  Format: {}", args.format.trim());
    if let Some(graph) = &args.graph {
        println!("  Graph: {}", graph.trim());
    }
    Ok(())
}

pub async fn run_with_deps(args: &QueryArgs, executor: &dyn SparqlExecutor) -> Result<()> {
    // Validate inputs
    validate_sparql_query(&args.query)?;
    validate_output_format(&args.format)?;
    validate_graph_path(&args.graph)?;

    // Show progress for query execution
    println!("🔍 Executing SPARQL query...");

    let results = executor.execute(args.query.clone(), args.graph.clone())?;

    // Show progress for large result sets
    if results.bindings.len() > 100 {
        println!("📊 Processing {} results...", results.bindings.len());
    }

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

    println!("\n📊 {} results", results.bindings.len());
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
                eq(String::from("SELECT ?s ?p ?o WHERE { ?s ?p ?o }")),
                eq(Some(String::from("data.ttl"))),
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
