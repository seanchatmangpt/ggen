///! Graph tools - delegates to ggen CLI commands

use serde_json::Value;
use crate::cli_helper::call_ggen_cli;
use crate::error::{Result, get_string_param, get_optional_string_param, success_response};

/// Execute SPARQL query - delegates to `ggen graph query`
pub async fn query(params: Value) -> Result<Value> {
    let sparql = get_string_param(&params, "sparql")?;
    let graph_name = get_optional_string_param(&params, "graph");

    tracing::info!("Delegating to ggen graph query");

    let mut args = vec!["graph", "query", &sparql];
    let graph_str;

    if let Some(g) = graph_name {
        args.push("--graph");
        graph_str = g;
        args.push(&graph_str);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Load RDF data from file - delegates to `ggen graph load`
pub async fn load(params: Value) -> Result<Value> {
    let file = get_string_param(&params, "file")?;
    let graph = get_optional_string_param(&params, "graph");
    let format = get_optional_string_param(&params, "format");

    tracing::info!("Delegating to ggen graph load: {}", file);

    let mut args = vec!["graph", "load", &file];
    let graph_str;
    let format_str;

    if let Some(g) = graph {
        args.push("--graph");
        graph_str = g;
        args.push(&graph_str);
    }
    if let Some(f) = format {
        args.push("--format");
        format_str = f;
        args.push(&format_str);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

/// Export graph to file - delegates to `ggen graph export`
pub async fn export(params: Value) -> Result<Value> {
    let output = get_string_param(&params, "output")?;
    let graph = get_optional_string_param(&params, "graph");
    let format = get_optional_string_param(&params, "format");

    tracing::info!("Delegating to ggen graph export to: {}", output);

    let mut args = vec!["graph", "export", &output];
    let graph_str;
    let format_str;

    if let Some(g) = graph {
        args.push("--graph");
        graph_str = g;
        args.push(&graph_str);
    }
    if let Some(f) = format {
        args.push("--format");
        format_str = f;
        args.push(&format_str);
    }

    let result = call_ggen_cli(&args).await?;
    Ok(success_response(result))
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[tokio::test]
    async fn test_query_requires_sparql() {
        let params = json!({});
        let result = query(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_load_requires_file() {
        let params = json!({});
        let result = load(params).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_export_requires_output() {
        let params = json!({});
        let result = export(params).await;
        assert!(result.is_err());
    }
}
