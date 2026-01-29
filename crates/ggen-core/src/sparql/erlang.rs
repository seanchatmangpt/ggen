//! SPARQL query utilities for Erlang domain extraction
//!
//! This module provides SPARQL query builders for extracting Erlang-specific
//! domain data from RDF ontologies, including module definitions, supervision
//! trees, application dependencies, and configuration parameters.

use ggen_utils::error::{Error, Result};

/// Build SPARQL query to extract Erlang module definitions
///
/// Queries for modules with their types (gen_server, gen_statem, supervisor, etc.),
/// behavior, and callback functions.
///
/// Expected RDF structure:
/// ```turtle
/// @prefix erlang: <http://ggen.org/erlang#> .
///
/// erlang:job_worker a erlang:GenServer ;
///     erlang:moduleName "job_worker" ;
///     erlang:behavior "gen_server" ;
///     erlang:hasCallback erlang:init, erlang:handle_call .
/// ```
///
/// # Returns
///
/// A SPARQL SELECT query string.
///
/// # Examples
///
/// ```rust
/// use ggen_core::sparql::erlang::query_modules;
///
/// let query = query_modules();
/// assert!(query.contains("SELECT"));
/// assert!(query.contains("?module"));
/// ```
pub fn query_modules() -> String {
    r#"
PREFIX erlang: <http://ggen.org/erlang#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>

SELECT ?module ?moduleName ?behavior ?moduleType
WHERE {
    ?module rdf:type ?moduleType .
    ?module erlang:moduleName ?moduleName .

    OPTIONAL {
        ?module erlang:behavior ?behavior .
    }

    FILTER (
        ?moduleType IN (
            erlang:GenServer,
            erlang:GenStatem,
            erlang:Supervisor,
            erlang:Application,
            erlang:Worker
        )
    )
}
ORDER BY ?moduleName
"#
    .to_string()
}

/// Build SPARQL query to extract supervisor tree structure
///
/// Queries for supervisor hierarchies with child worker specifications,
/// restart strategies, and supervision intensities.
///
/// Expected RDF structure:
/// ```turtle
/// @prefix erlang: <http://ggen.org/erlang#> .
///
/// erlang:job_sup a erlang:Supervisor ;
///     erlang:moduleName "job_sup" ;
///     erlang:restartStrategy "one_for_one" ;
///     erlang:hasChild erlang:worker1, erlang:worker2 .
///
/// erlang:worker1 a erlang:ChildSpec ;
///     erlang:childId "worker1" ;
///     erlang:childModule "job_worker" ;
///     erlang:childType "worker" .
/// ```
///
/// # Returns
///
/// A SPARQL CONSTRUCT query string.
///
/// # Examples
///
/// ```rust
/// use ggen_core::sparql::erlang::query_supervision_tree;
///
/// let query = query_supervision_tree();
/// assert!(query.contains("CONSTRUCT"));
/// assert!(query.contains("Supervisor"));
/// ```
pub fn query_supervision_tree() -> String {
    r#"
PREFIX erlang: <http://ggen.org/erlang#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

CONSTRUCT {
    ?supervisor erlang:moduleName ?supName .
    ?supervisor erlang:restartStrategy ?strategy .
    ?supervisor erlang:intensity ?intensity .
    ?supervisor erlang:period ?period .
    ?supervisor erlang:hasChild ?child .
    ?child erlang:childId ?childId .
    ?child erlang:childModule ?childModule .
    ?child erlang:childType ?childType .
    ?child erlang:childArgs ?childArgs .
}
WHERE {
    ?supervisor rdf:type erlang:Supervisor .
    ?supervisor erlang:moduleName ?supName .

    OPTIONAL { ?supervisor erlang:restartStrategy ?strategy . }
    OPTIONAL { ?supervisor erlang:intensity ?intensity . }
    OPTIONAL { ?supervisor erlang:period ?period . }

    OPTIONAL {
        ?supervisor erlang:hasChild ?child .
        ?child erlang:childId ?childId .
        ?child erlang:childModule ?childModule .

        OPTIONAL { ?child erlang:childType ?childType . }
        OPTIONAL { ?child erlang:childArgs ?childArgs . }
    }
}
"#
    .to_string()
}

/// Build SPARQL query to extract module dependencies
///
/// Queries for module-to-module dependencies and external application
/// dependencies (from OTP or third-party libraries).
///
/// Expected RDF structure:
/// ```turtle
/// @prefix erlang: <http://ggen.org/erlang#> .
///
/// erlang:job_worker erlang:dependsOn erlang:job_db .
/// erlang:job_processor erlang:requiresApp "kernel", "stdlib", "cowboy" .
/// ```
///
/// # Returns
///
/// A SPARQL SELECT query string.
///
/// # Examples
///
/// ```rust
/// use ggen_core::sparql::erlang::query_dependencies;
///
/// let query = query_dependencies();
/// assert!(query.contains("dependsOn"));
/// ```
pub fn query_dependencies() -> String {
    r#"
PREFIX erlang: <http://ggen.org/erlang#>

SELECT ?module ?moduleName ?dependency ?depModule ?application
WHERE {
    ?module erlang:moduleName ?moduleName .

    OPTIONAL {
        ?module erlang:dependsOn ?dependency .
        ?dependency erlang:moduleName ?depModule .
    }

    OPTIONAL {
        ?module erlang:requiresApp ?application .
    }
}
ORDER BY ?moduleName
"#
    .to_string()
}

/// Build SPARQL query to extract configuration parameters
///
/// Queries for application configuration parameters with their types,
/// default values, and validation rules.
///
/// Expected RDF structure:
/// ```turtle
/// @prefix erlang: <http://ggen.org/erlang#> .
/// @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
///
/// erlang:job_processor erlang:hasConfig erlang:config_pool_size .
///
/// erlang:config_pool_size a erlang:ConfigParam ;
///     erlang:paramName "pool_size" ;
///     erlang:paramType "integer()" ;
///     erlang:defaultValue "10"^^xsd:integer ;
///     erlang:required true .
/// ```
///
/// # Returns
///
/// A SPARQL SELECT query string.
///
/// # Examples
///
/// ```rust
/// use ggen_core::sparql::erlang::query_config_params;
///
/// let query = query_config_params();
/// assert!(query.contains("ConfigParam"));
/// ```
pub fn query_config_params() -> String {
    r#"
PREFIX erlang: <http://ggen.org/erlang#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?app ?appName ?param ?paramName ?paramType ?defaultValue ?required ?description
WHERE {
    ?app erlang:hasConfig ?param .
    ?app erlang:moduleName ?appName .

    ?param a erlang:ConfigParam .
    ?param erlang:paramName ?paramName .
    ?param erlang:paramType ?paramType .

    OPTIONAL { ?param erlang:defaultValue ?defaultValue . }
    OPTIONAL { ?param erlang:required ?required . }
    OPTIONAL { ?param erlang:description ?description . }
}
ORDER BY ?appName ?paramName
"#
    .to_string()
}

/// Build SPARQL query to extract gen_server state and callbacks
///
/// Queries for gen_server state record definitions and callback
/// function implementations.
///
/// Expected RDF structure:
/// ```turtle
/// @prefix erlang: <http://ggen.org/erlang#> .
///
/// erlang:job_worker a erlang:GenServer ;
///     erlang:stateRecord erlang:worker_state .
///
/// erlang:worker_state a erlang:Record ;
///     erlang:recordName "worker_state" ;
///     erlang:hasField erlang:field_jobs, erlang:field_pool .
///
/// erlang:field_jobs erlang:fieldName "jobs" ;
///     erlang:fieldType "queue:queue()" .
/// ```
///
/// # Returns
///
/// A SPARQL CONSTRUCT query string.
///
/// # Examples
///
/// ```rust
/// use ggen_core::sparql::erlang::query_gen_server_state;
///
/// let query = query_gen_server_state();
/// assert!(query.contains("GenServer"));
/// assert!(query.contains("stateRecord"));
/// ```
pub fn query_gen_server_state() -> String {
    r#"
PREFIX erlang: <http://ggen.org/erlang#>
PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>

CONSTRUCT {
    ?server erlang:moduleName ?serverName .
    ?server erlang:stateRecord ?record .
    ?record erlang:recordName ?recordName .
    ?record erlang:hasField ?field .
    ?field erlang:fieldName ?fieldName .
    ?field erlang:fieldType ?fieldType .
}
WHERE {
    ?server rdf:type erlang:GenServer .
    ?server erlang:moduleName ?serverName .

    OPTIONAL {
        ?server erlang:stateRecord ?record .
        ?record erlang:recordName ?recordName .

        OPTIONAL {
            ?record erlang:hasField ?field .
            ?field erlang:fieldName ?fieldName .
            ?field erlang:fieldType ?fieldType .
        }
    }
}
"#
    .to_string()
}

/// Validate SPARQL query syntax for Erlang domain
///
/// Performs basic validation of SPARQL query syntax to ensure it's
/// well-formed before execution.
///
/// # Arguments
///
/// * `query` - The SPARQL query string to validate
///
/// # Returns
///
/// `Ok(())` if the query is syntactically valid.
///
/// # Errors
///
/// Returns an error if the query is malformed or contains invalid syntax.
///
/// # Examples
///
/// ```rust
/// use ggen_core::sparql::erlang::{query_modules, validate_query};
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let query = query_modules();
/// validate_query(&query)?;
/// # Ok(())
/// # }
/// ```
pub fn validate_query(query: &str) -> Result<()> {
    if query.trim().is_empty() {
        return Err(Error::new("SPARQL query cannot be empty"));
    }

    // Check for required SPARQL keywords
    let query_upper = query.to_uppercase();
    let has_select = query_upper.contains("SELECT");
    let has_construct = query_upper.contains("CONSTRUCT");
    let has_ask = query_upper.contains("ASK");
    let has_describe = query_upper.contains("DESCRIBE");

    if !has_select && !has_construct && !has_ask && !has_describe {
        return Err(Error::new(
            "SPARQL query must contain SELECT, CONSTRUCT, ASK, or DESCRIBE",
        ));
    }

    // Check for WHERE clause (required for SELECT, optional for CONSTRUCT)
    if (has_select || has_construct) && !query_upper.contains("WHERE") {
        return Err(Error::new(
            "SPARQL SELECT and CONSTRUCT queries must have a WHERE clause",
        ));
    }

    // Basic brace matching
    let open_braces = query.matches('{').count();
    let close_braces = query.matches('}').count();
    if open_braces != close_braces {
        return Err(Error::new(&format!(
            "Unmatched braces in SPARQL query: {} open, {} close",
            open_braces, close_braces
        )));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_query_modules_contains_select() {
        // Arrange & Act
        let query = query_modules();

        // Assert
        assert!(query.contains("SELECT"));
        assert!(query.contains("?module"));
        assert!(query.contains("?moduleName"));
        assert!(query.contains("erlang:GenServer"));
    }

    #[test]
    fn test_query_supervision_tree_contains_construct() {
        // Arrange & Act
        let query = query_supervision_tree();

        // Assert
        assert!(query.contains("CONSTRUCT"));
        assert!(query.contains("erlang:Supervisor"));
        assert!(query.contains("erlang:hasChild"));
    }

    #[test]
    fn test_query_dependencies_valid() {
        // Arrange & Act
        let query = query_dependencies();

        // Assert
        assert!(query.contains("SELECT"));
        assert!(query.contains("erlang:dependsOn"));
        assert!(query.contains("erlang:requiresApp"));
    }

    #[test]
    fn test_query_config_params_valid() {
        // Arrange & Act
        let query = query_config_params();

        // Assert
        assert!(query.contains("SELECT"));
        assert!(query.contains("erlang:ConfigParam"));
        assert!(query.contains("?paramName"));
    }

    #[test]
    fn test_query_gen_server_state_valid() {
        // Arrange & Act
        let query = query_gen_server_state();

        // Assert
        assert!(query.contains("CONSTRUCT"));
        assert!(query.contains("erlang:GenServer"));
        assert!(query.contains("erlang:stateRecord"));
    }

    #[test]
    fn test_validate_query_valid_select() {
        // Arrange
        let query = query_modules();

        // Act
        let result = validate_query(&query);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_query_empty() {
        // Arrange
        let query = "";

        // Act
        let result = validate_query(query);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_validate_query_no_keyword() {
        // Arrange
        let query = "PREFIX erlang: <http://example.org/> WHERE { ?s ?p ?o }";

        // Act
        let result = validate_query(query);

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("SELECT, CONSTRUCT"));
    }

    #[test]
    fn test_validate_query_unmatched_braces() {
        // Arrange
        let query = "SELECT ?s WHERE { ?s ?p ?o";

        // Act
        let result = validate_query(query);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unmatched braces"));
    }
}
