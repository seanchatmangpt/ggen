//! Demonstration of SPARQL JSON conversion functionality
//! This shows how to convert between SPARQL queries and JSON representations

use serde::{Deserialize, Serialize};
use serde_json::{json, Value};

/// JSON representation of a SPARQL query for AI generation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SparqlQueryJson {
    /// Query type (SELECT, ASK, CONSTRUCT, DESCRIBE)
    pub query_type: String,
    /// Variables to select (for SELECT queries)
    pub variables: Vec<String>,
    /// WHERE clause patterns
    pub where_clause: Vec<TriplePattern>,
    /// Optional ORDER BY clause
    pub order_by: Option<Vec<OrderByClause>>,
    /// Optional LIMIT clause
    pub limit: Option<u32>,
    /// Optional OFFSET clause
    pub offset: Option<u32>,
    /// FILTER conditions
    pub filters: Vec<String>,
}

/// Represents a triple pattern in the WHERE clause
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TriplePattern {
    /// Subject (can be variable like ?s or URI)
    pub subject: String,
    /// Predicate (can be variable like ?p or URI)
    pub predicate: String,
    /// Object (can be variable like ?o, literal, or URI)
    pub object: String,
}

/// ORDER BY clause specification
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrderByClause {
    /// Variable to order by
    pub variable: String,
    /// Direction (ASC or DESC)
    pub direction: String,
}

/// SPARQL JSON converter
pub struct SparqlJsonConverter;

impl SparqlJsonConverter {
    /// Convert JSON SPARQL representation back to SPARQL query string
    pub fn json_to_sparql(json_query: &SparqlQueryJson) -> String {
        let mut query = String::new();

        // Add query type and variables for SELECT queries
        match json_query.query_type.to_uppercase().as_str() {
            "SELECT" => {
                if json_query.variables.is_empty() {
                    query.push_str("SELECT *");
                } else {
                    query.push_str(&format!("SELECT {}", json_query.variables.join(" ")));
                }
            }
            "ASK" => query.push_str("ASK"),
            "CONSTRUCT" => query.push_str("CONSTRUCT"),
            "DESCRIBE" => query.push_str("DESCRIBE"),
            _ => return format!("ERROR: Unknown query type: {}", json_query.query_type),
        }

        query.push_str("\nWHERE {\n");

        // Add WHERE clause patterns
        for pattern in &json_query.where_clause {
            query.push_str(&format!("    {} {} {} .\n", pattern.subject, pattern.predicate, pattern.object));
        }

        query.push_str("}\n");

        // Add filters
        for filter in &json_query.filters {
            query.push_str(&format!("FILTER({})\n", filter));
        }

        // Add ORDER BY clause
        if let Some(order_clauses) = &json_query.order_by {
            query.push_str("ORDER BY ");
            let order_parts: Vec<String> = order_clauses
                .iter()
                .map(|clause| format!("{}({})", clause.direction.to_uppercase(), clause.variable))
                .collect();
            query.push_str(&order_parts.join(" "));
            query.push('\n');
        }

        // Add LIMIT and OFFSET
        if let Some(limit) = json_query.limit {
            query.push_str(&format!("LIMIT {}\n", limit));
        }

        if let Some(offset) = json_query.offset {
            query.push_str(&format!("OFFSET {}\n", offset));
        }

        query
    }

    /// Parse JSON string to SparqlQueryJson and convert to SPARQL
    pub fn json_string_to_sparql(json_str: &str) -> Result<String, Box<dyn std::error::Error>> {
        let json_query: SparqlQueryJson = serde_json::from_str(json_str)?;
        Ok(Self::json_to_sparql(&json_query))
    }

    /// Create example SPARQL JSON structures for testing
    pub fn create_example_queries() -> Vec<(String, SparqlQueryJson)> {
        vec![
            (
                "Simple SELECT query".to_string(),
                SparqlQueryJson {
                    query_type: "SELECT".to_string(),
                    variables: vec!["?s".to_string(), "?p".to_string(), "?o".to_string()],
                    where_clause: vec![
                        TriplePattern {
                            subject: "?s".to_string(),
                            predicate: "?p".to_string(),
                            object: "?o".to_string(),
                        }
                    ],
                    order_by: None,
                    limit: None,
                    offset: None,
                    filters: vec![],
                }
            ),
            (
                "Complex query with filters".to_string(),
                SparqlQueryJson {
                    query_type: "SELECT".to_string(),
                    variables: vec!["?person".to_string(), "?name".to_string(), "?age".to_string()],
                    where_clause: vec![
                        TriplePattern {
                            subject: "?person".to_string(),
                            predicate: "http://xmlns.com/foaf/0.1/name".to_string(),
                            object: "?name".to_string(),
                        },
                        TriplePattern {
                            subject: "?person".to_string(),
                            predicate: "http://example.org/age".to_string(),
                            object: "?age".to_string(),
                        }
                    ],
                    order_by: Some(vec![
                        OrderByClause {
                            variable: "?name".to_string(),
                            direction: "ASC".to_string(),
                        }
                    ]),
                    limit: Some(10),
                    offset: None,
                    filters: vec!["FILTER(?age > 18)".to_string()],
                }
            ),
            (
                "ASK query".to_string(),
                SparqlQueryJson {
                    query_type: "ASK".to_string(),
                    variables: vec![],
                    where_clause: vec![
                        TriplePattern {
                            subject: "?s".to_string(),
                            predicate: "?p".to_string(),
                            object: "?o".to_string(),
                        }
                    ],
                    order_by: None,
                    limit: None,
                    offset: None,
                    filters: vec![],
                }
            ),
        ]
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔍 SPARQL JSON Conversion Demonstration");
    println!("=====================================");

    let examples = SparqlJsonConverter::create_example_queries();

    for (description, json_query) in examples {
        println!("\n📋 {}", description);
        println!("JSON: {}", serde_json::to_string_pretty(&json_query)?);

        let sparql = SparqlJsonConverter::json_to_sparql(&json_query);
        println!("SPARQL:");
        println!("{}", sparql);

        // Test round-trip conversion
        let json_str = serde_json::to_string(&json_query)?;
        let sparql_from_json = SparqlJsonConverter::json_string_to_sparql(&json_str)?;
        println!("✅ Round-trip conversion successful: {}", sparql == sparql_from_json);
    }

    // Demonstrate the conversion process described in the user query
    println!("\n🚀 Demonstrating 'generate the sparql as json then convert' process:");
    println!("==================================================================");

    // Step 1: Generate SPARQL as JSON (simulated)
    let sample_query = SparqlQueryJson {
        query_type: "SELECT".to_string(),
        variables: vec!["?person".to_string(), "?name".to_string()],
        where_clause: vec![
            TriplePattern {
                subject: "?person".to_string(),
                predicate: "http://xmlns.com/foaf/0.1/name".to_string(),
                object: "?name".to_string(),
            }
        ],
        order_by: None,
        limit: None,
        offset: None,
        filters: vec![],
    };

    println!("1. Generated SPARQL JSON:");
    let json_output = serde_json::to_string_pretty(&sample_query)?;
    println!("{}", json_output);

    // Step 2: Convert JSON back to SPARQL
    println!("\n2. Converted back to SPARQL:");
    let final_sparql = SparqlJsonConverter::json_to_sparql(&sample_query);
    println!("{}", final_sparql);

    println!("\n✅ SPARQL JSON conversion demonstration complete!");
    println!("\n💡 Usage in ggen:");
    println!("   ggen ai sparql -d \"Find all people\" -f json     # Generate as JSON");
    println!("   ggen ai sparql -d \"Find all people\" -f sparql   # Generate as SPARQL");
    println!("   ggen ai sparql -d \"Find all people\" -f convert-json-to-sparql  # Convert JSON to SPARQL");

    Ok(())
}
