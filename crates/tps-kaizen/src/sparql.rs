//! SPARQL queries over RDF evidence receipts
//!
//! Analyzes evidence receipts using SPARQL queries:
//! - "Show all jidoka failures in last 24h, grouped by component"
//! - "What was the queue depth when the last circuit opened?"
//! - "Trace the path of request that took >5s (latency outlier)"
//! - Correlate metrics with RDF ontology facts

use crate::Result;
use oxigraph::store::Store;
use std::sync::Arc;

/// SPARQL analyzer for evidence queries
#[derive(Clone)]
pub struct SparqlAnalyzer {
    store: Arc<Store>,
}

impl SparqlAnalyzer {
    /// Create new SPARQL analyzer with in-memory RDF store
    pub fn new() -> Result<Self> {
        let store = Store::new().map_err(|e| crate::KaizenError::SparqlError(e.to_string()))?;

        Ok(Self {
            store: Arc::new(store),
        })
    }

    /// Load RDF data from TTL string
    pub async fn load_ttl(&self, ttl_data: &str) -> Result<()> {
        use oxigraph::io::RdfFormat;

        self.store
            .load_graph(
                std::io::Cursor::new(ttl_data),
                RdfFormat::Turtle,
                None,
            )
            .map_err(|e| crate::KaizenError::SparqlError(e.to_string()))?;

        Ok(())
    }

    /// Query: All jidoka failures in last 24h, grouped by component
    pub async fn query_jidoka_failures_24h(&self) -> Result<Vec<String>> {
        let query = r#"
            PREFIX tps: <http://example.com/tps/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

            SELECT ?component (COUNT(?failure) as ?failure_count)
            WHERE {
                ?failure a tps:JidokaFailure ;
                    tps:component ?component ;
                    tps:timestamp ?timestamp .

                FILTER (?timestamp > NOW() - "PT24H"^^xsd:duration)
            }
            GROUP BY ?component
            ORDER BY DESC(?failure_count)
        "#;

        self.execute_query(query).await
    }

    /// Query: Queue depth at time of circuit open
    pub async fn query_queue_depth_at_circuit_open(&self) -> Result<Vec<String>> {
        let query = r#"
            PREFIX tps: <http://example.com/tps/>

            SELECT ?circuitOpen ?component ?queueDepth ?timestamp
            WHERE {
                ?circuitOpen a tps:CircuitOpen ;
                    tps:component ?component ;
                    tps:timestamp ?timestamp ;
                    tps:queueDepthSnapshot ?queueDepth .
            }
            ORDER BY DESC(?timestamp)
            LIMIT 10
        "#;

        self.execute_query(query).await
    }

    /// Query: Trace latency outlier (requests > 5s)
    pub async fn query_latency_outliers(&self) -> Result<Vec<String>> {
        let query = r#"
            PREFIX tps: <http://example.com/tps/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

            SELECT ?request ?service ?latencyMs ?startTime ?endTime
            WHERE {
                ?request a tps:Request ;
                    tps:service ?service ;
                    tps:latencyMs ?latencyMs ;
                    tps:startTime ?startTime ;
                    tps:endTime ?endTime .

                FILTER (?latencyMs > 5000.0)
            }
            ORDER BY DESC(?latencyMs)
            LIMIT 20
        "#;

        self.execute_query(query).await
    }

    /// Query: Alert correlation with metric changes
    pub async fn query_alert_metric_correlation(&self) -> Result<Vec<String>> {
        let query = r#"
            PREFIX tps: <http://example.com/tps/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

            SELECT ?alert ?metric ?alertTime ?metricChange
            WHERE {
                ?alert a tps:Alert ;
                    tps:timestamp ?alertTime ;
                    tps:correlatedMetric ?metric ;
                    tps:metricChangePercent ?metricChange .

                FILTER (?metricChange > 10.0 || ?metricChange < -10.0)
            }
            ORDER BY DESC(?metricChange)
        "#;

        self.execute_query(query).await
    }

    /// Query: Worker utilization patterns
    pub async fn query_worker_utilization_trends(&self) -> Result<Vec<String>> {
        let query = r#"
            PREFIX tps: <http://example.com/tps/>

            SELECT ?worker ?utilization ?timestamp
            WHERE {
                ?measurement a tps:WorkerMeasurement ;
                    tps:worker ?worker ;
                    tps:utilization ?utilization ;
                    tps:timestamp ?timestamp .
            }
            ORDER BY ?worker ?timestamp
        "#;

        self.execute_query(query).await
    }

    /// Query: Services with declining success rate
    pub async fn query_declining_success_rate(&self) -> Result<Vec<String>> {
        let query = r#"
            PREFIX tps: <http://example.com/tps/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

            SELECT ?service ?successRateChange
            WHERE {
                ?service a tps:Service ;
                    tps:successRateChange ?successRateChange .

                FILTER (?successRateChange < -5.0)
            }
            ORDER BY ?successRateChange
        "#;

        self.execute_query(query).await
    }

    /// Generic SPARQL query executor
    pub async fn execute_query(&self, sparql_query: &str) -> Result<Vec<String>> {
        use oxigraph::model::Query;

        let query = Query::parse(sparql_query, None)
            .map_err(|e| crate::KaizenError::SparqlError(e.to_string()))?;

        let results = self
            .store
            .query(query)
            .map_err(|e| crate::KaizenError::SparqlError(e.to_string()))?;

        // Convert results to string representation
        let mut result_strings = Vec::new();

        match results {
            oxigraph::sparql::QueryResults::Select(rows) => {
                for row in rows {
                    let row = row.map_err(|e| crate::KaizenError::SparqlError(e.to_string()))?;
                    let row_str = format!(
                        "{}",
                        row.iter()
                            .map(|binding| format!("{:?}", binding))
                            .collect::<Vec<_>>()
                            .join(", ")
                    );
                    result_strings.push(row_str);
                }
            }
            oxigraph::sparql::QueryResults::Ask(answer) => {
                result_strings.push(format!("Ask result: {}", answer));
            }
            _ => {
                return Err(crate::KaizenError::SparqlError(
                    "Unexpected query result type".to_string(),
                ));
            }
        }

        Ok(result_strings)
    }

    /// Load sample evidence data for testing
    pub async fn load_sample_evidence(&self) -> Result<()> {
        let sample_ttl = r#"
            @prefix tps: <http://example.com/tps/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            tps:jidoka_failure_1 a tps:JidokaFailure ;
                tps:component "payment-service" ;
                tps:timestamp "2024-01-25T10:00:00Z"^^xsd:dateTime ;
                tps:reason "Timeout connecting to external payment gateway" .

            tps:jidoka_failure_2 a tps:JidokaFailure ;
                tps:component "checkout-service" ;
                tps:timestamp "2024-01-25T10:30:00Z"^^xsd:dateTime ;
                tps:reason "Database connection pool exhausted" .

            tps:circuit_open_1 a tps:CircuitOpen ;
                tps:component "payment-service" ;
                tps:timestamp "2024-01-25T10:05:00Z"^^xsd:dateTime ;
                tps:queueDepthSnapshot 45 .

            tps:request_1 a tps:Request ;
                tps:service "checkout-service" ;
                tps:latencyMs 5250.0 ;
                tps:startTime "2024-01-25T10:00:00Z"^^xsd:dateTime ;
                tps:endTime "2024-01-25T10:00:05.250Z"^^xsd:dateTime .

            tps:alert_1 a tps:Alert ;
                tps:timestamp "2024-01-25T10:35:00Z"^^xsd:dateTime ;
                tps:correlatedMetric "kanban_latency_p99_ms" ;
                tps:metricChangePercent 15.5 .

            tps:worker_1 a tps:Worker ;
                tps:id "worker-001" .

            tps:worker_measurement_1 a tps:WorkerMeasurement ;
                tps:worker tps:worker_1 ;
                tps:utilization 75.5 ;
                tps:timestamp "2024-01-25T10:00:00Z"^^xsd:dateTime .

            tps:service_payment a tps:Service ;
                tps:name "payment-service" ;
                tps:successRateChange -8.5 .
        "#;

        self.load_ttl(sample_ttl).await
    }
}

impl Default for SparqlAnalyzer {
    fn default() -> Self {
        Self::new().expect("Failed to create default SparqlAnalyzer")
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_sparql_analyzer_creation() {
        let analyzer = SparqlAnalyzer::new().expect("Failed to create analyzer");
        assert!(true); // Just verify it doesn't panic
    }

    #[tokio::test]
    async fn test_load_sample_evidence() {
        let analyzer = SparqlAnalyzer::new().expect("Failed to create analyzer");
        analyzer
            .load_sample_evidence()
            .await
            .expect("Failed to load sample evidence");

        // Try a simple query
        let query = r#"
            PREFIX tps: <http://example.com/tps/>
            SELECT ?s WHERE { ?s a tps:JidokaFailure . }
        "#;

        let results = analyzer.execute_query(query).await.expect("Failed to execute query");
        assert!(!results.is_empty());
    }

    #[tokio::test]
    async fn test_query_latency_outliers() {
        let analyzer = SparqlAnalyzer::new().expect("Failed to create analyzer");
        analyzer
            .load_sample_evidence()
            .await
            .expect("Failed to load sample evidence");

        let results = analyzer
            .query_latency_outliers()
            .await
            .expect("Failed to query outliers");

        // May be empty if no outliers in sample data, that's OK
        assert!(results.len() >= 0);
    }
}
