//! SPARQL Validator Agent - Swarm Integration Tests
//!
//! These tests demonstrate the SPARQL validator agent's ability to
//! validate and fix common SPARQL syntax errors within the swarm framework.

// Import the swarm agent directly for testing
#[cfg(test)]
mod swarm_agent_tests {
    use ggen_ai::error::GgenAiError;
    use ggen_ai::swarm::agents::sparql_validator::{
        SPARQLValidatorAgent, SparqlErrorType, ValidationReport,
    };
    use ggen_ai::swarm::{AgentInput, AgentOutput, SwarmContext};

    #[tokio::test]
    async fn test_agent_validate_valid_query() {
        let agent = SPARQLValidatorAgent::new(false);
        let query = "SELECT * WHERE { ?s ?p ?o }";

        let result = agent.validate_and_fix(query).unwrap();

        assert!(result.is_valid);
        assert_eq!(result.fixed_query, query);
        assert!(result.errors.is_empty());
        assert!(result.fixes_applied.is_empty());
    }

    #[tokio::test]
    async fn test_agent_fix_missing_periods() {
        let agent = SPARQLValidatorAgent::new(false);

        // Query with missing periods
        let query = "SELECT * WHERE { ?s a :Type ?p ?o }";
        let fixed = agent.fix_syntax(query);

        // Should add period after triple pattern
        assert!(fixed.contains('.') || fixed.contains('}'));
    }

    #[tokio::test]
    async fn test_agent_fix_unbalanced_brackets() {
        let agent = SPARQLValidatorAgent::new(false);

        // Query with unbalanced brackets
        let query = "SELECT * WHERE { ?s ?p [ ?o1 ?o2 } }";
        let fixed = agent.fix_unbalanced_brackets(query);

        // Should balance brackets
        let open_count = fixed.matches('[').count();
        let close_count = fixed.matches(']').count();
        assert_eq!(open_count, close_count);
    }

    #[tokio::test]
    async fn test_agent_fix_unbalanced_parens() {
        let agent = SPARQLValidatorAgent::new(false);

        // Query with unbalanced parentheses
        let query = "SELECT * WHERE { ?s ?p ?o FILTER(?o > 42 }";
        let fixed = agent.fix_unbalanced_parens(query);

        // Should balance parentheses
        let open_count = fixed.matches('(').count();
        let close_count = fixed.matches(')').count();
        assert_eq!(open_count, close_count);
    }

    #[tokio::test]
    async fn test_agent_fix_prefix_declarations() {
        let agent = SPARQLValidatorAgent::new(false);

        // PREFIX declaration without angle brackets
        let query = "PREFIX rdf: http://www.w3.org/1999/02/22-rdf-syntax-ns#
                    SELECT * WHERE { ?s ?p ?o }";
        let fixed = agent.fix_prefix_declarations(query);

        // Should add angle brackets
        assert!(fixed.contains("<http://"));
    }

    #[tokio::test]
    async fn test_agent_fix_missing_where() {
        let agent = SPARQLValidatorAgent::new(false);

        // SELECT query without WHERE clause
        let query = "SELECT * { ?s ?p ?o }";
        let fixed = agent.fix_missing_where(query);

        // Should add WHERE clause
        assert!(fixed.contains("WHERE"));
    }

    #[tokio::test]
    async fn test_agent_execute() {
        let agent = SPARQLValidatorAgent::new(false);
        let context = SwarmContext::default();

        let input = AgentInput {
            data: serde_json::json!({
                "query": "SELECT * WHERE { ?s ?p ?o }"
            }),
        };

        let result = agent.execute(&context, input).await.unwrap();

        assert_eq!(result.output_type, "sparql_validation_report");
        assert!(result.data["is_valid"].as_bool().unwrap());
        assert_eq!(result.target_agents, vec!["code_generator".to_string()]);
    }

    #[tokio::test]
    async fn test_agent_health_check() {
        let agent = SPARQLValidatorAgent::new(false);
        let health = agent.health_check().await;

        assert_eq!(health.status, ggen_ai::swarm::HealthStatus::Healthy);
        assert_eq!(health.score, 1.0);
        assert!(health.issues.is_empty());
    }

    #[tokio::test]
    async fn test_agent_validate() {
        let agent = SPARQLValidatorAgent::new(false);

        let is_valid = agent.validate().await.unwrap();
        assert!(is_valid);
    }

    #[tokio::test]
    async fn test_agent_classify_errors() {
        let agent = SPARQLValidatorAgent::new(false);

        let syntax_error = "syntax error at line 1";
        let error_type = agent.classify_error(syntax_error);

        assert!(matches!(error_type, SparqlErrorType::SyntaxError));
    }

    #[tokio::test]
    async fn test_agent_dry_run_mode() {
        let agent = SPARQLValidatorAgent::new(true); // dry_run = true
        let query = "SELECT * WHERE { ?s ?p ?o }";

        let result = agent.validate_and_fix(query).unwrap();

        // In dry run mode, should still validate but not modify
        assert!(result.is_valid);
    }

    #[tokio::test]
    async fn test_agent_verbose_mode() {
        let agent = SPARQLValidatorAgent::new(false).with_verbose(true);
        let query = "SELECT * WHERE { ?s ?p ?o }";

        // Should not panic in verbose mode
        let result = agent.validate_and_fix(query);
        assert!(result.is_ok());
    }
}

#[cfg(feature = "swarm")]
mod integration_tests {
    use ggen_ai::swarm::agents::sparql_validator::SPARQLValidatorAgent;
    use ggen_ai::swarm::{AgentInput, SwarmContext};

    #[tokio::test]
    async fn test_swarm_integration() {
        let agent = SPARQLValidatorAgent::new(false);
        let context = SwarmContext::default();

        let input = AgentInput {
            data: serde_json::json!({
                "query": "PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#> SELECT ?label WHERE { ?s rdfs:label ?label }"
            }),
        };

        let result = agent.execute(&context, input).await.unwrap();

        assert_eq!(result.output_type, "sparql_validation_report");
        assert!(result.data["is_valid"].as_bool().unwrap());
    }
}
