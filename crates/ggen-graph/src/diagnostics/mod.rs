use crate::graph::DeterministicGraph;
use crate::GraphError;
use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct KnowledgeHook {
    pub name: String,
    pub sparql_query: String,
}

impl KnowledgeHook {
    pub fn new(name: String, sparql_query: String) -> Self {
        Self { name, sparql_query }
    }

    pub fn execute(&self, graph: &DeterministicGraph) -> Result<bool, GraphError> {
        let results = graph.query(&self.sparql_query)?;
        match results {
            oxigraph::sparql::QueryResults::Boolean(val) => Ok(val),
            oxigraph::sparql::QueryResults::Solutions(mut solutions) => match solutions.next() {
                Some(Ok(_)) => Ok(false),
                Some(Err(e)) => Err(e.into()),
                None => Ok(true),
            },
            _ => Ok(true),
        }
    }
}

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum DiagnosticStatus {
    Ok,
    Warning,
    Error,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiagnosticReport {
    pub overall_status: DiagnosticStatus,
}

pub struct DiagnosticsRunner;

impl DiagnosticsRunner {
    pub fn run_diagnostics(_graph: &DeterministicGraph) -> DiagnosticReport {
        DiagnosticReport {
            overall_status: DiagnosticStatus::Ok,
        }
    }
}
