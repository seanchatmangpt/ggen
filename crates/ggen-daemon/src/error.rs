use thiserror::Error;

#[derive(Debug, Error)]
pub enum DaemonError {
    #[error("ontology error: {0}")]
    Ontology(#[from] oxigraph::store::StorageError),

    #[error("SPARQL error: {0}")]
    Sparql(String),

    #[error("I/O error: {0}")]
    Io(#[from] std::io::Error),

    #[error("scheduler error: {0}")]
    Scheduler(String),

    #[error("dispatch error in {manifest}: {source}")]
    Dispatch { manifest: String, source: Box<DaemonError> },

    #[error("database error: {0}")]
    Database(String),

    #[error("JSON error: {0}")]
    Json(#[from] serde_json::Error),

    #[error("cron expression invalid: {expr} — {reason}")]
    BadCron { expr: String, reason: String },

    #[error("task join error: {0}")]
    Join(String),
}

impl From<oxigraph::store::LoaderError> for DaemonError {
    fn from(e: oxigraph::store::LoaderError) -> Self {
        DaemonError::Sparql(e.to_string())
    }
}

impl From<oxigraph::sparql::QueryEvaluationError> for DaemonError {
    fn from(e: oxigraph::sparql::QueryEvaluationError) -> Self {
        DaemonError::Sparql(e.to_string())
    }
}

// oxigraph::sparql::SparqlSyntaxError — parse errors from SPARQL query strings
impl From<oxigraph::sparql::SparqlSyntaxError> for DaemonError {
    fn from(e: oxigraph::sparql::SparqlSyntaxError) -> Self {
        DaemonError::Sparql(e.to_string())
    }
}

pub type Result<T> = std::result::Result<T, DaemonError>;
