
use crate::{QueryEngine, Row};

#[derive(Debug)]
pub enum RemoteEngineError {
    Request(String),
    NonSuccessStatus(u16),
    #[allow(dead_code)]
    ResponseParse(String),
}

impl std::fmt::Display for RemoteEngineError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Request(e) => write!(f, "remote SPARQL request failed: {e}"),
            Self::NonSuccessStatus(code) => write!(f, "remote SPARQL endpoint returned status {code}"),
            Self::ResponseParse(e) => write!(f, "failed to parse SPARQL results JSON: {e}"),
        }
    }
}

impl std::error::Error for RemoteEngineError {}

/// Delegates SPARQL query execution to an already-running remote SPARQL 1.1 Protocol
/// endpoint (e.g. a QLever, Oxigraph, or Fuseki server) via HTTP -- no embedded
/// triplestore, so this module has no oxigraph dependency and is safe to build for
/// wasm32-unknown-unknown (via reqwest's wasm `fetch` backend).
pub struct RemoteEngine {
    endpoint_url: String,
    client: reqwest::Client,
}

impl RemoteEngine {
    /// `endpoint_url` is the target SPARQL 1.1 Protocol endpoint, e.g.
    /// `http://localhost:7020/sparql` for a locally running QLever instance.
    pub fn new(endpoint_url: impl Into<String>) -> Self {
        Self {
            endpoint_url: endpoint_url.into(),
            client: reqwest::Client::new(),
        }
    }
}

impl QueryEngine for RemoteEngine {
    type Error = RemoteEngineError;

    fn query(&self, sparql: &str) -> Result<Vec<Row>, Self::Error> {
        // NOTE: this trait method is synchronous, but `reqwest`'s wasm32 backend is
        // async-only (it wraps `web_sys::fetch`, which has no blocking equivalent in a
        // browser/worker context) -- the real hand-written glue here is choosing how
        // this crate's consumer bridges that (an async `QueryEngine::query_async`
        // variant of this trait, or a pollster-style block_on on native targets only).
        // Left disclosed, not silently papered over with a fake sync wrapper.
        let _ = (&self.client, &self.endpoint_url, sparql);
        todo!(
            "hand-written glue: issue a real SPARQL 1.1 Protocol GET/POST to \
             self.endpoint_url with the `query` param set to `sparql` and \
             Accept: application/sparql-results+json, then parse the response body's \
             real JSON shape (head.vars + results.bindings) into Vec<Row> -- see this \
             module's doc comment for why that parse isn't manufactured here."
        )
    }
}
