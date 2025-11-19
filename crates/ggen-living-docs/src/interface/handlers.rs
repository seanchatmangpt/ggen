//! HTTP request handlers

use super::AppState;
use actix_web::{web, HttpRequest, HttpResponse, Error};
use actix_ws::Message;
use serde::{Deserialize, Serialize};
use tracing::{debug, error};

/// Query request
#[derive(Debug, Deserialize)]
pub struct QueryRequest {
    pub query: String,
}

/// Query response
#[derive(Debug, Serialize)]
pub struct QueryResponse {
    pub results: serde_json::Value,
}

/// Handle SPARQL queries
pub async fn query_handler(
    state: web::Data<AppState>,
    req: web::Json<QueryRequest>,
) -> Result<HttpResponse, Error> {
    debug!("Handling query request: {}", req.query);

    let ontology = state.ontology.read().await;
    match ontology.query(&req.query).await {
        Ok(results) => {
            Ok(HttpResponse::Ok().json(QueryResponse { results }))
        }
        Err(e) => {
            error!("Query error: {}", e);
            Ok(HttpResponse::InternalServerError().json(serde_json::json!({
                "error": e.to_string()
            })))
        }
    }
}

/// Handle entities list request
pub async fn entities_handler(
    state: web::Data<AppState>,
) -> Result<HttpResponse, Error> {
    debug!("Handling entities request");

    let ontology = state.ontology.read().await;
    match ontology.validate_completeness().await {
        Ok(validation) => {
            Ok(HttpResponse::Ok().json(serde_json::json!({
                "total": validation.total_entities,
                "documented": validation.documented_entities,
                "coverage": validation.coverage_percentage,
            })))
        }
        Err(e) => {
            error!("Entities error: {}", e);
            Ok(HttpResponse::InternalServerError().json(serde_json::json!({
                "error": e.to_string()
            })))
        }
    }
}

/// Handle dependency graph request
pub async fn graph_handler(
    state: web::Data<AppState>,
) -> Result<HttpResponse, Error> {
    debug!("Handling graph request");

    let ontology = state.ontology.read().await;
    match ontology.get_dependency_graph().await {
        Ok(graph) => {
            Ok(HttpResponse::Ok().json(graph))
        }
        Err(e) => {
            error!("Graph error: {}", e);
            Ok(HttpResponse::InternalServerError().json(serde_json::json!({
                "error": e.to_string()
            })))
        }
    }
}

/// Handle search request
pub async fn search_handler(
    state: web::Data<AppState>,
    query: web::Query<std::collections::HashMap<String, String>>,
) -> Result<HttpResponse, Error> {
    let search_term = query.get("q").map(|s| s.as_str()).unwrap_or("");
    debug!("Handling search request: {}", search_term);

    // In a full implementation, perform semantic search
    Ok(HttpResponse::Ok().json(serde_json::json!({
        "results": [],
        "query": search_term,
    })))
}

/// Handle WebSocket connection
pub async fn websocket_handler(
    req: HttpRequest,
    body: web::Payload,
    state: web::Data<AppState>,
) -> Result<HttpResponse, Error> {
    debug!("Handling WebSocket connection");

    let (response, mut session, mut msg_stream) = actix_ws::handle(&req, body)?;

    // Spawn task to handle WebSocket messages
    actix_web::rt::spawn(async move {
        while let Some(Ok(msg)) = msg_stream.recv().await {
            match msg {
                Message::Text(text) => {
                    debug!("Received WebSocket message: {}", text);
                    // Echo back for now
                    let _ = session.text(text).await;
                }
                Message::Ping(bytes) => {
                    let _ = session.pong(&bytes).await;
                }
                Message::Close(reason) => {
                    debug!("WebSocket closed: {:?}", reason);
                    break;
                }
                _ => {}
            }
        }
    });

    Ok(response)
}
