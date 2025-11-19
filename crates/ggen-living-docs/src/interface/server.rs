//! Web server implementation

use super::{AppState, handlers};
use crate::{Error, Result, config::InterfaceConfig, ontology::CodeOntology};
use actix_web::{web, App, HttpServer, middleware};
use actix_files as fs;
use tracing::{info, instrument};

/// Interactive documentation server
pub struct InteractiveServer {
    config: InterfaceConfig,
    ontology: CodeOntology,
}

impl InteractiveServer {
    /// Create a new interactive server
    pub fn new(config: InterfaceConfig, ontology: CodeOntology) -> Result<Self> {
        Ok(Self { config, ontology })
    }

    /// Start the server
    #[instrument(skip(self))]
    pub async fn start(&self, addr: &str) -> Result<()> {
        info!("Starting interactive documentation server at {}", addr);

        let state = AppState::new(self.ontology.clone(), self.config.clone());

        HttpServer::new(move || {
            App::new()
                .app_data(web::Data::new(state.clone()))
                .wrap(middleware::Logger::default())
                .wrap(middleware::Compress::default())
                // API routes
                .route("/api/query", web::post().to(handlers::query_handler))
                .route("/api/entities", web::get().to(handlers::entities_handler))
                .route("/api/graph", web::get().to(handlers::graph_handler))
                .route("/api/search", web::get().to(handlers::search_handler))
                // WebSocket route
                .route("/ws", web::get().to(handlers::websocket_handler))
                // Static files
                .service(fs::Files::new("/", "./static").index_file("index.html"))
        })
        .bind(addr)
        .map_err(|e| Error::WebServer(format!("Failed to bind server: {}", e)))?
        .run()
        .await
        .map_err(|e| Error::WebServer(format!("Server error: {}", e)))?;

        Ok(())
    }
}
