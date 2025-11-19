//! Interactive storytelling interface for documentation
//!
//! Provides a web-based interactive interface for exploring documentation.

mod server;
mod handlers;
mod websocket;

pub use server::InteractiveServer;

use crate::{Error, Result};
use actix_web::{web, App, HttpServer, middleware};
use actix_files as fs;
use crate::ontology::CodeOntology;
use crate::config::InterfaceConfig;
use std::sync::Arc;
use tokio::sync::RwLock;
use tracing::{info, instrument};

/// State shared across the web application
#[derive(Clone)]
pub struct AppState {
    pub ontology: Arc<RwLock<CodeOntology>>,
    pub config: InterfaceConfig,
}

impl AppState {
    pub fn new(ontology: CodeOntology, config: InterfaceConfig) -> Self {
        Self {
            ontology: Arc::new(RwLock::new(ontology)),
            config,
        }
    }
}
