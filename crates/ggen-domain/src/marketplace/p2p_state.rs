//! P2P registry state management
//!
//! This module manages the global P2P registry state, allowing commands
//! to share a single P2P node instance.

use ggen_utils::error::{Result, GgenError};
use std::sync::{Arc, Mutex};
use once_cell::sync::Lazy;

#[cfg(feature = "p2p")]
use ggen_marketplace::backend::p2p::P2PRegistry;

/// Global P2P registry state
#[cfg(feature = "p2p")]
static P2P_STATE: Lazy<Arc<Mutex<Option<Arc<P2PRegistry>>>>> =
    Lazy::new(|| Arc::new(Mutex::new(None)));

/// Configuration for P2P node
#[derive(Debug, Clone)]
pub struct P2PNodeConfig {
    pub listen_addresses: Vec<String>,
    pub bootstrap_nodes: Vec<String>,
    pub dht_server_mode: bool,
    pub config_file: Option<std::path::PathBuf>,
}

impl Default for P2PNodeConfig {
    fn default() -> Self {
        Self {
            listen_addresses: vec!["/ip4/0.0.0.0/tcp/0".to_string()],
            bootstrap_nodes: Vec::new(),
            dht_server_mode: true,
            config_file: None,
        }
    }
}

/// Initialize the global P2P registry
#[cfg(feature = "p2p")]
pub async fn init_p2p_registry(config: P2PNodeConfig) -> Result<Arc<P2PRegistry>> {
    use ggen_marketplace::backend::p2p::P2PConfig;

    // Parse listen addresses
    let listen_addresses: Result<Vec<_>> = config.listen_addresses
        .iter()
        .map(|addr| {
            addr.parse()
                .map_err(|e| GgenError::invalid_input(format!("Invalid listen address '{}': {}", addr, e)))
        })
        .collect();
    let listen_addresses = listen_addresses?;

    // Parse bootstrap nodes
    let bootstrap_nodes: Result<Vec<_>> = config.bootstrap_nodes
        .iter()
        .map(|node| {
            node.parse()
                .map_err(|e| GgenError::invalid_input(format!("Invalid bootstrap node '{}': {}", node, e)))
        })
        .collect();
    let bootstrap_nodes = bootstrap_nodes?;

    // Create P2P config
    let p2p_config = P2PConfig {
        bootstrap_nodes,
        dht_server_mode: config.dht_server_mode,
        listen_addresses,
        ..Default::default()
    };

    // Create registry
    let registry = P2PRegistry::new(p2p_config).await
        .map_err(|e| GgenError::network_error(format!("Failed to create P2P registry: {}", e)))?;

    let registry = Arc::new(registry);

    // Store in global state
    let mut state = P2P_STATE.lock()
        .map_err(|_| GgenError::internal_error("Failed to acquire P2P state lock"))?;
    *state = Some(registry.clone());

    Ok(registry)
}

/// Get the global P2P registry instance
#[cfg(feature = "p2p")]
pub fn get_p2p_registry() -> Result<Arc<P2PRegistry>> {
    let state = P2P_STATE.lock()
        .map_err(|_| GgenError::internal_error("Failed to acquire P2P state lock"))?;

    state.clone()
        .ok_or_else(|| GgenError::invalid_state(
            "P2P node not initialized. Run 'ggen marketplace p2p start' first."
        ))
}

/// Check if P2P registry is initialized
#[cfg(feature = "p2p")]
pub fn is_p2p_initialized() -> bool {
    P2P_STATE.lock()
        .map(|state| state.is_some())
        .unwrap_or(false)
}

/// Shutdown the global P2P registry
#[cfg(feature = "p2p")]
pub fn shutdown_p2p_registry() -> Result<()> {
    let mut state = P2P_STATE.lock()
        .map_err(|_| GgenError::internal_error("Failed to acquire P2P state lock"))?;
    *state = None;
    Ok(())
}

#[cfg(not(feature = "p2p"))]
pub async fn init_p2p_registry(_config: P2PNodeConfig) -> Result<()> {
    Err(GgenError::feature_not_enabled(
        "p2p",
        "Rebuild with --features p2p to enable P2P functionality"
    ))
}

#[cfg(not(feature = "p2p"))]
pub fn is_p2p_initialized() -> bool {
    false
}
