pub mod cli_helper;
pub mod error;
pub mod schema;
pub mod server;
pub mod swarm;
pub mod tools;
pub mod utils;

pub use server::GgenMcpServer;

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_server_creation() {
        let _server = GgenMcpServer::new().await.unwrap();
        // Server should initialize without panicking
    }
}
