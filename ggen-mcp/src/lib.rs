// pub mod agents;
pub mod error;
pub mod schema;
pub mod server;
pub mod tools;
pub mod utils;

pub use server::GgenMcpServer;

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_server_creation() {
        let _server = GgenMcpServer::new();
        // Server should initialize without panicking
    }
}
