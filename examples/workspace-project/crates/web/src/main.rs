use axum::{routing::get, Router};

#[tokio::main]
async fn main() {
    println!("Web server starting...");
    let app = Router::new().route("/", get(|| async { "Workspace Project" }));
    let listener = tokio::net::TcpListener::bind("127.0.0.1:8080").await.unwrap();
    axum::serve(listener, app).await.unwrap();
}

#[cfg(test)]
mod tests {
    #[test]
    fn test_web() {
        assert!(true);
    }
}
