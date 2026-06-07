use tower_lsp_max::{LspService, Server};
use tower_lsp_max_playground::Backend;

#[tokio::main]
async fn main() {
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive("tower_lsp_max_playground=debug".parse().unwrap()),
        )
        .with_writer(std::io::stderr)
        .init();

    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::build(Backend::new).finish();

    let _ = Server::new(stdin, stdout, socket).serve(service).await;
}
