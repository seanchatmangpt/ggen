pub mod analyzers;
pub mod check;
pub mod error;
pub mod handlers;
pub mod init;
pub mod intel;
pub mod pack;
pub mod protocol;
pub mod route;
pub mod server;
pub mod state;
pub mod utils;

pub use check::{
    capture_request, check_content, check_files, check_files_in_root, check_files_with_routes,
    discover_law_surfaces, CheckReport, FileReport, RouteSummary,
};
pub use init::{init as init_project, InitReport};
pub use intel::{
    compute_metrics, field_status, mine, replay_case, verify_promotion, Attribution, CaseReplay,
    FieldReadiness, FieldStatus, ImproveMetrics, IntelLog, MineReport, PromotionReplay,
    RepairReceipt,
};
pub use pack::{
    default_pack_dir, emit as emit_pack, load_manifest, manifest_is_current, pack_hash_at,
    verify_pack, EmitReport, PackManifest, PackOptions, PackProvenance, PackReplay, PolicyEntry,
    RouteEntry, DEFAULT_AGENTS,
};
pub use route::{
    envelope_for_diagnostic, family_of_diagnostic, route_case_id, route_plan_for_diagnostic,
    RepairFamily, RepairRoute, RouteBindings, RouteEnvelope, RoutePlan, RoutePlanRef, RouteRefusal,
    RouteRegistry,
};
pub use server::GgenLanguageServer;
pub use state::ServerState;

use tower_lsp::{LspService, Server};

/// Run the LSP server over stdio (the transport editors use).
pub async fn run_stdio() -> anyhow::Result<()> {
    let stdin = tokio::io::stdin();
    let stdout = tokio::io::stdout();

    let (service, socket) = LspService::new(GgenLanguageServer::new);
    Server::new(stdin, stdout, socket).serve(service).await;
    Ok(())
}

/// HTTP transport is not implemented; stdio is the supported transport.
pub async fn run_http(_port: u16) -> anyhow::Result<()> {
    Err(anyhow::anyhow!(
        "HTTP transport not implemented; use --transport stdio"
    ))
}
