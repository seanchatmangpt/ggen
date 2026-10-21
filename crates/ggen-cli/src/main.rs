//! ggen CLI entry point
//!
//! This is the main entry point for the ggen command-line interface. It delegates
//! to the `ggen_cli_lib` crate for the actual CLI implementation.

#[tokio::main]
async fn main() {
    match ggen_cli_lib::cli_match().await {
        Ok(()) => {
            // Successful execution
            std::process::exit(0);
        }
        Err(e) => {
            // Error occurred. Refusal codes map to distinct exit codes at the
            // CLI boundary (v26.10.20 refusal envelope mapping, lane A05/I1):
            // a `REFUSED_STALE_LEASE` refusal (FreshEpochLease gate, carried
            // in the FM-WRITE-012 payload and mirrored by abb-sbb) exits 5 so
            // automation can distinguish a stale epoch from any other
            // failure; everything else exits 1.
            eprintln!("ERROR: {e}");
            let msg = e.to_string();
            if msg.contains("REFUSED_STALE_LEASE") {
                std::process::exit(5);
            }
            std::process::exit(1);
        }
    }
}
