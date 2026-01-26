fn main() {
    // BLOCKER FIX: Temporarily disable napi_build::setup() to verify it's the root cause
    // of async-trait proc-macro compilation failure across the workspace.
    //
    // Root cause analysis:
    // - napi_build::setup() sets environment variables that affect compiler target configuration
    // - These variables propagate to proc-macro compilation for async-stream, tokio, etc.
    // - Result: "cannot produce proc-macro for X as target does not support these crate types"
    //
    // Solution: Skip napi_build setup during workspace compilation
    // napi_build::setup();

    // NOTE: This is a temporary fix to unblock the workspace.
    // For ggen-node-specific builds (e.g., npm module generation),
    // users should build ggen-node explicitly with: cargo build -p ggen-node
    // In that context, napi_build would need to be re-enabled for N-API bindings.

    // TODO: Implement proper isolation to enable napi_build only for explicit ggen-node builds
}
