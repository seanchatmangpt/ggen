// rust/knhk-cli/src/main.rs
// KNHKS CLI - Main Entry Point
// Noun-Verb Command Interface based on CONVO.txt API

mod commands;

use clap_noun_verb::{noun, verb, CliBuilder};

/// Boot noun - system initialization
#[noun]
fn boot() {
    // Boot commands registered via verbs
}

/// Initialize Σ and Q
#[verb]
fn init(sigma: String, q: String) {
    if let Err(e) = commands::boot::init(sigma, q) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Connect noun - connector management
#[noun]
fn connect() {
    // Connect commands registered via verbs
}

/// Register a connector
#[verb]
fn register(name: String, schema: String, source: String) {
    if let Err(e) = commands::connect::register(name, schema, source) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// List connectors
#[verb]
fn list_connectors() {
    if let Err(e) = commands::connect::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Cover noun - cover definition
#[noun]
fn cover() {
    // Cover commands registered via verbs
}

/// Define cover over O
#[verb]
fn define(select: String, shard: String) {
    if let Err(e) = commands::cover::define(select, shard) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// List covers
#[verb]
fn list_covers() {
    if let Err(e) = commands::cover::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Admit noun - delta admission
#[noun]
fn admit() {
    // Admit commands registered via verbs
}

/// Admit Δ into O
#[verb]
fn delta(delta_file: String) {
    if let Err(e) = commands::admit::delta(delta_file) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Reflex noun - reflex declaration
#[noun]
fn reflex() {
    // Reflex commands registered via verbs
}

/// Declare a reflex
#[verb]
fn declare(name: String, op: String, pred: u64, off: u64, len: u64) {
    if let Err(e) = commands::reflex::declare(name, op, pred, off, len) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// List reflexes
#[verb]
fn list_reflexes() {
    if let Err(e) = commands::reflex::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Epoch noun - epoch operations
#[noun]
fn epoch() {
    // Epoch commands registered via verbs
}

/// Create epoch
#[verb]
fn create_epoch(id: String, tau: u32, lambda: String) {
    if let Err(e) = commands::epoch::create(id, tau, lambda) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Run epoch
#[verb]
fn run_epoch(id: String) {
    if let Err(e) = commands::epoch::run(id) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// List epochs
#[verb]
fn list_epochs() {
    if let Err(e) = commands::epoch::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Route noun - action routing
#[noun]
fn route() {
    // Route commands registered via verbs
}

/// Install route
#[verb]
fn install_route(name: String, kind: String, target: String) {
    if let Err(e) = commands::route::install(name, kind, target) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// List routes
#[verb]
fn list_routes() {
    if let Err(e) = commands::route::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Receipt noun - receipt operations
#[noun]
fn receipt() {
    // Receipt commands registered via verbs
}

/// Get receipt
#[verb]
fn get(id: String) {
    if let Err(e) = commands::receipt::get(id) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Merge receipts
#[verb]
fn merge_receipts(ids: String) {
    if let Err(e) = commands::receipt::merge(ids) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// List receipts
#[verb]
fn list_receipts() {
    if let Err(e) = commands::receipt::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Verify receipt integrity
#[verb]
fn verify_receipt(id: String) {
    if let Err(e) = commands::receipt::verify(id) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Show receipt details
#[verb]
fn show_receipt(id: String) {
    if let Err(e) = commands::receipt::show(id) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Pipeline noun - ETL pipeline operations
#[noun]
fn pipeline() {
    // Pipeline commands registered via verbs
}

/// Execute pipeline
#[verb]
fn run_pipeline(connectors: Option<String>, schema: Option<String>) {
    if let Err(e) = commands::pipeline::run(connectors, schema) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Show pipeline status
#[verb]
fn pipeline_status() {
    if let Err(e) = commands::pipeline::status() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Metrics noun - metrics operations
#[noun]
fn metrics() {
    // Metrics commands registered via verbs
}

/// Get metrics
#[verb]
fn get_metrics() {
    if let Err(e) = commands::metrics::get() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Coverage noun - coverage operations
#[noun]
fn coverage() {
    // Coverage commands registered via verbs
}

/// Get coverage
#[verb]
fn get_coverage() {
    if let Err(e) = commands::coverage::get() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Context noun - context management
#[noun]
fn context() {
    // Context commands registered via verbs
}

/// List contexts
#[verb]
fn list_contexts() {
    if let Err(e) = commands::context::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Show current context
#[verb]
fn current_context() {
    if let Err(e) = commands::context::current() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Create context
#[verb]
fn create_context(id: String, name: String, schema: String) {
    if let Err(e) = commands::context::create(id, name, schema) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Use context
#[verb]
fn use_context(id: String) {
    if let Err(e) = commands::context::use_context(id) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Hook noun - knowledge hook operations
#[noun]
fn hook() {
    // Hook commands registered via verbs
}

/// Create a hook
#[verb]
fn create_hook(name: String, op: String, pred: u64, off: u64, len: u64) {
    if let Err(e) = commands::hook::create(name, op, pred, off, len, None, None, None, None) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// List hooks
#[verb]
fn list_hooks() {
    if let Err(e) = commands::hook::list() {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Evaluate a hook
#[verb]
fn eval_hook(name: String) {
    if let Err(e) = commands::hook::eval(name) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

/// Show hook details
#[verb]
fn show_hook(name: String) {
    if let Err(e) = commands::hook::show(name) {
        eprintln!("Error: {}", e);
        std::process::exit(1);
    }
}

fn main() {
    CliBuilder::default()
        .noun(boot)
        .noun(connect)
        .noun(cover)
        .noun(admit)
        .noun(reflex)
        .noun(epoch)
        .noun(route)
        .noun(receipt)
        .noun(pipeline)
        .noun(metrics)
        .noun(coverage)
        .noun(hook)
        .noun(context)
        .run();
}
