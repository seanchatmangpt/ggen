//! `receiptctl` — binary entry point.
//!
//! Auto-discovers every `#[verb]` command via `clap_noun_verb::run()`'s
//! distributed-slice registry scan (generated routes live in
//! `src/clap_noun_verb_routes.rs`).

use std::process;

fn main() {
    match receiptctl::run() {
        Ok(()) => process::exit(0),
        Err(e) => {
            eprintln!("ERROR: {}", e);
            process::exit(1);
        }
    }
}
