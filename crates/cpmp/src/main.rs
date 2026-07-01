//! `cpmp` binary entry point.
//!
//! The command surface lives in the library as clap-noun-verb `#[verb]`
//! functions (see [`cpmp::catalog`]); this binary just runs the auto-discovered
//! CLI via [`cpmp::run_cli`]. Keeping the verbs in the library ensures their
//! registrations are linked into the binary.

fn main() {
    if let Err(e) = cpmp::run_cli() {
        eprintln!("ERROR: {e}");
        std::process::exit(1);
    }
}
