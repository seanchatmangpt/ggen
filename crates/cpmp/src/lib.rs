pub mod capability;
pub mod catalog; // clap-noun-verb command surface (`cpmp catalog <verb>`)
pub mod classification;
pub mod db;
pub mod entry;
pub mod models;
pub mod ocel;
pub mod projection;
pub mod receipt;
pub mod registry;
pub mod scanner;
pub mod symbol;
pub mod tier;

/// Run the `cpmp` CLI.
///
/// clap-noun-verb auto-discovers the `#[verb]` functions in [`catalog`] and
/// dispatches to them. Keeping this entry point in the library (alongside the
/// verb modules) ensures their registrations are linked into the binary.
pub fn run_cli() -> clap_noun_verb::Result<()> {
    clap_noun_verb::run()
}
