pub mod cmds;

#[tokio::main]
async fn main() {
    if let Err(e) = clap_noun_verb::run().await {
        eprintln!("{}", e);
        std::process::exit(1);
    }
}
