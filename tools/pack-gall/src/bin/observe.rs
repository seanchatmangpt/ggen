use ggen_pack_gall::{observe, parse_args, write_json};
use std::env;

fn main() {
    if let Err(error) = run() {
        eprintln!("PACK-GALL-OBSERVE-001: {error}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let parsed = parse_args(&args, &["--root", "--out"])?;
    let root = parsed.get("--root").expect("validated root");
    let out = parsed.get("--out").expect("validated out");
    let observation = observe(root)?;
    write_json(out, &observation)?;
    println!(
        "{}",
        serde_json::to_string(&observation).map_err(|e| e.to_string())?
    );
    Ok(())
}
