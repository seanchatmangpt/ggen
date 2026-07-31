use ggen_pack_gall::{issue_receipt, parse_args, read_observation, verify, write_json};
use std::env;

fn main() {
    if let Err(error) = run() {
        eprintln!("PACK-GALL-VERIFY-001: {error}");
        std::process::exit(1);
    }
}

fn run() -> Result<(), String> {
    let args: Vec<String> = env::args().collect();
    let parsed = parse_args(
        &args,
        &["--root", "--observation", "--report", "--receipt"],
    )?;
    let root = parsed.get("--root").expect("validated root");
    let observation_path = parsed
        .get("--observation")
        .expect("validated observation");
    let report_path = parsed.get("--report").expect("validated report");
    let receipt_path = parsed.get("--receipt").expect("validated receipt");
    let observation = read_observation(observation_path)?;
    let report = verify(root, &observation)?;
    write_json(report_path, &report)?;
    let receipt = issue_receipt(report_path, observation_path, &report)?;
    write_json(receipt_path, &receipt)?;
    println!(
        "{}",
        serde_json::to_string(&report).map_err(|e| e.to_string())?
    );
    if report.standing != "PARTIAL_ALIVE" {
        return Err("one or more Gall checkpoints failed".to_string());
    }
    Ok(())
}
