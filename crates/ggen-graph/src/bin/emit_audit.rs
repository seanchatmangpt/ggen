//! Binary to emit the self-audit log and coverage matrix for Vision 2030 compliance.

use ggen_graph::ocel::{generate_coverage_matrix, generate_self_audit_log};
use std::fs::{self, File};
use std::io::BufWriter;
use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let audit_dir = Path::new("crates/ggen-graph/audit");
    if !audit_dir.exists() {
        fs::create_dir_all(audit_dir)?;
    }

    let self_audit_path = audit_dir.join("vision2030.self_audit.ocel.json");
    let coverage_path = audit_dir.join("vision2030.coverage.json");

    let log = generate_self_audit_log();
    let coverage = generate_coverage_matrix();

    let self_audit_file = File::create(&self_audit_path)?;
    let self_audit_writer = BufWriter::new(self_audit_file);
    serde_json::to_writer_pretty(self_audit_writer, &log)?;

    let coverage_file = File::create(&coverage_path)?;
    let coverage_writer = BufWriter::new(coverage_file);
    serde_json::to_writer_pretty(coverage_writer, &coverage)?;

    println!("Successfully emitted self-audit files in crates/ggen-graph/audit/");
    Ok(())
}
