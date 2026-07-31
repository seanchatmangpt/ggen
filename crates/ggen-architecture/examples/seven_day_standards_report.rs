use ggen_architecture::{seven_day_standards_profile, StandardStatus, TestingBblockStanding};
use serde::Serialize;

#[derive(Serialize)]
struct VerifierReport {
    schema: &'static str,
    profile_id: String,
    profile_version: String,
    profile_digest: String,
    checkpoints: usize,
    admitted: usize,
    inherited: usize,
    pending: usize,
    bounded_certification_ready: bool,
    standards_crown_ready: bool,
    testing_protocol_id: String,
    testing_suites: usize,
    testing_suites_alive: usize,
    testing_suites_pending: usize,
    testing_bblock_standing: TestingBblockStanding,
    required_broker: String,
    direct_actuation: bool,
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let profile = seven_day_standards_profile();
    profile.validate()?;
    let admitted = profile
        .checkpoints
        .iter()
        .filter(|checkpoint| checkpoint.status == StandardStatus::Admitted)
        .count();
    let inherited = profile
        .checkpoints
        .iter()
        .filter(|checkpoint| checkpoint.status == StandardStatus::Inherited)
        .count();
    let pending = profile
        .checkpoints
        .iter()
        .filter(|checkpoint| checkpoint.status == StandardStatus::PendingCheckpoint)
        .count();
    let report = VerifierReport {
        schema: "ggen.seven-day-standards.verifier-report.v1",
        profile_id: profile.id.clone(),
        profile_version: profile.version.clone(),
        profile_digest: profile.digest()?,
        checkpoints: profile.checkpoints.len(),
        admitted,
        inherited,
        pending,
        bounded_certification_ready: admitted + inherited == profile.promoting_checkpoints().len(),
        standards_crown_ready: pending == 0,
        testing_protocol_id: profile.testing_bblock.id.clone(),
        testing_suites: profile.testing_bblock.suites.len(),
        testing_suites_alive: profile.testing_bblock.alive_count(),
        testing_suites_pending: profile.testing_bblock.pending_count(),
        testing_bblock_standing: profile.testing_bblock.standing(),
        required_broker: profile.root_broker,
        direct_actuation: false,
    };
    println!("{}", serde_json::to_string_pretty(&report)?);
    Ok(())
}
