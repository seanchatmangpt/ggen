//! Process-level acceptance for the canonical enterprise architecture CLI.
//!
//! These tests execute the real `ggen-ea` binary against a governed model and
//! therefore verify the CLI boundary rather than substituting unit-level proof.

use std::{
    error::Error,
    path::PathBuf,
    process::{Command, Output},
};

use serde_json::Value;

fn fixture() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../crates/ggen-architecture/examples/enterprise-model.json")
}

fn execute(arguments: &[&str]) -> Result<Output, Box<dyn Error>> {
    Ok(Command::new(env!("CARGO_BIN_EXE_ggen-ea"))
        .args(arguments)
        .output()?)
}

fn require_success(output: &Output, operation: &str) -> Result<(), Box<dyn Error>> {
    if output.status.success() {
        return Ok(());
    }
    Err(format!(
        "{operation} failed with {:?}: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    )
    .into())
}

#[test]
fn enterprise_cli_executes_governed_graph_paths() -> Result<(), Box<dyn Error>> {
    let fixture = fixture();
    let model = fixture.to_string_lossy().into_owned();

    let validation = execute(&["validate", "--model", &model, "--json"])?;
    require_success(&validation, "validate")?;
    let violations: Value = serde_json::from_slice(&validation.stdout)?;
    if violations.as_array().is_none_or(|values| !values.is_empty()) {
        return Err("the governed fixture must have zero structural violations".into());
    }

    let governance = execute(&["governance", "--model", &model, "--json"])?;
    require_success(&governance, "governance")?;
    let assessment: Value = serde_json::from_slice(&governance.stdout)?;
    if assessment["structurally_valid"] != Value::Bool(true)
        || assessment["statically_complete"] != Value::Bool(true)
    {
        return Err("the governed fixture must reach static governance closure".into());
    }

    let trace = execute(&[
        "trace",
        "--model",
        &model,
        "--subject",
        "req:residency",
        "--max-depth",
        "8",
        "--json",
    ])?;
    require_success(&trace, "trace")?;
    let hops: Value = serde_json::from_slice(&trace.stdout)?;
    if !hops.as_array().is_some_and(|values| {
        values
            .iter()
            .any(|hop| hop["to"] == Value::String("receipt:deployment".to_owned()))
    }) {
        return Err("requirement trace must reach immutable evidence".into());
    }

    let impact = execute(&[
        "impact",
        "--model",
        &model,
        "--subject",
        "req:residency",
        "--json",
    ])?;
    require_success(&impact, "impact")?;
    let impact_report: Value = serde_json::from_slice(&impact.stdout)?;
    if !impact_report["impacted"]
        .as_array()
        .is_some_and(|values| values.contains(&Value::String("cap:data".to_owned())))
    {
        return Err("impact closure must include the constrained capability".into());
    }

    let portfolio = execute(&["portfolio", "--model", &model, "--json"])?;
    require_success(&portfolio, "portfolio")?;
    let portfolio: Value = serde_json::from_slice(&portfolio.stdout)?;
    if portfolio["cap:data"] != Value::Array(vec![Value::String("svc:data".to_owned())]) {
        return Err("capability portfolio must preserve the admitted realization".into());
    }

    let view = execute(&[
        "view",
        "--model",
        &model,
        "--viewpoint",
        "application",
        "--json",
    ])?;
    require_success(&view, "view")?;
    let application_view: Value = serde_json::from_slice(&view.stdout)?;
    if application_view["elements"]["app:new"]["kind"] != Value::String("application".to_owned())
    {
        return Err("application viewpoint must contain the target application".into());
    }

    let transition = execute(&[
        "transition-order",
        "--model",
        &model,
        "--transition",
        "transition:modernize-data",
        "--json",
    ])?;
    require_success(&transition, "transition-order")?;
    let order: Vec<String> = serde_json::from_slice(&transition.stdout)?;
    if order != ["wp:build".to_owned(), "wp:cutover".to_owned()] {
        return Err("transition order must preserve dependency closure".into());
    }

    let receipt = execute(&[
        "receipt",
        "--model",
        &model,
        "--operation",
        "admit-enterprise-model",
        "--json",
    ])?;
    require_success(&receipt, "receipt")?;
    let receipt: Value = serde_json::from_slice(&receipt.stdout)?;
    if receipt["schema"] != Value::String("ggen.enterprise-architecture.receipt.v1".to_owned())
        || receipt["subject_digest"].as_str().is_none()
        || receipt["consequence_digest"].as_str().is_none()
    {
        return Err("receipt must bind schema, subject identity, and consequence".into());
    }

    Ok(())
}
