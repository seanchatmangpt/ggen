use anyhow::{bail, Context, Result};
use clap::Parser;
use serde_json::{json, Map, Value};
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fs;
use std::path::{Component, Path, PathBuf};
use std::process::Command;

const SCHEMA: &str = "urn:ggen:enterprise-connection:v1";

#[derive(Debug, Parser)]
#[command(
    name = "ggen-foundry-connection",
    about = "Bind a CATALOG envelope to ggen's architecture-foundry manufacture boundary"
)]
struct Cli {
    #[arg(long)]
    input: PathBuf,
    #[arg(long)]
    program: PathBuf,
    #[arg(long)]
    revision: String,
    #[arg(long)]
    out: PathBuf,
    /// Exact ggen binary to execute. Must be paired with --project-root.
    #[arg(long)]
    ggen_bin: Option<PathBuf>,
    /// Candidate ggen project to manufacture. Must be paired with --ggen-bin.
    #[arg(long)]
    project_root: Option<PathBuf>,
    /// Relative generated output that must exist after sync. Repeatable.
    #[arg(long = "required-output")]
    required_outputs: Vec<PathBuf>,
}

fn sha256(data: &[u8]) -> String {
    format!("sha256:{:x}", Sha256::digest(data))
}

fn require_object<'a>(value: &'a Value, name: &str) -> Result<&'a Map<String, Value>> {
    value
        .as_object()
        .with_context(|| format!("REFUSED:SCHEMA:{name}:expected object"))
}

fn require_string<'a>(value: &'a Value, name: &str) -> Result<&'a str> {
    value
        .as_str()
        .filter(|s| !s.is_empty())
        .with_context(|| format!("REFUSED:SCHEMA:{name}:expected non-empty string"))
}

fn producer_identity(env: &Value) -> Result<String> {
    let p = require_object(&env["producer"], "producer")?;
    Ok(format!(
        "{}@{}",
        require_string(&p["repository"], "producer.repository")?,
        require_string(&p["revision"], "producer.revision")?
    ))
}

fn validate_input(raw: &[u8], env: &Value) -> Result<()> {
    if env["schema"] != SCHEMA {
        bail!("REFUSED:SCHEMA_VERSION");
    }
    if env["stage"] != "CATALOG" {
        bail!("REFUSED:TRANSITION:MANUFACTURE_REQUIRES_CATALOG");
    }
    if env["authority"]["do_authority"] != Value::Bool(false) {
        bail!("REFUSED:AMBIENT_ACTUATION");
    }
    if serde_json::to_vec(env)? != raw {
        bail!("REFUSED:NON_CANONICAL");
    }
    Ok(())
}

fn safe_relative(path: &Path) -> bool {
    !path.as_os_str().is_empty()
        && !path.is_absolute()
        && path
            .components()
            .all(|component| matches!(component, Component::Normal(_) | Component::CurDir))
}

fn output_path(prefix: &str, relative: &Path) -> String {
    let rel = relative
        .components()
        .filter_map(|component| match component {
            Component::Normal(part) => Some(part.to_string_lossy().into_owned()),
            _ => None,
        })
        .collect::<Vec<_>>()
        .join("/");
    format!("{prefix}/{rel}")
}

struct ManufactureEvidence {
    receipt_transport_digest: String,
    receipt_artifact_path: String,
    required_artifacts: Vec<Value>,
    verification_stdout_digest: String,
}

fn execute_native_manufacture(
    ggen_bin: &Path, project_root: &Path, required_outputs: &[PathBuf],
) -> Result<ManufactureEvidence> {
    if !ggen_bin.is_file() {
        bail!("BUILD_BROKEN:GGEN_BINARY_MISSING:{}", ggen_bin.display());
    }
    if !project_root.is_dir() {
        bail!("REFUSED:PROJECT_ROOT_MISSING:{}", project_root.display());
    }
    if !project_root.join("ggen.toml").is_file() {
        bail!(
            "REFUSED:GGEN_PROJECT_MANIFEST_MISSING:{}",
            project_root.display()
        );
    }
    if required_outputs.is_empty() {
        bail!("REFUSED:REQUIRED_OUTPUT_EMPTY");
    }
    for relative in required_outputs {
        if !safe_relative(relative) {
            bail!("REFUSED:UNSAFE_REQUIRED_OUTPUT:{}", relative.display());
        }
    }

    let sync = Command::new(ggen_bin)
        .args(["sync", "run"])
        .current_dir(project_root)
        .output()
        .with_context(|| format!("BUILD_BROKEN:GGEN_SYNC_SPAWN:{}", ggen_bin.display()))?;
    if !sync.status.success() {
        bail!(
            "BUILD_BROKEN:GGEN_SYNC_RUN:status={}:stdout_sha256={}:stderr_sha256={}",
            sync.status,
            sha256(&sync.stdout),
            sha256(&sync.stderr)
        );
    }

    let verify = Command::new(ggen_bin)
        .args(["receipt", "verify"])
        .current_dir(project_root)
        .output()
        .with_context(|| {
            format!(
                "BUILD_BROKEN:GGEN_RECEIPT_VERIFY_SPAWN:{}",
                ggen_bin.display()
            )
        })?;
    if !verify.status.success() {
        bail!(
            "BUILD_BROKEN:GGEN_RECEIPT_VERIFY:status={}:stdout_sha256={}:stderr_sha256={}",
            verify.status,
            sha256(&verify.stdout),
            sha256(&verify.stderr)
        );
    }

    let receipt = project_root.join(".ggen-v2/receipt.json");
    if !receipt.is_file() {
        bail!("BUILD_BROKEN:GGEN_NATIVE_RECEIPT_MISSING");
    }
    let receipt_bytes = fs::read(&receipt).context("read native ggen receipt")?;
    let receipt_transport_digest = sha256(&receipt_bytes);

    let prefix = project_root
        .file_name()
        .and_then(|name| name.to_str())
        .filter(|name| !name.is_empty())
        .context("REFUSED:PROJECT_ROOT_NAME")?;
    let receipt_artifact_path = format!("{prefix}/.ggen-v2/receipt.json");

    let mut required_artifacts = Vec::new();
    for relative in required_outputs {
        let absolute = project_root.join(relative);
        if !absolute.is_file() {
            bail!(
                "BUILD_BROKEN:GGEN_REQUIRED_OUTPUT_MISSING:{}",
                relative.display()
            );
        }
        let bytes = fs::read(&absolute)
            .with_context(|| format!("read required output {}", absolute.display()))?;
        required_artifacts.push(json!({
            "path": output_path(prefix, relative),
            "role": "ggen:manufactured-output",
            "media_type": "application/octet-stream",
            "digest": sha256(&bytes)
        }));
    }

    Ok(ManufactureEvidence {
        receipt_transport_digest,
        receipt_artifact_path,
        required_artifacts,
        verification_stdout_digest: sha256(&verify.stdout),
    })
}

fn main() -> Result<()> {
    let cli = Cli::parse();
    if cli.revision.len() != 40
        || !cli
            .revision
            .bytes()
            .all(|b| b.is_ascii_hexdigit() && !b.is_ascii_uppercase())
    {
        bail!("REFUSED:REVISION");
    }
    match (&cli.ggen_bin, &cli.project_root) {
        (Some(_), Some(_)) | (None, None) => {}
        _ => bail!("REFUSED:MANUFACTURE_ARGS:ggen-bin and project-root must be paired"),
    }
    if cli.ggen_bin.is_none() && !cli.required_outputs.is_empty() {
        bail!("REFUSED:MANUFACTURE_ARGS:required-output needs native manufacture");
    }

    let input_raw =
        fs::read(&cli.input).with_context(|| format!("read {}", cli.input.display()))?;
    let mut env: Value = serde_json::from_slice(&input_raw).context("REFUSED:INPUT_JSON")?;
    validate_input(&input_raw, &env)?;
    let program_raw =
        fs::read(&cli.program).with_context(|| format!("read {}", cli.program.display()))?;
    let program: Value = serde_json::from_slice(&program_raw).context("REFUSED:PROGRAM_JSON")?;
    if program["schema_version"] != "ggen.enterprise-architecture-foundry.work-program/1" {
        bail!("REFUSED:PROGRAM_SCHEMA");
    }
    let requested = program["initial_solution_packs"]
        .as_array()
        .context("REFUSED:PROGRAM_PACKS")?;
    let kernel = require_string(&program["manufacturing_kernel"], "manufacturing_kernel")?;
    let parent_producer = producer_identity(&env)?;
    let input_digest = sha256(&input_raw);
    let program_digest = sha256(&program_raw);

    let packs = env["packs"]
        .as_array_mut()
        .context("REFUSED:SCHEMA:packs")?;
    let mut seen: BTreeSet<String> = packs
        .iter()
        .filter_map(|item| item.get("name").and_then(Value::as_str).map(str::to_owned))
        .collect();
    for item in requested {
        let name = require_string(item, "initial_solution_packs[]")?;
        if seen.insert(name.to_owned()) {
            packs.push(json!({
                "name": name,
                "version": null,
                "digest": null,
                "admission": "REQUESTED"
            }));
        }
    }

    let native = match (&cli.ggen_bin, &cli.project_root) {
        (Some(ggen_bin), Some(project_root)) => Some(execute_native_manufacture(
            ggen_bin,
            project_root,
            &cli.required_outputs,
        )?),
        _ => None,
    };

    let evidence = env["evidence"]
        .as_array_mut()
        .context("REFUSED:SCHEMA:evidence")?;
    evidence.push(json!({
        "kind": "ggen-foundry-work-program",
        "identity": format!("{}@{}", cli.program.display(), program_digest),
        "digest": program_digest.clone()
    }));

    if let Some(native) = &native {
        evidence.push(json!({
            "kind": "ggen-native-sync-receipt",
            "identity": format!("ggen receipt verify:exit=0:stdout={}", native.verification_stdout_digest),
            "digest": native.receipt_transport_digest
        }));
        let artifacts = env["artifacts"]
            .as_array_mut()
            .context("REFUSED:SCHEMA:artifacts")?;
        artifacts.push(json!({
            "path": native.receipt_artifact_path,
            "role": "ggen:native-sync-receipt",
            "media_type": "application/json",
            "digest": native.receipt_transport_digest
        }));
        artifacts.extend(native.required_artifacts.iter().cloned());
    }

    let labels = env["labels"]
        .as_object_mut()
        .context("REFUSED:SCHEMA:labels")?;
    labels.insert(
        "manufacturing_kernel".into(),
        Value::String(kernel.to_owned()),
    );
    labels.insert(
        "manufacture_adapter".into(),
        Value::String("ggen-foundry-connection".into()),
    );
    labels.insert(
        "native_ggen_sync".into(),
        Value::String(
            if native.is_some() {
                "EXECUTED"
            } else {
                "NOT_EXECUTED"
            }
            .into(),
        ),
    );

    env["stage"] = Value::String("MANUFACTURE".into());
    env["producer"] = json!({
        "repository": "seanchatmangpt/ggen",
        "revision": cli.revision,
        "component": "tools/architecture-foundry:ggen-foundry-connection"
    });
    env["subject"]["kind"] = Value::String(
        if native.is_some() {
            "enterprise-architecture-manufactured-artifact-set"
        } else {
            "enterprise-architecture-manufacture-plan"
        }
        .into(),
    );
    env["subject"]["revision"] = Value::String(
        native
            .as_ref()
            .map(|evidence| evidence.receipt_transport_digest.clone())
            .unwrap_or_else(|| program_digest.clone()),
    );
    env["authority"] = json!({"ceiling": "CONSTRUCT_ONLY", "do_authority": false});
    env["standing"] = if let Some(native) = &native {
        json!({
            "state": "ALIVE",
            "claim": format!(
                "GGEN_SYNC_RUN_EXECUTED; NATIVE_RECEIPT_VERIFY_EXECUTED; REQUIRED_OUTPUTS_VERIFIED={}; EXTERNAL_WORLD_ACTUATION_NOT_GRANTED",
                native.required_artifacts.len()
            )
        })
    } else {
        json!({
            "state": "PARTIAL_ALIVE",
            "claim": "MANUFACTURE_PLAN_BOUND_TO_GGEN_FOUNDRY; GGEN_SYNC_OUTPUT_NOT_EXECUTED_BY_CONNECTION_ADAPTER"
        })
    };
    env["parent"] = json!({"digest": input_digest, "producer": parent_producer});
    env["next"] = json!([{"consumer": "seanchatmangpt/gymact", "operation": "exercise"}]);

    let out_bytes = serde_json::to_vec(&env)?;
    if let Some(parent) = cli.out.parent() {
        fs::create_dir_all(parent)?;
    }
    fs::write(&cli.out, &out_bytes)?;
    println!(
        "{}",
        serde_json::to_string(&json!({
            "stage": "MANUFACTURE",
            "standing": if native.is_some() { "ALIVE" } else { "PARTIAL_ALIVE" },
            "out": cli.out,
            "digest": sha256(&out_bytes),
            "do_authority": false
        }))?
    );
    Ok(())
}
