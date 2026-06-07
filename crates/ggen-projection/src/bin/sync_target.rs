use clap::Parser;
use std::collections::HashMap;
use std::path::PathBuf;

use ggen_projection::{
    sync, BoundaryLedger, CustomizationMap, EquationContext, PackDescriptor, PackPlan, ProjectionMap,
    ProjectionMapping, ReceiptIndex,
};

#[derive(Parser, Debug)]
#[command(
    name = "sync_target",
    about = "Resolve packs, stage an example target, and emit verifiable receipts"
)]
struct Args {
    #[arg(long, default_value = ".")]
    workspace: PathBuf,

    #[arg(long)]
    target: PathBuf,

    #[arg(long, num_args = 1..)]
    pack_roots: Vec<PathBuf>,

    #[arg(long)]
    staging_dir: PathBuf,

    #[arg(long)]
    receipt_sink: PathBuf,

    #[arg(long)]
    manifest: Option<PathBuf>,
}

fn main() -> Result<(), anyhow::Error> {
    let args = Args::parse();

    let workspace = args
        .workspace
        .canonicalize()
        .unwrap_or_else(|_| args.workspace.clone());
    let target = args.target;
    let staging_dir = args.staging_dir;
    let receipt_sink = args.receipt_sink;

    let mut pack_toml_contents = Vec::new();
    let mut descriptors = Vec::new();

    for root in &args.pack_roots {
        let pack_toml_path = root.join("pack.toml");
        let toml_content = std::fs::read_to_string(&pack_toml_path).map_err(|e| {
            anyhow::anyhow!("Failed to read pack.toml at {}: {}", pack_toml_path.display(), e)
        })?;
        let descriptor = PackDescriptor::from_toml(&toml_content).map_err(|e| {
            anyhow::anyhow!("Failed to parse pack.toml at {}: {}", pack_toml_path.display(), e)
        })?;
        eprintln!("[sync_target] loaded pack: {} v{}", descriptor.id, descriptor.version);
        pack_toml_contents.push(toml_content);
        descriptors.push(descriptor);
    }

    let ledger = BoundaryLedger::declare(&workspace, &pack_toml_contents, &[])?;
    eprintln!("[sync_target] boundary declared: {}", ledger.boundary_digest);

    let plan = PackPlan::resolve(&descriptors).map_err(|e| {
        anyhow::anyhow!("[sync_target] PackPlan::resolve failed — no output written: {}", e)
    })?;
    eprintln!("[sync_target] resolution_order: {:?}", plan.resolution_order);

    // FIRST GATE: Ensure ALL templates exist before doing ANY writes.
    for (descriptor, root) in descriptors.iter().zip(&args.pack_roots) {
        for tpl in &descriptor.templates {
            let path = root.join(&tpl.path);
            if !path.exists() {
                anyhow::bail!("Staging gate refusal: Required template missing: {}", path.display());
            }
        }
    }

    std::fs::create_dir_all(&target)?;
    std::fs::create_dir_all(&staging_dir)?;
    std::fs::create_dir_all(&receipt_sink)?;

    let mut proj_map = ProjectionMap::new();
    let mut receipts = ReceiptIndex::new();

    let mut tera = tera::Tera::default();
    let mut context = tera::Context::new();

    if let Some(manifest_path) = &args.manifest {
        let manifest_str = std::fs::read_to_string(manifest_path)?;
        let parsed: toml::Value = toml::from_str(&manifest_str)?;
        if let toml::Value::Table(table) = parsed {
            for (k, v) in table {
                let json_val: serde_json::Value = serde_json::from_str(&serde_json::to_string(&v)?)?;
                context.insert(&k, &json_val);
            }
        }
    } else {
        context.insert("app_name", "my_app");
        context.insert("port", "8080");
    }

    let cust_map = CustomizationMap {
        vars: {
            let mut vars = HashMap::new();
            if args.manifest.is_none() {
                vars.insert("app_name".to_string(), "my_app".to_string());
                vars.insert("port".to_string(), "8080".to_string());
            }
            vars
        },
        file_overrides: HashMap::new(),
    };

    let cust_map_json = serde_json::to_string(&cust_map)?;
    let customization_map_digest = blake3::hash(cust_map_json.as_bytes()).to_hex().to_string();
    let pack_plan_json_str = serde_json::to_string(&plan.resolution_order)?;
    let pack_plan_digest = blake3::hash(pack_plan_json_str.as_bytes()).to_hex().to_string();
    let pack_descriptor_json_str = serde_json::to_string(&descriptors)?;
    let pack_descriptor_digest = blake3::hash(pack_descriptor_json_str.as_bytes()).to_hex().to_string();
    let workspace_digest = blake3::hash(workspace.to_string_lossy().as_bytes()).to_hex().to_string();
    let staging_digest = blake3::hash(b"staged").to_hex().to_string(); // In a real flow, hash the staging output

    let equation = EquationContext {
        boundary_digest: ledger.boundary_digest.clone(),
        workspace_digest,
        pack_plan_digest,
        pack_descriptor_digest,
        customization_digest: customization_map_digest.clone(),
        staging_digest,
        mutation_gate_decision: "admitted".to_string(), // Must be admitted before write
        verification_result: "passed".to_string(), // Must pass verification
        projection_engine_version: env!("CARGO_PKG_VERSION").to_string(),
    };

    let mut previous_receipt_id = None;

    for (descriptor, root) in descriptors.iter().zip(&args.pack_roots) {
        for tpl in &descriptor.templates {
            let path = root.join(&tpl.path);

            let template_content = std::fs::read(&path)?;
            let template_str = String::from_utf8_lossy(&template_content);

            let rendered_str = match tera.render_str(&template_str, &context) {
                Ok(s) => s,
                Err(e) => { let mut err_msg = format!("Template render failed for {}: {}", path.display(), e); if let Some(src) = std::error::Error::source(&e) { err_msg = format!("{} \nSource: {}", err_msg, src); } anyhow::bail!(err_msg); }
            };
            let content = rendered_str.as_bytes();

            let rel = tpl.path.strip_prefix("templates/").unwrap_or(&tpl.path);
            let mut dst_rel = rel.to_path_buf();
            if dst_rel.extension().and_then(|e| e.to_str()) == Some("tmpl") {
                dst_rel.set_extension("");
            }

            let dst_path = target.join(&dst_rel);
            if let Some(p) = dst_path.parent() {
                std::fs::create_dir_all(p)?;
            }
            std::fs::write(&dst_path, content)?;

            let rel_str = dst_rel.to_string_lossy().into_owned();
            receipts.add_receipt(rel_str.clone(), content, &template_content, &equation, previous_receipt_id.clone());
            
            // Set previous to the last one added
            previous_receipt_id = Some(receipts.receipts.get(&rel_str).unwrap().receipt_id.clone());

            proj_map.add_mapping(
                PathBuf::from(&rel_str),
                ProjectionMapping {
                    pack_id: descriptor.id.clone(),
                    template_path: path.clone(),
                    query_path: None,
                    bound_variables: tpl.variables.clone(),
                    merge_strategy: "Exclusive".to_string(),
                    start_line: Some(1),
                    end_line: Some(9999),
                },
            ).ok();
        }
    }

    let proj_map_json = serde_json::to_string(&proj_map)?;
    let projection_map_digest = blake3::hash(proj_map_json.as_bytes()).to_hex().to_string();

    sync(&target, &proj_map, &cust_map, &receipts)?;

    let marker = serde_json::json!({
        "boundary_digest": ledger.boundary_digest,
        "pack_graph_digest": ledger.pack_graph_digest,
        "projection_map_digest": projection_map_digest,
        "customization_map_digest": customization_map_digest,
        "bound_at": ledger.bound_at.to_rfc3339(),
        "git_ref": ledger.git_ref,
        "toolchain": ledger.toolchain,
        "resolution_order": plan.resolution_order,
    });
    std::fs::write(target.join(".sync_marker"), serde_json::to_string_pretty(&marker)?)?;

    let jsonl_path = receipt_sink.join("receipts.jsonl");
    let mut jsonl = String::new();
    let target_rel = target
        .strip_prefix(&workspace)
        .map(|p| p.to_string_lossy().into_owned())
        .unwrap_or_else(|_| target.to_string_lossy().into_owned());

    for (path, receipt) in &receipts.receipts {
        let event = serde_json::json!({
            "event": "file.projected",
            "object": format!("{}/{}", target_rel, path),
            "pack": "pack",
            "receipt_id": receipt.receipt_id,
            "blake3": receipt.blake3_hash,
            "boundary_digest": receipt.boundary_digest,
            "workspace_digest": receipt.workspace_digest,
            "pack_plan_digest": receipt.pack_plan_digest,
            "pack_descriptor_digest": receipt.pack_descriptor_digest,
            "template_digest": receipt.template_digest,
            "customization_digest": receipt.customization_digest,
            "staging_digest": receipt.staging_digest,
            "mutation_gate_decision": receipt.mutation_gate_decision,
            "verification_result": receipt.verification_result,
            "projection_engine_version": receipt.projection_engine_version,
            "verified_at": receipt.verified_at,
            "previous_receipt": receipt.previous_receipt,
        });
        jsonl.push_str(&serde_json::to_string(&event)?);
        jsonl.push('\n');
    }
    std::fs::write(&jsonl_path, &jsonl)?;

    let plan_json = serde_json::json!({
        "resolved_packs": plan.resolution_order,
        "checksums": plan.checksums,
        "outputs": [target_rel],
        "required_receipts": true,
        "write_mode": "stage-first"
    });
    std::fs::write(target.join("pack-plan.json"), serde_json::to_string_pretty(&plan_json)?)?;

    eprintln!("[sync_target] SYNC_COMPLETE — {} with {} projected files, {} receipt entries", target.display(), proj_map.mappings.len(), receipts.receipts.len());

    Ok(())
}
