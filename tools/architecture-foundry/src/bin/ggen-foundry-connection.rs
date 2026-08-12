use anyhow::{bail, Context, Result};
use clap::Parser;
use serde_json::{json, Map, Value};
use sha2::{Digest, Sha256};
use std::collections::BTreeSet;
use std::fs;
use std::path::PathBuf;

const SCHEMA: &str = "urn:ggen:enterprise-connection:v1";

#[derive(Debug, Parser)]
#[command(name = "ggen-foundry-connection", about = "Bind a CATALOG envelope to the ggen architecture-foundry manufacture boundary")]
struct Cli {
    #[arg(long)] input: PathBuf,
    #[arg(long)] program: PathBuf,
    #[arg(long)] revision: String,
    #[arg(long)] out: PathBuf,
}

fn sha256(data: &[u8]) -> String { format!("sha256:{:x}", Sha256::digest(data)) }
fn require_object<'a>(value: &'a Value,name:&str)->Result<&'a Map<String,Value>> { value.as_object().with_context(||format!("REFUSED:SCHEMA:{name}:expected object")) }
fn require_string<'a>(value:&'a Value,name:&str)->Result<&'a str> { value.as_str().filter(|s|!s.is_empty()).with_context(||format!("REFUSED:SCHEMA:{name}:expected non-empty string")) }
fn producer_identity(env:&Value)->Result<String> { let p=require_object(&env["producer"],"producer")?; Ok(format!("{}@{}",require_string(&p["repository"],"producer.repository")?,require_string(&p["revision"],"producer.revision")?)) }
fn validate_input(raw:&[u8],env:&Value)->Result<()> {
    if env["schema"]!=SCHEMA { bail!("REFUSED:SCHEMA_VERSION"); }
    if env["stage"]!="CATALOG" { bail!("REFUSED:TRANSITION:MANUFACTURE_REQUIRES_CATALOG"); }
    if env["authority"]["do_authority"]!=Value::Bool(false) { bail!("REFUSED:AMBIENT_ACTUATION"); }
    if serde_json::to_vec(env)?!=raw { bail!("REFUSED:NON_CANONICAL"); }
    Ok(())
}

fn main()->Result<()> {
    let cli=Cli::parse();
    if cli.revision.len()!=40 || !cli.revision.bytes().all(|b|b.is_ascii_hexdigit()&&!b.is_ascii_uppercase()) { bail!("REFUSED:REVISION"); }
    let input_raw=fs::read(&cli.input).with_context(||format!("read {}",cli.input.display()))?;
    let mut env:Value=serde_json::from_slice(&input_raw).context("REFUSED:INPUT_JSON")?; validate_input(&input_raw,&env)?;
    let program_raw=fs::read(&cli.program).with_context(||format!("read {}",cli.program.display()))?;
    let program:Value=serde_json::from_slice(&program_raw).context("REFUSED:PROGRAM_JSON")?;
    if program["schema_version"]!="ggen.enterprise-architecture-foundry.work-program/1" { bail!("REFUSED:PROGRAM_SCHEMA"); }
    let requested=program["initial_solution_packs"].as_array().context("REFUSED:PROGRAM_PACKS")?;
    let kernel=require_string(&program["manufacturing_kernel"],"manufacturing_kernel")?;
    let parent_producer=producer_identity(&env)?; let input_digest=sha256(&input_raw); let program_digest=sha256(&program_raw);
    let packs=env["packs"].as_array_mut().context("REFUSED:SCHEMA:packs")?;
    let mut seen:BTreeSet<String>=packs.iter().filter_map(|item|item.get("name").and_then(Value::as_str).map(str::to_owned)).collect();
    for item in requested { let name=require_string(item,"initial_solution_packs[]")?; if seen.insert(name.to_owned()) { packs.push(json!({"name":name,"version":null,"digest":null,"admission":"REQUESTED"})); } }
    let evidence=env["evidence"].as_array_mut().context("REFUSED:SCHEMA:evidence")?;
    evidence.push(json!({"kind":"ggen-foundry-work-program","identity":format!("{}@{}",cli.program.display(),program_digest),"digest":program_digest.clone()}));
    let labels=env["labels"].as_object_mut().context("REFUSED:SCHEMA:labels")?;
    labels.insert("manufacturing_kernel".into(),Value::String(kernel.to_owned())); labels.insert("manufacture_adapter".into(),Value::String("ggen-foundry-connection".into()));
    env["stage"]=Value::String("MANUFACTURE".into());
    env["producer"]=json!({"repository":"seanchatmangpt/ggen","revision":cli.revision,"component":"tools/architecture-foundry:ggen-foundry-connection"});
    env["subject"]["kind"]=Value::String("enterprise-architecture-manufacture-plan".into()); env["subject"]["revision"]=Value::String(program_digest.clone());
    env["authority"]=json!({"ceiling":"CONSTRUCT_ONLY","do_authority":false});
    env["standing"]=json!({"state":"PARTIAL_ALIVE","claim":"MANUFACTURE_PLAN_BOUND_TO_GGEN_FOUNDRY; GGEN_SYNC_OUTPUT_NOT_EXECUTED_BY_CONNECTION_ADAPTER"});
    env["parent"]=json!({"digest":input_digest,"producer":parent_producer}); env["next"]=json!([{"consumer":"seanchatmangpt/gymact","operation":"exercise"}]);
    let out_bytes=serde_json::to_vec(&env)?; if let Some(parent)=cli.out.parent(){fs::create_dir_all(parent)?;} fs::write(&cli.out,&out_bytes)?;
    println!("{}",serde_json::to_string(&json!({"stage":"MANUFACTURE","standing":"PARTIAL_ALIVE","out":cli.out,"digest":sha256(&out_bytes),"do_authority":false}))?); Ok(())
}
