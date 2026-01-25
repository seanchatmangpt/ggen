use anyhow::{anyhow, bail, Context, Result};
use base64::Engine;
use chrono::{DateTime, Utc};
use clap::Parser;
use reqwest::StatusCode;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};
use std::{collections::BTreeMap, path::PathBuf};
use tokio::process::Command;

#[derive(Parser, Debug)]
#[command(name = "autonomics-catalog-controller")]
struct Args {
    /// Git repo URL containing ontology + templates + infra
    #[arg(long, env = "CATALOG_REPO")]
    repo: String,

    /// Branch/tag
    #[arg(long, env = "CATALOG_BRANCH", default_value = "main")]
    branch: String,

    /// GCP project id
    #[arg(long, env = "GOOGLE_CLOUD_PROJECT")]
    project_id: String,

    /// Region (Cloud Run + Terraform defaults)
    #[arg(long, env = "REGION", default_value = "us-central1")]
    region: String,

    /// GCS bucket for Terraform state
    #[arg(long, env = "TF_STATE_BUCKET")]
    tf_state_bucket: String,

    /// Terraform working dir inside repo
    #[arg(long, env = "TF_WORKDIR", default_value = "infra/catalog")]
    tf_workdir: String,

    /// Firestore collection for controller receipts
    #[arg(long, env = "CATALOG_FIRESTORE_COLLECTION", default_value = "catalog_runs")]
    firestore_collection: String,

    /// Optional: verify deployed service health URLs listed in infra outputs
    #[arg(long, env = "VERIFY_HEALTH", default_value_t = true)]
    verify_health: bool,

    /// Optional: call Cloud Build per SKU (expects infra/cloudbuild/cloudbuild.yaml in repo)
    #[arg(long, env = "TRIGGER_CLOUDBUILD", default_value_t = true)]
    trigger_cloudbuild: bool,
}

#[derive(Serialize, Deserialize, Debug, Clone)]
struct Receipt {
    ts: DateTime<Utc>,
    kind: String,
    decision: String,
    project_id: String,
    repo: String,
    branch: String,
    details: serde_json::Value,

    // chaining
    prev_chain_hash_b64: String,
}

#[derive(Debug, Clone)]
struct ReceiptLedger {
    project_id: String,
    collection: String,
    http: reqwest::Client,
}

impl ReceiptLedger {
    fn new(project_id: String, collection: String) -> Self {
        Self {
            project_id,
            collection,
            http: reqwest::Client::new(),
        }
    }

    async fn access_token(&self) -> Result<String> {
        // Cloud Run metadata server
        let url = "http://metadata.google.internal/computeMetadata/v1/instance/service-accounts/default/token";
        let resp = self
            .http
            .get(url)
            .header("Metadata-Flavor", "Google")
            .timeout(std::time::Duration::from_secs(3))
            .send()
            .await
            .context("metadata token request failed")?;

        if resp.status() != StatusCode::OK {
            bail!("metadata token status={}", resp.status());
        }
        let v: serde_json::Value = resp.json().await?;
        let tok = v
            .get("access_token")
            .and_then(|x| x.as_str())
            .ok_or_else(|| anyhow!("missing access_token"))?;
        Ok(tok.to_string())
    }

    fn canonical_json(value: &serde_json::Value) -> Vec<u8> {
        // Deterministic canonicalization: stable key ordering via BTreeMap recursion.
        fn canon(v: &serde_json::Value) -> serde_json::Value {
            match v {
                serde_json::Value::Object(map) => {
                    let mut bt = BTreeMap::new();
                    for (k, vv) in map.iter() {
                        bt.insert(k.clone(), canon(vv));
                    }
                    serde_json::Value::Object(bt.into_iter().collect())
                }
                serde_json::Value::Array(arr) => {
                    serde_json::Value::Array(arr.iter().map(canon).collect())
                }
                _ => v.clone(),
            }
        }
        serde_json::to_vec(&canon(value)).expect("canonical json encode")
    }

    fn hash_receipt_json(receipt_json: &serde_json::Value) -> [u8; 32] {
        let mut hasher = Sha256::new();
        hasher.update(Self::canonical_json(receipt_json));
        let out = hasher.finalize();
        let mut h = [0u8; 32];
        h.copy_from_slice(&out);
        h
    }

    fn hash_chain(prev: [u8; 32], receipt_json: &serde_json::Value) -> [u8; 32] {
        let rh = Self::hash_receipt_json(receipt_json);
        let mut hasher = Sha256::new();
        hasher.update(prev);
        hasher.update(rh);
        let out = hasher.finalize();
        let mut h = [0u8; 32];
        h.copy_from_slice(&out);
        h
    }

    async fn get_last_hash(&self, token: &str, run_id: &str) -> Result<[u8; 32]> {
        // doc: {collection}/{run_id}/state/head
        let url = format!(
            "https://firestore.googleapis.com/v1/projects/{}/databases/(default)/documents/{}/{}/state/head",
            self.project_id, self.collection, run_id
        );

        let resp = self
            .http
            .get(&url)
            .bearer_auth(token)
            .timeout(std::time::Duration::from_secs(5))
            .send()
            .await?;

        if resp.status() == StatusCode::NOT_FOUND {
            return Ok([0u8; 32]);
        }
        if resp.status() != StatusCode::OK {
            let body = resp.text().await.unwrap_or_default();
            bail!("firestore head get status={} body={}", resp.status(), body);
        }

        let doc: serde_json::Value = resp.json().await?;
        let last_b64 = doc
            .get("fields")
            .and_then(|f| f.get("last_chain_hash"))
            .and_then(|v| v.get("stringValue"))
            .and_then(|s| s.as_str())
            .unwrap_or("");

        if last_b64.is_empty() {
            return Ok([0u8; 32]);
        }
        let bytes = base64::engine::general_purpose::STANDARD
            .decode(last_b64)
            .context("decode last_chain_hash b64")?;

        if bytes.len() != 32 {
            bail!("last_chain_hash wrong len {}", bytes.len());
        }
        let mut h = [0u8; 32];
        h.copy_from_slice(&bytes);
        Ok(h)
    }

    async fn put_last_hash(&self, token: &str, run_id: &str, chain_hash: [u8; 32]) -> Result<()> {
        let url = format!(
            "https://firestore.googleapis.com/v1/projects/{}/databases/(default)/documents/{}/{}/state/head?updateMask.fieldPaths=last_chain_hash",
            self.project_id, self.collection, run_id
        );

        let body = serde_json::json!({
          "fields": {
            "last_chain_hash": { "stringValue": base64::engine::general_purpose::STANDARD.encode(chain_hash) }
          }
        });

        let resp = self
            .http
            .post(&url)
            .bearer_auth(token)
            .json(&body)
            .timeout(std::time::Duration::from_secs(8))
            .send()
            .await?;

        if !(resp.status() == StatusCode::OK || resp.status() == StatusCode::CREATED) {
            let t = resp.text().await.unwrap_or_default();
            bail!("firestore head put status={} body={}", resp.status(), t);
        }
        Ok(())
    }

    async fn append_receipt(
        &self,
        token: &str,
        run_id: &str,
        receipt_json: serde_json::Value,
        chain_hash: [u8; 32],
    ) -> Result<()> {
        let ts_ms = Utc::now().timestamp_millis();
        let doc_id = format!(
            "{}_{}",
            ts_ms,
            base64::engine::general_purpose::STANDARD.encode(chain_hash)
        );

        let url = format!(
            "https://firestore.googleapis.com/v1/projects/{}/databases/(default)/documents/{}/{}/receipts?documentId={}",
            self.project_id, self.collection, run_id, doc_id
        );

        let body = serde_json::json!({
          "fields": {
            "ts_ms": { "integerValue": ts_ms.to_string() },
            "chain_hash": { "stringValue": base64::engine::general_purpose::STANDARD.encode(chain_hash) },
            "receipt_json": { "stringValue": serde_json::to_string(&receipt_json)? }
          }
        });

        let resp = self
            .http
            .post(&url)
            .bearer_auth(token)
            .json(&body)
            .timeout(std::time::Duration::from_secs(8))
            .send()
            .await?;

        if !(resp.status() == StatusCode::OK || resp.status() == StatusCode::CREATED) {
            let t = resp.text().await.unwrap_or_default();
            bail!("firestore append status={} body={}", resp.status(), t);
        }

        Ok(())
    }

    async fn emit(
        &self,
        run_id: &str,
        kind: &str,
        decision: &str,
        base: &Args,
        details: serde_json::Value,
    ) -> Result<()> {
        let token = self.access_token().await?;

        let prev = self.get_last_hash(&token, run_id).await?;
        let prev_b64 = base64::engine::general_purpose::STANDARD.encode(prev);

        let receipt = Receipt {
            ts: Utc::now(),
            kind: kind.to_string(),
            decision: decision.to_string(),
            project_id: base.project_id.clone(),
            repo: base.repo.clone(),
            branch: base.branch.clone(),
            details,
            prev_chain_hash_b64: prev_b64,
        };

        let receipt_json = serde_json::to_value(&receipt)?;
        let chain = Self::hash_chain(prev, &receipt_json);

        self.append_receipt(&token, run_id, receipt_json, chain).await?;
        self.put_last_hash(&token, run_id, chain).await?;

        // Cloud Logging picks stdout JSON
        println!(
            "{}",
            serde_json::json!({
              "run_id": run_id,
              "kind": kind,
              "decision": decision,
              "chain_hash_b64": base64::engine::general_purpose::STANDARD.encode(chain)
            })
        );

        Ok(())
    }
}

async fn run_cmd(cmd: &mut Command) -> Result<()> {
    let status = cmd.status().await.context("spawn command")?;
    if !status.success() {
        bail!("command failed status={:?}", status);
    }
    Ok(())
}

async fn run_cmd_capture(cmd: &mut Command) -> Result<String> {
    let out = cmd.output().await.context("spawn command")?;
    if !out.status.success() {
        bail!(
            "command failed status={:?} stderr={}",
            out.status,
            String::from_utf8_lossy(&out.stderr)
        );
    }
    Ok(String::from_utf8_lossy(&out.stdout).trim().to_string())
}

#[tokio::main]
async fn main() -> Result<()> {
    let args = Args::parse();
    let run_id = format!("run_{}", Utc::now().format("%Y%m%dT%H%M%SZ"));

    let ledger = ReceiptLedger::new(
        args.project_id.clone(),
        args.firestore_collection.clone(),
    );

    // Start receipt (jidoka if Firestore unavailable)
    ledger
        .emit(
            &run_id,
            "catalog_run_started",
            "accept",
            &args,
            serde_json::json!({}),
        )
        .await
        .context("emit start receipt failed")?;

    let workdir = tempfile_dir()?;
    let repo_dir = workdir.join("repo");

    // 1) clone repo
    ledger
        .emit(
            &run_id,
            "git_clone",
            "attempt",
            &args,
            serde_json::json!({"dir": repo_dir}),
        )
        .await?;
    run_cmd(
        Command::new("git")
            .arg("clone")
            .arg("--depth=1")
            .arg("--branch")
            .arg(&args.branch)
            .arg(&args.repo)
            .arg(&repo_dir),
    )
    .await
    .context("git clone failed")?;
    ledger
        .emit(
            &run_id,
            "git_clone",
            "ok",
            &args,
            serde_json::json!({}),
        )
        .await?;

    // 2) run ggen sync_ontology (real generator)
    ledger
        .emit(
            &run_id,
            "ggen_sync_ontology",
            "attempt",
            &args,
            serde_json::json!({}),
        )
        .await?;
    run_cmd(
        Command::new("ggen")
            .current_dir(&repo_dir)
            .arg("sync_ontology"),
    )
    .await
    .context("ggen sync_ontology failed")?;
    ledger
        .emit(
            &run_id,
            "ggen_sync_ontology",
            "ok",
            &args,
            serde_json::json!({}),
        )
        .await?;

    // 3) terraform init/apply (catalog root)
    let tf_dir = repo_dir.join(&args.tf_workdir);

    ledger
        .emit(
            &run_id,
            "terraform_init",
            "attempt",
            &args,
            serde_json::json!({"dir": &tf_dir}),
        )
        .await?;
    run_cmd(
        Command::new("terraform")
            .current_dir(&tf_dir)
            .arg("init")
            .arg("-input=false")
            .arg("-upgrade")
            .arg(format!("-backend-config=bucket={}", args.tf_state_bucket))
            .arg("-backend-config=prefix=autonomics/catalog"),
    )
    .await
    .context("terraform init failed")?;
    ledger
        .emit(
            &run_id,
            "terraform_init",
            "ok",
            &args,
            serde_json::json!({}),
        )
        .await?;

    ledger
        .emit(
            &run_id,
            "terraform_apply",
            "attempt",
            &args,
            serde_json::json!({}),
        )
        .await?;
    run_cmd(
        Command::new("terraform")
            .current_dir(&tf_dir)
            .arg("apply")
            .arg("-auto-approve")
            .arg("-input=false")
            .arg(format!("-var=project_id={}", args.project_id))
            .arg(format!("-var=region={}", args.region)),
    )
    .await
    .context("terraform apply failed")?;
    ledger
        .emit(
            &run_id,
            "terraform_apply",
            "ok",
            &args,
            serde_json::json!({}),
        )
        .await?;

    // 4) optionally trigger Cloud Build (build+deploy SKUs)
    if args.trigger_cloudbuild {
        ledger
            .emit(
                &run_id,
                "cloudbuild_submit",
                "attempt",
                &args,
                serde_json::json!({}),
            )
            .await?;
        // Cloud Build runs from repo root. Must contain infra/cloudbuild/cloudbuild.yaml
        run_cmd(
            Command::new("gcloud")
                .current_dir(&repo_dir)
                .arg("builds")
                .arg("submit")
                .arg("--config")
                .arg("infra/cloudbuild/cloudbuild.yaml")
                .arg("--project")
                .arg(&args.project_id),
        )
        .await
        .context("cloudbuild submit failed")?;
        ledger
            .emit(
                &run_id,
                "cloudbuild_submit",
                "ok",
                &args,
                serde_json::json!({}),
            )
            .await?;
    }

    // 5) verify health for deployed services (read terraform output)
    if args.verify_health {
        ledger
            .emit(
                &run_id,
                "verify_health",
                "attempt",
                &args,
                serde_json::json!({}),
            )
            .await?;

        // expects outputs sku_service_urls as map
        let output_json = run_cmd_capture(
            Command::new("terraform")
                .current_dir(&tf_dir)
                .arg("output")
                .arg("-json"),
        )
        .await
        .context("terraform output -json failed")?;

        let outv: serde_json::Value = serde_json::from_str(&output_json)?;
        if let Some(urls) = outv.get("sku_service_urls").and_then(|v| v.get("value")) {
            if let Some(map) = urls.as_object() {
                let http = reqwest::Client::new();
                for (k, urlv) in map {
                    let url = urlv.as_str().unwrap_or("");
                    if url.is_empty() {
                        bail!("empty service url for {}", k);
                    }
                    let health = format!("{}/health", url);
                    let resp = http
                        .get(&health)
                        .timeout(std::time::Duration::from_secs(6))
                        .send()
                        .await
                        .with_context(|| format!("GET {}", health))?;

                    if resp.status() != StatusCode::OK {
                        let body = resp.text().await.unwrap_or_default();
                        bail!(
                            "health failed sku={} status={} body={}",
                            k, resp.status(), body
                        );
                    }
                }
            }
        }

        ledger
            .emit(
                &run_id,
                "verify_health",
                "ok",
                &args,
                serde_json::json!({}),
            )
            .await?;
    }

    ledger
        .emit(
            &run_id,
            "catalog_run_completed",
            "accept",
            &args,
            serde_json::json!({}),
        )
        .await?;

    Ok(())
}

fn tempfile_dir() -> Result<PathBuf> {
    let base = std::env::temp_dir();
    let name = format!("autonomics_controller_{}", Utc::now().timestamp_millis());
    let dir = base.join(name);
    std::fs::create_dir_all(&dir)?;
    Ok(dir)
}
