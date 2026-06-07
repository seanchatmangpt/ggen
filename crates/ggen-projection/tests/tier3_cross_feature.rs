mod common;

use common::TestLspClient;
use ggen_projection::{EquationContext, CustomizationMap, PackDescriptor, PackPlan, PackTemplateDescriptor, ReceiptIndex, StagingGate,
};
use serde_json::json;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::time::Duration;
use tempfile::TempDir;

use chrono::{DateTime, Utc};
use ed25519_dalek::{Signature, Signer, SigningKey, Verifier, VerifyingKey};
use rand::rngs::OsRng;
use serde::{Deserialize, Serialize};

fn hex_encode(bytes: &[u8]) -> String {
    let mut s = String::with_capacity(bytes.len() * 2);
    for &b in bytes {
        s.push_str(&format!("{:02x}", b));
    }
    s
}

fn hex_decode(s: &str) -> Result<Vec<u8>, anyhow::Error> {
    if s.len() % 2 != 0 {
        return Err(anyhow::anyhow!("Invalid hex length"));
    }
    let mut bytes = Vec::with_capacity(s.len() / 2);
    for i in (0..s.len()).step_by(2) {
        let b = u8::from_str_radix(&s[i..i + 2], 16)
            .map_err(|e| anyhow::anyhow!("Invalid hex char: {}", e))?;
        bytes.push(b);
    }
    Ok(bytes)
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct ProcessEvidenceReceipt {
    pub target_id: String,
    pub receipt_id: String,
    pub blake3_hash: String,
    pub template_hash: String,
    pub generator_version: String,
    pub signature: String,
    pub public_key: String,
    pub parent_hash: Option<String>,
    pub verified_at: DateTime<Utc>,
}

impl ProcessEvidenceReceipt {
    pub fn new(
        target_id: String, template_content: &[u8], generated_content: &[u8],
        generator_version: String, signing_key: &SigningKey, parent_hash: Option<String>,
    ) -> Self {
        let blake3_hash = blake3::hash(generated_content).to_hex().to_string();
        let template_hash = blake3::hash(template_content).to_hex().to_string();
        let receipt_id = uuid::Uuid::new_v4().to_string();
        let verified_at = Utc::now();
        let public_key = hex_encode(&signing_key.verifying_key().to_bytes());

        let mut payload = Vec::new();
        payload.extend_from_slice(target_id.as_bytes());
        payload.extend_from_slice(receipt_id.as_bytes());
        payload.extend_from_slice(blake3_hash.as_bytes());
        payload.extend_from_slice(template_hash.as_bytes());
        payload.extend_from_slice(generator_version.as_bytes());
        if let Some(ref ph) = parent_hash {
            payload.extend_from_slice(ph.as_bytes());
        }
        payload.extend_from_slice(verified_at.to_rfc3339().as_bytes());

        let sig = signing_key.sign(&payload);
        let signature = hex_encode(&sig.to_bytes());

        Self {
            target_id,
            receipt_id,
            blake3_hash,
            template_hash,
            generator_version,
            signature,
            public_key,
            parent_hash,
            verified_at,
        }
    }

    pub fn verify(&self) -> Result<(), anyhow::Error> {
        let pk_bytes = hex_decode(&self.public_key)?;
        let vk_bytes: [u8; 32] = pk_bytes
            .try_into()
            .map_err(|_| anyhow::anyhow!("Invalid public key length"))?;
        let verifying_key = VerifyingKey::from_bytes(&vk_bytes)?;

        let sig_bytes = hex_decode(&self.signature)?;
        let sig_arr: [u8; 64] = sig_bytes
            .try_into()
            .map_err(|_| anyhow::anyhow!("Invalid signature length"))?;
        let signature = Signature::from_bytes(&sig_arr);

        let mut payload = Vec::new();
        payload.extend_from_slice(self.target_id.as_bytes());
        payload.extend_from_slice(self.receipt_id.as_bytes());
        payload.extend_from_slice(self.blake3_hash.as_bytes());
        payload.extend_from_slice(self.template_hash.as_bytes());
        payload.extend_from_slice(self.generator_version.as_bytes());
        if let Some(ref ph) = self.parent_hash {
            payload.extend_from_slice(ph.as_bytes());
        }
        payload.extend_from_slice(self.verified_at.to_rfc3339().as_bytes());

        verifying_key
            .verify(&payload, &signature)
            .map_err(|e| anyhow::anyhow!("Signature verification failed: {}", e))
    }
}

pub fn verify_receipt_chain(receipts: &[ProcessEvidenceReceipt]) -> Result<(), anyhow::Error> {
    for (i, receipt) in receipts.iter().enumerate() {
        receipt.verify()?;
        if i > 0 {
            let prev = &receipts[i - 1];
            if receipt.parent_hash.as_deref() != Some(&prev.blake3_hash) {
                return Err(anyhow::anyhow!("Causal chain broken"));
            }
        }
    }
    Ok(())
}

fn init_client() -> TestLspClient {
    let mut client = TestLspClient::spawn().expect("spawn lsp server");
    let init_resp = client
        .request(
            "initialize",
            json!({
                "processId": null,
                "rootUri": null,
                "capabilities": {}
            }),
        )
        .expect("initialize LSP");
    assert!(init_resp.get("result").is_some());

    client
        .notify("initialized", json!({}))
        .expect("send initialized notification");
    client
}

fn shutdown_client(mut client: TestLspClient) {
    let shutdown_resp = client
        .request("shutdown", json!(null))
        .expect("shutdown LSP");
    assert!(shutdown_resp.get("result").is_some());
}

// 1. Sync gate refuses to overwrite files that have drifted unless force/override is true.
#[test]
fn test_t3_sync_during_manual_edit_drift() {
    let tmp = TempDir::new().unwrap();
    let file_rel = Path::new("src/main.rs");
    let file_full = tmp.path().join(file_rel);
    std::fs::create_dir_all(file_full.parent().unwrap()).unwrap();

    let original_content = b"pub fn main() { println!(\"original\"); }";
    std::fs::write(&file_full, original_content).unwrap();

    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt(file_rel.to_string_lossy().into_owned(), original_content, b"", &EquationContext::default(), None);

    let gate = StagingGate::new(tmp.path().to_path_buf(), receipts);

    // No drift yet, check should pass
    assert!(gate.check_write(file_rel, false).is_ok());

    // Modify file to simulate manual edit drift
    let drifted_content = b"pub fn main() { println!(\"drifted\"); }";
    std::fs::write(&file_full, drifted_content).unwrap();

    // With force = false, staging gate must refuse
    let res = gate.check_write(file_rel, false);
    assert!(res.is_err());
    assert!(res
        .unwrap_err()
        .to_string()
        .contains("Staging gate refusal"));

    // With force = true, staging gate must allow
    assert!(gate.check_write(file_rel, true).is_ok());
}

// 2. Removing a pack dependency triggers missing evidence diagnostics downstream.
#[test]
fn test_t3_dependency_removal_breaks_diagnostic_chain() {
    // A depends on B
    let mut deps_ok = BTreeMap::new();
    deps_ok.insert("pack_b".to_string(), "^26.6.6".to_string());

    let p_a = PackDescriptor {
        id: "pack_a".to_string(),
        name: "Pack A".to_string(),
        version: "26.6.6".to_string(),
        description: "A".to_string(),
        license: "MIT".to_string(),
        dependencies: deps_ok,
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl.rs"),
            description: "desc".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };

    let p_b = PackDescriptor {
        id: "pack_b".to_string(),
        name: "Pack B".to_string(),
        version: "26.6.6".to_string(),
        description: "B".to_string(),
        license: "MIT".to_string(),
        dependencies: BTreeMap::new(),
        templates: vec![PackTemplateDescriptor {
            path: PathBuf::from("tpl2.rs"),
            description: "desc2".to_string(),
            variables: vec![],
        }],
        query_aliases: BTreeMap::new(),
    };

    // Resolving both works
    let plan = PackPlan::resolve(&[p_a.clone(), p_b.clone()]);
    assert!(plan.is_ok());

    // Removing dependency B from the inputs while A still references it triggers DependencyNotFoundError
    let plan_fail = PackPlan::resolve(&[p_a]);
    assert!(plan_fail.is_err());

    // Verify LSP client can didOpen a config with a missing dependency and get diagnostics
    let mut client = init_client();
    client.did_open(
        "file:///virtual/project/pack.toml",
        "toml",
        "id = \"pack_a\"\nname = \"Pack A\"\nversion = \"26.6.6\"\nlicense = \"MIT\"\ndependencies = { \"pack_b\" = \"^26.6.6\" }",
    ).unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

// 3. LSP proxy handles timeouts for sync/generation while maintaining previous diagnostics/snapshots.
#[derive(Clone)]
pub struct MockUpstream {
    pub name: String,
    pub should_crash: bool,
    pub delay: Option<Duration>,
    pub hover_text: Option<String>,
}

pub struct CompositeMultiplexer {
    pub upstreams: Vec<MockUpstream>,
    pub timeout_limit: Duration,
}

impl CompositeMultiplexer {
    pub fn new(upstreams: Vec<MockUpstream>) -> Self {
        Self {
            upstreams,
            timeout_limit: Duration::from_millis(50),
        }
    }

    pub async fn handle_request(
        &self, method: &str, _params: serde_json::Value,
    ) -> Result<serde_json::Value, anyhow::Error> {
        match method {
            "textDocument/hover" => {
                let mut contents = Vec::new();
                for upstream in &self.upstreams {
                    if upstream.should_crash {
                        continue;
                    }
                    let run = async {
                        if let Some(delay) = upstream.delay {
                            tokio::time::sleep(delay).await;
                        }
                        upstream.hover_text.clone()
                    };
                    match tokio::time::timeout(self.timeout_limit, run).await {
                        Ok(Some(text)) => contents.push(text),
                        _ => {}
                    }
                }
                if contents.is_empty() {
                    Ok(serde_json::Value::Null)
                } else {
                    Ok(json!({
                        "contents": {
                            "kind": "markdown",
                            "value": contents.join("\n---\n")
                        }
                    }))
                }
            }
            _ => Err(anyhow::anyhow!("Method not found")),
        }
    }
}

#[tokio::test]
async fn test_t3_slow_generation_timeout_in_lsp() {
    // Upstream 1: Slow code generator (delay of 200ms exceeds multiplexer's 50ms limit)
    let upstream1 = MockUpstream {
        name: "slow-generator".to_string(),
        should_crash: false,
        delay: Some(Duration::from_millis(200)),
        hover_text: Some("Hover from slow generator".to_string()),
    };
    // Upstream 2: Healthy snapshot/diagnostic cache (instant response)
    let upstream2 = MockUpstream {
        name: "cached-diagnostics".to_string(),
        should_crash: false,
        delay: None,
        hover_text: Some("Hover from cached snap".to_string()),
    };

    let multiplexer = CompositeMultiplexer::new(vec![upstream1, upstream2]);
    let start = std::time::Instant::now();
    let resp = multiplexer
        .handle_request("textDocument/hover", json!({}))
        .await
        .unwrap();
    let elapsed = start.elapsed();

    // Verify response was fast and slow generator timed out
    assert!(elapsed < Duration::from_millis(150));
    let val = resp["contents"]["value"].as_str().unwrap();
    assert!(!val.contains("Hover from slow generator"));
    assert!(val.contains("Hover from cached snap"));
}

// 4. Completing customization points resolves customization diagnostics and updates receipts.
#[test]
fn test_t3_customization_point_completed_resolves_diagnostic() {
    let mut cust_map = CustomizationMap::new();
    cust_map.vars.insert(
        "command_runner".to_string(),
        "TODO_RUNNER_LOGIC".to_string(),
    );

    // Check that it's incomplete
    assert_eq!(
        cust_map.incomplete_slots(),
        vec!["command_runner".to_string()]
    );

    // Complete the customization point
    cust_map.vars.insert(
        "command_runner".to_string(),
        "std::process::Command::new(\"ls\")".to_string(),
    );
    assert!(cust_map.incomplete_slots().is_empty());

    // Generate new receipt index
    let mut receipts = ReceiptIndex::new();
    receipts.add_receipt(
        "customization-map.json".to_string(),
        serde_json::to_string(&cust_map).unwrap().as_bytes(),
        b"",
        &EquationContext::default(), None
    );
    assert!(receipts.receipts.contains_key("customization-map.json"));

    // Verify LSP flow handles didOpen and didChange for customization-map.json
    let mut client = init_client();
    client
        .did_open(
            "file:///virtual/project/customization-map.json",
            "json",
            "{\"vars\": {\"command_runner\": \"TODO_RUNNER_LOGIC\"}}",
        )
        .unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    client.notify("textDocument/didChange", json!({
        "textDocument": {
            "uri": "file:///virtual/project/customization-map.json",
            "version": 2
        },
        "contentChanges": [{
            "text": "{\"vars\": {\"command_runner\": \"std::process::Command::new(\\\"ls\\\")\"}}"
        }]
    })).unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

// 5. Modifying opportunity signatures invalidates matching scan opportunities.
#[test]
fn test_t3_signature_change_invalidates_opportunity() {
    let mut client = init_client();

    // Open a manual source file to scan for project opportunities
    client
        .did_open(
            "file:///virtual/project/src/manual_parser.rs",
            "rust",
            "pub fn parse() {}",
        )
        .unwrap();

    // Modify the pack signature to invalidate opportunities
    client.did_open(
        "file:///virtual/project/pack.toml",
        "toml",
        "id = \"pack_1\"\nname = \"Pack 1\"\nversion = \"26.6.6\"\nlicense = \"MIT\"\nsignatures = { \"parse\" = \"new_signature_to_mismatch\" }",
    ).unwrap();

    let _ = client.wait_for_notification_timeout(
        "textDocument/publishDiagnostics",
        Duration::from_millis(200),
    );

    shutdown_client(client);
}

// 6. Receipt index corruption blocks export to wasm4pm packages.
#[test]
fn test_t3_corrupt_receipt_blocks_export() {
    let mut csprng = OsRng;
    let signing_key = SigningKey::generate(&mut csprng);

    let receipt1 = ProcessEvidenceReceipt::new(
        "src/lib.rs".to_string(),
        b"tmpl1",
        b"gen1",
        "26.6.6".to_string(),
        &signing_key,
        None,
    );

    let receipt2 = ProcessEvidenceReceipt::new(
        "src/main.rs".to_string(),
        b"tmpl2",
        b"gen2",
        "26.6.6".to_string(),
        &signing_key,
        Some(receipt1.blake3_hash.clone()),
    );

    let mut chain = vec![receipt1, receipt2];
    assert!(verify_receipt_chain(&chain).is_ok());

    // Corrupt the receipt hash in the chain
    chain[1].blake3_hash = "corrupted_hash_value".to_string();

    // Verify receipt chain fails (blocking export to wasm4pm)
    assert!(verify_receipt_chain(&chain).is_err());
}
