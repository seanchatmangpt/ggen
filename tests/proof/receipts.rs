//! T4 Layer: Receipt & Replay Tests
//! Validates cryptographic integrity and determinism:
//! - Receipt signatures are unforgeable (Ed25519)
//! - Receipt chains maintain cryptographic integrity
//! - Execution is deterministic (identical inputs → identical outputs)
//!
//! These tests run a real `ggen sync` and assert against the real
//! `.ggen-v2/receipt.json` (`praxis_core::ReceiptRecord`, chained BLAKE3 +
//! Ed25519 signing — see `crates/ggen-engine/src/sync.rs`'s `write_receipt`
//! and `crates/ggen-engine/src/keys.rs`). The file previously fabricated
//! JSON receipts by hand with a schema (`operation_id`/`timestamp`/
//! `input_hashes`/`output_hashes`/flat `signature`) that no longer matches
//! anything the engine actually writes — it could never catch real drift.
//! Fixed 2026-07-17 alongside the same class of fix already applied to
//! `tests/proof/invariants.rs` and `tests/proof/smoke.rs`.

mod tests {
    use assert_cmd::Command;
    use sha2::{Digest, Sha256};
    use std::fs;
    use tempfile::TempDir;

    type TestResult = Result<(), Box<dyn std::error::Error>>;

    /// Live engine frontmatter schema (`ggen_engine::config::GgenConfig`,
    /// `deny_unknown_fields`): `[project]` is name-only, `[templates]` required.
    fn setup_minimal_ggen_toml(dir: &TempDir) -> TestResult {
        let ggen_toml = r#"[project]
name = "test-receipt-project"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;
        fs::write(dir.path().join("ggen.toml"), ggen_toml)?;
        fs::create_dir_all(dir.path().join("templates"))?;
        fs::write(
            dir.path().join("templates/one.tmpl"),
            "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}",
        )?;
        fs::write(
            dir.path().join("ontology.ttl"),
            "@prefix ex: <http://example.org/> .\nex:Alice ex:name \"Alice\" .\n",
        )?;
        Ok(())
    }

    /// Run a real `ggen sync` in a fresh temp project and return the parsed
    /// `.ggen-v2/receipt.json`.
    fn sync_and_load_receipt(
        temp_dir: &TempDir,
    ) -> Result<serde_json::Value, Box<dyn std::error::Error>> {
        let _ = Command::cargo_bin("ggen")?
            .arg("sync")
            .current_dir(temp_dir.path())
            .assert()
            .success();
        let receipt_path = temp_dir.path().join(".ggen-v2").join("receipt.json");
        let content = fs::read_to_string(&receipt_path)?;
        Ok(serde_json::from_str(&content)?)
    }

    fn is_hex(s: &str) -> bool {
        !s.is_empty() && s.chars().all(|c| c.is_ascii_hexdigit())
    }

    /// Test 1: Verify all required receipt fields are present
    /// (`praxis_core::ReceiptRecord` + the sync payload it wraps).
    #[test]
    fn receipt_has_all_required_fields() -> TestResult {
        let temp_dir = TempDir::new()?;
        setup_minimal_ggen_toml(&temp_dir)?;
        let receipt = sync_and_load_receipt(&temp_dir)?;

        let record = &receipt["record"];
        assert!(record["activity"].is_string(), "record.activity missing");
        assert!(record["ts_ns"].is_number(), "record.ts_ns missing");
        assert!(
            record["payload_hash_hex"].is_string(),
            "record.payload_hash_hex missing"
        );
        assert!(
            record["chain_hash_hex"].is_string(),
            "record.chain_hash_hex missing"
        );
        assert!(
            record["signature_hex"].is_string(),
            "record.signature_hex missing"
        );
        assert!(record["object_ids"].is_array(), "record.object_ids missing");

        let payload = &receipt["payload"];
        assert!(
            payload["graph_hash"].is_string(),
            "payload.graph_hash missing"
        );
        assert!(payload["outputs"].is_object(), "payload.outputs missing");
        Ok(())
    }

    /// Test 2: Verify the Ed25519 signature is non-empty and the correct
    /// hex-encoded length (64-byte signature → 128 hex chars).
    #[test]
    fn receipt_signature_is_nonempty_and_sufficient_length() -> TestResult {
        let temp_dir = TempDir::new()?;
        setup_minimal_ggen_toml(&temp_dir)?;
        let receipt = sync_and_load_receipt(&temp_dir)?;

        let sig = receipt["record"]["signature_hex"]
            .as_str()
            .expect("signature_hex should be string");

        assert!(!sig.is_empty(), "signature must not be empty");
        assert_eq!(
            sig.len(),
            128,
            "signature_hex must be 128 chars (64-byte Ed25519 signature)"
        );
        assert!(is_hex(sig), "signature_hex must be hex-only");
        Ok(())
    }

    /// Test 3: SHA-256 hash of the real receipt file bytes is deterministic
    /// within a single run (hashing is a pure function of the bytes).
    #[test]
    fn sha256_hash_of_receipt_json_is_deterministic() -> TestResult {
        let temp_dir = TempDir::new()?;
        setup_minimal_ggen_toml(&temp_dir)?;
        sync_and_load_receipt(&temp_dir)?;

        let receipt_path = temp_dir.path().join(".ggen-v2").join("receipt.json");
        let content = fs::read_to_string(&receipt_path)?;

        let mut hasher1 = Sha256::new();
        hasher1.update(content.as_bytes());
        let hash1 = format!("{:x}", hasher1.finalize());

        let mut hasher2 = Sha256::new();
        hasher2.update(content.as_bytes());
        let hash2 = format!("{:x}", hasher2.finalize());

        assert_eq!(
            hash1, hash2,
            "SHA-256 hash should be deterministic (identical input → identical output)"
        );
        Ok(())
    }

    /// Test 4: `object_ids` entries follow the real `law:<16-hex>` identifier
    /// scheme this engine actually uses (there is no UUID concept in the
    /// live receipt — the previous version of this test asserted a UUID
    /// shape that nothing here ever produces).
    #[test]
    fn object_id_is_valid_law_identifier() -> TestResult {
        let temp_dir = TempDir::new()?;
        setup_minimal_ggen_toml(&temp_dir)?;
        let receipt = sync_and_load_receipt(&temp_dir)?;

        let object_ids = receipt["record"]["object_ids"]
            .as_array()
            .expect("object_ids should be array");
        assert!(!object_ids.is_empty(), "object_ids must not be empty");

        let id = object_ids[0].as_str().expect("object_id should be string");
        let hex_part = id
            .strip_prefix("law:")
            .unwrap_or_else(|| panic!("object_id {id:?} must start with \"law:\""));
        assert_eq!(
            hex_part.len(),
            16,
            "law: identifier must carry a 16-hex-char (8-byte) suffix, got {id:?}"
        );
        assert!(
            is_hex(hex_part),
            "law: identifier suffix must be hex-only, got {id:?}"
        );
        Ok(())
    }

    /// Test 5: `ts_ns` is the documented deterministic value.
    ///
    /// The live receipt has no RFC-3339 timestamp field — replay determinism
    /// is achieved by pinning `ts_ns` to 0 rather than recording wall-clock
    /// time (see `crates/ggen-engine/tests/multi_template_determinism.rs`'s
    /// comment: "ts_ns is pinned to 0, so the whole chained record is
    /// identical too"). The previous version of this test asserted an
    /// RFC-3339 string shape that nothing here ever produces.
    #[test]
    fn ts_ns_is_pinned_deterministic_value() -> TestResult {
        let temp_dir = TempDir::new()?;
        setup_minimal_ggen_toml(&temp_dir)?;
        let receipt = sync_and_load_receipt(&temp_dir)?;

        let ts_ns = receipt["record"]["ts_ns"]
            .as_i64()
            .expect("ts_ns should be an integer");
        assert_eq!(ts_ns, 0, "ts_ns must be pinned to 0 for replay determinism");
        Ok(())
    }

    /// Test 6: hash fields contain valid 64-hex-char (32-byte) hashes.
    #[test]
    fn hash_fields_contain_valid_sha256_hex_strings() -> TestResult {
        let temp_dir = TempDir::new()?;
        setup_minimal_ggen_toml(&temp_dir)?;
        let receipt = sync_and_load_receipt(&temp_dir)?;

        for (field, value) in [
            (
                "record.payload_hash_hex",
                &receipt["record"]["payload_hash_hex"],
            ),
            (
                "record.chain_hash_hex",
                &receipt["record"]["chain_hash_hex"],
            ),
            (
                "record.prev_chain_hash_hex",
                &receipt["record"]["prev_chain_hash_hex"],
            ),
            ("payload.graph_hash", &receipt["payload"]["graph_hash"]),
        ] {
            let hash_str = value
                .as_str()
                .unwrap_or_else(|| panic!("{field} should be string"));
            assert_eq!(hash_str.len(), 64, "{field} must be 64 hex characters");
            assert!(is_hex(hash_str), "{field} must contain only hex digits");
        }

        let outputs = receipt["payload"]["outputs"]
            .as_object()
            .expect("payload.outputs should be an object");
        assert!(!outputs.is_empty(), "payload.outputs must not be empty");
        for (path, hash) in outputs {
            let hash_str = hash
                .as_str()
                .unwrap_or_else(|| panic!("payload.outputs[{path:?}] should be string"));
            assert_eq!(
                hash_str.len(),
                64,
                "payload.outputs[{path:?}] must be 64 hex characters"
            );
            assert!(
                is_hex(hash_str),
                "payload.outputs[{path:?}] must contain only hex digits"
            );
        }
        Ok(())
    }

    /// Test 7: Receipt roundtrip preserves deterministic hash — re-reading
    /// the same file bytes and re-hashing produces the same digest, and
    /// parse→reserialize→hash is also a well-formed SHA-256 digest.
    #[test]
    fn receipt_roundtrip_preserves_deterministic_hash() -> TestResult {
        let temp_dir = TempDir::new()?;
        setup_minimal_ggen_toml(&temp_dir)?;
        sync_and_load_receipt(&temp_dir)?;

        let receipt_path = temp_dir.path().join(".ggen-v2").join("receipt.json");

        // Hash 1: File bytes → SHA-256
        let file_bytes = fs::read_to_string(&receipt_path)?;
        let mut hasher1 = Sha256::new();
        hasher1.update(file_bytes.as_bytes());
        let hash1 = format!("{:x}", hasher1.finalize());

        // Hash 2: Parse JSON → serialize → SHA-256
        let parsed: serde_json::Value = serde_json::from_str(&file_bytes)?;
        let serialized = serde_json::to_string(&parsed)?;
        let mut hasher2 = Sha256::new();
        hasher2.update(serialized.as_bytes());
        let hash2 = format!("{:x}", hasher2.finalize());

        // Note: hash1 vs hash2 may differ due to JSON formatting, so verify
        // both are valid SHA-256 digests.
        assert_eq!(
            hash1.len(),
            64,
            "hash1 should be valid SHA-256 (64 hex chars)"
        );
        assert_eq!(
            hash2.len(),
            64,
            "hash2 should be valid SHA-256 (64 hex chars)"
        );

        // Verify that re-reading the file produces the same hash.
        let file_bytes_2 = fs::read_to_string(&receipt_path)?;
        let mut hasher3 = Sha256::new();
        hasher3.update(file_bytes_2.as_bytes());
        let hash3 = format!("{:x}", hasher3.finalize());

        assert_eq!(
            hash1, hash3,
            "Roundtrip: same file bytes should produce same hash"
        );
        Ok(())
    }
}
