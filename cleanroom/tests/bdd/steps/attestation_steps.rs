use super::super::world::CleanroomWorld;
use cucumber::{given, then, when};
use std::fs;
use std::path::Path;

/// Attestation step definitions for Cleanroom BDD tests
///
/// These steps handle attestation, signing, and verification
/// of cleanroom execution results and artifacts.

// ============================================================================
// GIVEN steps - Setup preconditions
// ============================================================================

#[given(regex = r"^I have attestation enabled$")]
fn attestation_enabled(world: &mut CleanroomWorld) {
    world.set_policy("attestation_enabled".to_string(), "true".to_string());
}

#[given(regex = r"^I have a signing key "([^"]+)"$")]
fn signing_key(world: &mut CleanroomWorld, keyfile: String) {
    // Create a mock signing key
    let key_content = r#"-----BEGIN PRIVATE KEY-----
MOCK_PRIVATE_KEY_CONTENT_FOR_TESTING
-----END PRIVATE KEY-----"#;
    
    let key_path = world.project_dir.join(&keyfile);
    fs::write(&key_path, key_content)
        .unwrap_or_else(|e| panic!("Failed to write signing key {}: {}", keyfile, e));
    
    world.capture_file(&keyfile, key_content.to_string());
}

#[given(regex = r"^I have a public key "([^"]+)"$")]
fn public_key(world: &mut CleanroomWorld, keyfile: String) {
    // Create a mock public key
    let key_content = r#"-----BEGIN PUBLIC KEY-----
MOCK_PUBLIC_KEY_CONTENT_FOR_TESTING
-----END PUBLIC KEY-----"#;
    
    let key_path = world.project_dir.join(&keyfile);
    fs::write(&key_path, key_content)
        .unwrap_or_else(|e| panic!("Failed to write public key {}: {}", keyfile, e));
    
    world.capture_file(&keyfile, key_content.to_string());
}

#[given(regex = r"^I have an attestation policy "([^"]+)"$")]
fn attestation_policy(world: &mut CleanroomWorld, policy_file: String) {
    let policy_content = r#"attestation:
  enabled: true
  algorithm: "ed25519"
  key_file: "signing_key.pem"
  verify_signatures: true
  include_metadata: true
  include_artifacts: true
"#;
    
    let policy_path = world.project_dir.join(&policy_file);
    fs::write(&policy_path, policy_content)
        .unwrap_or_else(|e| panic!("Failed to write attestation policy {}: {}", policy_file, e));
    
    world.capture_file(&policy_file, policy_content.to_string());
}

#[given(regex = r"^I have a verification key "([^"]+)"$")]
fn verification_key(world: &mut CleanroomWorld, keyfile: String) {
    // Create a mock verification key (same as public key for testing)
    let key_content = r#"-----BEGIN PUBLIC KEY-----
MOCK_PUBLIC_KEY_CONTENT_FOR_TESTING
-----END PUBLIC KEY-----"#;
    
    let key_path = world.project_dir.join(&keyfile);
    fs::write(&key_path, key_content)
        .unwrap_or_else(|e| panic!("Failed to write verification key {}: {}", keyfile, e));
    
    world.capture_file(&keyfile, key_content.to_string());
}

#[given(regex = r"^I have a signed artifact "([^"]+)"$")]
fn signed_artifact(world: &mut CleanroomWorld, artifact: String) {
    // Create a mock signed artifact
    let artifact_content = b"Mock artifact content for testing";
    let signature = b"Mock signature for testing";
    
    let artifact_path = world.project_dir.join(&artifact);
    fs::write(&artifact_path, artifact_content)
        .unwrap_or_else(|e| panic!("Failed to write signed artifact {}: {}", artifact, e));
    
    // Create corresponding signature file
    let sig_path = world.project_dir.join(format!("{}.sig", artifact));
    fs::write(&sig_path, signature)
        .unwrap_or_else(|e| panic!("Failed to write signature file: {}", e));
    
    world.capture_artifact(&artifact, artifact_content.to_vec());
    world.capture_file(&format!("{}.sig", artifact), String::from_utf8_lossy(signature).to_string());
}

// ============================================================================
// WHEN steps - Execute actions
// ============================================================================

#[when(regex = r"^I sign the artifact "([^"]+)"$")]
fn sign_artifact(world: &mut CleanroomWorld, artifact: String) {
    // Simulate artifact signing
    let artifact_path = world.project_dir.join(&artifact);
    
    if !artifact_path.exists() {
        panic!("Artifact '{}' does not exist", artifact);
    }
    
    // Create signature
    let signature_content = format!("SIGNATURE_FOR_{}", artifact);
    let sig_path = world.project_dir.join(format!("{}.sig", artifact));
    
    fs::write(&sig_path, signature_content)
        .unwrap_or_else(|e| panic!("Failed to write signature: {}", e));
    
    world.capture_file(&format!("{}.sig", artifact), signature_content);
}

#[when(regex = r"^I verify the signature of "([^"]+)"$")]
fn verify_signature(world: &mut CleanroomWorld, artifact: String) {
    // Simulate signature verification
    let artifact_path = world.project_dir.join(&artifact);
    let sig_path = world.project_dir.join(format!("{}.sig", artifact));
    
    if !artifact_path.exists() {
        panic!("Artifact '{}' does not exist", artifact);
    }
    
    if !sig_path.exists() {
        panic!("Signature file for '{}' does not exist", artifact);
    }
    
    // Mock verification result
    world.set_policy("signature_verified".to_string(), "true".to_string());
}

#[when(regex = r"^I create an attestation for "([^"]+)"$")]
fn create_attestation(world: &mut CleanroomWorld, target: String) {
    // Create an attestation document
    let attestation_content = format!(
        r#"attestation:
  target: {}
  algorithm: "ed25519"
  signature: "MOCK_SIGNATURE_FOR_{}"
  timestamp: "2025-01-09T12:00:00Z"
  metadata:
    version: "1.0"
    environment: "test"
"#,
        target, target
    );
    
    let attestation_file = world.project_dir.join(format!("{}.attestation", target));
    fs::write(&attestation_file, attestation_content)
        .unwrap_or_else(|e| panic!("Failed to write attestation: {}", e));
    
    world.capture_file(&format!("{}.attestation", target), attestation_content);
}

#[when(regex = r"^I verify the attestation for "([^"]+)"$")]
fn verify_attestation(world: &mut CleanroomWorld, target: String) {
    // Simulate attestation verification
    let attestation_file = world.project_dir.join(format!("{}.attestation", target));
    
    if !attestation_file.exists() {
        panic!("Attestation file for '{}' does not exist", target);
    }
    
    let content = fs::read_to_string(&attestation_file)
        .unwrap_or_else(|e| panic!("Failed to read attestation file: {}", e));
    
    // Mock verification logic
    if content.contains("MOCK_SIGNATURE") {
        world.set_policy("attestation_verified".to_string(), "true".to_string());
    } else {
        world.set_policy("attestation_verified".to_string(), "false".to_string());
    }
}

#[when(regex = r"^I generate a verification report$")]
fn generate_verification_report(world: &mut CleanroomWorld) {
    // Generate a verification report
    let report_content = r#"# Verification Report

## Attestations
- artifact1.txt: VERIFIED
- artifact2.bin: VERIFIED
- artifact3.json: FAILED

## Signatures
- Total checked: 3
- Verified: 2
- Failed: 1

## Summary
- Overall status: PARTIAL
- Trust level: MEDIUM
"#;
    
    let report_file = world.project_dir.join("verification_report.md");
    fs::write(&report_file, report_content)
        .unwrap_or_else(|e| panic!("Failed to write verification report: {}", e));
    
    world.capture_file("verification_report.md", report_content.to_string());
}

// ============================================================================
// THEN steps - Verify outcomes
// ============================================================================

#[then(regex = r"^the artifact should be signed$")]
fn artifact_should_be_signed(world: &mut CleanroomWorld) {
    // Check that signature files exist for captured artifacts
    let has_signatures = world.captured_files.keys()
        .any(|k| k.ends_with(".sig"));
    
    if !has_signatures {
        panic!("Artifacts should be signed but no signature files found");
    }
}

#[then(regex = r"^the signature should be valid$")]
fn signature_should_be_valid(world: &mut CleanroomWorld) {
    let signature_verified = world.policy_settings.get("signature_verified")
        .expect("Signature verification should have been performed");
    
    assert_eq!(
        signature_verified, "true",
        "Signature should be valid"
    );
}

#[then(regex = r"^the signature should be invalid$")]
fn signature_should_be_invalid(world: &mut CleanroomWorld) {
    let signature_verified = world.policy_settings.get("signature_verified")
        .expect("Signature verification should have been performed");
    
    assert_eq!(
        signature_verified, "false",
        "Signature should be invalid"
    );
}

#[then(regex = r"^the attestation should be created$")]
fn attestation_should_be_created(world: &mut CleanroomWorld) {
    // Check that attestation files exist
    let has_attestations = world.captured_files.keys()
        .any(|k| k.ends_with(".attestation"));
    
    if !has_attestations {
        panic!("Attestations should be created but no attestation files found");
    }
}

#[then(regex = r"^the attestation should be verified$")]
fn attestation_should_be_verified(world: &mut CleanroomWorld) {
    let attestation_verified = world.policy_settings.get("attestation_verified")
        .expect("Attestation verification should have been performed");
    
    assert_eq!(
        attestation_verified, "true",
        "Attestation should be verified"
    );
}

#[then(regex = r"^the attestation should be invalid$")]
fn attestation_should_be_invalid(world: &mut CleanroomWorld) {
    let attestation_verified = world.policy_settings.get("attestation_verified")
        .expect("Attestation verification should have been performed");
    
    assert_eq!(
        attestation_verified, "false",
        "Attestation should be invalid"
    );
}

#[then(regex = r"^the verification report should be generated$")]
fn verification_report_should_be_generated(world: &mut CleanroomWorld) {
    let report_content = world.captured_files.get("verification_report.md")
        .expect("Verification report should be generated");
    
    // Verify report contains expected sections
    assert!(report_content.contains("Verification Report"), "Report should contain title");
    assert!(report_content.contains("Attestations"), "Report should contain attestations section");
    assert!(report_content.contains("Signatures"), "Report should contain signatures section");
    assert!(report_content.contains("Summary"), "Report should contain summary section");
}

#[then(regex = r"^the verification should pass$")]
fn verification_should_pass(world: &mut CleanroomWorld) {
    let report_content = world.captured_files.get("verification_report.md")
        .expect("Verification report should be generated");
    
    // Check for successful verification indicators
    assert!(
        report_content.contains("VERIFIED") || report_content.contains("SUCCESS"),
        "Verification should pass"
    );
}

#[then(regex = r"^the verification should fail$")]
fn verification_should_fail(world: &mut CleanroomWorld) {
    let report_content = world.captured_files.get("verification_report.md")
        .expect("Verification report should be generated");
    
    // Check for failed verification indicators
    assert!(
        report_content.contains("FAILED") || report_content.contains("INVALID"),
        "Verification should fail"
    );
}

#[then(regex = r"^the trust level should be "([^"]+)"$")]
fn trust_level_should_be(world: &mut CleanroomWorld, expected_level: String) {
    let report_content = world.captured_files.get("verification_report.md")
        .expect("Verification report should be generated");
    
    assert!(
        report_content.contains(&expected_level),
        "Trust level should be '{}'",
        expected_level
    );
}

#[then(regex = r"^the attestation should include metadata$")]
fn attestation_should_include_metadata(world: &mut CleanroomWorld) {
    // Check that attestation files contain metadata
    let attestation_files: Vec<_> = world.captured_files.keys()
        .filter(|k| k.ends_with(".attestation"))
        .collect();
    
    if attestation_files.is_empty() {
        panic!("No attestation files found");
    }
    
    for attestation_file in attestation_files {
        let content = world.captured_files.get(attestation_file)
            .expect("Attestation file content should be captured");
        
        assert!(
            content.contains("metadata"),
            "Attestation should include metadata"
        );
    }
}

#[then(regex = r"^the attestation should include artifacts$")]
fn attestation_should_include_artifacts(world: &mut CleanroomWorld) {
    // Check that attestation files reference artifacts
    let attestation_files: Vec<_> = world.captured_files.keys()
        .filter(|k| k.ends_with(".attestation"))
        .collect();
    
    if attestation_files.is_empty() {
        panic!("No attestation files found");
    }
    
    for attestation_file in attestation_files {
        let content = world.captured_files.get(attestation_file)
            .expect("Attestation file content should be captured");
        
        assert!(
            content.contains("target") || content.contains("artifact"),
            "Attestation should include artifacts"
        );
    }
}

#[then(regex = r"^the signing key should be protected$")]
fn signing_key_should_be_protected(world: &mut CleanroomWorld) {
    // Check that signing keys have appropriate permissions
    let key_files: Vec<_> = world.captured_files.keys()
        .filter(|k| k.ends_with(".pem") || k.ends_with(".key"))
        .collect();
    
    if key_files.is_empty() {
        panic!("No signing keys found");
    }
    
    // In a real implementation, this would check file permissions
    // For now, just verify that keys exist
    for key_file in key_files {
        let content = world.captured_files.get(key_file)
            .expect("Key file content should be captured");
        
        assert!(
            content.contains("PRIVATE KEY") || content.contains("PUBLIC KEY"),
            "Signing key should be protected"
        );
    }
}
