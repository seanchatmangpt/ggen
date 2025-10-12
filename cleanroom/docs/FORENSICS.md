# Forensics and Attestation

Cleanroom provides comprehensive forensics capabilities for test execution analysis, debugging, and compliance.

## Overview

Forensics collection captures complete test execution context:

- **Execution logs**: All stdout/stderr from test runs
- **Environment state**: System and process environment (redacted)
- **Binary artifacts**: Test binaries and dependencies
- **Configuration files**: Test configuration and setup
- **Coverage data**: Code coverage information
- **Attestation data**: Cryptographic proof of execution

## Artifact Collection

The `ArtifactCollector` gathers execution artifacts:

```rust
use cleanroom::{ArtifactCollector, RunInfo};

let collector = ArtifactCollector::new()
    .unwrap()
    .with_redaction(true); // Enable environment redaction

let run_info = RunInfo {
    scenario_name: "integration_test".to_string(),
    backend: "docker".to_string(),
    workdir: PathBuf::from("/tmp/test"),
    log_files: vec![PathBuf::from("test.log")],
    binary_files: vec![PathBuf::from("target/debug/mycli")],
    config_files: vec![PathBuf::from("test_config.toml")],
};

let bundle = collector.collect(&run_info).unwrap();
```

## Forensics Bundle

The `ForensicsBundle` contains all execution artifacts:

```rust
pub struct ForensicsBundle {
    pub metadata: BundleMetadata,
    pub logs: Vec<LogEntry>,
    pub environment: HashMap<String, String>, // Redacted
    pub coverage: Option<CoverageArtifact>,
    pub attestation: Option<AttestationArtifact>,
    pub binaries: Vec<BinaryArtifact>,
    pub configs: Vec<ConfigArtifact>,
}
```

### Bundle Metadata

```rust
pub struct BundleMetadata {
    pub version: String,
    pub created_at: u64,
    pub scenario_name: String,
    pub bundle_id: String,
    pub description: Option<String>,
}
```

### Log Entries

Structured logging with context:

```rust
pub struct LogEntry {
    pub timestamp: u64,
    pub level: LogLevel,
    pub component: String,
    pub message: String,
    pub context: HashMap<String, String>,
}
```

## Environment Redaction

Sensitive environment variables are automatically redacted:

### Redaction Rules

- `*_KEY` → `[REDACTED]`
- `*_TOKEN` → `[REDACTED]`
- `*_SECRET` → `[REDACTED]`
- `*_PASSWORD` → `[REDACTED]`
- `AWS_*` → `[REDACTED]`
- `GITHUB_*` → `[REDACTED]`
- `DOCKER_*` → `[REDACTED]`

### Custom Redaction

```rust
impl ArtifactCollector {
    pub fn with_redaction(self, redact: bool) -> Self
    pub fn add_redaction_pattern(self, pattern: &str) -> Self
}
```

## Bundle Persistence

Save and load forensics bundles:

```rust
use cleanroom::ArtifactCollector;

// Save bundle
let bundle_path = PathBuf::from("test_forensics.json");
collector.save_bundle(&bundle, bundle_path.clone()).unwrap();

// Load bundle
let loaded_bundle = collector.load_bundle(bundle_path).unwrap();
```

## Attestation Generation

Cryptographic attestation of test execution:

```rust
use cleanroom::{AttestationGenerator, RunInfo};

let generator = AttestationGenerator::new()
    .with_signing(Some("private_key.pem".to_string()));

let run_info = RunInfo {
    scenario_name: "security_test".to_string(),
    backend: "docker".to_string(),
    image_digests: HashMap::from([
        ("rust:1-slim".to_string(), "sha256:abc123...".to_string()),
    ]),
    policy: "locked".to_string(),
    environment: HashMap::new(),
};

let attestation = generator.generate(&run_info).unwrap();
```

## Attestation Structure

```rust
pub struct Attestation {
    pub timestamp: u64,
    pub image_digests: HashMap<String, String>,
    pub policy: PolicyAttestation,
    pub environment: EnvironmentAttestation,
    pub coverage: Option<CoverageAttestation>,
    pub signature: Option<String>,
}
```

### Policy Attestation

```rust
pub struct PolicyAttestation {
    pub net_profile: String,
    pub fs_profile: String,
    pub proc_profile: String,
    pub resource_limits: HashMap<String, String>,
}
```

### Environment Attestation

```rust
pub struct EnvironmentAttestation {
    pub host_os: String,
    pub engine_version: Option<String>,
    pub backend: String,
    pub env_vars: HashMap<String, String>, // Redacted
}
```

## Bundle Replay

Replay test execution from forensics bundle:

```rust
use cleanroom::{ForensicsBundle, ArtifactCollector};

let bundle: ForensicsBundle = collector.load_bundle("test_bundle.json").unwrap();

// Replay execution
let replay_result = replay_from_bundle(&bundle).unwrap();

// Verify results match original
assert_eq!(replay_result.stdout, bundle.logs[0].message);
```

## Integration with CI/CD

### Automated Collection

```yaml
# .github/workflows/test.yml
- name: Run Tests
  run: cargo test --all-features

- name: Collect Forensics
  run: |
    cleanroom collect-artifacts \
      --scenario integration_test \
      --output forensics.json \
      --include-logs \
      --include-binaries \
      --include-configs

- name: Upload Artifacts
  uses: actions/upload-artifact@v3
  with:
    name: test-forensics
    path: forensics.json
```

### Compliance Reporting

```rust
use cleanroom::{ArtifactCollector, AttestationGenerator};

// Generate compliance report
let compliance_report = generate_compliance_report(&bundle).unwrap();

// Verify security requirements
assert!(verify_security_requirements(&bundle).unwrap());
```

## Artifact Types

### Binary Artifacts

Test binaries and dependencies with hashes:

```rust
pub struct BinaryArtifact {
    pub name: String,
    pub path: String,
    pub hash: String,    // SHA-256 hash
    pub data: String,    // Base64 encoded binary
}
```

### Configuration Artifacts

Test configuration files:

```rust
pub struct ConfigArtifact {
    pub name: String,
    pub path: String,
    pub data: String,    // File contents
}
```

### Coverage Artifacts

Coverage data with path remapping:

```rust
pub struct CoverageArtifact {
    pub format: String,
    pub data: String,    // Base64 encoded coverage data
    pub path_remap: Vec<PathRemap>,
}
```

## Security Considerations

### Data Protection

- Sensitive environment variables are redacted
- Binary data is hashed, not stored in plain text
- Signatures provide tamper detection
- Access controls limit bundle distribution

### Privacy

- Personal information is not collected
- User data is not stored in bundles
- Execution context is sanitized

### Compliance

- GDPR compliance for personal data
- SOC 2 considerations for security
- Audit trails for regulatory requirements

## Performance Impact

Forensics collection adds minimal overhead:

| Operation | Overhead | Storage |
|-----------|----------|---------|
| Log collection | <1% | 1-10KB |
| Environment capture | <1% | 1-5KB |
| Binary hashing | <1% | 64B per binary |
| Bundle serialization | <1% | 10-100KB |
| Signature generation | <1% | 512B |

## Best Practices

### Collection Strategy

1. **Selective collection**: Only collect necessary artifacts
2. **Log filtering**: Filter sensitive information from logs
3. **Binary selection**: Only include relevant binaries
4. **Compression**: Compress large bundles for storage

### Analysis Workflow

1. **Automated triage**: Use scripts to analyze bundle contents
2. **Diff analysis**: Compare bundles across test runs
3. **Trend monitoring**: Track forensics data over time
4. **Alerting**: Set up alerts for anomalous patterns

### Storage and Retention

1. **Versioned storage**: Store bundles with version metadata
2. **Retention policies**: Define bundle retention periods
3. **Archival**: Archive old bundles for compliance
4. **Cleanup**: Remove temporary files after processing

## Troubleshooting

### Bundle Corruption

If bundles fail to load:

```rust
// Validate bundle integrity
let is_valid = validate_bundle(&bundle_path).unwrap();
if !is_valid {
    eprintln!("Bundle is corrupted or tampered with");
}
```

### Missing Artifacts

If expected artifacts are missing:

```rust
// Check what artifacts were collected
let bundle = collector.load_bundle("test.json").unwrap();
println!("Collected artifacts: {:?}", bundle.binaries.len());
println!("Log entries: {:?}", bundle.logs.len());
```

### Performance Issues

If forensics collection impacts test performance:

```rust
// Use minimal collection for performance-critical tests
let collector = ArtifactCollector::new()
    .unwrap()
    .with_minimal_collection(); // Only essential artifacts
```

## Integration Examples

### Test Debugging

```rust
use cleanroom::ArtifactCollector;

// Collect detailed forensics for debugging
let collector = ArtifactCollector::new().unwrap();

let bundle = collector.collect(&run_info).unwrap();

// Analyze bundle for debugging
for log in &bundle.logs {
    if log.level == LogLevel::Error {
        println!("Error at {}: {}", log.timestamp, log.message);
    }
}
```

### Compliance Auditing

```rust
use cleanroom::{AttestationGenerator, verify_compliance};

// Generate attestation for audit
let attestation = generator.generate(&run_info).unwrap();

// Verify compliance requirements
let compliance_status = verify_compliance(&attestation).unwrap();
assert!(compliance_status.passed, "Compliance check failed");
```

### Historical Analysis

```rust
use cleanroom::ArtifactCollector;

// Load historical bundles for trend analysis
let bundles = load_bundle_history("test_scenario").unwrap();

// Analyze trends
let coverage_trend = analyze_coverage_trend(&bundles);
let performance_trend = analyze_performance_trend(&bundles);
```

## Future Enhancements

### Enhanced Analytics

- Machine learning-based anomaly detection
- Automated root cause analysis
- Performance regression detection
- Security threat identification

### Advanced Attestation

- Hardware security module integration
- Blockchain-based attestation
- Zero-knowledge proofs for privacy
- Distributed attestation verification
