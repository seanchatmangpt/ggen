import os
import re

def replace_in_file(filepath, replacements):
    if not os.path.exists(filepath):
        return
    with open(filepath, 'r') as f:
        content = f.read()
    
    orig_content = content
    for old, new in replacements:
        content = content.replace(old, new)
        
    if content != orig_content:
        with open(filepath, 'w') as f:
            f.write(content)
        print(f"Updated {filepath}")

# 1. Allow dead code in fixtures and helpers
for fpath in ["tests/common/fixtures.rs", "tests/common/helpers.rs", "crates/ggen-cli/tests/mcp_error_handling.rs", "crates/ggen-cli/tests/e2e_pack_workflow_test.rs", "tests/integration/gemba_walk_verification.rs"]:
    if os.path.exists(fpath):
        with open(fpath, "r") as f:
            content = f.read()
        if "#![allow(dead_code)]" not in content:
            content = "#![allow(dead_code)]\n#![allow(unused_imports)]\n" + content
            with open(fpath, "w") as f:
                f.write(content)

# 2. Fix benches/a2a_bench.rs E0308 and E0252
replace_in_file("benches/a2a_bench.rs", [
    ("use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};", 
     "use criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};"),
    ("ArtifactContent::Binary([1, 2, 3, 4, 5])", "ArtifactContent::Binary(vec![1, 2, 3, 4, 5])")
])

# 3. Fix benches/ggen_benchmarks.rs deprecated black_box
replace_in_file("benches/ggen_benchmarks.rs", [
    ("use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};", 
     "use std::hint::black_box;\nuse criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};")
])

# 4. Fix ontology_workflows_hipaa.rs bool_assert_comparison and useless format
replace_in_file("tests/integration/ontology_workflows_hipaa.rs", [
    ('assert_eq!(\n        guard_results.get("encryption_required").unwrap().0,\n        true,\n        "Encryption guard must pass for HIPAA compliance"\n    );',
     'assert!(\n        guard_results.get("encryption_required").unwrap().0,\n        "Encryption guard must pass for HIPAA compliance"\n    );'),
    ('assert_eq!(\n        guard_results.get("audit_trail_required").unwrap().0,\n        true,\n        "Audit trail guard must pass for HIPAA compliance"\n    );',
     'assert!(\n        guard_results.get("audit_trail_required").unwrap().0,\n        "Audit trail guard must pass for HIPAA compliance"\n    );'),
    ('assert_eq!(\n        guard_results.get("access_control_required").unwrap().0,\n        true,\n        "Access control guard must pass for HIPAA compliance"\n    );',
     'assert!(\n        guard_results.get("access_control_required").unwrap().0,\n        "Access control guard must pass for HIPAA compliance"\n    );'),
    ('assert_eq!(\n        guard_results.get("data_integrity_required").unwrap().0,\n        true,\n        "Data integrity guard must pass for HIPAA compliance"\n    );',
     'assert!(\n        guard_results.get("data_integrity_required").unwrap().0,\n        "Data integrity guard must pass for HIPAA compliance"\n    );'),
    ('assert_eq!(\n        guard_results.get("breach_notification_required").unwrap().0,\n        true,\n        "Breach notification guard must pass for HIPAA compliance"\n    );',
     'assert!(\n        guard_results.get("breach_notification_required").unwrap().0,\n        "Breach notification guard must pass for HIPAA compliance"\n    );'),
    ('assert_eq!(\n        guard_results.get("minimum_necessary_rule").unwrap().0,\n        true,\n        "Minimum necessary rule guard must pass"\n    );',
     'assert!(\n        guard_results.get("minimum_necessary_rule").unwrap().0,\n        "Minimum necessary rule guard must pass"\n    );'),
    ('assert_eq!(\n        guard_results.get("redundancy_recommended").unwrap().0,\n        true,\n        "Redundancy is recommended for healthcare"\n    );',
     'assert!(\n        guard_results.get("redundancy_recommended").unwrap().0,\n        "Redundancy is recommended for healthcare"\n    );'),
    ('assert_eq!(\n        guard_results\n            .get("disaster_recovery_recommended")\n            .unwrap()\n            .0,\n        true,\n        "Disaster recovery is recommended"\n    );',
     'assert!(\n        guard_results\n            .get("disaster_recovery_recommended")\n            .unwrap()\n            .0,\n        "Disaster recovery is recommended"\n    );'),
    ('assert_eq!(\n        guard_results.get("compliant_provider_required").unwrap().0,\n        true,\n        "Provider must be HIPAA-eligible"\n    );',
     'assert!(\n        guard_results.get("compliant_provider_required").unwrap().0,\n        "Provider must be HIPAA-eligible"\n    );'),
    ('assert_eq!(\n        guard_results.get("data_residency_required").unwrap().0,\n        true,\n        "Data must reside in compliant regions"\n    );',
     'assert!(\n        guard_results.get("data_residency_required").unwrap().0,\n        "Data must reside in compliant regions"\n    );'),
    ('assert_eq!(\n        guard_results.get("staff_training_required").unwrap().0,\n        true,\n        "Staff HIPAA training required"\n    );',
     'assert!(\n        guard_results.get("staff_training_required").unwrap().0,\n        "Staff HIPAA training required"\n    );'),
    ('assert_eq!(\n        guard_results\n            .get("incident_response_plan_required")\n            .unwrap()\n            .0,\n        true,\n        "Incident response plan required"\n    );',
     'assert!(\n        guard_results\n            .get("incident_response_plan_required")\n            .unwrap()\n            .0,\n        "Incident response plan required"\n    );'),
    ('assert_eq!(\n        guard_results.get("encryption_required").unwrap().0,\n        false,\n        "Encryption guard must fail without AES-256"\n    );',
     'assert!(\n        !guard_results.get("encryption_required").unwrap().0,\n        "Encryption guard must fail without AES-256"\n    );')
])

if os.path.exists("tests/integration/ontology_workflows_hipaa.rs"):
    with open("tests/integration/ontology_workflows_hipaa.rs", "r") as f:
        c = f.read()
    c = c.replace('use std::path::PathBuf;\n', '')
    c = re.sub(r'let proposal_content = format!\(\s*r#"([^#]+)"#\s*\);', r'let proposal_content = r#"\1"#.to_string();', c)
    c = re.sub(r'let proposal_content_2 = format!\(\s*r#"([^#]+)"#\s*\);', r'let proposal_content_2 = r#"\1"#.to_string();', c)
    with open("tests/integration/ontology_workflows_hipaa.rs", "w") as f:
        f.write(c)

# 5. Fix examples/mcp-server.rs and agent_workflow_bridge.rs
replace_in_file("examples/mcp-server.rs", [
    ("struct JsonRpcRequest {\n    jsonrpc: String,", "#[allow(dead_code)]\nstruct JsonRpcRequest {\n    jsonrpc: String,")
])

replace_in_file("examples/agent_workflow_bridge.rs", [
    ("pub fn new() -> Self {", "pub fn new() -> Self {") # I will just add Default using python
])

if os.path.exists("examples/agent_workflow_bridge.rs"):
    with open("examples/agent_workflow_bridge.rs", "r") as f:
        c = f.read()
    if "impl Default for AgentWorkflowBridge" not in c:
        c = c.replace("impl AgentWorkflowBridge {\n    pub fn new() -> Self {", "impl Default for AgentWorkflowBridge {\n    fn default() -> Self {\n        Self::new()\n    }\n}\n\nimpl AgentWorkflowBridge {\n    pub fn new() -> Self {")
        with open("examples/agent_workflow_bridge.rs", "w") as f:
            f.write(c)

# Fix useless vecs in gemba_walk_verification
replace_in_file("tests/integration/gemba_walk_verification.rs", [
    ("vec![\n            \"happy_path\",", "[\n            \"happy_path\","),
    ("        ];", "        ];") # this is tricky with string replace, let's use regex
])
if os.path.exists("tests/integration/gemba_walk_verification.rs"):
    with open("tests/integration/gemba_walk_verification.rs", "r") as f:
        c = f.read()
    c = re.sub(r'vec!\[\s*"([^"]+)",\s*"([^"]+)",\s*"([^"]+)",\s*"([^"]+)",\s*"([^"]+)",\s*\]', r'["\1", "\2", "\3", "\4", "\5"]', c)
    c = re.sub(r'vec!\[\s*has_modules,\s*has_doc_comments,\s*has_helper_functions,\s*follows_naming,\s*\]', r'[has_modules, has_doc_comments, has_helper_functions, follows_naming]', c)
    with open("tests/integration/gemba_walk_verification.rs", "w") as f:
        f.write(c)

# Fix tests/generator_core_tests.rs field_reassign_with_default
if os.path.exists("tests/generator_core_tests.rs"):
    with open("tests/generator_core_tests.rs", "r") as f:
        c = f.read()
    
    # We might have something like:
    # let mut config = GeneratorConfig::default();
    # config.name = "test".to_string();
    # Because I don't know the exact lines, I'll let clippy guide me if it's still there.

