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

# 1. tests/cli_command_tests.rs
if os.path.exists("tests/cli_command_tests.rs"):
    with open("tests/cli_command_tests.rs", "r") as f:
        c = f.read()
    c = re.sub(r'(\s+)Command::cargo_bin\("ggen"\)\?', r'\1let _ = Command::cargo_bin("ggen")?', c)
    with open("tests/cli_command_tests.rs", "w") as f:
        f.write(c)

# 2. crates/ggen-graph/src/bin/gall_materialize_evidence_graph.rs
replace_in_file("crates/ggen-graph/src/bin/gall_materialize_evidence_graph.rs", [
    ("use oxigraph::io::{RdfFormat, RdfParser};", "use oxigraph::io::RdfFormat;"),
    ("use oxigraph::model::Quad;\n", ""),
    ("use oxigraph::store::Store;\n", ""),
    ("use std::path::{Path, PathBuf};", "use std::path::Path;"),
    ("path.extension().map_or(false, |ext| ext == \"ttl\")", "path.extension().is_some_and(|ext| ext == \"ttl\")")
])

# 3. crates/ggen-graph/tests/ocel_self_audit.rs
replace_in_file("crates/ggen-graph/tests/ocel_self_audit.rs", [
    ("while let Some(sol_res) = solutions.next() {", "for sol_res in solutions {")
])

# 4. crates/ggen-cli/tests/llm_e2e_test.rs
replace_in_file("crates/ggen-cli/tests/llm_e2e_test.rs", [
    ("struct TestProject {\n    temp_dir: TempDir,", "struct TestProject {\n    #[allow(dead_code)]\n    temp_dir: TempDir,")
])

# 5. crates/ggen-graph/src/bin/gall_observe_clean_room.rs
replace_in_file("crates/ggen-graph/src/bin/gall_observe_clean_room.rs", [
    ("if let Ok(QueryResults::Solutions(mut solutions)) = store.query(query) {", 
     "#[allow(deprecated)]\n                if let Ok(QueryResults::Solutions(mut solutions)) = store.query(query) {")
])

# 6. tests/integration/lifecycle_simple_tests.rs
replace_in_file("tests/integration/lifecycle_simple_tests.rs", [
    ("use tempfile::TempDir;\n", "")
])

# 7. tests/generator_core_tests.rs field_reassign_with_default
replace_in_file("tests/generator_core_tests.rs", [
    ("let mut result = GenerationResult::default();\n    result.success_count = 9;\n    result.error_count = 1;", "let mut result = GenerationResult { success_count: 9, error_count: 1, ..Default::default() };"),
    ("let mut result = GenerationResult::default();\n    result.success_count = 10;\n    result.duration = Duration::from_secs(2);", "let mut result = GenerationResult { success_count: 10, duration: Duration::from_secs(2), ..Default::default() };"),
    ("let mut result = GenerationResult::default();\n    result.success_count = 7;\n    result.error_count = 3;", "let mut result = GenerationResult { success_count: 7, error_count: 3, ..Default::default() };"),
    ("let mut result = GenerationResult::default();\n    result.success_count = 10;\n    result.duration = Duration::from_secs(0);", "let mut result = GenerationResult { success_count: 10, duration: Duration::from_secs(0), ..Default::default() };"),
    ("let mut result = GenerationResult::default();\n    result.success_count = 100;\n    result.duration = Duration::from_millis(500);", "let mut result = GenerationResult { success_count: 100, duration: Duration::from_millis(500), ..Default::default() };")
])

# 8. benches/jidoka_bench.rs
replace_in_file("benches/jidoka_bench.rs", [
    ("use criterion::{black_box, criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};", 
     "use std::hint::black_box;\nuse criterion::{criterion_group, criterion_main, BenchmarkId, Criterion, Throughput};")
])

