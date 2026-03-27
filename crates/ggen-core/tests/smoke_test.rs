//! Smoke test: exercise all generators through the unified dispatch
//!
//! Chicago TDD — BLACK BOX tests: verify each generator produces non-empty output
//! containing the service name or language-specific marker.
//!
//! These tests use the public `generate_service` API only; no internal implementation
//! details are tested (WvdA: no coupling to internals).

use ggen_core::codegen::{generate_service, GeneratorLanguage};

// ─────────────────────────────────────────────────────────────────────────────
// Go generator
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn smoke_go_generates_service_struct() {
    let result = generate_service(GeneratorLanguage::Go, "OrderService", 8080);
    assert!(result.is_ok(), "Go generator must not fail");
    let code = result.unwrap();
    assert!(!code.is_empty(), "Go output must be non-empty");
    assert!(
        code.contains("OrderService"),
        "Go output must contain the service name"
    );
    // Go package declaration
    assert!(code.contains("package"), "Go output must contain a package declaration");
}

// ─────────────────────────────────────────────────────────────────────────────
// Elixir generator
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn smoke_elixir_generates_genserver_module() {
    let result = generate_service(GeneratorLanguage::Elixir, "PaymentWorker", 9089);
    assert!(result.is_ok(), "Elixir generator must not fail");
    let code = result.unwrap();
    assert!(!code.is_empty(), "Elixir output must be non-empty");
    assert!(
        code.contains("PaymentWorker"),
        "Elixir output must contain the module name"
    );
    // Elixir GenServer marker
    assert!(
        code.contains("GenServer") || code.contains("defmodule"),
        "Elixir output must contain GenServer or defmodule"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Rust generator
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn smoke_rust_generates_struct_skeleton() {
    let result = generate_service(GeneratorLanguage::Rust, "AnalyticsEngine", 8090);
    assert!(result.is_ok(), "Rust generator must not fail");
    let code = result.unwrap();
    assert!(!code.is_empty(), "Rust output must be non-empty");
    assert!(
        code.contains("AnalyticsEngine"),
        "Rust output must contain the service name"
    );
    assert!(code.contains("8090"), "Rust output must embed the port number");
}

// ─────────────────────────────────────────────────────────────────────────────
// TypeScript generator
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn smoke_typescript_generates_module_stub() {
    let result = generate_service(GeneratorLanguage::TypeScript, "FrontendBFF", 3000);
    assert!(result.is_ok(), "TypeScript generator must not fail");
    let code = result.unwrap();
    assert!(!code.is_empty(), "TypeScript output must be non-empty");
    assert!(
        code.contains("FrontendBFF"),
        "TypeScript output must contain the service name"
    );
    assert!(code.contains("3000"), "TypeScript output must embed the port number");
    // TypeScript export marker
    assert!(code.contains("export"), "TypeScript output must contain export statement");
}

// ─────────────────────────────────────────────────────────────────────────────
// Python generator
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn smoke_python_generates_fastapi_main() {
    let result = generate_service(GeneratorLanguage::Python, "DataPipeline", 8001);
    assert!(result.is_ok(), "Python generator must not fail");
    let code = result.unwrap();
    assert!(!code.is_empty(), "Python output must be non-empty");
    // Python output is a main.py — may contain the service name or FastAPI boilerplate
    assert!(
        code.contains("DataPipeline") || code.contains("fastapi") || code.contains("uvicorn") || code.contains("app"),
        "Python output must reference the service or FastAPI patterns"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Terraform generator
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn smoke_terraform_generates_aws_provider() {
    let result = generate_service(GeneratorLanguage::Terraform, "infra", 0);
    assert!(result.is_ok(), "Terraform generator must not fail");
    let code = result.unwrap();
    assert!(!code.is_empty(), "Terraform output must be non-empty");
    assert!(
        code.contains("hashicorp/aws"),
        "Terraform output must reference hashicorp/aws provider"
    );
    assert!(
        code.contains("required_providers"),
        "Terraform output must contain required_providers block"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Language enum — exhaustive coverage check
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn smoke_all_language_variants_succeed() {
    let variants = vec![
        GeneratorLanguage::Go,
        GeneratorLanguage::Elixir,
        GeneratorLanguage::Rust,
        GeneratorLanguage::TypeScript,
        GeneratorLanguage::Python,
        GeneratorLanguage::Terraform,
    ];

    for lang in variants {
        let label = format!("{:?}", lang);
        let result = generate_service(lang, "SmokeService", 9999);
        assert!(
            result.is_ok(),
            "Generator {:?} must succeed for basic service spec",
            label
        );
        assert!(
            !result.unwrap().is_empty(),
            "Generator {:?} must produce non-empty output",
            label
        );
    }
}
