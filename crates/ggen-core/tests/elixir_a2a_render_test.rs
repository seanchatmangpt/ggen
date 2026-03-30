//! RED tests for elixir-a2a template rendering.
//!
//! Verifies that the three elixir-a2a templates render valid Elixir from a minimal context.
//! Run: cargo test elixir_a2a_render -p ggen-core --test elixir_a2a_render_test

use ggen_core::register::register_all;
use serde_json::json;
use tera::{Context, Tera};

fn render_template(name: &str, ctx: &Context) -> Result<String, tera::Error> {
    let template = std::fs::read_to_string(format!("templates/elixir-a2a/{name}"))
        .unwrap_or_else(|_| panic!("Failed to read templates/elixir-a2a/{name}"));
    let mut tera = Tera::default();
    register_all(&mut tera);
    tera.render_str(&template, ctx)
}

fn minimal_agents_ctx() -> Context {
    let mut ctx = Context::new();
    ctx.insert(
        "agents",
        &json!([{
            "agentName": "invoice-agent",
            "description": "Handles invoice queries",
            "version": "1.0",
            "appModule": "MyApp",
            "path": "invoice",
            "skills": "query-invoice,approve-invoice"
        }]),
    );
    ctx
}

// ── agents.ex.tera ─────────────────────────────────────────────────────────

#[test]
fn agents_template_renders_defmodule() {
    let ctx = minimal_agents_ctx();
    let output = render_template("agents.ex.tera", &ctx).expect("agents.ex.tera failed to render");

    assert!(
        output.contains("defmodule MyApp.Agents.InvoiceAgent"),
        "Missing module declaration. Got:\n{output}"
    );
    assert!(
        output.contains("use A2A.Agent"),
        "Missing use A2A.Agent. Got:\n{output}"
    );
    assert!(
        output.contains(r#"name: "invoice-agent""#),
        "Missing agent name. Got:\n{output}"
    );
    assert!(
        output.contains("handle_message"),
        "Missing handle_message/2. Got:\n{output}"
    );
}

#[test]
fn agents_template_uses_default_app_module() {
    let mut ctx = Context::new();
    ctx.insert(
        "agents",
        &json!([{
            "agentName": "billing-agent",
            "description": "Billing agent"
        }]),
    );

    let output = render_template("agents.ex.tera", &ctx).expect("agents.ex.tera failed to render");

    assert!(
        output.contains("defmodule MyApp.Agents.BillingAgent"),
        "Expected default appModule=MyApp. Got:\n{output}"
    );
}

// ── router.ex.tera ─────────────────────────────────────────────────────────

#[test]
fn router_template_renders_forward() {
    let mut ctx = Context::new();
    ctx.insert("router_module", "MyAppWeb.A2ARouter");
    ctx.insert(
        "agents",
        &json!([{
            "agentName": "invoice-agent",
            "appModule": "MyApp",
            "path": "invoice",
            "description": "Invoice agent"
        }]),
    );

    let output = render_template("router.ex.tera", &ctx).expect("router.ex.tera failed to render");

    assert!(
        output.contains("defmodule MyAppWeb.A2ARouter"),
        "Missing router module. Got:\n{output}"
    );
    assert!(
        output.contains(r#"forward "/invoice""#),
        "Missing forward directive. Got:\n{output}"
    );
    assert!(
        output.contains("A2A.Plug"),
        "Missing A2A.Plug reference. Got:\n{output}"
    );
    assert!(
        output.contains("MyApp.Agents.InvoiceAgent"),
        "Missing agent module reference. Got:\n{output}"
    );
}

#[test]
fn router_template_defaults_router_module() {
    let mut ctx = Context::new();
    ctx.insert(
        "agents",
        &json!([{"agentName": "test-agent", "appModule": "MyApp"}]),
    );

    let output = render_template("router.ex.tera", &ctx).expect("router.ex.tera failed to render");

    assert!(
        output.contains("defmodule MyAppWeb.A2ARouter"),
        "Expected default router module. Got:\n{output}"
    );
}

// ── supervisor.ex.tera ─────────────────────────────────────────────────────

#[test]
fn supervisor_template_renders_agent_supervisor() {
    let mut ctx = Context::new();
    ctx.insert("supervisor_module", "MyApp.A2ASupervisor");
    ctx.insert(
        "agents",
        &json!([{
            "agentName": "invoice-agent",
            "appModule": "MyApp"
        }]),
    );

    let output =
        render_template("supervisor.ex.tera", &ctx).expect("supervisor.ex.tera failed to render");

    assert!(
        output.contains("defmodule MyApp.A2ASupervisor"),
        "Missing supervisor module. Got:\n{output}"
    );
    assert!(
        output.contains("A2A.AgentSupervisor"),
        "Missing A2A.AgentSupervisor. Got:\n{output}"
    );
    assert!(
        output.contains("MyApp.Agents.InvoiceAgent"),
        "Missing agent reference in supervisor. Got:\n{output}"
    );
}

#[test]
fn supervisor_template_includes_test_stubs() {
    let mut ctx = Context::new();
    ctx.insert(
        "agents",
        &json!([{"agentName": "invoice-agent", "appModule": "MyApp"}]),
    );

    let output =
        render_template("supervisor.ex.tera", &ctx).expect("supervisor.ex.tera failed to render");

    assert!(
        output.contains("InvoiceAgentTest"),
        "Missing ExUnit test stub. Got:\n{output}"
    );
    assert!(
        output.contains("use ExUnit.Case"),
        "Missing ExUnit.Case. Got:\n{output}"
    );
    assert!(
        output.contains("A2A.call"),
        "Missing A2A.call in test stub. Got:\n{output}"
    );
}
