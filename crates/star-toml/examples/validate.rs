//! Demonstrates the full validation engine end to end — Pydantic-grade error
//! collection plus Van der Aalst-inspired process-mining analytics.
//!
//! Run with: `cargo run -p star-toml --example validate`
//!
//! Loads a deliberately broken TOML config, validates it, and prints:
//! - the Pydantic-style multi-error report
//! - auto-derived repair hints for every failure
//! - the Van der Aalst conformance fitness score
//! - the variant fingerprint (stable across runs with the same error pattern)
//! - errors grouped by top-level config section (object-centric view)
//! - a DECLARE cross-field constraint violation

#![allow(clippy::expect_used)]

use serde::Deserialize;
use star_toml::{from_str, Severity, Validate, Validator};

#[derive(Debug, Deserialize)]
struct App {
    name: String,
    workers: u32,
    log_level: String,
    server: Server,
}

#[derive(Debug, Deserialize)]
struct Server {
    host: String,
    port: u16,
    #[serde(default)]
    tls: Option<Tls>,
}

#[derive(Debug, Deserialize)]
struct Tls {
    enabled: bool,
    cert_path: String,
    key_path: String,
}

impl Validate for Tls {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("cert_path", &self.cert_path);
        v.check_non_empty("key_path", &self.key_path);
        // DECLARE co-existence: enabled ⟺ both paths non-empty
        v.check_consistent(
            "cert_path",
            &["enabled"],
            !self.enabled || !self.cert_path.is_empty(),
            "tls_cert_required",
            "cert_path must be set when TLS is enabled",
        );
    }
}

impl Validate for Server {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("host", &self.host);
        v.check_range("port", self.port, 1..=65535);
        if let Some(tls) = &self.tls {
            v.field("tls", |v| tls.validate(v));
        }
        // Advisory: best practice, not a hard requirement
        v.with_severity(Severity::Advisory, |v| {
            v.check_predicate(
                "port",
                self.port != 80,
                "avoid_well_known_port",
                "prefer a port above 1024 in production",
            );
        });
    }
}

impl Validate for App {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("name", &self.name);
        v.check_range("workers", self.workers, 1..=1024);
        v.check_one_of(
            "log_level",
            &self.log_level,
            &["trace", "debug", "info", "warn", "error"],
        );
        v.field("server", |v| self.server.validate(v));
    }
}

const BROKEN_CONFIG: &str = r#"
name = ""
workers = 0
log_level = "verbose"

[server]
host = ""
port = 80

[server.tls]
enabled = true
cert_path = ""
key_path = ""
"#;

fn main() {
    let app: App = from_str(BROKEN_CONFIG).expect("config is valid TOML");

    match app.check() {
        Ok(()) => println!("config is valid"),
        Err(report) => {
            // ── 1. Pydantic-style report ────────────────────────────────────
            println!("{report}\n");

            // ── 2. Conformance fitness (Van der Aalst alignment metric) ─────
            println!(
                "fitness: {:.1}%  (variant: {:016x})\n",
                report.fitness() * 100.0,
                report.variant_id(),
            );

            // ── 3. Structured errors with repair hints ──────────────────────
            println!("--- structured (with repair hints) ---");
            for e in report.errors() {
                println!(
                    "  [{sev:<8}] {loc:<28} [{code:<20}]  fix → {hint}",
                    sev = e.severity.to_string(),
                    loc = e.loc.to_string(),
                    code = e.code(),
                    hint = e.repair_hint(),
                );
            }

            // ── 4. Object-centric grouping (Van der Aalst OCEL view) ────────
            println!("\n--- by config section (object-centric) ---");
            for (section, errors) in report.by_section() {
                println!("  [{section}]  {} error(s)", errors.len());
                for e in errors {
                    println!("    • {} [{}]", e.loc, e.code());
                }
            }

            // ── 5. Fatal / warning counts ───────────────────────────────────
            let fatals: usize = report.errors_above(Severity::Fatal).count();
            let warnings: usize = report
                .errors_above(Severity::Advisory)
                .filter(|e| e.severity == Severity::Advisory)
                .count();
            println!(
                "\nfatal={fatals}  advisory={warnings}  has_fatal={}",
                report.has_fatal()
            );
        }
    }
}
