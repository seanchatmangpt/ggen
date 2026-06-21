//! Demonstrates the Pydantic-grade validation engine end to end.
//!
//! Run with: `cargo run -p star-toml --example validate`
//!
//! Loads a deliberately broken TOML config from a string, validates it, and prints the
//! full multi-error report — showing that every failure is collected at once, each with
//! a precise location, the offending value, and a machine-matchable code.

use serde::Deserialize;
use star_toml::{from_str, Validate, Validator};

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
    cert_path: String,
    key_path: String,
}

impl Validate for Tls {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("cert_path", &self.cert_path);
        v.check_non_empty("key_path", &self.key_path);
    }
}

impl Validate for Server {
    fn validate(&self, v: &mut Validator) {
        v.check_non_empty("host", &self.host);
        v.check_range("port", self.port, 1..=65535);
        if let Some(tls) = &self.tls {
            v.field("tls", |v| tls.validate(v));
        }
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
port = 0

[server.tls]
cert_path = "/etc/ssl/cert.pem"
key_path = ""
"#;

fn main() {
    // Parsing succeeds — the TOML is well-formed; it's the *values* that are wrong.
    let app: App = from_str(BROKEN_CONFIG).expect("config is valid TOML");

    match app.check() {
        Ok(()) => println!("config is valid"),
        Err(report) => {
            // The whole report renders Pydantic-style:
            println!("{report}\n");

            // ...and each error is structured for programmatic handling:
            println!("--- structured ---");
            for e in report.errors() {
                println!(
                    "  loc={:<22} code={:<14} input={:?}",
                    e.loc.to_string(),
                    e.code(),
                    e.input.as_deref().unwrap_or("-"),
                );
            }
        }
    }
}
