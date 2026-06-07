use std::io::{self, Read};
use wasm4pm_algos::gall::{check_gall_conformance, GallVerdict};
use serde_json::json;
use ocel_core::OCEL;

fn main() {
    let mut content = String::new();
    if io::stdin().read_to_string(&mut content).is_err() {
        print_blocked("Failed to read input from stdin.");
        return;
    }

    let ocel: OCEL = match serde_json::from_str(&content) {
        Ok(val) => val,
        Err(_) => {
            print_blocked("Input is not a valid JSON/OCEL structure.");
            return;
        }
    };

    // check_gall_conformance(&ocel)
    let verdict = check_gall_conformance(ocel);

    match verdict {
        GallVerdict::Blocked { reason } => {
            println!("{}", json!({
                "issues": [
                    {
                        "code": "WASM4PM-VERDICT-BLOCKED",
                        "message": format!("BLOCKED: {}", reason)
                    }
                ]
            }));
        }
        GallVerdict::Fit { fitness } => {
            println!("{}", json!({
                "issues": [
                    {
                        "code": "WASM4PM-VERDICT-FIT",
                        "message": format!("FIT: (Fitness: {:.1})", fitness)
                    }
                ]
            }));
        }
        GallVerdict::Deviation { fitness, missing } => {
            println!("{}", json!({
                "issues": [
                    {
                        "code": "WASM4PM-VERDICT-DEVIATION",
                        "message": format!("DEVIATION: (Fitness: {:.1}). Missing: {}", fitness, missing.join(", "))
                    }
                ]
            }));
        }
        GallVerdict::Inconclusive { reason } => {
            println!("{}", json!({
                "issues": [
                    {
                        "code": "WASM4PM-VERDICT-INCONCLUSIVE",
                        "message": format!("INCONCLUSIVE: {}", reason)
                    }
                ]
            }));
        }
    }
}

fn print_blocked(msg: &str) {
    println!("{}", json!({
        "issues": [
            {
                "code": "WASM4PM-CONFORMANCE-BLOCKED",
                "message": format!("CONFORMANCE-BLOCKED: {}", msg)
            }
        ]
    }));
}
