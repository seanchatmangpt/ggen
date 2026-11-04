//! `sjira` — DS-8 CLI skeleton (PF-2 descope: SKELETON ONLY).
//!
//! Arg-parse + ONE happy-path subcommand (`sjira admit <file>`) over a
//! real file. The full verb surface (frontier/execute/complete/lease/
//! epoch/pack) is a named NEXT-WAVE item — see
//! `docs/jira/v26.11.3/receipts/ds-8-cli-skeleton.md` for the descope
//! record; every absent verb refuses typed rather than silently
//! succeeding.
//!
//! Exit-code law (non-conflicting by construction: aligned with
//! complete.rs's three-exit vocabulary, distinct from ggen CLI's exit-5
//! Andon code):
//!
//! * `0` — admitted (prints identity + definition digest).
//! * `1` — typed kernel refusal (`admit_work_order` refused; prints the
//!   typed refusal line, `REFUSED(...)`).
//! * `2` — invalid invocation: usage error, unknown subcommand, missing
//!   argument, or unreadable file (before the kernel is consulted).

use std::process::exit;

fn main() {
    let args: Vec<String> = std::env::args().skip(1).collect();

    let Some("admit") = args.first().map(String::as_str) else {
        usage_exit(args.first().map(String::as_str));
    };
    let Some(file) = args.get(1) else {
        eprintln!("USAGE: sjira admit <work-order.json>");
        exit(2);
    };
    if args.len() > 2 {
        eprintln!("USAGE: sjira admit takes exactly one argument");
        exit(2);
    }

    // Real file IO — the kernel is consulted only after the file reads.
    let body = match std::fs::read_to_string(file) {
        Ok(b) => b,
        Err(e) => {
            eprintln!("INVALID INVOCATION: cannot read {file}: {e}");
            exit(2);
        }
    };

    match serde_json::from_str::<serde_json::Value>(&body) {
        Err(e) => {
            eprintln!("INVALID INVOCATION: {file} is not valid JSON: {e}");
            exit(2);
        }
        Ok(order) => match ggen_sjira::admit_work_order(&order) {
            Ok(admitted) => {
                println!(
                    "{}",
                    serde_json::json!({
                        "admitted": true,
                        "identity": admitted.work_order.identity,
                        "definition_digest": admitted.definition_digest,
                    })
                );
                exit(0);
            }
            Err(refusal) => {
                eprintln!("REFUSED: {refusal}");
                exit(1);
            }
        },
    }
}

fn usage_exit(seen: Option<&str>) -> ! {
    match seen {
        Some(cmd) if cmd != "admit" => {
            eprintln!(
                "INVALID INVOCATION: unknown subcommand {cmd:?} — the v26.11.3 \
                 skeleton exposes only `admit`; the full verb surface is a \
                 named next-wave item"
            );
            exit(2);
        }
        _ => {
            eprintln!("USAGE: sjira admit <work-order.json>");
            exit(2);
        }
    }
}
