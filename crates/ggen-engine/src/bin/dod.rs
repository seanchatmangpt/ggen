//! src/bin/dod.rs — Definition of Done verification
//! Exit 0: all pass  |  Exit 1: soft (tests ok, artifacts stale)  |  Exit 2: hard (broken)

#![allow(clippy::print_stdout)]

use std::process::{exit, Command};

fn run(cmd: &str, args: &[&str]) -> bool {
    Command::new(cmd)
        .args(args)
        .status()
        .map(|s| s.success())
        .unwrap_or(false)
}

fn main() {
    let fmt_ok = run("cargo", &["fmt", "--all", "--check"]);
    let lint_ok = run(
        "cargo",
        &[
            "clippy",
            "--workspace",
            "--all-features",
            "--",
            "-D",
            "warnings",
        ],
    );
    let test_ok = run("cargo", &["test", "--workspace", "--all-features"]);
    let hard_ok = fmt_ok && lint_ok && test_ok;

    // Soft check: receipts/ directory must exist and be non-empty
    let receipts_exist = std::fs::read_dir("receipts")
        .map(|mut d| d.next().is_some())
        .unwrap_or(false);

    if !hard_ok {
        eprintln!("[DOD] HARD FAILURE: fmt={fmt_ok} lint={lint_ok} test={test_ok}");
        exit(2);
    }
    if !receipts_exist {
        eprintln!("[DOD] SOFT FAILURE: receipts/ missing or empty");
        exit(1);
    }
    println!("[DOD] ALL PASS");
    exit(0);
}
