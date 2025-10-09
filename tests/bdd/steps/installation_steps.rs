use cucumber::{given, then, when, World};
use std::fs;
use assert_cmd::Command;
use predicates::prelude::*;

use crate::world::RgenWorld;

pub fn steps() -> Vec<cucumber::Step<World>> {
    vec![
        when!("I run {string}", run_command),
        then!("rgen should be installed", rgen_should_be_installed),
        then!("{string} should output {string}", command_should_output),
    ]
}

#[when("I run {string}")]
fn run_command(world: &mut RgenWorld, command: &str) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    if args.is_empty() {
        panic!("Empty command provided");
    }
    
    let binary = args[0];
    let cmd_args = &args[1..];
    
    let mut cmd = if binary == "cargo" {
        Command::new("cargo")
    } else {
        Command::new(binary)
    };
    
    let output = cmd
        .args(cmd_args)
        .current_dir(&world.project_dir)
        .output()
        .expect(&format!("Failed to run command: {}", command));
    
    world.last_output = Some(output.clone());
    world.last_exit_code = output.status.code();
}

#[then("rgen should be installed")]
fn rgen_should_be_installed(_world: &mut RgenWorld) {
    // Verify rgen binary exists and is executable
    let mut cmd = Command::cargo_bin("rgen").expect("rgen binary not found");
    let output = cmd.arg("--version").output().expect("Failed to run rgen --version");
    
    assert!(output.status.success(), "rgen --version failed: {}", 
        String::from_utf8_lossy(&output.stderr));
}

#[then("{string} should output {string}")]
fn command_should_output(world: &mut RgenWorld, command: &str, expected: &str) {
    let args: Vec<&str> = command.split_whitespace().collect();
    
    let mut cmd = if args[0] == "rgen" {
        Command::cargo_bin("rgen").expect("rgen binary not found")
    } else {
        Command::new(args[0])
    };
    
    let output = cmd
        .args(&args[1..])
        .current_dir(&world.project_dir)
        .output()
        .expect(&format!("Failed to run command: {}", command));
    
    assert!(output.status.success(), "Command failed: {}", 
        String::from_utf8_lossy(&output.stderr));
    
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(stdout.contains(expected), 
        "Expected '{}' in output, got: {}", expected, stdout);
}