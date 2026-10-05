use std::process::{Command, ExitCode};

fn resolve_binary() -> Option<String> {
    if let Ok(path) = std::env::var("PM4PYTEST_BIN") {
        if !path.is_empty() && std::path::Path::new(&path).exists() {
            return Some(path);
        }
    }
    // PATH lookup: run a cheap probe that only succeeds if the binary is on PATH.
    if Command::new("pm4pytest")
        .arg("--_pm4pytest_cli_probe")
        .stdout(std::process::Stdio::null())
        .stderr(std::process::Stdio::null())
        .status()
        .is_ok()
    {
        return Some("pm4pytest".to_string());
    }
    None
}

fn main() -> ExitCode {
    let Some(binary) = resolve_binary() else {
        eprintln!("REFUSED:PM4PYTEST_BINARY_NOT_FOUND");
        return ExitCode::from(2);
    };

    let args: Vec<String> = std::env::args().skip(1).collect();
    match Command::new(&binary).args(&args).status() {
        Ok(status) => match status.code() {
            Some(code) => ExitCode::from(code as u8),
            None => ExitCode::FAILURE, // terminated by signal
        },
        Err(e) => {
            eprintln!("REFUSED:PM4PYTEST_BINARY_NOT_FOUND ({e})");
            ExitCode::from(2)
        }
    }
}
