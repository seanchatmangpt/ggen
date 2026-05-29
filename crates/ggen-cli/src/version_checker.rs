use std::fs;
use std::io::IsTerminal;
use std::path::{Path, PathBuf};
use std::time::SystemTime;
use colored::Colorize;

/// Checks if there is a newer compiled ggen binary in the target directory compared to the running one,
/// and prints a warning if so.
pub fn check_outdated_binary() {
    // Check if GGEN_SKIP_OUTDATED_WARNING or standard CI bypass variables are present. If so, return early.
    if std::env::var_os("GGEN_SKIP_OUTDATED_WARNING").is_some() {
        return;
    }
    for var in &["CI", "GITHUB_ACTIONS", "TRAVIS", "CIRCLECI", "GITLAB_CI", "JENKINS_URL"] {
        if std::env::var_os(var).is_some() {
            return;
        }
    }

    // Check if !std::io::stderr().is_terminal(). If so, return early.
    // (Bypassed if GGEN_TEST_FORCE_TERMINAL is set to "1" for integration tests)
    if !std::io::stderr().is_terminal() && std::env::var("GGEN_TEST_FORCE_TERMINAL").as_deref() != Ok("1") {
        return;
    }

    // Retrieve the current executable path via std::env::current_exe(). If this fails, return early.
    let current_exe = match std::env::current_exe() {
        Ok(path) => path,
        Err(_) => return,
    };

    let current_exe_canonical = current_exe.canonicalize().unwrap_or_else(|_| current_exe.clone());

    let running_metadata = match fs::metadata(&current_exe_canonical) {
        Ok(meta) => meta,
        Err(_) => return,
    };

    let running_mtime = match running_metadata.modified() {
        Ok(mtime) => mtime,
        Err(_) => return,
    };

    // Find the cargo target folder and workspace root. Walk up parent directories of current directory or the current executable looking for Cargo.toml and a target/ directory.
    let (workspace_root, target_dir) = match find_workspace_root_and_target_dir(&current_exe_canonical) {
        Some(paths) => paths,
        None => return,
    };

    // Check <workspace_root>/target/debug/ggen and <workspace_root>/target/release/ggen (or ggen.exe on Windows).
    let bin_name = if cfg!(windows) { "ggen.exe" } else { "ggen" };
    let debug_bin = target_dir.join("debug").join(bin_name);
    let release_bin = target_dir.join("release").join(bin_name);

    let mut candidates = Vec::new();
    if debug_bin.is_file() {
        candidates.push(debug_bin);
    }
    if release_bin.is_file() {
        candidates.push(release_bin);
    }

    let mut newest_path = None;
    let mut newest_mtime = running_mtime;

    for candidate in candidates {
        // Ensure candidate is not the running binary
        if let Ok(cand_canonical) = candidate.canonicalize() {
            if cand_canonical == current_exe_canonical {
                continue;
            }
        } else {
            continue;
        }

        if let Ok(meta) = fs::metadata(&candidate) {
            if let Ok(mtime) = meta.modified() {
                if mtime > newest_mtime {
                    newest_mtime = mtime;
                    newest_path = Some(candidate);
                }
            }
        }
    }

    // If a target binary has a strictly newer mtime and is not the running binary, print a warning to stderr
    if let Some(latest_path) = newest_path {
        if let Ok(diff) = newest_mtime.duration_since(running_mtime) {
            let duration_str = format_duration(diff);
            eprintln!("{}: running an outdated 'ggen' binary", "warning".yellow().bold());
            eprintln!("   --> current: {} (compiled {} older)", current_exe_canonical.display(), duration_str);
            eprintln!("   --> latest:  {}", latest_path.display());
            eprintln!("{}: compile the latest changes or update your installation with 'cargo install --path {}'", "info".green().bold(), workspace_root.display());
        }
    }
}

fn find_workspace_root_and_target_dir(current_exe: &Path) -> Option<(PathBuf, PathBuf)> {
    // 1. Walk up from the current executable parent
    let mut current = current_exe.parent();
    while let Some(path) = current {
        let target_dir = path.join("target");
        if path.join("Cargo.toml").exists() && target_dir.is_dir() {
            return Some((path.to_path_buf(), target_dir));
        }
        current = path.parent();
    }

    // 2. Walk up from the current working directory
    if let Ok(cwd) = std::env::current_dir() {
        let mut current = Some(cwd.as_path());
        while let Some(path) = current {
            let target_dir = path.join("target");
            if path.join("Cargo.toml").exists() && target_dir.is_dir() {
                return Some((path.to_path_buf(), target_dir));
            }
            current = path.parent();
        }
    }

    None
}

fn format_duration(d: std::time::Duration) -> String {
    let secs = d.as_secs();
    if secs < 60 {
        format!("{}s", secs)
    } else if secs < 3600 {
        format!("{}m", secs / 60)
    } else if secs < 86400 {
        let hours = secs / 3600;
        let mins = (secs % 3600) / 60;
        if mins > 0 {
            format!("{}h {}m", hours, mins)
        } else {
            format!("{}h", hours)
        }
    } else {
        let days = secs / 86400;
        let hours = (secs % 86400) / 3600;
        if hours > 0 {
            format!("{}d {}h", days, hours)
        } else {
            format!("{}d", days)
        }
    }
}
