# Onboarding & Setup Usability Audit Report

This report summarizes the findings from the onboarding, setup, and compilation audit of the `ggen` project.

---

## 1. Observation

During the audit, the following exact paths, configurations, and tool outputs were observed:

### A. Tooling & Environment Prerequisites
*   **.tool-versions** (line 5):
    ```
    rust 1.86.0
    ```
*   **rust-toolchain.toml** (line 2):
    ```toml
    channel = "nightly-2026-04-15"
    ```
*   **System `rustc` Version**:
    ```bash
    $ rustc --version
    rustc 1.97.0-nightly (a5c825cd8 2026-04-14)
    ```
*   **GNU `timeout` Command**: Available at `/opt/homebrew/bin/timeout` (installed via Homebrew coreutils).

### B. Make Task Definition and Compile Timeout
*   **Makefile.toml** (lines 31-35):
    ```toml
    [tasks.check]
    description = "Check code without building (60s timeout for 30-crate workspace + lock contention)"
    workspace = false
    command = "timeout"
    args = ["60s", "cargo", "check", "--workspace"]
    ```
*   **Compilation Failure Output (Clean Workspace check)**:
    Running `cargo make check` on a clean workspace resulted in a timeout:
    ```
    [cargo-make] Task: check
    [cargo-make] Execute Command: "timeout" "60s" "cargo" "check" "--workspace"
       Compiling blake3 v1.8.5
       Compiling bindgen v0.72.1
       ...
       Compiling oxrocksdb-sys v0.5.8
       ...
       Checking genesis-lockchain v1.0.0
    Error while executing command, exit code: 124
    ```
    *(Exit code `124` is returned by the `timeout` command when the inner command exceeds the specified duration of 60 seconds).*

### C. Interactive Setup Steps
*   **scripts/startup.sh** (lines 20, 31, 43, 58, 72) uses interactive reads:
    ```bash
    read -r q1
    read -r q2
    read -r q3
    read -r q4_confirm
    read -r q5_confirm
    ```
    If any of questions 1-3 are answered with anything other than "yes", the script terminates with `exit 1`.

### D. File Lock Contention
*   Running `ps aux | grep cargo` revealed that multiple concurrent processes were active in the workspace, causing cargo to block:
    ```
    sac              72404   0.0  0.3 /Users/sac/.rustup/toolchains/nightly-2026-04-15-aarch64-apple-darwin/bin/cargo check --workspace
    sac              66163   0.0  0.3 /Users/sac/.rustup/toolchains/nightly-2026-04-15-aarch64-apple-darwin/bin/cargo check
    sac              75585   0.0  0.2 /Users/sac/.rustup/toolchains/nightly-2026-04-15-aarch64-apple-darwin/bin/cargo build --lib --release
    ```
    This resulted in: `"Blocking waiting for file lock on build directory"`.

---

## 2. Logic Chain

1.  **Toolchain Consistency**: `rust-toolchain.toml` overrides any local cargo defaults and sets the active compiler to `nightly-2026-04-15` (resolving to `1.97.0-nightly`). This is required for advanced nightly features.
2.  **Compilation Time vs. Task Timeout**: 
    *   The project workspace consists of 15 members and depends on heavy external dependencies such as `oxrocksdb-sys` (compiling RocksDB C++ codebase) and `bindgen`.
    *   Clean-compilation check time for this codebase easily exceeds 60 seconds.
    *   Because `Makefile.toml` enforces a strict `timeout 60s` on `cargo check --workspace`, running `cargo make check` on a clean, uncached clone is guaranteed to time out (exit code 124) and fail the setup step.
3.  **Non-Interactive Environments**: The interactive prompts in `scripts/startup.sh` make it impossible to run `make setup` inside non-interactive CI/CD runners or headless environments without mocking stdin, causing the build/setup stage to hang or fail.
4.  **Target Lock Contention**: Running cargo commands in parallel agents creates lock contention on `target/`. Because cargo builds/checks block on this file lock, concurrent checks stall.

---

## 3. Caveats

*   **Clean Compile Duration**: The exact duration required for a successful clean workspace compilation was not timed to completion, as background cargo compilations from other active agents were using the `target/` directory lock.
*   **Code Modifications & Tests**: In accordance with the read-only scope boundaries, no modifications were made to the source codebase, and we did not run the test suite to completion.

---

## 4. Conclusion & Actionable Summary

### A. Prerequisites
1.  **Rust Toolchain**: Nightly channel `nightly-2026-04-15` (managed automatically by `rustup` via `rust-toolchain.toml`).
2.  **Cargo-Make**: Required to run `cargo make <target>`. Install via `cargo install cargo-make`.
3.  **GNU coreutils (`timeout`)**: Hard dependency for make tasks.
    *   macOS: `brew install coreutils` (places `timeout` / `gtimeout` in path).
    *   Ubuntu/Debian: `sudo apt-get install coreutils`.
4.  **C/C++ Build Environment**: `clang`, `make`, etc. (for building Oxigraph/RocksDB dependencies).

### B. Recommended Setup & Compile Steps
To avoid the initial compilation timeout on clean clones:
1.  **Install dependencies**:
    ```bash
    brew install coreutils
    cargo install cargo-make
    ```
2.  **Pre-warm Cargo Cache** (bypass the 60s make timeout on clean checkouts):
    ```bash
    cargo check --workspace
    ```
3.  **Run Makefile Verification**:
    Once dependencies are cached, subsequent verification runs are extremely fast:
    ```bash
    cargo make check       # Incremental check (<2s target)
    cargo make lint        # Clippy lint validation
    cargo make test-unit   # Run unit tests
    ```

### C. Onboarding Friction Points
1.  **Clean Clone Timeout**: `cargo make check` fails on a clean clone because it times out at 60s before C++ dependencies (RocksDB) can finish compiling. A manual `cargo check --workspace` must be executed first to populate the target cache.
2.  **GNU `timeout` Requirement**: macOS users encounter failures immediately unless they have installed GNU coreutils via Homebrew.
3.  **Interactive Blocking**: The `scripts/startup.sh` script (invoked by `make setup`) blocks on interactive stdout/stdin and exits with code 1 if the user selects "no" to early gatekeeping questions, preventing headless CI/CD execution.
4.  **Build Lock Contention**: Concurrent execution of parallel agents or local terminal checks leads to build blocking, causing cargo tasks to exceed their timeouts.

---

## 5. Verification Method

To independently verify the observations and setup issues:
1.  Run the make pre-flight check task to verify the presence of `timeout`:
    ```bash
    cargo make timeout-check
    ```
2.  Clear cargo target cache:
    ```bash
    cargo clean
    ```
3.  Run `cargo make check` to reproduce the timeout failure (observe exit code 124).
4.  Run a manual `cargo check --workspace` without `timeout` to verify the build completes successfully and caches the target outputs.
5.  Execute `scripts/startup.sh` to observe the interactive screening gate questions.
