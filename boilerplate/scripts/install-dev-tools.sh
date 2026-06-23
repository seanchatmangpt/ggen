#!/usr/bin/env bash
# Install all development tools needed for the project.
# Uses taiki-e/install-action patterns for speed where available.
set -euo pipefail

info() { echo "[install-dev-tools] $*"; }

# cargo-nextest — fast test runner (10-30× faster than cargo test)
if ! command -v cargo-nextest &>/dev/null; then
    info "Installing cargo-nextest..."
    cargo install cargo-nextest --locked
fi

# cargo-hack — feature flag combinatorial testing
if ! command -v cargo-hack &>/dev/null; then
    info "Installing cargo-hack..."
    cargo install cargo-hack --locked
fi

# cargo-deny — license, advisory, ban checks
if ! command -v cargo-deny &>/dev/null; then
    info "Installing cargo-deny..."
    cargo install cargo-deny --locked
fi

# cargo-tarpaulin — code coverage
if ! command -v cargo-tarpaulin &>/dev/null; then
    info "Installing cargo-tarpaulin..."
    cargo install cargo-tarpaulin --locked
fi

# cargo-mutants — mutation testing
if ! command -v cargo-mutants &>/dev/null; then
    info "Installing cargo-mutants..."
    cargo install cargo-mutants --locked
fi

# cargo-bloat — binary size analysis
if ! command -v cargo-bloat &>/dev/null; then
    info "Installing cargo-bloat..."
    cargo install cargo-bloat --locked
fi

# cargo-audit — security vulnerability scanner
if ! command -v cargo-audit &>/dev/null; then
    info "Installing cargo-audit..."
    cargo install cargo-audit --locked
fi

# cargo-make — task runner
if ! command -v cargo-make &>/dev/null; then
    info "Installing cargo-make..."
    cargo install cargo-make --locked
fi

# typos — fast spell checker
if ! command -v typos &>/dev/null; then
    info "Installing typos..."
    cargo install typos-cli --locked
fi

info "All dev tools installed."
info "Run './scripts/install-hooks.sh' to install git hooks."
