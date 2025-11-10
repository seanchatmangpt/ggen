#!/bin/bash
set -euo pipefail

# Workflow Engine CLI Deployment Script
# Deploys workflow engine with database setup and configuration

SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
PROJECT_ROOT="$(cd "$SCRIPT_DIR/.." && pwd)"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
NC='\033[0m' # No Color

# Configuration
DB_URL="${WORKFLOW_DB_URL:-postgresql://postgres:postgres@localhost:5432/workflow_engine}"
REDIS_URL="${WORKFLOW_REDIS_URL:-redis://localhost:6379}"
BUILD_MODE="${BUILD_MODE:-release}"
INSTALL_DIR="${INSTALL_DIR:-$HOME/.local/bin}"

log_info() {
    echo -e "${GREEN}[INFO]${NC} $1"
}

log_warn() {
    echo -e "${YELLOW}[WARN]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

check_dependencies() {
    log_info "Checking dependencies..."

    if ! command -v cargo &> /dev/null; then
        log_error "Rust/Cargo not found. Install from https://rustup.rs"
        exit 1
    fi

    if ! command -v psql &> /dev/null; then
        log_warn "PostgreSQL client not found. Install for database setup."
    fi

    if ! command -v redis-cli &> /dev/null; then
        log_warn "Redis client not found. Install for event bus testing."
    fi

    log_info "Dependencies check complete"
}

setup_database() {
    log_info "Setting up PostgreSQL database..."

    # Parse database URL
    DB_NAME=$(echo "$DB_URL" | sed -E 's/.*\/([^?]+).*/\1/')

    if command -v psql &> /dev/null; then
        # Create database if it doesn't exist
        psql "$DB_URL" -c "SELECT 1" &> /dev/null || {
            log_info "Creating database: $DB_NAME"
            createdb "$DB_NAME" || log_warn "Failed to create database"
        }

        # Run migrations
        log_info "Running database migrations..."
        psql "$DB_URL" -f "$PROJECT_ROOT/migrations/001_initial_schema.sql" || log_warn "Migration failed"
    else
        log_warn "PostgreSQL client not available, skipping database setup"
    fi
}

build_project() {
    log_info "Building workflow-engine-cli ($BUILD_MODE mode)..."

    cd "$PROJECT_ROOT"

    if [ "$BUILD_MODE" = "release" ]; then
        cargo build --release
        BINARY_PATH="target/release/workflow-engine"
    else
        cargo build
        BINARY_PATH="target/debug/workflow-engine"
    fi

    if [ ! -f "$BINARY_PATH" ]; then
        log_error "Build failed: binary not found at $BINARY_PATH"
        exit 1
    fi

    log_info "Build successful: $BINARY_PATH"
}

run_tests() {
    log_info "Running tests..."

    cd "$PROJECT_ROOT"

    # Unit tests
    log_info "Running unit tests..."
    cargo test --lib

    # Integration tests (if database is available)
    if command -v psql &> /dev/null; then
        log_info "Running integration tests..."
        WORKFLOW_DB_URL="$DB_URL" cargo test --test integration_test
    else
        log_warn "Skipping integration tests (no database)"
    fi
}

install_binary() {
    log_info "Installing workflow-engine to $INSTALL_DIR..."

    mkdir -p "$INSTALL_DIR"

    if [ "$BUILD_MODE" = "release" ]; then
        cp "$PROJECT_ROOT/target/release/workflow-engine" "$INSTALL_DIR/"
    else
        cp "$PROJECT_ROOT/target/debug/workflow-engine" "$INSTALL_DIR/"
    fi

    chmod +x "$INSTALL_DIR/workflow-engine"

    log_info "Installation complete"

    # Check if INSTALL_DIR is in PATH
    if [[ ":$PATH:" != *":$INSTALL_DIR:"* ]]; then
        log_warn "$INSTALL_DIR is not in PATH. Add it with:"
        echo "  export PATH=\"\$PATH:$INSTALL_DIR\""
    fi
}

setup_config() {
    log_info "Setting up configuration..."

    CONFIG_DIR="${XDG_CONFIG_HOME:-$HOME/.config}/workflow-engine"
    mkdir -p "$CONFIG_DIR"

    cat > "$CONFIG_DIR/config.toml" <<EOF
[engine]
max_concurrent_instances = 100
task_timeout = "30s"
enable_persistence = true
snapshot_interval = "10s"

[database]
url = "$DB_URL"
pool_size = 10

[events]
bus_type = "redis"
url = "$REDIS_URL"

[metrics]
enabled = true
port = 9090
export_format = "prometheus"

[logging]
level = "info"
format = "json"
EOF

    log_info "Configuration created at $CONFIG_DIR/config.toml"
}

verify_installation() {
    log_info "Verifying installation..."

    if ! command -v workflow-engine &> /dev/null; then
        log_error "workflow-engine command not found in PATH"
        exit 1
    fi

    # Test basic commands
    workflow-engine --version || log_error "Failed to get version"
    workflow-engine workflow --help &> /dev/null || log_error "Failed to get workflow help"

    log_info "Installation verified successfully"
}

main() {
    log_info "Starting workflow-engine-cli deployment..."

    check_dependencies
    setup_database
    build_project
    run_tests
    install_binary
    setup_config
    verify_installation

    log_info "========================================="
    log_info "Deployment complete!"
    log_info "========================================="
    log_info "Binary location: $INSTALL_DIR/workflow-engine"
    log_info "Config location: ${XDG_CONFIG_HOME:-$HOME/.config}/workflow-engine/config.toml"
    log_info ""
    log_info "Try these commands:"
    log_info "  workflow-engine --version"
    log_info "  workflow-engine workflow list"
    log_info "  workflow-engine process start --help"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --skip-tests)
            SKIP_TESTS=1
            shift
            ;;
        --skip-db)
            SKIP_DB=1
            shift
            ;;
        --debug)
            BUILD_MODE=debug
            shift
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo ""
            echo "Options:"
            echo "  --skip-tests    Skip running tests"
            echo "  --skip-db       Skip database setup"
            echo "  --debug         Build in debug mode"
            echo "  --help          Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
done

# Run main deployment
if [ "${SKIP_DB:-0}" -eq 1 ]; then
    setup_database() { log_info "Skipping database setup"; }
fi

if [ "${SKIP_TESTS:-0}" -eq 1 ]; then
    run_tests() { log_info "Skipping tests"; }
fi

main
