#!/bin/bash
# Advanced Rust Project Deployment Script
# Demonstrates comprehensive deployment automation with ggen

set -euo pipefail

# Configuration
PROJECT_NAME="advanced-rust-project"
BUILD_DIR="generated"
RELEASE_DIR="release"
LOG_DIR="logs"
DEPLOY_DIR="deploy"

# Environment configuration
ENVIRONMENT="${GGEN_ENV:-development}"
DEPLOY_HOST="${DEPLOY_HOST:-localhost}"
DEPLOY_PORT="${DEPLOY_PORT:-8080}"
DEPLOY_USER="${DEPLOY_USER:-deploy}"
DEPLOY_PATH="${DEPLOY_PATH:-/opt/$PROJECT_NAME}"

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
YELLOW='\033[1;33m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

# Logging functions
log_info() {
    echo -e "${BLUE}[INFO]${NC} $1"
}

log_success() {
    echo -e "${GREEN}[SUCCESS]${NC} $1"
}

log_warning() {
    echo -e "${YELLOW}[WARNING]${NC} $1"
}

log_error() {
    echo -e "${RED}[ERROR]${NC} $1"
}

# Error handling
handle_error() {
    log_error "Deployment failed at line $1"
    exit 1
}

trap 'handle_error $LINENO' ERR

# Check prerequisites
check_prerequisites() {
    log_info "Checking deployment prerequisites..."
    
    # Check required tools
    local required_tools=("ssh" "scp" "rsync" "systemctl")
    for tool in "${required_tools[@]}"; do
        if ! command -v "$tool" &> /dev/null; then
            log_error "$tool not found. Please install required tools."
            exit 1
        fi
    done
    
    # Check release artifacts
    if [ ! -f "$RELEASE_DIR/$PROJECT_NAME" ]; then
        log_error "Release binary not found. Please run build first."
        exit 1
    fi
    
    # Check configuration files
    if [ ! -f "ggen.toml" ]; then
        log_warning "ggen.toml not found. Using default configuration."
    fi
    
    if [ ! -f "make.toml" ]; then
        log_warning "make.toml not found. Using default configuration."
    fi
    
    log_success "Prerequisites check completed"
}

# Pre-deployment checks
pre_deployment_checks() {
    log_info "Running pre-deployment checks..."
    
    # Check target host connectivity
    log_info "Checking target host connectivity..."
    if ! ssh -o ConnectTimeout=10 "$DEPLOY_USER@$DEPLOY_HOST" "echo 'Connection successful'" &>/dev/null; then
        log_error "Cannot connect to target host: $DEPLOY_HOST"
        exit 1
    fi
    
    # Check target directory
    log_info "Checking target directory..."
    if ! ssh "$DEPLOY_USER@$DEPLOY_HOST" "test -d $DEPLOY_PATH" &>/dev/null; then
        log_warning "Target directory does not exist: $DEPLOY_PATH"
        log_info "Creating target directory..."
        ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo mkdir -p $DEPLOY_PATH && sudo chown $DEPLOY_USER:$DEPLOY_USER $DEPLOY_PATH"
    fi
    
    # Check disk space
    log_info "Checking disk space..."
    local available_space=$(ssh "$DEPLOY_USER@$DEPLOY_HOST" "df -h $DEPLOY_PATH | tail -1 | awk '{print \$4}'")
    log_info "Available disk space: $available_space"
    
    # Check system resources
    log_info "Checking system resources..."
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "free -h && df -h"
    
    log_success "Pre-deployment checks completed"
}

# Backup existing deployment
backup_existing_deployment() {
    log_info "Backing up existing deployment..."
    
    local backup_dir="$DEPLOY_PATH/backups/$(date +%Y%m%d_%H%M%S)"
    
    # Create backup directory
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "mkdir -p $backup_dir"
    
    # Backup existing binary
    if ssh "$DEPLOY_USER@$DEPLOY_HOST" "test -f $DEPLOY_PATH/$PROJECT_NAME"; then
        ssh "$DEPLOY_USER@$DEPLOY_HOST" "cp $DEPLOY_PATH/$PROJECT_NAME $backup_dir/"
        log_success "Backed up existing binary"
    fi
    
    # Backup existing configuration
    if ssh "$DEPLOY_USER@$DEPLOY_HOST" "test -d $DEPLOY_PATH/config"; then
        ssh "$DEPLOY_USER@$DEPLOY_HOST" "cp -r $DEPLOY_PATH/config $backup_dir/"
        log_success "Backed up existing configuration"
    fi
    
    # Backup existing logs
    if ssh "$DEPLOY_USER@$DEPLOY_HOST" "test -d $DEPLOY_PATH/logs"; then
        ssh "$DEPLOY_USER@$DEPLOY_HOST" "cp -r $DEPLOY_PATH/logs $backup_dir/"
        log_success "Backed up existing logs"
    fi
    
    log_success "Backup completed: $backup_dir"
}

# Deploy application
deploy_application() {
    log_info "Deploying application to $ENVIRONMENT environment..."
    
    # Create deployment directory structure
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "mkdir -p $DEPLOY_PATH/{bin,config,logs,data}"
    
    # Deploy binary
    log_info "Deploying binary..."
    scp "$RELEASE_DIR/$PROJECT_NAME" "$DEPLOY_USER@$DEPLOY_HOST:$DEPLOY_PATH/bin/"
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "chmod +x $DEPLOY_PATH/bin/$PROJECT_NAME"
    
    # Deploy configuration
    log_info "Deploying configuration..."
    if [ -f "ggen.toml" ]; then
        scp "ggen.toml" "$DEPLOY_USER@$DEPLOY_HOST:$DEPLOY_PATH/config/"
    fi
    
    if [ -f "make.toml" ]; then
        scp "make.toml" "$DEPLOY_USER@$DEPLOY_HOST:$DEPLOY_PATH/config/"
    fi
    
    # Deploy documentation
    if [ -d "$RELEASE_DIR/docs" ]; then
        log_info "Deploying documentation..."
        rsync -avz "$RELEASE_DIR/docs/" "$DEPLOY_USER@$DEPLOY_HOST:$DEPLOY_PATH/docs/"
    fi
    
    # Deploy systemd service file
    log_info "Deploying systemd service..."
    cat > /tmp/"$PROJECT_NAME".service << EOF
[Unit]
Description=$PROJECT_NAME Service
After=network.target

[Service]
Type=simple
User=$DEPLOY_USER
WorkingDirectory=$DEPLOY_PATH
ExecStart=$DEPLOY_PATH/bin/$PROJECT_NAME
Restart=always
RestartSec=5
Environment=RUST_LOG=info
Environment=GGEN_ENV=$ENVIRONMENT

[Install]
WantedBy=multi-user.target
EOF
    
    scp /tmp/"$PROJECT_NAME".service "$DEPLOY_USER@$DEPLOY_HOST:/tmp/"
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo mv /tmp/$PROJECT_NAME.service /etc/systemd/system/ && sudo systemctl daemon-reload"
    
    log_success "Application deployment completed"
}

# Start services
start_services() {
    log_info "Starting services..."
    
    # Stop existing service
    if ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl is-active --quiet $PROJECT_NAME"; then
        log_info "Stopping existing service..."
        ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl stop $PROJECT_NAME"
    fi
    
    # Start new service
    log_info "Starting new service..."
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl start $PROJECT_NAME"
    
    # Enable service
    log_info "Enabling service..."
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl enable $PROJECT_NAME"
    
    # Wait for service to start
    log_info "Waiting for service to start..."
    sleep 5
    
    # Check service status
    if ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl is-active --quiet $PROJECT_NAME"; then
        log_success "Service started successfully"
    else
        log_error "Service failed to start"
        ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl status $PROJECT_NAME"
        exit 1
    fi
}

# Health checks
health_checks() {
    log_info "Running health checks..."
    
    # Check service status
    log_info "Checking service status..."
    if ! ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl is-active --quiet $PROJECT_NAME"; then
        log_error "Service is not running"
        exit 1
    fi
    
    # Check health endpoint
    log_info "Checking health endpoint..."
    local health_url="http://$DEPLOY_HOST:$DEPLOY_PORT/health"
    local max_attempts=30
    local attempt=1
    
    while [ $attempt -le $max_attempts ]; do
        if curl -s -f "$health_url" >/dev/null 2>&1; then
            log_success "Health check passed"
            break
        fi
        
        if [ $attempt -eq $max_attempts ]; then
            log_error "Health check failed after $max_attempts attempts"
            exit 1
        fi
        
        log_info "Health check attempt $attempt/$max_attempts failed, retrying..."
        sleep 2
        ((attempt++))
    done
    
    # Check metrics endpoint
    log_info "Checking metrics endpoint..."
    local metrics_url="http://$DEPLOY_HOST:$DEPLOY_PORT/metrics"
    if curl -s -f "$metrics_url" >/dev/null 2>&1; then
        log_success "Metrics endpoint is accessible"
    else
        log_warning "Metrics endpoint is not accessible"
    fi
    
    log_success "Health checks completed"
}

# Post-deployment tasks
post_deployment_tasks() {
    log_info "Running post-deployment tasks..."
    
    # Clean up old backups (keep last 5)
    log_info "Cleaning up old backups..."
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "cd $DEPLOY_PATH/backups && ls -t | tail -n +6 | xargs -r rm -rf"
    
    # Update deployment info
    log_info "Updating deployment info..."
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "cat > $DEPLOY_PATH/deployment-info.json << 'EOF'
{
    \"project\": \"$PROJECT_NAME\",
    \"environment\": \"$ENVIRONMENT\",
    \"deployment_date\": \"$(date -u +%Y-%m-%dT%H:%M:%SZ)\",
    \"deployed_by\": \"$(whoami)\",
    \"deploy_host\": \"$DEPLOY_HOST\",
    \"deploy_path\": \"$DEPLOY_PATH\",
    \"service_status\": \"active\"
}
EOF"
    
    # Send deployment notification
    if [ -n "${SLACK_WEBHOOK_URL:-}" ]; then
        log_info "Sending deployment notification..."
        curl -X POST -H 'Content-type: application/json' \
            --data "{\"text\":\"✅ $PROJECT_NAME deployed to $ENVIRONMENT environment successfully!\"}" \
            "$SLACK_WEBHOOK_URL"
    fi
    
    log_success "Post-deployment tasks completed"
}

# Rollback deployment
rollback_deployment() {
    log_info "Rolling back deployment..."
    
    # Stop current service
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl stop $PROJECT_NAME"
    
    # Find latest backup
    local latest_backup=$(ssh "$DEPLOY_USER@$DEPLOY_HOST" "ls -t $DEPLOY_PATH/backups | head -1")
    
    if [ -z "$latest_backup" ]; then
        log_error "No backup found for rollback"
        exit 1
    fi
    
    # Restore from backup
    log_info "Restoring from backup: $latest_backup"
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "cp $DEPLOY_PATH/backups/$latest_backup/$PROJECT_NAME $DEPLOY_PATH/bin/"
    
    # Start service
    ssh "$DEPLOY_USER@$DEPLOY_HOST" "sudo systemctl start $PROJECT_NAME"
    
    log_success "Rollback completed"
}

# Generate deployment report
generate_deployment_report() {
    log_info "Generating deployment report..."
    
    mkdir -p "$LOG_DIR"
    
    # Create deployment report
    cat > "$LOG_DIR/deployment-report.md" << EOF
# Deployment Report

**Project**: $PROJECT_NAME  
**Environment**: $ENVIRONMENT  
**Deployment Date**: $(date)  
**Deployment Status**: SUCCESS  

## Deployment Steps

1. ✅ Prerequisites check
2. ✅ Pre-deployment checks
3. ✅ Backup existing deployment
4. ✅ Deploy application
5. ✅ Start services
6. ✅ Health checks
7. ✅ Post-deployment tasks

## Deployment Details

- **Target Host**: $DEPLOY_HOST
- **Deploy Path**: $DEPLOY_PATH
- **Service Name**: $PROJECT_NAME
- **Port**: $DEPLOY_PORT
- **User**: $DEPLOY_USER

## Health Check Results

- **Service Status**: Active
- **Health Endpoint**: http://$DEPLOY_HOST:$DEPLOY_PORT/health
- **Metrics Endpoint**: http://$DEPLOY_HOST:$DEPLOY_PORT/metrics

## Next Steps

1. Monitor application logs
2. Check system metrics
3. Verify functionality
4. Update monitoring dashboards

EOF
    
    log_success "Deployment report generated"
}

# Main deployment function
main() {
    log_info "Starting deployment process for $PROJECT_NAME..."
    log_info "Environment: $ENVIRONMENT"
    log_info "Target Host: $DEPLOY_HOST"
    
    # Create log directory
    mkdir -p "$LOG_DIR"
    
    # Redirect output to log file
    exec > >(tee -a "$LOG_DIR/deployment.log")
    exec 2>&1
    
    # Run deployment steps
    check_prerequisites
    pre_deployment_checks
    backup_existing_deployment
    deploy_application
    start_services
    health_checks
    post_deployment_tasks
    generate_deployment_report
    
    log_success "Deployment process completed successfully!"
    log_info "Application is running at: http://$DEPLOY_HOST:$DEPLOY_PORT"
    log_info "Deployment log available in: $LOG_DIR/deployment.log"
    log_info "Deployment report available in: $LOG_DIR/deployment-report.md"
}

# Parse command line arguments
while [[ $# -gt 0 ]]; do
    case $1 in
        --rollback)
            rollback_deployment
            exit 0
            ;;
        --health-check)
            health_checks
            exit 0
            ;;
        --backup-only)
            backup_existing_deployment
            exit 0
            ;;
        --deploy-only)
            deploy_application
            exit 0
            ;;
        --help)
            echo "Usage: $0 [OPTIONS]"
            echo "Options:"
            echo "  --rollback      Rollback to previous deployment"
            echo "  --health-check  Run health checks only"
            echo "  --backup-only   Create backup only"
            echo "  --deploy-only   Deploy application only"
            echo "  --help          Show this help message"
            exit 0
            ;;
        *)
            log_error "Unknown option: $1"
            exit 1
            ;;
    esac
    shift
done

# Run main function
main
