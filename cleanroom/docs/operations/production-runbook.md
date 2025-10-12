# Production Runbook

This runbook provides step-by-step procedures for operating the Cleanroom Testing Framework in production environments.

## Table of Contents

1. [Prerequisites](#prerequisites)
2. [Installation](#installation)
3. [Configuration](#configuration)
4. [Deployment](#deployment)
5. [Monitoring](#monitoring)
6. [Maintenance](#maintenance)
7. [Troubleshooting](#troubleshooting)
8. [Emergency Procedures](#emergency-procedures)

## Prerequisites

### System Requirements

#### Minimum Requirements
- **CPU**: 4 cores, 2.4 GHz
- **Memory**: 8 GB RAM
- **Disk**: 100 GB SSD
- **Network**: 1 Gbps connection

#### Recommended Requirements
- **CPU**: 8 cores, 3.0 GHz
- **Memory**: 32 GB RAM
- **Disk**: 500 GB NVMe SSD
- **Network**: 10 Gbps connection

#### Operating System
- **Linux**: Ubuntu 20.04+ LTS, CentOS 8+, RHEL 8+
- **macOS**: 11.0+ (for development only)
- **Windows**: Not supported for production

### Software Dependencies

#### Required Software
```bash
# Docker Engine
docker --version  # >= 20.10.0

# Docker Compose
docker-compose --version  # >= 2.0.0

# Rust Toolchain
rustc --version  # >= 1.70.0
cargo --version  # >= 1.70.0

# System Tools
curl --version
jq --version
```

#### Optional Software
```bash
# Monitoring Tools
prometheus --version
grafana-server --version

# Log Aggregation
fluentd --version
elasticsearch --version

# Security Tools
vault --version
```

### Network Requirements

#### Port Requirements
- **8080**: Cleanroom API server
- **9090**: Prometheus metrics
- **3000**: Grafana dashboard
- **9200**: Elasticsearch (if used)
- **24224**: Fluentd (if used)

#### Firewall Rules
```bash
# Allow inbound traffic
sudo ufw allow 8080/tcp
sudo ufw allow 9090/tcp
sudo ufw allow 3000/tcp

# Allow outbound traffic
sudo ufw allow out 443/tcp
sudo ufw allow out 80/tcp
sudo ufw allow out 53/udp
```

## Installation

### 1. System Preparation

#### Update System Packages
```bash
# Ubuntu/Debian
sudo apt update && sudo apt upgrade -y

# CentOS/RHEL
sudo yum update -y
```

#### Install Required Packages
```bash
# Ubuntu/Debian
sudo apt install -y curl wget git build-essential pkg-config libssl-dev

# CentOS/RHEL
sudo yum install -y curl wget git gcc gcc-c++ make pkgconfig openssl-devel
```

#### Install Docker
```bash
# Install Docker
curl -fsSL https://get.docker.com -o get-docker.sh
sudo sh get-docker.sh

# Add user to docker group
sudo usermod -aG docker $USER

# Start Docker service
sudo systemctl start docker
sudo systemctl enable docker

# Verify installation
docker --version
docker run hello-world
```

#### Install Rust
```bash
# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env

# Verify installation
rustc --version
cargo --version
```

### 2. Cleanroom Installation

#### Clone Repository
```bash
git clone https://github.com/sac/ggen.git
cd ggen/cleanroom
```

#### Build from Source
```bash
# Build release version
cargo build --release

# Install globally
sudo cp target/release/cleanroom /usr/local/bin/

# Verify installation
cleanroom --version
```

#### Build Docker Image
```bash
# Build Docker image
docker build -t cleanroom:latest .

# Verify image
docker images | grep cleanroom
```

### 3. Service Installation

#### Create Systemd Service
```bash
# Create service file
sudo tee /etc/systemd/system/cleanroom.service > /dev/null <<EOF
[Unit]
Description=Cleanroom Testing Framework
After=docker.service
Requires=docker.service

[Service]
Type=simple
User=cleanroom
Group=cleanroom
WorkingDirectory=/opt/cleanroom
ExecStart=/usr/local/bin/cleanroom server
Restart=always
RestartSec=10
Environment=RUST_LOG=info
Environment=CLEANROOM_CONFIG=/etc/cleanroom/config.toml

[Install]
WantedBy=multi-user.target
EOF

# Create user and group
sudo useradd -r -s /bin/false cleanroom
sudo groupadd cleanroom

# Create directories
sudo mkdir -p /opt/cleanroom
sudo mkdir -p /etc/cleanroom
sudo mkdir -p /var/log/cleanroom

# Set permissions
sudo chown -R cleanroom:cleanroom /opt/cleanroom
sudo chown -R cleanroom:cleanroom /var/log/cleanroom
```

## Configuration

### 1. Basic Configuration

#### Create Configuration File
```bash
# Create main configuration
sudo tee /etc/cleanroom/config.toml > /dev/null <<EOF
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 120
test_execution_timeout = 300
enable_deterministic_execution = true
deterministic_seed = 42
enable_coverage_tracking = true
enable_snapshot_testing = true
enable_tracing = true

[cleanroom.resource_limits]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592  # 8GB
max_disk_usage_bytes = 107374182400  # 100GB
max_network_bandwidth_bytes_per_sec = 104857600  # 100MB/s
max_container_count = 50
max_test_execution_time = 300
enable_resource_monitoring = true
resource_cleanup_timeout = 60

[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080, 9090]
enable_data_redaction = true
redaction_patterns = [
    "password\\s*=\\s*[^\\s]+",
    "token\\s*=\\s*[^\\s]+",
    "api_key\\s*=\\s*[^\\s]+"
]
enable_audit_logging = true
security_level = "High"

[cleanroom.performance_monitoring]
enable_monitoring = true
metrics_interval = 10
enable_profiling = false
enable_memory_tracking = true

[cleanroom.performance_monitoring.thresholds]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592
max_test_execution_time = 300
max_container_startup_time = 120
EOF
```

#### Environment Variables
```bash
# Create environment file
sudo tee /etc/cleanroom/environment > /dev/null <<EOF
RUST_LOG=info
CLEANROOM_CONFIG=/etc/cleanroom/config.toml
CLEANROOM_DATA_DIR=/opt/cleanroom/data
CLEANROOM_LOG_DIR=/var/log/cleanroom
CLEANROOM_TEMP_DIR=/tmp/cleanroom
EOF
```

### 2. Advanced Configuration

#### Database Configuration
```bash
# PostgreSQL configuration
sudo tee /etc/cleanroom/postgres.toml > /dev/null <<EOF
[postgres]
host = "localhost"
port = 5432
database = "cleanroom"
username = "cleanroom"
password = "secure_password"
max_connections = 100
connection_timeout = 30
EOF
```

#### Redis Configuration
```bash
# Redis configuration
sudo tee /etc/cleanroom/redis.toml > /dev/null <<EOF
[redis]
host = "localhost"
port = 6379
password = "secure_password"
database = 0
max_connections = 50
connection_timeout = 30
EOF
```

#### Monitoring Configuration
```bash
# Prometheus configuration
sudo tee /etc/cleanroom/prometheus.yml > /dev/null <<EOF
global:
  scrape_interval: 15s
  evaluation_interval: 15s

scrape_configs:
  - job_name: 'cleanroom'
    static_configs:
      - targets: ['localhost:9090']
    scrape_interval: 10s
    metrics_path: /metrics
EOF
```

## Deployment

### 1. Single Node Deployment

#### Start Services
```bash
# Start Docker
sudo systemctl start docker

# Start Cleanroom
sudo systemctl start cleanroom

# Enable auto-start
sudo systemctl enable cleanroom

# Check status
sudo systemctl status cleanroom
```

#### Verify Deployment
```bash
# Check service status
cleanroom status

# Check API endpoint
curl http://localhost:8080/health

# Check metrics
curl http://localhost:9090/metrics

# Check logs
sudo journalctl -u cleanroom -f
```

### 2. Multi-Node Deployment

#### Load Balancer Configuration
```bash
# Nginx configuration
sudo tee /etc/nginx/sites-available/cleanroom > /dev/null <<EOF
upstream cleanroom_backend {
    server cleanroom-1:8080;
    server cleanroom-2:8080;
    server cleanroom-3:8080;
}

server {
    listen 80;
    server_name cleanroom.example.com;

    location / {
        proxy_pass http://cleanroom_backend;
        proxy_set_header Host \$host;
        proxy_set_header X-Real-IP \$remote_addr;
        proxy_set_header X-Forwarded-For \$proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto \$scheme;
    }

    location /metrics {
        proxy_pass http://cleanroom_backend;
        proxy_set_header Host \$host;
    }
}
EOF

# Enable site
sudo ln -s /etc/nginx/sites-available/cleanroom /etc/nginx/sites-enabled/
sudo nginx -t
sudo systemctl reload nginx
```

#### Docker Compose Deployment
```bash
# Create docker-compose.yml
cat > docker-compose.yml <<EOF
version: '3.8'

services:
  cleanroom-1:
    image: cleanroom:latest
    ports:
      - "8081:8080"
    environment:
      - CLEANROOM_NODE_ID=1
      - CLEANROOM_CLUSTER_MODE=true
    volumes:
      - ./config.toml:/etc/cleanroom/config.toml:ro
      - ./data:/opt/cleanroom/data
    restart: unless-stopped

  cleanroom-2:
    image: cleanroom:latest
    ports:
      - "8082:8080"
    environment:
      - CLEANROOM_NODE_ID=2
      - CLEANROOM_CLUSTER_MODE=true
    volumes:
      - ./config.toml:/etc/cleanroom/config.toml:ro
      - ./data:/opt/cleanroom/data
    restart: unless-stopped

  cleanroom-3:
    image: cleanroom:latest
    ports:
      - "8083:8080"
    environment:
      - CLEANROOM_NODE_ID=3
      - CLEANROOM_CLUSTER_MODE=true
    volumes:
      - ./config.toml:/etc/cleanroom/config.toml:ro
      - ./data:/opt/cleanroom/data
    restart: unless-stopped

  prometheus:
    image: prom/prometheus:latest
    ports:
      - "9090:9090"
    volumes:
      - ./prometheus.yml:/etc/prometheus/prometheus.yml:ro
    command:
      - '--config.file=/etc/prometheus/prometheus.yml'
      - '--storage.tsdb.path=/prometheus'
      - '--web.console.libraries=/etc/prometheus/console_libraries'
      - '--web.console.templates=/etc/prometheus/consoles'
    restart: unless-stopped

  grafana:
    image: grafana/grafana:latest
    ports:
      - "3000:3000"
    environment:
      - GF_SECURITY_ADMIN_PASSWORD=admin
    volumes:
      - grafana-storage:/var/lib/grafana
    restart: unless-stopped

volumes:
  grafana-storage:
EOF

# Start services
docker-compose up -d

# Check status
docker-compose ps
```

### 3. Kubernetes Deployment

#### Namespace
```bash
# Create namespace
kubectl create namespace cleanroom
```

#### ConfigMap
```bash
# Create ConfigMap
kubectl create configmap cleanroom-config \
  --from-file=config.toml \
  --namespace=cleanroom
```

#### Deployment
```bash
# Create deployment
cat > cleanroom-deployment.yaml <<EOF
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cleanroom
  namespace: cleanroom
spec:
  replicas: 3
  selector:
    matchLabels:
      app: cleanroom
  template:
    metadata:
      labels:
        app: cleanroom
    spec:
      containers:
      - name: cleanroom
        image: cleanroom:latest
        ports:
        - containerPort: 8080
        env:
        - name: CLEANROOM_CONFIG
          value: "/etc/cleanroom/config.toml"
        volumeMounts:
        - name: config
          mountPath: /etc/cleanroom
        resources:
          requests:
            memory: "2Gi"
            cpu: "1000m"
          limits:
            memory: "4Gi"
            cpu: "2000m"
      volumes:
      - name: config
        configMap:
          name: cleanroom-config
EOF

# Apply deployment
kubectl apply -f cleanroom-deployment.yaml
```

#### Service
```bash
# Create service
cat > cleanroom-service.yaml <<EOF
apiVersion: v1
kind: Service
metadata:
  name: cleanroom-service
  namespace: cleanroom
spec:
  selector:
    app: cleanroom
  ports:
  - port: 80
    targetPort: 8080
  type: LoadBalancer
EOF

# Apply service
kubectl apply -f cleanroom-service.yaml
```

## Monitoring

### 1. Health Checks

#### Basic Health Check
```bash
# Check service health
curl -f http://localhost:8080/health || echo "Service unhealthy"

# Check container health
docker ps --filter "health=unhealthy"

# Check system resources
free -h
df -h
top -bn1 | head -20
```

#### Automated Health Checks
```bash
# Create health check script
sudo tee /usr/local/bin/cleanroom-health-check > /dev/null <<EOF
#!/bin/bash

# Check service status
if ! systemctl is-active --quiet cleanroom; then
    echo "ERROR: Cleanroom service is not running"
    exit 1
fi

# Check API endpoint
if ! curl -f http://localhost:8080/health > /dev/null 2>&1; then
    echo "ERROR: Cleanroom API is not responding"
    exit 1
fi

# Check resource usage
CPU_USAGE=\$(top -bn1 | grep "Cpu(s)" | awk '{print \$2}' | cut -d'%' -f1)
if (( \$(echo "\$CPU_USAGE > 90" | bc -l) )); then
    echo "WARNING: High CPU usage: \$CPU_USAGE%"
fi

MEMORY_USAGE=\$(free | grep Mem | awk '{printf("%.1f", \$3/\$2 * 100.0)}')
if (( \$(echo "\$MEMORY_USAGE > 90" | bc -l) )); then
    echo "WARNING: High memory usage: \$MEMORY_USAGE%"
fi

echo "Health check passed"
exit 0
EOF

# Make executable
sudo chmod +x /usr/local/bin/cleanroom-health-check

# Add to cron
echo "*/5 * * * * /usr/local/bin/cleanroom-health-check" | sudo crontab -
```

### 2. Log Monitoring

#### Log Aggregation
```bash
# Configure Fluentd
sudo tee /etc/fluentd/fluent.conf > /dev/null <<EOF
<source>
  @type tail
  path /var/log/cleanroom/*.log
  pos_file /var/log/fluentd/cleanroom.log.pos
  tag cleanroom
  format json
</source>

<match cleanroom>
  @type elasticsearch
  host localhost
  port 9200
  index_name cleanroom
  type_name log
</match>
EOF

# Start Fluentd
sudo systemctl start fluentd
sudo systemctl enable fluentd
```

#### Log Analysis
```bash
# Search for errors
grep -i error /var/log/cleanroom/*.log | tail -20

# Search for warnings
grep -i warning /var/log/cleanroom/*.log | tail -20

# Monitor real-time logs
tail -f /var/log/cleanroom/cleanroom.log | grep -E "(ERROR|WARN|FATAL)"
```

### 3. Metrics Collection

#### Prometheus Configuration
```bash
# Start Prometheus
docker run -d \
  --name prometheus \
  -p 9090:9090 \
  -v /etc/cleanroom/prometheus.yml:/etc/prometheus/prometheus.yml \
  prom/prometheus:latest
```

#### Grafana Dashboard
```bash
# Start Grafana
docker run -d \
  --name grafana \
  -p 3000:3000 \
  -e GF_SECURITY_ADMIN_PASSWORD=admin \
  grafana/grafana:latest

# Import dashboard
curl -X POST \
  http://admin:admin@localhost:3000/api/dashboards/db \
  -H 'Content-Type: application/json' \
  -d @cleanroom-dashboard.json
```

## Maintenance

### 1. Routine Maintenance

#### Daily Tasks
```bash
# Check service status
sudo systemctl status cleanroom

# Review logs
sudo journalctl -u cleanroom --since "1 day ago"

# Check resource usage
cleanroom metrics

# Clean up old containers
docker system prune -f
```

#### Weekly Tasks
```bash
# Update system packages
sudo apt update && sudo apt upgrade -y

# Restart services
sudo systemctl restart cleanroom

# Backup configuration
tar -czf cleanroom-config-$(date +%Y%m%d).tar.gz /etc/cleanroom/

# Check disk usage
df -h
```

#### Monthly Tasks
```bash
# Update Cleanroom
git pull origin main
cargo build --release
sudo systemctl stop cleanroom
sudo cp target/release/cleanroom /usr/local/bin/
sudo systemctl start cleanroom

# Rotate logs
sudo logrotate /etc/logrotate.d/cleanroom

# Security updates
sudo apt autoremove -y
```

### 2. Backup Procedures

#### Configuration Backup
```bash
# Create backup script
sudo tee /usr/local/bin/cleanroom-backup > /dev/null <<EOF
#!/bin/bash

BACKUP_DIR="/opt/backups/cleanroom"
DATE=\$(date +%Y%m%d_%H%M%S)

# Create backup directory
mkdir -p \$BACKUP_DIR

# Backup configuration
tar -czf \$BACKUP_DIR/config-\$DATE.tar.gz /etc/cleanroom/

# Backup data
tar -czf \$BACKUP_DIR/data-\$DATE.tar.gz /opt/cleanroom/data/

# Backup logs
tar -czf \$BACKUP_DIR/logs-\$DATE.tar.gz /var/log/cleanroom/

# Keep only last 30 days
find \$BACKUP_DIR -name "*.tar.gz" -mtime +30 -delete

echo "Backup completed: \$DATE"
EOF

# Make executable
sudo chmod +x /usr/local/bin/cleanroom-backup

# Schedule backup
echo "0 2 * * * /usr/local/bin/cleanroom-backup" | sudo crontab -
```

#### Database Backup
```bash
# PostgreSQL backup
pg_dump -h localhost -U cleanroom cleanroom > cleanroom-backup-$(date +%Y%m%d).sql

# Redis backup
redis-cli --rdb cleanroom-backup-$(date +%Y%m%d).rdb
```

### 3. Update Procedures

#### Rolling Updates
```bash
# Update script
sudo tee /usr/local/bin/cleanroom-update > /dev/null <<EOF
#!/bin/bash

set -e

echo "Starting Cleanroom update..."

# Backup current version
sudo cp /usr/local/bin/cleanroom /usr/local/bin/cleanroom.backup

# Stop service
sudo systemctl stop cleanroom

# Update code
cd /opt/cleanroom
git pull origin main

# Build new version
cargo build --release

# Install new version
sudo cp target/release/cleanroom /usr/local/bin/

# Start service
sudo systemctl start cleanroom

# Verify update
sleep 10
if curl -f http://localhost:8080/health > /dev/null 2>&1; then
    echo "Update successful"
    sudo rm /usr/local/bin/cleanroom.backup
else
    echo "Update failed, rolling back"
    sudo cp /usr/local/bin/cleanroom.backup /usr/local/bin/cleanroom
    sudo systemctl restart cleanroom
    exit 1
fi
EOF

# Make executable
sudo chmod +x /usr/local/bin/cleanroom-update
```

## Troubleshooting

### 1. Common Issues

#### Service Won't Start
```bash
# Check service status
sudo systemctl status cleanroom

# Check logs
sudo journalctl -u cleanroom -n 50

# Check configuration
cleanroom config validate

# Check dependencies
docker --version
systemctl status docker
```

#### High Resource Usage
```bash
# Check resource usage
top -bn1 | head -20
free -h
df -h

# Check container usage
docker stats --no-stream

# Check specific processes
ps aux | grep cleanroom
```

#### Network Issues
```bash
# Check network connectivity
ping google.com
curl -I http://localhost:8080/health

# Check firewall
sudo ufw status
sudo iptables -L

# Check ports
netstat -tulpn | grep :8080
```

### 2. Performance Issues

#### Slow Response Times
```bash
# Check response times
curl -w "@curl-format.txt" -o /dev/null -s http://localhost:8080/health

# Check database performance
sudo -u postgres psql -c "SELECT * FROM pg_stat_activity;"

# Check disk I/O
iostat -x 1 5
```

#### Memory Leaks
```bash
# Monitor memory usage
watch -n 1 'free -h'

# Check process memory
ps aux --sort=-%mem | head -10

# Check container memory
docker stats --no-stream
```

### 3. Security Issues

#### Unauthorized Access
```bash
# Check access logs
grep "unauthorized" /var/log/cleanroom/*.log

# Check failed logins
grep "Failed password" /var/log/auth.log

# Check firewall rules
sudo ufw status verbose
```

#### Suspicious Activity
```bash
# Check network connections
netstat -tulpn | grep cleanroom

# Check process tree
pstree -p cleanroom

# Check file permissions
find /opt/cleanroom -type f -perm /o+w
```

## Emergency Procedures

### 1. Service Outage

#### Immediate Response
```bash
# Check service status
sudo systemctl status cleanroom

# Restart service
sudo systemctl restart cleanroom

# Check logs
sudo journalctl -u cleanroom -n 100

# Notify team
curl -X POST "https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK" \
  -H 'Content-type: application/json' \
  -d '{"text":"🚨 Cleanroom service outage detected"}'
```

#### Escalation
```bash
# If restart fails
sudo systemctl stop cleanroom
sudo systemctl start cleanroom

# If still failing
sudo journalctl -u cleanroom --since "1 hour ago" > /tmp/cleanroom-error.log
# Send logs to team

# Emergency rollback
sudo cp /usr/local/bin/cleanroom.backup /usr/local/bin/cleanroom
sudo systemctl restart cleanroom
```

### 2. Data Loss

#### Immediate Response
```bash
# Stop all services
sudo systemctl stop cleanroom

# Check data integrity
find /opt/cleanroom/data -type f -exec md5sum {} \; > /tmp/data-checksums

# Restore from backup
tar -xzf /opt/backups/cleanroom/data-$(date +%Y%m%d).tar.gz -C /
```

#### Recovery
```bash
# Restore database
psql -h localhost -U cleanroom cleanroom < cleanroom-backup-$(date +%Y%m%d).sql

# Restore Redis
redis-cli --pipe < cleanroom-backup-$(date +%Y%m%d).rdb

# Restart services
sudo systemctl start cleanroom
```

### 3. Security Breach

#### Immediate Response
```bash
# Isolate system
sudo ufw deny in
sudo ufw deny out

# Stop services
sudo systemctl stop cleanroom

# Preserve evidence
sudo cp -r /var/log/cleanroom /tmp/security-incident-$(date +%Y%m%d)
sudo cp -r /opt/cleanroom/data /tmp/security-incident-$(date +%Y%m%d)

# Notify security team
curl -X POST "https://hooks.slack.com/services/SECURITY/TEAM/WEBHOOK" \
  -H 'Content-type: application/json' \
  -d '{"text":"🚨 Security breach detected in Cleanroom"}'
```

#### Investigation
```bash
# Check access logs
grep -E "(unauthorized|failed|denied)" /var/log/cleanroom/*.log

# Check system logs
grep -E "(cleanroom|docker)" /var/log/syslog

# Check network connections
netstat -tulpn | grep cleanroom

# Check file integrity
find /opt/cleanroom -type f -exec md5sum {} \; > /tmp/integrity-check
```

## Contact Information

### Emergency Contacts
- **On-call Engineer**: [Phone Number]
- **Security Team**: [Phone Number]
- **Infrastructure Team**: [Phone Number]

### Escalation Procedures
1. **Level 1**: On-call engineer (15 minutes)
2. **Level 2**: Senior engineer (1 hour)
3. **Level 3**: Engineering manager (4 hours)
4. **Level 4**: Director of engineering (24 hours)

### Communication Channels
- **Slack**: #cleanroom-alerts
- **Email**: cleanroom-alerts@company.com
- **PagerDuty**: Cleanroom On-call
- **Phone**: [Emergency Hotline]

## Appendix

### A. Configuration Templates

#### Production Configuration
```toml
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 120
test_execution_timeout = 300
enable_deterministic_execution = true
deterministic_seed = 42
enable_coverage_tracking = true
enable_snapshot_testing = true
enable_tracing = true

[cleanroom.resource_limits]
max_cpu_usage_percent = 80.0
max_memory_usage_bytes = 8589934592
max_disk_usage_bytes = 107374182400
max_network_bandwidth_bytes_per_sec = 104857600
max_container_count = 50
max_test_execution_time = 300
enable_resource_monitoring = true
resource_cleanup_timeout = 60

[cleanroom.security_policy]
enable_network_isolation = true
enable_filesystem_isolation = true
enable_process_isolation = true
allowed_ports = [5432, 6379, 8080, 9090]
enable_data_redaction = true
redaction_patterns = [
    "password\\s*=\\s*[^\\s]+",
    "token\\s*=\\s*[^\\s]+",
    "api_key\\s*=\\s*[^\\s]+"
]
enable_audit_logging = true
security_level = "High"
```

### B. Monitoring Scripts

#### Health Check Script
```bash
#!/bin/bash
# Health check script for Cleanroom

set -e

echo "Starting Cleanroom health check..."

# Check service status
if ! systemctl is-active --quiet cleanroom; then
    echo "ERROR: Cleanroom service is not running"
    exit 1
fi

# Check API endpoint
if ! curl -f http://localhost:8080/health > /dev/null 2>&1; then
    echo "ERROR: Cleanroom API is not responding"
    exit 1
fi

# Check resource usage
CPU_USAGE=$(top -bn1 | grep "Cpu(s)" | awk '{print $2}' | cut -d'%' -f1)
if (( $(echo "$CPU_USAGE > 90" | bc -l) )); then
    echo "WARNING: High CPU usage: $CPU_USAGE%"
fi

MEMORY_USAGE=$(free | grep Mem | awk '{printf("%.1f", $3/$2 * 100.0)}')
if (( $(echo "$MEMORY_USAGE > 90" | bc -l) )); then
    echo "WARNING: High memory usage: $MEMORY_USAGE%"
fi

echo "Health check passed"
exit 0
```

### C. Troubleshooting Commands

#### System Diagnostics
```bash
# System information
uname -a
lsb_release -a
docker --version
rustc --version

# Service status
systemctl status cleanroom
journalctl -u cleanroom -n 50

# Resource usage
free -h
df -h
top -bn1 | head -20

# Network status
netstat -tulpn | grep cleanroom
ss -tulpn | grep cleanroom

# Process information
ps aux | grep cleanroom
pstree -p cleanroom
```

#### Log Analysis
```bash
# Error logs
grep -i error /var/log/cleanroom/*.log | tail -20

# Warning logs
grep -i warning /var/log/cleanroom/*.log | tail -20

# Recent logs
tail -100 /var/log/cleanroom/cleanroom.log

# Real-time monitoring
tail -f /var/log/cleanroom/cleanroom.log | grep -E "(ERROR|WARN|FATAL)"
```

This runbook provides comprehensive procedures for operating the Cleanroom Testing Framework in production. Regular updates and testing of these procedures are essential for maintaining reliable operations.
