# Disaster Recovery Guide

This guide provides comprehensive procedures for disaster recovery, backup, and business continuity for the Cleanroom Testing Framework.

## Table of Contents

1. [Disaster Recovery Overview](#disaster-recovery-overview)
2. [Backup Strategies](#backup-strategies)
3. [Recovery Procedures](#recovery-procedures)
4. [Failover Procedures](#failover-procedures)
5. [Data Recovery](#data-recovery)
6. [Service Recovery](#service-recovery)
7. [Testing Procedures](#testing-procedures)
8. [Documentation and Communication](#documentation-and-communication)
9. [Lessons Learned](#lessons-learned)

## Disaster Recovery Overview

### Recovery Objectives

The Cleanroom Testing Framework disaster recovery plan aims to achieve:

- **Recovery Time Objective (RTO)**: < 4 hours for critical services
- **Recovery Point Objective (RPO)**: < 1 hour data loss
- **Availability Target**: 99.9% uptime
- **Data Integrity**: 100% data consistency
- **Service Continuity**: Minimal disruption to operations

### Disaster Scenarios

#### Tier 1: Critical Disasters
- **Complete data center failure**
- **Major security breach**
- **Catastrophic hardware failure**
- **Natural disasters**

#### Tier 2: Significant Disasters
- **Database corruption**
- **Network infrastructure failure**
- **Application server failure**
- **Storage system failure**

#### Tier 3: Minor Disasters
- **Single service failure**
- **Configuration errors**
- **Performance degradation**
- **Partial data loss**

### Recovery Team Structure

#### Core Team
- **Incident Commander**: Overall disaster recovery coordination
- **Technical Lead**: Technical recovery procedures
- **Operations Lead**: Infrastructure and system recovery
- **Data Lead**: Database and data recovery
- **Communication Lead**: Stakeholder communication

#### Extended Team
- **Security Team**: Security incident response
- **Legal Team**: Compliance and legal requirements
- **Vendor Management**: Third-party service coordination
- **Customer Success**: Customer communication and support

## Backup Strategies

### 1. Data Backup

#### Database Backup
```bash
#!/bin/bash
# PostgreSQL backup script

BACKUP_DIR="/opt/backups/cleanroom/database"
DATE=$(date +%Y%m%d_%H%M%S)
RETENTION_DAYS=30

# Create backup directory
mkdir -p $BACKUP_DIR

# Full database backup
echo "Starting full database backup..."
pg_dump -h localhost -U cleanroom -d cleanroom \
    --format=custom \
    --compress=9 \
    --file="$BACKUP_DIR/cleanroom_full_$DATE.dump"

# Verify backup
if [ $? -eq 0 ]; then
    echo "Database backup completed successfully"
    
    # Test backup integrity
    pg_restore --list "$BACKUP_DIR/cleanroom_full_$DATE.dump" > /dev/null
    if [ $? -eq 0 ]; then
        echo "Backup integrity verified"
    else
        echo "ERROR: Backup integrity check failed"
        exit 1
    fi
else
    echo "ERROR: Database backup failed"
    exit 1
fi

# Clean up old backups
find $BACKUP_DIR -name "cleanroom_full_*.dump" -mtime +$RETENTION_DAYS -delete

echo "Database backup process completed"
```

#### Redis Backup
```bash
#!/bin/bash
# Redis backup script

BACKUP_DIR="/opt/backups/cleanroom/redis"
DATE=$(date +%Y%m%d_%H%M%S)
RETENTION_DAYS=30

# Create backup directory
mkdir -p $BACKUP_DIR

# Redis RDB backup
echo "Starting Redis backup..."
redis-cli --rdb "$BACKUP_DIR/redis_$DATE.rdb"

# Verify backup
if [ -f "$BACKUP_DIR/redis_$DATE.rdb" ]; then
    echo "Redis backup completed successfully"
    
    # Test backup integrity
    redis-cli --pipe < "$BACKUP_DIR/redis_$DATE.rdb" > /dev/null 2>&1
    if [ $? -eq 0 ]; then
        echo "Redis backup integrity verified"
    else
        echo "ERROR: Redis backup integrity check failed"
        exit 1
    fi
else
    echo "ERROR: Redis backup failed"
    exit 1
fi

# Clean up old backups
find $BACKUP_DIR -name "redis_*.rdb" -mtime +$RETENTION_DAYS -delete

echo "Redis backup process completed"
```

#### File System Backup
```bash
#!/bin/bash
# File system backup script

BACKUP_DIR="/opt/backups/cleanroom/filesystem"
DATE=$(date +%Y%m%d_%H%M%S)
RETENTION_DAYS=30
SOURCE_DIRS=("/opt/cleanroom" "/etc/cleanroom" "/var/log/cleanroom")

# Create backup directory
mkdir -p $BACKUP_DIR

# Backup each directory
for dir in "${SOURCE_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo "Backing up $dir..."
        tar -czf "$BACKUP_DIR/$(basename $dir)_$DATE.tar.gz" -C "$(dirname $dir)" "$(basename $dir)"
        
        if [ $? -eq 0 ]; then
            echo "Backup of $dir completed successfully"
        else
            echo "ERROR: Backup of $dir failed"
            exit 1
        fi
    else
        echo "WARNING: Directory $dir does not exist"
    fi
done

# Clean up old backups
find $BACKUP_DIR -name "*.tar.gz" -mtime +$RETENTION_DAYS -delete

echo "File system backup process completed"
```

### 2. Configuration Backup

#### System Configuration
```bash
#!/bin/bash
# System configuration backup script

BACKUP_DIR="/opt/backups/cleanroom/config"
DATE=$(date +%Y%m%d_%H%M%S)
RETENTION_DAYS=90

# Create backup directory
mkdir -p $BACKUP_DIR

# Backup system configuration
echo "Starting system configuration backup..."

# Systemd services
systemctl list-unit-files --type=service | grep cleanroom > "$BACKUP_DIR/systemd_services_$DATE.txt"

# Environment variables
env | grep CLEANROOM > "$BACKUP_DIR/environment_$DATE.txt"

# Network configuration
ip addr show > "$BACKUP_DIR/network_$DATE.txt"
ip route show > "$BACKUP_DIR/routing_$DATE.txt"

# Firewall rules
sudo ufw status > "$BACKUP_DIR/firewall_$DATE.txt"
sudo iptables -L > "$BACKUP_DIR/iptables_$DATE.txt"

# SSL certificates
if [ -d "/etc/ssl/certs" ]; then
    tar -czf "$BACKUP_DIR/ssl_certs_$DATE.tar.gz" -C /etc/ssl/certs .
fi

# Clean up old backups
find $BACKUP_DIR -name "*.txt" -mtime +$RETENTION_DAYS -delete
find $BACKUP_DIR -name "*.tar.gz" -mtime +$RETENTION_DAYS -delete

echo "System configuration backup completed"
```

#### Application Configuration
```bash
#!/bin/bash
# Application configuration backup script

BACKUP_DIR="/opt/backups/cleanroom/app_config"
DATE=$(date +%Y%m%d_%H%M%S)
RETENTION_DAYS=90

# Create backup directory
mkdir -p $BACKUP_DIR

# Backup application configuration
echo "Starting application configuration backup..."

# Main configuration
cp /etc/cleanroom/config.toml "$BACKUP_DIR/config_$DATE.toml"

# Database configuration
cp /etc/cleanroom/postgres.toml "$BACKUP_DIR/postgres_$DATE.toml" 2>/dev/null || true
cp /etc/cleanroom/redis.toml "$BACKUP_DIR/redis_$DATE.toml" 2>/dev/null || true

# Monitoring configuration
cp /etc/prometheus/prometheus.yml "$BACKUP_DIR/prometheus_$DATE.yml" 2>/dev/null || true
cp /etc/grafana/grafana.ini "$BACKUP_DIR/grafana_$DATE.ini" 2>/dev/null || true

# Docker configuration
cp /etc/docker/daemon.json "$BACKUP_DIR/docker_daemon_$DATE.json" 2>/dev/null || true

# Clean up old backups
find $BACKUP_DIR -name "*.toml" -mtime +$RETENTION_DAYS -delete
find $BACKUP_DIR -name "*.yml" -mtime +$RETENTION_DAYS -delete
find $BACKUP_DIR -name "*.ini" -mtime +$RETENTION_DAYS -delete
find $BACKUP_DIR -name "*.json" -mtime +$RETENTION_DAYS -delete

echo "Application configuration backup completed"
```

### 3. Automated Backup

#### Backup Scheduling
```bash
#!/bin/bash
# Automated backup scheduling script

# Create backup user
sudo useradd -r -s /bin/false cleanroom-backup

# Create backup directory
sudo mkdir -p /opt/backups/cleanroom
sudo chown -R cleanroom-backup:cleanroom-backup /opt/backups/cleanroom

# Create backup scripts directory
sudo mkdir -p /opt/cleanroom/scripts/backup
sudo chown -R cleanroom-backup:cleanroom-backup /opt/cleanroom/scripts/backup

# Copy backup scripts
sudo cp database_backup.sh /opt/cleanroom/scripts/backup/
sudo cp redis_backup.sh /opt/cleanroom/scripts/backup/
sudo cp filesystem_backup.sh /opt/cleanroom/scripts/backup/
sudo cp config_backup.sh /opt/cleanroom/scripts/backup/

# Make scripts executable
sudo chmod +x /opt/cleanroom/scripts/backup/*.sh

# Create cron jobs
sudo crontab -u cleanroom-backup - <<EOF
# Database backup every 6 hours
0 */6 * * * /opt/cleanroom/scripts/backup/database_backup.sh

# Redis backup every 6 hours
30 */6 * * * /opt/cleanroom/scripts/backup/redis_backup.sh

# File system backup daily at 2 AM
0 2 * * * /opt/cleanroom/scripts/backup/filesystem_backup.sh

# Configuration backup daily at 3 AM
0 3 * * * /opt/cleanroom/scripts/backup/config_backup.sh

# Weekly full backup on Sunday at 1 AM
0 1 * * 0 /opt/cleanroom/scripts/backup/full_backup.sh
EOF

echo "Automated backup scheduling completed"
```

#### Backup Monitoring
```bash
#!/bin/bash
# Backup monitoring script

BACKUP_DIR="/opt/backups/cleanroom"
LOG_FILE="/var/log/cleanroom/backup_monitor.log"
ALERT_EMAIL="alerts@company.com"

# Check backup status
check_backup_status() {
    local backup_type=$1
    local max_age_hours=$2
    
    case $backup_type in
        "database")
            latest_backup=$(find $BACKUP_DIR/database -name "*.dump" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)
            ;;
        "redis")
            latest_backup=$(find $BACKUP_DIR/redis -name "*.rdb" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)
            ;;
        "filesystem")
            latest_backup=$(find $BACKUP_DIR/filesystem -name "*.tar.gz" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)
            ;;
        *)
            echo "Unknown backup type: $backup_type"
            return 1
            ;;
    esac
    
    if [ -z "$latest_backup" ]; then
        echo "ERROR: No $backup_type backup found" | tee -a $LOG_FILE
        send_alert "No $backup_type backup found"
        return 1
    fi
    
    backup_age=$(($(date +%s) - $(stat -c %Y "$latest_backup")))
    backup_age_hours=$((backup_age / 3600))
    
    if [ $backup_age_hours -gt $max_age_hours ]; then
        echo "ERROR: $backup_type backup is $backup_age_hours hours old (max: $max_age_hours)" | tee -a $LOG_FILE
        send_alert "$backup_type backup is $backup_age_hours hours old"
        return 1
    else
        echo "OK: $backup_type backup is $backup_age_hours hours old" | tee -a $LOG_FILE
        return 0
    fi
}

# Send alert
send_alert() {
    local message=$1
    echo "$(date): $message" | mail -s "Cleanroom Backup Alert" $ALERT_EMAIL
}

# Check all backup types
echo "=== Backup Status Check ===" | tee -a $LOG_FILE
echo "Date: $(date)" | tee -a $LOG_FILE

check_backup_status "database" 12
check_backup_status "redis" 12
check_backup_status "filesystem" 48

echo "=== Backup Status Check Completed ===" | tee -a $LOG_FILE
```

## Recovery Procedures

### 1. Database Recovery

#### Full Database Recovery
```bash
#!/bin/bash
# Full database recovery script

BACKUP_DIR="/opt/backups/cleanroom/database"
TARGET_DB="cleanroom"
TARGET_USER="cleanroom"

echo "=== Full Database Recovery ==="
echo "Date: $(date)"

# List available backups
echo "Available backups:"
ls -la $BACKUP_DIR/*.dump

# Get latest backup
LATEST_BACKUP=$(find $BACKUP_DIR -name "*.dump" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)

if [ -z "$LATEST_BACKUP" ]; then
    echo "ERROR: No backup found"
    exit 1
fi

echo "Using backup: $LATEST_BACKUP"

# Stop application
echo "Stopping application..."
sudo systemctl stop cleanroom

# Drop existing database
echo "Dropping existing database..."
sudo -u postgres dropdb $TARGET_DB

# Create new database
echo "Creating new database..."
sudo -u postgres createdb $TARGET_DB

# Restore from backup
echo "Restoring from backup..."
pg_restore -h localhost -U $TARGET_USER -d $TARGET_DB --clean --if-exists "$LATEST_BACKUP"

if [ $? -eq 0 ]; then
    echo "Database recovery completed successfully"
    
    # Start application
    echo "Starting application..."
    sudo systemctl start cleanroom
    
    # Verify recovery
    echo "Verifying recovery..."
    psql -h localhost -U $TARGET_USER -d $TARGET_DB -c "SELECT COUNT(*) FROM tests;"
    
    if [ $? -eq 0 ]; then
        echo "Database recovery verified"
    else
        echo "ERROR: Database recovery verification failed"
        exit 1
    fi
else
    echo "ERROR: Database recovery failed"
    exit 1
fi

echo "Full database recovery completed"
```

#### Point-in-Time Recovery
```bash
#!/bin/bash
# Point-in-time recovery script

BACKUP_DIR="/opt/backups/cleanroom/database"
TARGET_DB="cleanroom"
TARGET_USER="cleanroom"
RECOVERY_TIME=$1

if [ -z "$RECOVERY_TIME" ]; then
    echo "Usage: $0 <recovery_time>"
    echo "Example: $0 '2024-01-15 14:30:00'"
    exit 1
fi

echo "=== Point-in-Time Recovery ==="
echo "Date: $(date)"
echo "Recovery time: $RECOVERY_TIME"

# Get base backup
BASE_BACKUP=$(find $BACKUP_DIR -name "*.dump" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)

if [ -z "$BASE_BACKUP" ]; then
    echo "ERROR: No base backup found"
    exit 1
fi

echo "Using base backup: $BASE_BACKUP"

# Stop application
echo "Stopping application..."
sudo systemctl stop cleanroom

# Create recovery directory
RECOVERY_DIR="/tmp/cleanroom_recovery_$(date +%Y%m%d_%H%M%S)"
mkdir -p $RECOVERY_DIR

# Restore base backup
echo "Restoring base backup..."
pg_restore -h localhost -U $TARGET_USER -d $TARGET_DB --clean --if-exists "$BASE_BACKUP"

# Apply WAL files up to recovery time
echo "Applying WAL files..."
sudo -u postgres psql -d $TARGET_DB -c "SELECT pg_recovery_to_time('$RECOVERY_TIME');"

if [ $? -eq 0 ]; then
    echo "Point-in-time recovery completed successfully"
    
    # Start application
    echo "Starting application..."
    sudo systemctl start cleanroom
    
    # Verify recovery
    echo "Verifying recovery..."
    psql -h localhost -U $TARGET_USER -d $TARGET_DB -c "SELECT COUNT(*) FROM tests;"
    
    if [ $? -eq 0 ]; then
        echo "Point-in-time recovery verified"
    else
        echo "ERROR: Point-in-time recovery verification failed"
        exit 1
    fi
else
    echo "ERROR: Point-in-time recovery failed"
    exit 1
fi

# Clean up
rm -rf $RECOVERY_DIR

echo "Point-in-time recovery completed"
```

### 2. Application Recovery

#### Service Recovery
```bash
#!/bin/bash
# Service recovery script

echo "=== Service Recovery ==="
echo "Date: $(date)"

# Check service status
echo "Checking service status..."
sudo systemctl status cleanroom

# Stop service
echo "Stopping service..."
sudo systemctl stop cleanroom

# Check for corrupted binary
echo "Checking binary integrity..."
md5sum /usr/local/bin/cleanroom

# Restore binary from backup
echo "Restoring binary from backup..."
sudo cp /opt/backups/cleanroom/filesystem/cleanroom_$(date +%Y%m%d).tar.gz /tmp/
cd /tmp
tar -xzf cleanroom_$(date +%Y%m%d).tar.gz
sudo cp cleanroom/cleanroom /usr/local/bin/
sudo chmod +x /usr/local/bin/cleanroom

# Restore configuration
echo "Restoring configuration..."
sudo cp /opt/backups/cleanroom/app_config/config_$(date +%Y%m%d).toml /etc/cleanroom/config.toml

# Start service
echo "Starting service..."
sudo systemctl start cleanroom

# Verify service
echo "Verifying service..."
sleep 10
curl -f http://localhost:8080/health

if [ $? -eq 0 ]; then
    echo "Service recovery completed successfully"
else
    echo "ERROR: Service recovery failed"
    exit 1
fi

echo "Service recovery completed"
```

#### Configuration Recovery
```bash
#!/bin/bash
# Configuration recovery script

BACKUP_DIR="/opt/backups/cleanroom/app_config"
DATE=$1

if [ -z "$DATE" ]; then
    echo "Usage: $0 <backup_date>"
    echo "Example: $0 20240115"
    exit 1
fi

echo "=== Configuration Recovery ==="
echo "Date: $(date)"
echo "Restoring configuration from: $DATE"

# Stop service
echo "Stopping service..."
sudo systemctl stop cleanroom

# Restore main configuration
if [ -f "$BACKUP_DIR/config_$DATE.toml" ]; then
    echo "Restoring main configuration..."
    sudo cp "$BACKUP_DIR/config_$DATE.toml" /etc/cleanroom/config.toml
else
    echo "ERROR: Main configuration backup not found"
    exit 1
fi

# Restore database configuration
if [ -f "$BACKUP_DIR/postgres_$DATE.toml" ]; then
    echo "Restoring database configuration..."
    sudo cp "$BACKUP_DIR/postgres_$DATE.toml" /etc/cleanroom/postgres.toml
fi

# Restore Redis configuration
if [ -f "$BACKUP_DIR/redis_$DATE.toml" ]; then
    echo "Restoring Redis configuration..."
    sudo cp "$BACKUP_DIR/redis_$DATE.toml" /etc/cleanroom/redis.toml
fi

# Restore monitoring configuration
if [ -f "$BACKUP_DIR/prometheus_$DATE.yml" ]; then
    echo "Restoring Prometheus configuration..."
    sudo cp "$BACKUP_DIR/prometheus_$DATE.yml" /etc/prometheus/prometheus.yml
    sudo systemctl restart prometheus
fi

if [ -f "$BACKUP_DIR/grafana_$DATE.ini" ]; then
    echo "Restoring Grafana configuration..."
    sudo cp "$BACKUP_DIR/grafana_$DATE.ini" /etc/grafana/grafana.ini
    sudo systemctl restart grafana
fi

# Start service
echo "Starting service..."
sudo systemctl start cleanroom

# Verify service
echo "Verifying service..."
sleep 10
curl -f http://localhost:8080/health

if [ $? -eq 0 ]; then
    echo "Configuration recovery completed successfully"
else
    echo "ERROR: Configuration recovery failed"
    exit 1
fi

echo "Configuration recovery completed"
```

### 3. Infrastructure Recovery

#### Server Recovery
```bash
#!/bin/bash
# Server recovery script

echo "=== Server Recovery ==="
echo "Date: $(date)"

# Check system status
echo "Checking system status..."
uptime
free -h
df -h

# Check network connectivity
echo "Checking network connectivity..."
ping -c 3 google.com

# Check service dependencies
echo "Checking service dependencies..."
sudo systemctl status docker
sudo systemctl status postgresql
sudo systemctl status redis

# Restart dependencies if needed
echo "Restarting dependencies..."
sudo systemctl restart docker
sudo systemctl restart postgresql
sudo systemctl restart redis

# Wait for services to start
echo "Waiting for services to start..."
sleep 30

# Verify dependencies
echo "Verifying dependencies..."
docker ps
sudo -u postgres psql -c "SELECT 1;"
redis-cli ping

# Start main service
echo "Starting main service..."
sudo systemctl start cleanroom

# Verify main service
echo "Verifying main service..."
sleep 10
curl -f http://localhost:8080/health

if [ $? -eq 0 ]; then
    echo "Server recovery completed successfully"
else
    echo "ERROR: Server recovery failed"
    exit 1
fi

echo "Server recovery completed"
```

## Failover Procedures

### 1. Active-Passive Failover

#### Primary Server Failure
```bash
#!/bin/bash
# Primary server failure failover script

SECONDARY_SERVER="cleanroom-secondary.company.com"
PRIMARY_SERVER="cleanroom-primary.company.com"

echo "=== Primary Server Failure Failover ==="
echo "Date: $(date)"

# Check primary server status
echo "Checking primary server status..."
ping -c 3 $PRIMARY_SERVER

if [ $? -eq 0 ]; then
    echo "Primary server is reachable, checking service..."
    curl -f http://$PRIMARY_SERVER:8080/health
    
    if [ $? -eq 0 ]; then
        echo "Primary server is healthy, no failover needed"
        exit 0
    fi
fi

echo "Primary server is down, initiating failover..."

# Update DNS to point to secondary server
echo "Updating DNS records..."
# This would typically be done through your DNS provider's API
# For example, using Cloudflare API:
# curl -X PUT "https://api.cloudflare.com/client/v4/zones/$ZONE_ID/dns_records/$RECORD_ID" \
#   -H "Authorization: Bearer $API_TOKEN" \
#   -H "Content-Type: application/json" \
#   --data '{"type":"A","name":"cleanroom","content":"'$SECONDARY_SERVER_IP'","ttl":300}'

# Start services on secondary server
echo "Starting services on secondary server..."
ssh $SECONDARY_SERVER "sudo systemctl start cleanroom"

# Verify secondary server
echo "Verifying secondary server..."
sleep 30
curl -f http://$SECONDARY_SERVER:8080/health

if [ $? -eq 0 ]; then
    echo "Failover completed successfully"
    
    # Notify team
    echo "Failover completed successfully" | mail -s "Cleanroom Failover Alert" alerts@company.com
    
    # Update monitoring
    curl -X POST "https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK" \
      -H 'Content-type: application/json' \
      -d '{"text":"✅ Cleanroom failover completed successfully"}'
else
    echo "ERROR: Failover failed"
    exit 1
fi

echo "Primary server failure failover completed"
```

#### Secondary Server Activation
```bash
#!/bin/bash
# Secondary server activation script

echo "=== Secondary Server Activation ==="
echo "Date: $(date)"

# Check system status
echo "Checking system status..."
uptime
free -h
df -h

# Check network connectivity
echo "Checking network connectivity..."
ping -c 3 google.com

# Start all services
echo "Starting all services..."
sudo systemctl start docker
sudo systemctl start postgresql
sudo systemctl start redis
sudo systemctl start cleanroom

# Wait for services to start
echo "Waiting for services to start..."
sleep 60

# Verify all services
echo "Verifying all services..."
docker ps
sudo -u postgres psql -c "SELECT 1;"
redis-cli ping
curl -f http://localhost:8080/health

if [ $? -eq 0 ]; then
    echo "Secondary server activation completed successfully"
    
    # Notify team
    echo "Secondary server activated successfully" | mail -s "Cleanroom Secondary Server Alert" alerts@company.com
    
    # Update monitoring
    curl -X POST "https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK" \
      -H 'Content-type: application/json' \
      -d '{"text":"✅ Cleanroom secondary server activated successfully"}'
else
    echo "ERROR: Secondary server activation failed"
    exit 1
fi

echo "Secondary server activation completed"
```

### 2. Load Balancer Failover

#### Load Balancer Configuration
```bash
#!/bin/bash
# Load balancer failover configuration

PRIMARY_SERVERS=("cleanroom-1.company.com" "cleanroom-2.company.com")
SECONDARY_SERVERS=("cleanroom-3.company.com" "cleanroom-4.company.com")

echo "=== Load Balancer Failover Configuration ==="
echo "Date: $(date)"

# Check primary servers
echo "Checking primary servers..."
for server in "${PRIMARY_SERVERS[@]}"; do
    echo "Checking $server..."
    curl -f http://$server:8080/health
    
    if [ $? -ne 0 ]; then
        echo "WARNING: $server is not responding"
    fi
done

# Check secondary servers
echo "Checking secondary servers..."
for server in "${SECONDARY_SERVERS[@]}"; do
    echo "Checking $server..."
    curl -f http://$server:8080/health
    
    if [ $? -eq 0 ]; then
        echo "INFO: $server is healthy and ready for failover"
    else
        echo "WARNING: $server is not responding"
    fi
done

# Update load balancer configuration
echo "Updating load balancer configuration..."
# This would typically be done through your load balancer's API
# For example, using HAProxy:
# echo "server cleanroom-1 $PRIMARY_SERVERS[0]:8080 check" >> /etc/haproxy/haproxy.cfg
# echo "server cleanroom-2 $PRIMARY_SERVERS[1]:8080 check" >> /etc/haproxy/haproxy.cfg
# echo "server cleanroom-3 $SECONDARY_SERVERS[0]:8080 check backup" >> /etc/haproxy/haproxy.cfg
# echo "server cleanroom-4 $SECONDARY_SERVERS[1]:8080 check backup" >> /etc/haproxy/haproxy.cfg

# Reload load balancer
echo "Reloading load balancer..."
sudo systemctl reload haproxy

echo "Load balancer failover configuration completed"
```

## Data Recovery

### 1. Data Corruption Recovery

#### Database Corruption
```bash
#!/bin/bash
# Database corruption recovery script

BACKUP_DIR="/opt/backups/cleanroom/database"
TARGET_DB="cleanroom"
TARGET_USER="cleanroom"

echo "=== Database Corruption Recovery ==="
echo "Date: $(date)"

# Check database integrity
echo "Checking database integrity..."
sudo -u postgres psql -d $TARGET_DB -c "SELECT pg_database_size('$TARGET_DB');"

# Check for corruption
echo "Checking for corruption..."
sudo -u postgres psql -d $TARGET_DB -c "SELECT * FROM pg_stat_database WHERE datname = '$TARGET_DB';"

# Try to repair database
echo "Attempting to repair database..."
sudo -u postgres psql -d $TARGET_DB -c "REINDEX DATABASE $TARGET_DB;"

# Check if repair was successful
echo "Checking if repair was successful..."
sudo -u postgres psql -d $TARGET_DB -c "SELECT COUNT(*) FROM tests;"

if [ $? -eq 0 ]; then
    echo "Database repair completed successfully"
else
    echo "Database repair failed, restoring from backup..."
    
    # Stop application
    sudo systemctl stop cleanroom
    
    # Get latest backup
    LATEST_BACKUP=$(find $BACKUP_DIR -name "*.dump" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)
    
    if [ -z "$LATEST_BACKUP" ]; then
        echo "ERROR: No backup found"
        exit 1
    fi
    
    echo "Using backup: $LATEST_BACKUP"
    
    # Drop corrupted database
    sudo -u postgres dropdb $TARGET_DB
    
    # Create new database
    sudo -u postgres createdb $TARGET_DB
    
    # Restore from backup
    pg_restore -h localhost -U $TARGET_USER -d $TARGET_DB --clean --if-exists "$LATEST_BACKUP"
    
    if [ $? -eq 0 ]; then
        echo "Database recovery completed successfully"
        
        # Start application
        sudo systemctl start cleanroom
        
        # Verify recovery
        sleep 10
        curl -f http://localhost:8080/health
        
        if [ $? -eq 0 ]; then
            echo "Database recovery verified"
        else
            echo "ERROR: Database recovery verification failed"
            exit 1
        fi
    else
        echo "ERROR: Database recovery failed"
        exit 1
    fi
fi

echo "Database corruption recovery completed"
```

#### File System Corruption
```bash
#!/bin/bash
# File system corruption recovery script

BACKUP_DIR="/opt/backups/cleanroom/filesystem"
TARGET_DIRS=("/opt/cleanroom" "/etc/cleanroom" "/var/log/cleanroom")

echo "=== File System Corruption Recovery ==="
echo "Date: $(date)"

# Check file system integrity
echo "Checking file system integrity..."
fsck -f /dev/sda1

# Check for corrupted files
echo "Checking for corrupted files..."
for dir in "${TARGET_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo "Checking $dir..."
        find "$dir" -type f -exec file {} \; | grep -v "ASCII text\|UTF-8\|empty"
    fi
done

# Restore corrupted files from backup
echo "Restoring corrupted files from backup..."
for dir in "${TARGET_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        backup_file="$BACKUP_DIR/$(basename $dir)_$(date +%Y%m%d).tar.gz"
        
        if [ -f "$backup_file" ]; then
            echo "Restoring $dir from $backup_file..."
            sudo tar -xzf "$backup_file" -C "$(dirname $dir)"
        else
            echo "WARNING: No backup found for $dir"
        fi
    fi
done

# Verify restoration
echo "Verifying restoration..."
for dir in "${TARGET_DIRS[@]}"; do
    if [ -d "$dir" ]; then
        echo "Verifying $dir..."
        find "$dir" -type f -exec file {} \; | grep -v "ASCII text\|UTF-8\|empty"
    fi
done

echo "File system corruption recovery completed"
```

### 2. Partial Data Recovery

#### Selective Data Recovery
```bash
#!/bin/bash
# Selective data recovery script

BACKUP_DIR="/opt/backups/cleanroom/database"
TARGET_DB="cleanroom"
TARGET_USER="cleanroom"
RECOVERY_TABLE=$1

if [ -z "$RECOVERY_TABLE" ]; then
    echo "Usage: $0 <table_name>"
    echo "Example: $0 tests"
    exit 1
fi

echo "=== Selective Data Recovery ==="
echo "Date: $(date)"
echo "Recovering table: $RECOVERY_TABLE"

# Get latest backup
LATEST_BACKUP=$(find $BACKUP_DIR -name "*.dump" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)

if [ -z "$LATEST_BACKUP" ]; then
    echo "ERROR: No backup found"
    exit 1
fi

echo "Using backup: $LATEST_BACKUP"

# Create temporary database
TEMP_DB="cleanroom_temp_$(date +%Y%m%d_%H%M%S)"
echo "Creating temporary database: $TEMP_DB"
sudo -u postgres createdb $TEMP_DB

# Restore backup to temporary database
echo "Restoring backup to temporary database..."
pg_restore -h localhost -U $TARGET_USER -d $TEMP_DB --clean --if-exists "$LATEST_BACKUP"

# Export specific table
echo "Exporting table $RECOVERY_TABLE..."
pg_dump -h localhost -U $TARGET_USER -d $TEMP_DB -t $RECOVERY_TABLE --data-only > /tmp/${RECOVERY_TABLE}_data.sql

# Import data to main database
echo "Importing data to main database..."
sudo -u postgres psql -d $TARGET_DB -f /tmp/${RECOVERY_TABLE}_data.sql

# Clean up
echo "Cleaning up..."
sudo -u postgres dropdb $TEMP_DB
rm -f /tmp/${RECOVERY_TABLE}_data.sql

# Verify recovery
echo "Verifying recovery..."
sudo -u postgres psql -d $TARGET_DB -c "SELECT COUNT(*) FROM $RECOVERY_TABLE;"

if [ $? -eq 0 ]; then
    echo "Selective data recovery completed successfully"
else
    echo "ERROR: Selective data recovery failed"
    exit 1
fi

echo "Selective data recovery completed"
```

## Service Recovery

### 1. Service Restart Procedures

#### Graceful Service Restart
```bash
#!/bin/bash
# Graceful service restart script

echo "=== Graceful Service Restart ==="
echo "Date: $(date)"

# Check service status
echo "Checking service status..."
sudo systemctl status cleanroom

# Graceful shutdown
echo "Graceful shutdown..."
sudo systemctl stop cleanroom

# Wait for graceful shutdown
echo "Waiting for graceful shutdown..."
sleep 30

# Check if service is still running
if pgrep -f cleanroom > /dev/null; then
    echo "Service is still running, forcing shutdown..."
    sudo pkill -TERM cleanroom
    sleep 10
    
    if pgrep -f cleanroom > /dev/null; then
        echo "Service is still running, killing..."
        sudo pkill -KILL cleanroom
    fi
fi

# Start service
echo "Starting service..."
sudo systemctl start cleanroom

# Wait for service to start
echo "Waiting for service to start..."
sleep 30

# Verify service
echo "Verifying service..."
curl -f http://localhost:8080/health

if [ $? -eq 0 ]; then
    echo "Graceful service restart completed successfully"
else
    echo "ERROR: Graceful service restart failed"
    exit 1
fi

echo "Graceful service restart completed"
```

#### Emergency Service Restart
```bash
#!/bin/bash
# Emergency service restart script

echo "=== Emergency Service Restart ==="
echo "Date: $(date)"

# Force kill all cleanroom processes
echo "Force killing all cleanroom processes..."
sudo pkill -KILL cleanroom

# Wait for processes to die
echo "Waiting for processes to die..."
sleep 10

# Check for remaining processes
if pgrep -f cleanroom > /dev/null; then
    echo "WARNING: Some cleanroom processes are still running"
    sudo ps aux | grep cleanroom
fi

# Start service
echo "Starting service..."
sudo systemctl start cleanroom

# Wait for service to start
echo "Waiting for service to start..."
sleep 30

# Verify service
echo "Verifying service..."
curl -f http://localhost:8080/health

if [ $? -eq 0 ]; then
    echo "Emergency service restart completed successfully"
else
    echo "ERROR: Emergency service restart failed"
    exit 1
fi

echo "Emergency service restart completed"
```

### 2. Service Dependency Recovery

#### Dependency Recovery
```bash
#!/bin/bash
# Service dependency recovery script

echo "=== Service Dependency Recovery ==="
echo "Date: $(date)"

# Check Docker
echo "Checking Docker..."
sudo systemctl status docker

if [ $? -ne 0 ]; then
    echo "Docker is not running, starting..."
    sudo systemctl start docker
    sleep 10
fi

# Check PostgreSQL
echo "Checking PostgreSQL..."
sudo systemctl status postgresql

if [ $? -ne 0 ]; then
    echo "PostgreSQL is not running, starting..."
    sudo systemctl start postgresql
    sleep 30
fi

# Check Redis
echo "Checking Redis..."
sudo systemctl status redis

if [ $? -ne 0 ]; then
    echo "Redis is not running, starting..."
    sudo systemctl start redis
    sleep 10
fi

# Verify dependencies
echo "Verifying dependencies..."
docker ps
sudo -u postgres psql -c "SELECT 1;"
redis-cli ping

# Start main service
echo "Starting main service..."
sudo systemctl start cleanroom

# Wait for service to start
echo "Waiting for service to start..."
sleep 30

# Verify main service
echo "Verifying main service..."
curl -f http://localhost:8080/health

if [ $? -eq 0 ]; then
    echo "Service dependency recovery completed successfully"
else
    echo "ERROR: Service dependency recovery failed"
    exit 1
fi

echo "Service dependency recovery completed"
```

## Testing Procedures

### 1. Disaster Recovery Testing

#### Backup Testing
```bash
#!/bin/bash
# Backup testing script

BACKUP_DIR="/opt/backups/cleanroom"
TEST_DB="cleanroom_test_$(date +%Y%m%d_%H%M%S)"

echo "=== Backup Testing ==="
echo "Date: $(date)"

# Test database backup
echo "Testing database backup..."
LATEST_DB_BACKUP=$(find $BACKUP_DIR/database -name "*.dump" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)

if [ -n "$LATEST_DB_BACKUP" ]; then
    echo "Testing database backup: $LATEST_DB_BACKUP"
    
    # Create test database
    sudo -u postgres createdb $TEST_DB
    
    # Restore backup to test database
    pg_restore -h localhost -U cleanroom -d $TEST_DB --clean --if-exists "$LATEST_DB_BACKUP"
    
    if [ $? -eq 0 ]; then
        echo "Database backup test passed"
        
        # Verify data
        sudo -u postgres psql -d $TEST_DB -c "SELECT COUNT(*) FROM tests;"
        
        if [ $? -eq 0 ]; then
            echo "Database backup data verification passed"
        else
            echo "ERROR: Database backup data verification failed"
        fi
    else
        echo "ERROR: Database backup test failed"
    fi
    
    # Clean up test database
    sudo -u postgres dropdb $TEST_DB
else
    echo "WARNING: No database backup found"
fi

# Test file system backup
echo "Testing file system backup..."
LATEST_FS_BACKUP=$(find $BACKUP_DIR/filesystem -name "*.tar.gz" -type f -printf '%T@ %p\n' | sort -n | tail -1 | cut -d' ' -f2)

if [ -n "$LATEST_FS_BACKUP" ]; then
    echo "Testing file system backup: $LATEST_FS_BACKUP"
    
    # Extract backup to temporary directory
    TEST_DIR="/tmp/cleanroom_test_$(date +%Y%m%d_%H%M%S)"
    mkdir -p $TEST_DIR
    tar -xzf "$LATEST_FS_BACKUP" -C $TEST_DIR
    
    if [ $? -eq 0 ]; then
        echo "File system backup test passed"
        
        # Verify files
        find $TEST_DIR -type f | head -10
        
        if [ $? -eq 0 ]; then
            echo "File system backup data verification passed"
        else
            echo "ERROR: File system backup data verification failed"
        fi
    else
        echo "ERROR: File system backup test failed"
    fi
    
    # Clean up test directory
    rm -rf $TEST_DIR
else
    echo "WARNING: No file system backup found"
fi

echo "Backup testing completed"
```

#### Recovery Testing
```bash
#!/bin/bash
# Recovery testing script

echo "=== Recovery Testing ==="
echo "Date: $(date)"

# Test database recovery
echo "Testing database recovery..."
./database_recovery.sh

if [ $? -eq 0 ]; then
    echo "Database recovery test passed"
else
    echo "ERROR: Database recovery test failed"
fi

# Test service recovery
echo "Testing service recovery..."
./service_recovery.sh

if [ $? -eq 0 ]; then
    echo "Service recovery test passed"
else
    echo "ERROR: Service recovery test failed"
fi

# Test configuration recovery
echo "Testing configuration recovery..."
./config_recovery.sh $(date +%Y%m%d)

if [ $? -eq 0 ]; then
    echo "Configuration recovery test passed"
else
    echo "ERROR: Configuration recovery test failed"
fi

echo "Recovery testing completed"
```

### 2. Failover Testing

#### Failover Test
```bash
#!/bin/bash
# Failover testing script

PRIMARY_SERVER="cleanroom-primary.company.com"
SECONDARY_SERVER="cleanroom-secondary.company.com"

echo "=== Failover Testing ==="
echo "Date: $(date)"

# Test primary server
echo "Testing primary server..."
curl -f http://$PRIMARY_SERVER:8080/health

if [ $? -eq 0 ]; then
    echo "Primary server is healthy"
else
    echo "Primary server is not responding"
fi

# Test secondary server
echo "Testing secondary server..."
curl -f http://$SECONDARY_SERVER:8080/health

if [ $? -eq 0 ]; then
    echo "Secondary server is healthy"
else
    echo "Secondary server is not responding"
fi

# Test failover procedure
echo "Testing failover procedure..."
./primary_failover.sh

if [ $? -eq 0 ]; then
    echo "Failover test passed"
else
    echo "ERROR: Failover test failed"
fi

echo "Failover testing completed"
```

## Documentation and Communication

### 1. Incident Documentation

#### Incident Report Template
```markdown
# Incident Report Template

## Incident Summary
- **Incident ID**: INC-YYYY-MM-DD-001
- **Date**: YYYY-MM-DD
- **Time**: HH:MM:SS
- **Duration**: X hours Y minutes
- **Severity**: P1/P2/P3/P4
- **Status**: Resolved/Investigating/Mitigated

## Incident Description
Brief description of what happened.

## Root Cause Analysis
Detailed analysis of the root cause.

## Impact Assessment
- **Services Affected**: List of affected services
- **Users Affected**: Number of users affected
- **Data Loss**: Amount of data lost
- **Financial Impact**: Estimated financial impact

## Timeline
- **Detection Time**: When the incident was first detected
- **Response Time**: Time to first response
- **Resolution Time**: Time to resolution
- **Recovery Time**: Time to full recovery

## Actions Taken
List of actions taken during the incident.

## Lessons Learned
Key lessons learned from the incident.

## Recommendations
Recommendations for preventing similar incidents.

## Follow-up Actions
List of follow-up actions to be taken.
```

#### Communication Plan
```bash
#!/bin/bash
# Communication plan script

INCIDENT_ID="INC-$(date +%Y-%m-%d)-001"
SEVERITY=$1
MESSAGE=$2

if [ -z "$SEVERITY" ] || [ -z "$MESSAGE" ]; then
    echo "Usage: $0 <severity> <message>"
    echo "Severity: P1, P2, P3, P4"
    exit 1
fi

echo "=== Incident Communication ==="
echo "Incident ID: $INCIDENT_ID"
echo "Severity: $SEVERITY"
echo "Message: $MESSAGE"

# Send email notification
echo "Sending email notification..."
echo "Incident ID: $INCIDENT_ID
Severity: $SEVERITY
Message: $MESSAGE
Time: $(date)" | mail -s "Cleanroom Incident Alert" alerts@company.com

# Send Slack notification
echo "Sending Slack notification..."
curl -X POST "https://hooks.slack.com/services/YOUR/SLACK/WEBHOOK" \
  -H 'Content-type: application/json' \
  -d "{
    \"text\": \"🚨 Cleanroom Incident Alert\",
    \"attachments\": [{
      \"color\": \"danger\",
      \"fields\": [{
        \"title\": \"Incident ID\",
        \"value\": \"$INCIDENT_ID\",
        \"short\": true
      }, {
        \"title\": \"Severity\",
        \"value\": \"$SEVERITY\",
        \"short\": true
      }, {
        \"title\": \"Message\",
        \"value\": \"$MESSAGE\",
        \"short\": false
      }]
    }]
  }"

# Send PagerDuty notification
echo "Sending PagerDuty notification..."
curl -X POST "https://api.pagerduty.com/incidents" \
  -H "Authorization: Token token=YOUR_TOKEN" \
  -H "Content-Type: application/json" \
  -d "{
    \"incident\": {
      \"type\": \"incident\",
      \"title\": \"Cleanroom Incident: $INCIDENT_ID\",
      \"service\": {
        \"id\": \"YOUR_SERVICE_ID\",
        \"type\": \"service_reference\"
      },
      \"priority\": {
        \"id\": \"YOUR_PRIORITY_ID\",
        \"type\": \"priority_reference\"
      },
      \"body\": {
        \"type\": \"incident_body\",
        \"details\": \"$MESSAGE\"
      }
    }
  }"

echo "Incident communication completed"
```

### 2. Recovery Documentation

#### Recovery Checklist
```markdown
# Recovery Checklist

## Pre-Recovery
- [ ] Incident declared and team notified
- [ ] Recovery team assembled
- [ ] Recovery plan reviewed and approved
- [ ] Backup integrity verified
- [ ] Recovery environment prepared

## Recovery Execution
- [ ] Stop affected services
- [ ] Isolate affected systems
- [ ] Restore from backup
- [ ] Verify data integrity
- [ ] Start services
- [ ] Verify service functionality
- [ ] Update monitoring

## Post-Recovery
- [ ] Service functionality verified
- [ ] Data integrity confirmed
- [ ] Performance monitoring active
- [ ] Team notified of recovery completion
- [ ] Incident documentation updated
- [ ] Lessons learned documented
- [ ] Follow-up actions identified

## Sign-off
- [ ] Technical Lead approval
- [ ] Operations Lead approval
- [ ] Incident Commander approval
```

## Lessons Learned

### 1. Incident Analysis

#### Post-Incident Review
```bash
#!/bin/bash
# Post-incident review script

INCIDENT_ID=$1

if [ -z "$INCIDENT_ID" ]; then
    echo "Usage: $0 <incident_id>"
    exit 1
fi

echo "=== Post-Incident Review ==="
echo "Incident ID: $INCIDENT_ID"
echo "Date: $(date)"

# Collect incident data
echo "Collecting incident data..."
INCIDENT_LOG="/var/log/cleanroom/incident_$INCIDENT_ID.log"
SYSTEM_LOG="/var/log/cleanroom/cleanroom.log"
ERROR_LOG="/var/log/cleanroom/error.log"

# Analyze logs
echo "Analyzing logs..."
grep -i "error\|fail\|exception" $SYSTEM_LOG | tail -100 > $INCIDENT_LOG
grep -i "error\|fail\|exception" $ERROR_LOG | tail -100 >> $INCIDENT_LOG

# Collect system metrics
echo "Collecting system metrics..."
echo "=== System Metrics ===" >> $INCIDENT_LOG
uptime >> $INCIDENT_LOG
free -h >> $INCIDENT_LOG
df -h >> $INCIDENT_LOG
top -bn1 | head -20 >> $INCIDENT_LOG

# Collect service status
echo "Collecting service status..."
echo "=== Service Status ===" >> $INCIDENT_LOG
sudo systemctl status cleanroom >> $INCIDENT_LOG
sudo systemctl status docker >> $INCIDENT_LOG
sudo systemctl status postgresql >> $INCIDENT_LOG
sudo systemctl status redis >> $INCIDENT_LOG

# Collect network status
echo "Collecting network status..."
echo "=== Network Status ===" >> $INCIDENT_LOG
netstat -tulpn | grep cleanroom >> $INCIDENT_LOG
ss -tulpn | grep cleanroom >> $INCIDENT_LOG

echo "Post-incident review completed"
echo "Incident log: $INCIDENT_LOG"
```

### 2. Improvement Recommendations

#### Process Improvements
```markdown
# Process Improvement Recommendations

## Backup Improvements
- [ ] Implement incremental backups
- [ ] Add backup encryption
- [ ] Implement backup verification automation
- [ ] Add cross-region backup replication
- [ ] Implement backup retention policies

## Recovery Improvements
- [ ] Implement automated recovery procedures
- [ ] Add recovery time monitoring
- [ ] Implement recovery testing automation
- [ ] Add recovery validation checks
- [ ] Implement recovery rollback procedures

## Monitoring Improvements
- [ ] Add predictive monitoring
- [ ] Implement anomaly detection
- [ ] Add performance baseline monitoring
- [ ] Implement capacity planning alerts
- [ ] Add security monitoring

## Communication Improvements
- [ ] Implement automated incident communication
- [ ] Add stakeholder notification system
- [ ] Implement incident status updates
- [ ] Add recovery progress tracking
- [ ] Implement post-incident reporting

## Documentation Improvements
- [ ] Update recovery procedures
- [ ] Add troubleshooting guides
- [ ] Implement knowledge base
- [ ] Add training materials
- [ ] Implement procedure validation
```

This disaster recovery guide provides comprehensive procedures for ensuring business continuity and data protection for the Cleanroom Testing Framework. Regular testing and updates of these procedures are essential for maintaining effective disaster recovery capabilities.
