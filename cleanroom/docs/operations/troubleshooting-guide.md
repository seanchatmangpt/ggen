# Troubleshooting Guide

This guide provides comprehensive troubleshooting procedures for common issues encountered with the Cleanroom Testing Framework.

## Table of Contents

1. [Quick Diagnosis](#quick-diagnosis)
2. [Service Issues](#service-issues)
3. [Container Issues](#container-issues)
4. [Performance Issues](#performance-issues)
5. [Network Issues](#network-issues)
6. [Database Issues](#database-issues)
7. [Security Issues](#security-issues)
8. [Configuration Issues](#configuration-issues)
9. [Log Analysis](#log-analysis)
10. [Emergency Procedures](#emergency-procedures)

## Quick Diagnosis

### Health Check Commands

```bash
# Overall system health
cleanroom status

# Service status
sudo systemctl status cleanroom

# API health
curl -f http://localhost:8080/health

# Container status
docker ps -a

# Resource usage
free -h && df -h && top -bn1 | head -20

# Network connectivity
ping -c 3 google.com
```

### Quick Fixes

```bash
# Restart service
sudo systemctl restart cleanroom

# Restart Docker
sudo systemctl restart docker

# Clear Docker cache
docker system prune -f

# Check logs
sudo journalctl -u cleanroom -n 50

# Validate configuration
cleanroom config validate
```

## Service Issues

### 1. Service Won't Start

#### Symptoms
- Service fails to start
- Error messages in systemd logs
- Port conflicts
- Permission issues

#### Diagnosis
```bash
# Check service status
sudo systemctl status cleanroom

# Check detailed logs
sudo journalctl -u cleanroom -n 100

# Check configuration
cleanroom config validate

# Check dependencies
systemctl status docker
systemctl status postgresql
systemctl status redis
```

#### Common Causes and Solutions

##### Port Already in Use
```bash
# Check what's using the port
sudo netstat -tulpn | grep :8080
sudo lsof -i :8080

# Kill the process
sudo kill -9 <PID>

# Or change port in configuration
sudo sed -i 's/8080/8081/g' /etc/cleanroom/config.toml
sudo systemctl restart cleanroom
```

##### Permission Issues
```bash
# Check file permissions
ls -la /usr/local/bin/cleanroom
ls -la /etc/cleanroom/
ls -la /opt/cleanroom/

# Fix permissions
sudo chown cleanroom:cleanroom /usr/local/bin/cleanroom
sudo chown -R cleanroom:cleanroom /etc/cleanroom/
sudo chown -R cleanroom:cleanroom /opt/cleanroom/

# Check user exists
id cleanroom
```

##### Missing Dependencies
```bash
# Check Docker
docker --version
systemctl status docker

# Check Rust
rustc --version
cargo --version

# Check system packages
dpkg -l | grep -E "(build-essential|pkg-config|libssl-dev)"

# Install missing packages
sudo apt update
sudo apt install -y build-essential pkg-config libssl-dev
```

##### Configuration Errors
```bash
# Validate configuration
cleanroom config validate

# Check TOML syntax
toml-cli validate /etc/cleanroom/config.toml

# Check environment variables
env | grep CLEANROOM

# Reset to defaults
sudo cp /etc/cleanroom/config.toml.backup /etc/cleanroom/config.toml
```

### 2. Service Crashes

#### Symptoms
- Service starts then stops
- Segmentation faults
- Out of memory errors
- Panic messages

#### Diagnosis
```bash
# Check crash logs
sudo journalctl -u cleanroom --since "1 hour ago"

# Check core dumps
ls -la /var/crash/
ls -la /core

# Check memory usage
free -h
ps aux --sort=-%mem | head -10

# Check system limits
ulimit -a
```

#### Common Causes and Solutions

##### Out of Memory
```bash
# Check memory usage
free -h
cat /proc/meminfo

# Check swap usage
swapon -s
cat /proc/swaps

# Increase swap if needed
sudo fallocate -l 2G /swapfile
sudo chmod 600 /swapfile
sudo mkswap /swapfile
sudo swapon /swapfile

# Check memory limits in config
grep -i memory /etc/cleanroom/config.toml

# Reduce memory limits
sudo sed -i 's/max_memory_usage_bytes = .*/max_memory_usage_bytes = 4294967296/' /etc/cleanroom/config.toml
```

##### Segmentation Fault
```bash
# Enable core dumps
echo "core.%p" | sudo tee /proc/sys/kernel/core_pattern
ulimit -c unlimited

# Check for core dumps
ls -la /core*

# Analyze core dump
gdb /usr/local/bin/cleanroom /core.*

# Check for corrupted binary
md5sum /usr/local/bin/cleanroom
ls -la /usr/local/bin/cleanroom

# Reinstall if corrupted
sudo cp target/release/cleanroom /usr/local/bin/
```

##### Panic Errors
```bash
# Check panic logs
grep -i panic /var/log/cleanroom/*.log

# Check for assertion failures
grep -i "assertion failed" /var/log/cleanroom/*.log

# Check for unwrap failures
grep -i unwrap /var/log/cleanroom/*.log

# Enable debug logging
sudo sed -i 's/RUST_LOG=info/RUST_LOG=debug/' /etc/systemd/system/cleanroom.service
sudo systemctl daemon-reload
sudo systemctl restart cleanroom
```

### 3. Service Performance Issues

#### Symptoms
- Slow response times
- High CPU usage
- High memory usage
- Timeout errors

#### Diagnosis
```bash
# Check response times
curl -w "@curl-format.txt" -o /dev/null -s http://localhost:8080/health

# Check CPU usage
top -bn1 | grep "Cpu(s)"
htop

# Check memory usage
free -h
ps aux --sort=-%mem | head -10

# Check I/O usage
iostat -x 1 5
iotop
```

#### Common Causes and Solutions

##### High CPU Usage
```bash
# Check CPU usage by process
ps aux --sort=-%cpu | head -10

# Check for infinite loops
grep -i "loop\|while\|for" /var/log/cleanroom/*.log

# Check for excessive logging
grep -c "DEBUG" /var/log/cleanroom/*.log

# Reduce log level
sudo sed -i 's/RUST_LOG=debug/RUST_LOG=info/' /etc/systemd/system/cleanroom.service
sudo systemctl daemon-reload
sudo systemctl restart cleanroom

# Check for resource leaks
valgrind --tool=memcheck /usr/local/bin/cleanroom
```

##### High Memory Usage
```bash
# Check memory usage by process
ps aux --sort=-%mem | head -10

# Check for memory leaks
grep -i "out of memory\|memory leak" /var/log/cleanroom/*.log

# Check container memory usage
docker stats --no-stream

# Restart containers
docker restart $(docker ps -q)

# Check for large data structures
grep -i "large\|huge\|big" /var/log/cleanroom/*.log
```

##### Slow Response Times
```bash
# Check network latency
ping -c 10 localhost

# Check database performance
sudo -u postgres psql -c "SELECT * FROM pg_stat_activity;"

# Check disk I/O
iostat -x 1 5

# Check for blocking operations
grep -i "block\|wait\|lock" /var/log/cleanroom/*.log

# Check connection pool
grep -i "connection\|pool" /var/log/cleanroom/*.log
```

## Container Issues

### 1. Container Won't Start

#### Symptoms
- Container fails to start
- Docker daemon errors
- Image pull failures
- Port conflicts

#### Diagnosis
```bash
# Check Docker status
sudo systemctl status docker
docker --version

# Check container logs
docker logs <container_id>

# Check Docker events
docker events --since 1h

# Check available images
docker images

# Check disk space
df -h
```

#### Common Causes and Solutions

##### Docker Daemon Issues
```bash
# Restart Docker daemon
sudo systemctl restart docker

# Check Docker logs
sudo journalctl -u docker -n 50

# Check Docker configuration
cat /etc/docker/daemon.json

# Reset Docker if corrupted
sudo systemctl stop docker
sudo rm -rf /var/lib/docker
sudo systemctl start docker
```

##### Image Pull Failures
```bash
# Check network connectivity
ping -c 3 registry-1.docker.io

# Check DNS resolution
nslookup registry-1.docker.io

# Try manual pull
docker pull alpine:latest

# Check Docker registry
curl -I https://registry-1.docker.io/v2/

# Use alternative registry
docker pull quay.io/alpine:latest
```

##### Port Conflicts
```bash
# Check port usage
sudo netstat -tulpn | grep :5432
sudo lsof -i :5432

# Kill conflicting process
sudo kill -9 <PID>

# Use different ports
docker run -p 5433:5432 postgres:13
```

### 2. Container Performance Issues

#### Symptoms
- Slow container startup
- High resource usage
- Container crashes
- Network issues

#### Diagnosis
```bash
# Check container stats
docker stats --no-stream

# Check container logs
docker logs <container_id> --tail 100

# Check container processes
docker exec <container_id> ps aux

# Check container network
docker exec <container_id> netstat -tulpn
```

#### Common Causes and Solutions

##### Slow Container Startup
```bash
# Check container startup time
time docker run --rm alpine:latest echo "test"

# Check image size
docker images --format "table {{.Repository}}\t{{.Tag}}\t{{.Size}}"

# Use smaller base image
docker run --rm alpine:latest sh -c "echo 'test'"

# Check resource limits
docker run --memory=512m --cpus=1 alpine:latest

# Optimize Dockerfile
cat Dockerfile
```

##### High Resource Usage
```bash
# Check resource limits
docker run --memory=1g --cpus=2 alpine:latest

# Check for resource leaks
docker exec <container_id> free -h
docker exec <container_id> top

# Restart container
docker restart <container_id>

# Check for infinite loops
docker logs <container_id> | grep -i "loop\|while\|for"
```

##### Container Crashes
```bash
# Check exit codes
docker ps -a

# Check crash logs
docker logs <container_id>

# Check for OOM kills
dmesg | grep -i "killed process"

# Increase memory limits
docker run --memory=2g <image>
```

### 3. Container Network Issues

#### Symptoms
- Containers can't communicate
- Network timeouts
- DNS resolution failures
- Port binding issues

#### Diagnosis
```bash
# Check Docker networks
docker network ls
docker network inspect bridge

# Check container networking
docker exec <container_id> ip addr
docker exec <container_id> ping -c 3 8.8.8.8

# Check port binding
docker port <container_id>

# Check firewall rules
sudo ufw status
sudo iptables -L
```

#### Common Causes and Solutions

##### Network Connectivity Issues
```bash
# Check Docker network
docker network inspect bridge

# Create custom network
docker network create cleanroom-network

# Connect containers to network
docker run --network cleanroom-network <image>

# Check network connectivity
docker exec <container_id> ping <other_container_ip>
```

##### DNS Resolution Issues
```bash
# Check DNS configuration
docker exec <container_id> cat /etc/resolv.conf

# Use custom DNS
docker run --dns 8.8.8.8 <image>

# Check DNS resolution
docker exec <container_id> nslookup google.com
```

##### Port Binding Issues
```bash
# Check port availability
sudo netstat -tulpn | grep :8080

# Use different port
docker run -p 8081:8080 <image>

# Check port binding
docker port <container_id>
```

## Performance Issues

### 1. Slow Response Times

#### Symptoms
- API responses > 1 second
- Timeout errors
- High latency
- Poor user experience

#### Diagnosis
```bash
# Check response times
curl -w "@curl-format.txt" -o /dev/null -s http://localhost:8080/health

# Check API endpoints
curl -w "@curl-format.txt" -o /dev/null -s http://localhost:8080/api/v1/status

# Check database queries
sudo -u postgres psql -c "SELECT query, mean_time, calls FROM pg_stat_statements ORDER BY mean_time DESC LIMIT 10;"

# Check system load
uptime
loadavg
```

#### Common Causes and Solutions

##### Database Performance Issues
```sql
-- Check slow queries
SELECT query, mean_time, calls 
FROM pg_stat_statements 
ORDER BY mean_time DESC 
LIMIT 10;

-- Check index usage
SELECT schemaname, tablename, attname, n_distinct, correlation 
FROM pg_stats 
WHERE schemaname = 'public' 
ORDER BY n_distinct DESC;

-- Analyze table statistics
ANALYZE;

-- Check for table locks
SELECT * FROM pg_locks WHERE NOT granted;

-- Check for long-running queries
SELECT pid, now() - pg_stat_activity.query_start AS duration, query 
FROM pg_stat_activity 
WHERE (now() - pg_stat_activity.query_start) > interval '5 minutes';
```

##### Application Performance Issues
```bash
# Check CPU usage
top -bn1 | grep "Cpu(s)"

# Check memory usage
free -h

# Check I/O usage
iostat -x 1 5

# Check for blocking operations
grep -i "block\|wait\|lock" /var/log/cleanroom/*.log

# Check connection pool
grep -i "connection\|pool" /var/log/cleanroom/*.log
```

##### Network Performance Issues
```bash
# Check network latency
ping -c 10 localhost

# Check bandwidth
iperf3 -s &
iperf3 -c localhost

# Check for network errors
netstat -i
cat /proc/net/dev

# Check for packet loss
ping -c 100 localhost | grep "packet loss"
```

### 2. High Resource Usage

#### Symptoms
- CPU usage > 80%
- Memory usage > 80%
- Disk usage > 90%
- System slowdown

#### Diagnosis
```bash
# Check overall resource usage
htop
free -h
df -h

# Check per-process usage
ps aux --sort=-%cpu | head -10
ps aux --sort=-%mem | head -10

# Check I/O usage
iostat -x 1 5
iotop

# Check network usage
iftop
nethogs
```

#### Common Causes and Solutions

##### High CPU Usage
```bash
# Check CPU usage by process
ps aux --sort=-%cpu | head -10

# Check for infinite loops
grep -i "loop\|while\|for" /var/log/cleanroom/*.log

# Check for excessive logging
grep -c "DEBUG" /var/log/cleanroom/*.log

# Reduce log level
sudo sed -i 's/RUST_LOG=debug/RUST_LOG=info/' /etc/systemd/system/cleanroom.service
sudo systemctl daemon-reload
sudo systemctl restart cleanroom

# Check for resource leaks
valgrind --tool=memcheck /usr/local/bin/cleanroom
```

##### High Memory Usage
```bash
# Check memory usage by process
ps aux --sort=-%mem | head -10

# Check for memory leaks
grep -i "out of memory\|memory leak" /var/log/cleanroom/*.log

# Check container memory usage
docker stats --no-stream

# Restart containers
docker restart $(docker ps -q)

# Check for large data structures
grep -i "large\|huge\|big" /var/log/cleanroom/*.log
```

##### High Disk Usage
```bash
# Check disk usage
df -h
du -sh /opt/cleanroom/*
du -sh /var/log/cleanroom/*

# Check for large files
find /opt/cleanroom -type f -size +100M -exec ls -lh {} \;

# Clean up old logs
sudo find /var/log/cleanroom -name "*.log" -mtime +30 -delete

# Clean up Docker
docker system prune -f

# Check for disk errors
dmesg | grep -i "error\|fail"
```

### 3. Throughput Issues

#### Symptoms
- Low requests per second
- Queue buildup
- Backpressure
- System overload

#### Diagnosis
```bash
# Check request rate
curl -w "@curl-format.txt" -o /dev/null -s http://localhost:8080/health

# Check queue length
grep -i "queue\|backlog" /var/log/cleanroom/*.log

# Check connection count
netstat -an | grep :8080 | wc -l

# Check process count
ps aux | grep cleanroom | wc -l
```

#### Common Causes and Solutions

##### Low Throughput
```bash
# Check for bottlenecks
grep -i "slow\|bottleneck\|block" /var/log/cleanroom/*.log

# Check database performance
sudo -u postgres psql -c "SELECT * FROM pg_stat_activity;"

# Check connection pool
grep -i "connection\|pool" /var/log/cleanroom/*.log

# Increase worker threads
sudo sed -i 's/worker_threads = .*/worker_threads = 8/' /etc/cleanroom/config.toml
```

##### Queue Buildup
```bash
# Check queue length
grep -i "queue\|backlog" /var/log/cleanroom/*.log

# Check for deadlocks
grep -i "deadlock\|lock" /var/log/cleanroom/*.log

# Check for resource exhaustion
grep -i "exhausted\|limit" /var/log/cleanroom/*.log

# Increase queue size
sudo sed -i 's/queue_size = .*/queue_size = 1000/' /etc/cleanroom/config.toml
```

## Network Issues

### 1. Connectivity Issues

#### Symptoms
- Can't reach external services
- DNS resolution failures
- Timeout errors
- Connection refused

#### Diagnosis
```bash
# Check basic connectivity
ping -c 3 google.com
ping -c 3 8.8.8.8

# Check DNS resolution
nslookup google.com
dig google.com

# Check routing
traceroute google.com
ip route show

# Check firewall
sudo ufw status
sudo iptables -L
```

#### Common Causes and Solutions

##### DNS Issues
```bash
# Check DNS configuration
cat /etc/resolv.conf

# Test DNS servers
nslookup google.com 8.8.8.8
nslookup google.com 1.1.1.1

# Fix DNS configuration
sudo tee /etc/resolv.conf > /dev/null <<EOF
nameserver 8.8.8.8
nameserver 1.1.1.1
EOF

# Restart networking
sudo systemctl restart networking
```

##### Firewall Issues
```bash
# Check firewall status
sudo ufw status

# Allow required ports
sudo ufw allow 8080/tcp
sudo ufw allow 5432/tcp
sudo ufw allow 6379/tcp

# Check iptables rules
sudo iptables -L

# Reset iptables if needed
sudo iptables -F
sudo iptables -X
sudo iptables -t nat -F
sudo iptables -t nat -X
```

##### Routing Issues
```bash
# Check routing table
ip route show

# Check default gateway
ip route | grep default

# Test connectivity to gateway
ping -c 3 $(ip route | grep default | awk '{print $3}')

# Fix routing if needed
sudo ip route add default via <gateway_ip>
```

### 2. Port Issues

#### Symptoms
- Port already in use
- Can't bind to port
- Connection refused
- Port conflicts

#### Diagnosis
```bash
# Check port usage
sudo netstat -tulpn | grep :8080
sudo lsof -i :8080
sudo ss -tulpn | grep :8080

# Check port binding
curl -I http://localhost:8080/health

# Check port availability
nc -zv localhost 8080
```

#### Common Causes and Solutions

##### Port Already in Use
```bash
# Find process using port
sudo lsof -i :8080

# Kill the process
sudo kill -9 <PID>

# Or change port in configuration
sudo sed -i 's/8080/8081/g' /etc/cleanroom/config.toml
sudo systemctl restart cleanroom
```

##### Can't Bind to Port
```bash
# Check if port is privileged
if [ $((8080)) -lt 1024 ]; then
    echo "Port 8080 requires root privileges"
fi

# Check for port conflicts
sudo netstat -tulpn | grep :8080

# Use different port
sudo sed -i 's/8080/8081/g' /etc/cleanroom/config.toml
```

##### Port Conflicts
```bash
# Check for conflicting services
sudo systemctl status apache2
sudo systemctl status nginx
sudo systemctl status tomcat

# Stop conflicting services
sudo systemctl stop apache2
sudo systemctl disable apache2

# Check for Docker port conflicts
docker ps | grep 8080
```

### 3. SSL/TLS Issues

#### Symptoms
- SSL handshake failures
- Certificate errors
- TLS version issues
- Encryption problems

#### Diagnosis
```bash
# Check SSL certificate
openssl s_client -connect localhost:443 -servername localhost

# Check certificate validity
openssl x509 -in /path/to/cert.pem -text -noout

# Check TLS version
openssl s_client -connect localhost:443 -tls1_2

# Check cipher suites
openssl ciphers -v
```

#### Common Causes and Solutions

##### Certificate Issues
```bash
# Check certificate file
ls -la /etc/ssl/certs/cleanroom.pem

# Validate certificate
openssl x509 -in /etc/ssl/certs/cleanroom.pem -text -noout

# Check certificate chain
openssl verify -CAfile /etc/ssl/certs/ca.pem /etc/ssl/certs/cleanroom.pem

# Regenerate certificate if needed
openssl req -x509 -newkey rsa:4096 -keyout /etc/ssl/private/cleanroom.key -out /etc/ssl/certs/cleanroom.pem -days 365 -nodes
```

##### TLS Version Issues
```bash
# Check supported TLS versions
openssl s_client -connect localhost:443 -tls1_2
openssl s_client -connect localhost:443 -tls1_3

# Check TLS configuration
grep -i tls /etc/cleanroom/config.toml

# Update TLS configuration
sudo sed -i 's/tls_version = .*/tls_version = "1.3"/' /etc/cleanroom/config.toml
```

## Database Issues

### 1. Connection Issues

#### Symptoms
- Can't connect to database
- Connection timeouts
- Authentication failures
- Database unreachable

#### Diagnosis
```bash
# Check database status
sudo systemctl status postgresql
sudo systemctl status redis

# Test database connection
psql -h localhost -U cleanroom -d cleanroom -c "SELECT 1;"
redis-cli ping

# Check database logs
sudo tail -f /var/log/postgresql/postgresql-*.log
sudo tail -f /var/log/redis/redis-server.log

# Check network connectivity
telnet localhost 5432
telnet localhost 6379
```

#### Common Causes and Solutions

##### Database Service Down
```bash
# Start database services
sudo systemctl start postgresql
sudo systemctl start redis

# Enable auto-start
sudo systemctl enable postgresql
sudo systemctl enable redis

# Check service status
sudo systemctl status postgresql
sudo systemctl status redis
```

##### Authentication Issues
```bash
# Check user permissions
sudo -u postgres psql -c "SELECT usename, usesuper FROM pg_user WHERE usename = 'cleanroom';"

# Create database user
sudo -u postgres createuser -s cleanroom
sudo -u postgres createdb cleanroom

# Set password
sudo -u postgres psql -c "ALTER USER cleanroom PASSWORD 'secure_password';"

# Test connection
psql -h localhost -U cleanroom -d cleanroom -c "SELECT 1;"
```

##### Network Issues
```bash
# Check database configuration
sudo -u postgres psql -c "SHOW listen_addresses;"
sudo -u postgres psql -c "SHOW port;"

# Update PostgreSQL configuration
sudo sed -i "s/#listen_addresses = 'localhost'/listen_addresses = '*'/" /etc/postgresql/*/main/postgresql.conf
sudo systemctl restart postgresql

# Check firewall
sudo ufw allow 5432/tcp
sudo ufw allow 6379/tcp
```

### 2. Performance Issues

#### Symptoms
- Slow database queries
- High CPU usage
- Memory issues
- Lock contention

#### Diagnosis
```sql
-- Check slow queries
SELECT query, mean_time, calls 
FROM pg_stat_statements 
ORDER BY mean_time DESC 
LIMIT 10;

-- Check active connections
SELECT * FROM pg_stat_activity;

-- Check database size
SELECT pg_size_pretty(pg_database_size('cleanroom'));

-- Check table sizes
SELECT schemaname, tablename, pg_size_pretty(pg_total_relation_size(schemaname||'.'||tablename)) as size
FROM pg_tables 
WHERE schemaname = 'public'
ORDER BY pg_total_relation_size(schemaname||'.'||tablename) DESC;
```

#### Common Causes and Solutions

##### Slow Queries
```sql
-- Analyze tables
ANALYZE;

-- Check for missing indexes
SELECT schemaname, tablename, attname, n_distinct, correlation 
FROM pg_stats 
WHERE schemaname = 'public' 
ORDER BY n_distinct DESC;

-- Create missing indexes
CREATE INDEX CONCURRENTLY idx_table_column ON table_name (column_name);

-- Check for table bloat
SELECT schemaname, tablename, n_tup_ins, n_tup_upd, n_tup_del, n_dead_tup
FROM pg_stat_user_tables
ORDER BY n_dead_tup DESC;
```

##### High CPU Usage
```bash
# Check database CPU usage
ps aux | grep postgres

# Check for long-running queries
sudo -u postgres psql -c "SELECT pid, now() - pg_stat_activity.query_start AS duration, query FROM pg_stat_activity WHERE (now() - pg_stat_activity.query_start) > interval '5 minutes';"

# Kill long-running queries
sudo -u postgres psql -c "SELECT pg_terminate_backend(pid) FROM pg_stat_activity WHERE (now() - pg_stat_activity.query_start) > interval '10 minutes';"
```

##### Memory Issues
```bash
# Check database memory usage
ps aux | grep postgres

# Check shared memory
cat /proc/sys/kernel/shmmax
cat /proc/sys/kernel/shmall

# Increase shared memory
echo 'kernel.shmmax = 268435456' | sudo tee -a /etc/sysctl.conf
echo 'kernel.shmall = 268435456' | sudo tee -a /etc/sysctl.conf
sudo sysctl -p
```

### 3. Data Issues

#### Symptoms
- Data corruption
- Missing data
- Inconsistent data
- Backup failures

#### Diagnosis
```bash
# Check database integrity
sudo -u postgres psql -c "SELECT datname, pg_size_pretty(pg_database_size(datname)) FROM pg_database;"

# Check for corruption
sudo -u postgres psql -c "SELECT * FROM pg_stat_database WHERE datname = 'cleanroom';"

# Check table integrity
sudo -u postgres psql -d cleanroom -c "REINDEX DATABASE cleanroom;"

# Check backup status
ls -la /opt/backups/cleanroom/
```

#### Common Causes and Solutions

##### Data Corruption
```bash
# Check for corruption
sudo -u postgres psql -c "SELECT * FROM pg_stat_database WHERE datname = 'cleanroom';"

# Reindex database
sudo -u postgres psql -d cleanroom -c "REINDEX DATABASE cleanroom;"

# Check table integrity
sudo -u postgres psql -d cleanroom -c "VACUUM ANALYZE;"

# Restore from backup if needed
sudo -u postgres psql -d cleanroom < /opt/backups/cleanroom/cleanroom-backup-$(date +%Y%m%d).sql
```

##### Missing Data
```bash
# Check table counts
sudo -u postgres psql -d cleanroom -c "SELECT schemaname, tablename, n_tup_ins, n_tup_upd, n_tup_del FROM pg_stat_user_tables;"

# Check for data loss
sudo -u postgres psql -d cleanroom -c "SELECT COUNT(*) FROM table_name;"

# Restore from backup
sudo -u postgres psql -d cleanroom < /opt/backups/cleanroom/cleanroom-backup-$(date +%Y%m%d).sql
```

##### Backup Failures
```bash
# Check backup logs
tail -f /var/log/cleanroom-backup.log

# Test backup
pg_dump -h localhost -U cleanroom cleanroom > test-backup.sql

# Fix backup script
sudo nano /usr/local/bin/cleanroom-backup

# Schedule backup
echo "0 2 * * * /usr/local/bin/cleanroom-backup" | sudo crontab -
```

## Security Issues

### 1. Authentication Issues

#### Symptoms
- Login failures
- Password errors
- Token expiration
- Access denied

#### Diagnosis
```bash
# Check authentication logs
grep -i "auth\|login\|password" /var/log/cleanroom/*.log

# Check failed logins
grep "Failed password" /var/log/auth.log

# Check user accounts
id cleanroom
groups cleanroom

# Check permissions
ls -la /opt/cleanroom/
ls -la /etc/cleanroom/
```

#### Common Causes and Solutions

##### Password Issues
```bash
# Check password policy
grep -i password /etc/cleanroom/config.toml

# Reset password
sudo passwd cleanroom

# Check password expiration
chage -l cleanroom

# Update password policy
sudo sed -i 's/PASS_MAX_DAYS.*/PASS_MAX_DAYS 90/' /etc/login.defs
```

##### Token Issues
```bash
# Check token configuration
grep -i token /etc/cleanroom/config.toml

# Check token expiration
grep -i "token.*expire\|expire.*token" /var/log/cleanroom/*.log

# Regenerate token
cleanroom auth generate-token

# Check token validity
cleanroom auth validate-token <token>
```

##### Permission Issues
```bash
# Check file permissions
ls -la /opt/cleanroom/
ls -la /etc/cleanroom/

# Fix permissions
sudo chown -R cleanroom:cleanroom /opt/cleanroom/
sudo chown -R cleanroom:cleanroom /etc/cleanroom/

# Check directory permissions
sudo chmod 755 /opt/cleanroom/
sudo chmod 644 /etc/cleanroom/config.toml
```

### 2. Authorization Issues

#### Symptoms
- Access denied errors
- Permission denied
- Resource not accessible
- Unauthorized access

#### Diagnosis
```bash
# Check authorization logs
grep -i "access.*denied\|permission.*denied\|unauthorized" /var/log/cleanroom/*.log

# Check user permissions
id cleanroom
groups cleanroom

# Check file permissions
ls -la /opt/cleanroom/
ls -la /etc/cleanroom/

# Check process permissions
ps aux | grep cleanroom
```

#### Common Causes and Solutions

##### File Permission Issues
```bash
# Check file permissions
ls -la /opt/cleanroom/
ls -la /etc/cleanroom/

# Fix permissions
sudo chown -R cleanroom:cleanroom /opt/cleanroom/
sudo chown -R cleanroom:cleanroom /etc/cleanroom/

# Check directory permissions
sudo chmod 755 /opt/cleanroom/
sudo chmod 644 /etc/cleanroom/config.toml
```

##### Process Permission Issues
```bash
# Check process permissions
ps aux | grep cleanroom

# Check user permissions
id cleanroom
groups cleanroom

# Add user to required groups
sudo usermod -aG docker cleanroom
sudo usermod -aG adm cleanroom

# Restart service
sudo systemctl restart cleanroom
```

##### Resource Access Issues
```bash
# Check resource permissions
ls -la /dev/docker
ls -la /var/run/docker.sock

# Fix Docker permissions
sudo chown root:docker /var/run/docker.sock
sudo chmod 660 /var/run/docker.sock

# Add user to docker group
sudo usermod -aG docker cleanroom
```

### 3. Security Violations

#### Symptoms
- Security policy violations
- Unauthorized access attempts
- Suspicious activity
- Threat detection alerts

#### Diagnosis
```bash
# Check security logs
grep -i "security\|violation\|threat\|suspicious" /var/log/cleanroom/*.log

# Check audit logs
sudo tail -f /var/log/audit/audit.log

# Check failed logins
grep "Failed password" /var/log/auth.log

# Check network connections
netstat -tulpn | grep cleanroom
```

#### Common Causes and Solutions

##### Policy Violations
```bash
# Check security policy
grep -i "security.*policy\|policy.*violation" /var/log/cleanroom/*.log

# Review policy configuration
cat /etc/cleanroom/config.toml | grep -A 10 -B 10 security

# Update policy if needed
sudo nano /etc/cleanroom/config.toml

# Restart service
sudo systemctl restart cleanroom
```

##### Unauthorized Access
```bash
# Check access logs
grep -i "unauthorized\|access.*denied" /var/log/cleanroom/*.log

# Check user accounts
id cleanroom
groups cleanroom

# Check for suspicious users
grep -i "cleanroom" /etc/passwd
grep -i "cleanroom" /etc/group

# Remove suspicious users
sudo userdel suspicious_user
```

##### Threat Detection
```bash
# Check threat detection logs
grep -i "threat\|malware\|virus" /var/log/cleanroom/*.log

# Check system integrity
rpm -Va
dpkg -V

# Check for rootkits
sudo rkhunter --check

# Update security tools
sudo apt update && sudo apt upgrade -y
```

## Configuration Issues

### 1. Configuration Errors

#### Symptoms
- Service won't start
- Configuration validation errors
- Invalid settings
- Missing configuration

#### Diagnosis
```bash
# Validate configuration
cleanroom config validate

# Check TOML syntax
toml-cli validate /etc/cleanroom/config.toml

# Check configuration file
cat /etc/cleanroom/config.toml

# Check environment variables
env | grep CLEANROOM
```

#### Common Causes and Solutions

##### Syntax Errors
```bash
# Check TOML syntax
toml-cli validate /etc/cleanroom/config.toml

# Fix syntax errors
sudo nano /etc/cleanroom/config.toml

# Validate after fix
cleanroom config validate
```

##### Invalid Settings
```bash
# Check for invalid settings
grep -i "invalid\|error\|fail" /var/log/cleanroom/*.log

# Check configuration against schema
cleanroom config validate

# Reset to defaults
sudo cp /etc/cleanroom/config.toml.backup /etc/cleanroom/config.toml

# Restart service
sudo systemctl restart cleanroom
```

##### Missing Configuration
```bash
# Check for missing configuration
grep -i "missing\|not found\|undefined" /var/log/cleanroom/*.log

# Check required settings
grep -E "^\s*[a-zA-Z_][a-zA-Z0-9_]*\s*=" /etc/cleanroom/config.toml

# Add missing configuration
sudo nano /etc/cleanroom/config.toml
```

### 2. Environment Issues

#### Symptoms
- Environment variable errors
- Path issues
- Missing dependencies
- Version conflicts

#### Diagnosis
```bash
# Check environment variables
env | grep CLEANROOM

# Check PATH
echo $PATH

# Check dependencies
which docker
which rustc
which cargo

# Check versions
docker --version
rustc --version
cargo --version
```

#### Common Causes and Solutions

##### Environment Variable Issues
```bash
# Check environment variables
env | grep CLEANROOM

# Set missing variables
export CLEANROOM_CONFIG=/etc/cleanroom/config.toml
export CLEANROOM_DATA_DIR=/opt/cleanroom/data
export CLEANROOM_LOG_DIR=/var/log/cleanroom

# Add to systemd service
sudo systemctl edit cleanroom
```

##### Path Issues
```bash
# Check PATH
echo $PATH

# Add to PATH
export PATH=$PATH:/usr/local/bin

# Add to system PATH
echo 'export PATH=$PATH:/usr/local/bin' | sudo tee -a /etc/environment
```

##### Dependency Issues
```bash
# Check dependencies
which docker
which rustc
which cargo

# Install missing dependencies
sudo apt update
sudo apt install -y docker.io build-essential

# Install Rust
curl --proto '=https' --tlsv1.2 -sSf https://sh.rustup.rs | sh
source ~/.cargo/env
```

### 3. Version Conflicts

#### Symptoms
- Version mismatch errors
- Compatibility issues
- Feature not available
- Build failures

#### Diagnosis
```bash
# Check versions
docker --version
rustc --version
cargo --version
cleanroom --version

# Check compatibility
grep -i "version\|compatibility" /var/log/cleanroom/*.log

# Check feature availability
cleanroom features list
```

#### Common Causes and Solutions

##### Version Mismatch
```bash
# Check required versions
cat Cargo.toml | grep -E "version|edition"

# Update to required version
sudo apt update
sudo apt install -y docker.io=20.10.*

# Update Rust
rustup update

# Rebuild application
cargo build --release
sudo cp target/release/cleanroom /usr/local/bin/
```

##### Compatibility Issues
```bash
# Check compatibility matrix
cat docs/compatibility.md

# Update dependencies
cargo update

# Check for breaking changes
cargo check

# Fix compatibility issues
sudo nano Cargo.toml
```

##### Feature Availability
```bash
# Check available features
cleanroom features list

# Enable required features
cargo build --release --features "coverage,signing"

# Check feature configuration
grep -E "\[features\]|coverage|signing" Cargo.toml
```

## Log Analysis

### 1. Log Collection

#### Log Locations
```bash
# Application logs
/var/log/cleanroom/cleanroom.log
/var/log/cleanroom/error.log
/var/log/cleanroom/access.log

# System logs
/var/log/syslog
/var/log/auth.log
/var/log/kern.log

# Service logs
sudo journalctl -u cleanroom
sudo journalctl -u docker
sudo journalctl -u postgresql
```

#### Log Analysis Tools
```bash
# Basic log analysis
grep -i error /var/log/cleanroom/*.log
grep -i warning /var/log/cleanroom/*.log
grep -i fatal /var/log/cleanroom/*.log

# Advanced log analysis
awk '/ERROR/ {print $0}' /var/log/cleanroom/cleanroom.log
sed -n '/ERROR/,/WARN/p' /var/log/cleanroom/cleanroom.log

# Real-time log monitoring
tail -f /var/log/cleanroom/cleanroom.log | grep -E "(ERROR|WARN|FATAL)"
```

### 2. Common Log Patterns

#### Error Patterns
```bash
# Connection errors
grep -i "connection.*refused\|connection.*timeout\|connection.*failed" /var/log/cleanroom/*.log

# Authentication errors
grep -i "auth.*failed\|login.*failed\|password.*incorrect" /var/log/cleanroom/*.log

# Permission errors
grep -i "permission.*denied\|access.*denied\|unauthorized" /var/log/cleanroom/*.log

# Resource errors
grep -i "out of memory\|disk.*full\|resource.*exhausted" /var/log/cleanroom/*.log
```

#### Performance Patterns
```bash
# Slow operations
grep -i "slow\|timeout\|latency" /var/log/cleanroom/*.log

# High resource usage
grep -i "high.*cpu\|high.*memory\|high.*disk" /var/log/cleanroom/*.log

# Queue buildup
grep -i "queue.*full\|backlog\|buffer.*overflow" /var/log/cleanroom/*.log
```

#### Security Patterns
```bash
# Security violations
grep -i "security.*violation\|policy.*violation" /var/log/cleanroom/*.log

# Suspicious activity
grep -i "suspicious\|anomaly\|threat" /var/log/cleanroom/*.log

# Unauthorized access
grep -i "unauthorized\|access.*denied" /var/log/cleanroom/*.log
```

### 3. Log Monitoring

#### Real-time Monitoring
```bash
# Monitor all logs
tail -f /var/log/cleanroom/*.log

# Monitor specific patterns
tail -f /var/log/cleanroom/cleanroom.log | grep -E "(ERROR|WARN|FATAL)"

# Monitor with context
tail -f /var/log/cleanroom/cleanroom.log | grep -A 5 -B 5 "ERROR"
```

#### Log Aggregation
```bash
# Aggregate errors by hour
grep "ERROR" /var/log/cleanroom/cleanroom.log | awk '{print $1, $2}' | cut -d: -f1-2 | sort | uniq -c

# Aggregate errors by type
grep "ERROR" /var/log/cleanroom/cleanroom.log | awk '{print $NF}' | sort | uniq -c

# Aggregate performance metrics
grep "duration" /var/log/cleanroom/cleanroom.log | awk '{print $NF}' | sort -n
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

This troubleshooting guide provides comprehensive procedures for diagnosing and resolving common issues with the Cleanroom Testing Framework. Regular review and updates of troubleshooting procedures are essential for maintaining effective problem resolution capabilities.
