# Performance Tuning Guide

This guide provides comprehensive instructions for optimizing the performance of the Cleanroom Testing Framework.

## Table of Contents

1. [Performance Overview](#performance-overview)
2. [System Optimization](#system-optimization)
3. [Application Optimization](#application-optimization)
4. [Database Optimization](#database-optimization)
5. [Container Optimization](#container-optimization)
6. [Network Optimization](#network-optimization)
7. [Monitoring and Profiling](#monitoring-and-profiling)
8. [Benchmarking](#benchmarking)
9. [Performance Testing](#performance-testing)
10. [Troubleshooting](#troubleshooting)

## Performance Overview

### Performance Objectives

The Cleanroom Testing Framework aims to achieve:

- **Response Time**: P95 < 1 second for API requests
- **Throughput**: > 1000 requests/second
- **Test Execution**: < 30 seconds average per test
- **Resource Efficiency**: < 80% CPU and memory usage
- **Scalability**: Linear scaling with additional resources

### Key Performance Metrics

#### System Metrics
- **CPU Usage**: Processor utilization percentage
- **Memory Usage**: RAM consumption and availability
- **Disk I/O**: Read/write operations per second
- **Network I/O**: Bandwidth consumption and latency

#### Application Metrics
- **Request Rate**: API requests per second
- **Response Time**: API response latency (P50, P95, P99)
- **Error Rate**: Failed requests percentage
- **Throughput**: Tests executed per minute

#### Container Metrics
- **Container Startup Time**: Time to start containers
- **Container Resource Usage**: CPU, memory, disk per container
- **Container Lifecycle**: Start/stop/restart times

#### Database Metrics
- **Query Performance**: Query execution time
- **Connection Pool**: Active/idle connections
- **Cache Hit Rate**: Cache effectiveness
- **Lock Contention**: Database lock wait times

## System Optimization

### 1. Operating System Tuning

#### Kernel Parameters
```bash
# Optimize kernel parameters
sudo tee -a /etc/sysctl.conf > /dev/null <<EOF
# Network optimizations
net.core.rmem_max = 16777216
net.core.wmem_max = 16777216
net.ipv4.tcp_rmem = 4096 65536 16777216
net.ipv4.tcp_wmem = 4096 65536 16777216
net.ipv4.tcp_congestion_control = bbr

# File system optimizations
fs.file-max = 2097152
fs.nr_open = 1048576

# Memory optimizations
vm.swappiness = 10
vm.dirty_ratio = 15
vm.dirty_background_ratio = 5

# Process optimizations
kernel.pid_max = 4194304
kernel.threads-max = 2097152
EOF

# Apply changes
sudo sysctl -p
```

#### File System Optimization
```bash
# Check file system type
df -T

# Optimize ext4 file system
sudo tune2fs -o journal_data_writeback /dev/sda1
sudo tune2fs -O ^has_journal /dev/sda1

# Optimize XFS file system
sudo xfs_admin -l /dev/sda1
sudo xfs_admin -c "logsize=32m" /dev/sda1

# Mount options optimization
sudo tee -a /etc/fstab > /dev/null <<EOF
/dev/sda1 /opt/cleanroom ext4 defaults,noatime,nodiratime,data=writeback 0 2
EOF

# Remount with new options
sudo mount -o remount /opt/cleanroom
```

#### Memory Optimization
```bash
# Check memory configuration
cat /proc/meminfo

# Optimize memory allocation
echo 'vm.overcommit_memory = 1' | sudo tee -a /etc/sysctl.conf
echo 'vm.overcommit_ratio = 80' | sudo tee -a /etc/sysctl.conf

# Configure huge pages
echo 'vm.nr_hugepages = 1024' | sudo tee -a /etc/sysctl.conf
echo 'vm.hugetlb_shm_group = 0' | sudo tee -a /etc/sysctl.conf

# Apply changes
sudo sysctl -p
```

### 2. Hardware Optimization

#### CPU Optimization
```bash
# Check CPU information
lscpu
cat /proc/cpuinfo

# Set CPU governor to performance
echo 'performance' | sudo tee /sys/devices/system/cpu/cpu*/cpufreq/scaling_governor

# Disable CPU frequency scaling
sudo systemctl disable ondemand
sudo systemctl stop ondemand

# Set CPU affinity
taskset -c 0-3 /usr/local/bin/cleanroom
```

#### Storage Optimization
```bash
# Check storage performance
sudo hdparm -tT /dev/sda

# Optimize disk I/O scheduler
echo 'mq-deadline' | sudo tee /sys/block/sda/queue/scheduler

# Configure read-ahead
sudo blockdev --setra 8192 /dev/sda

# Enable TRIM for SSDs
sudo fstrim -v /

# Configure disk cache
echo 'vm.dirty_writeback_centisecs = 500' | sudo tee -a /etc/sysctl.conf
echo 'vm.dirty_expire_centisecs = 3000' | sudo tee -a /etc/sysctl.conf
```

#### Network Optimization
```bash
# Check network configuration
ip addr show
ethtool eth0

# Optimize network buffer sizes
echo 'net.core.rmem_default = 262144' | sudo tee -a /etc/sysctl.conf
echo 'net.core.rmem_max = 16777216' | sudo tee -a /etc/sysctl.conf
echo 'net.core.wmem_default = 262144' | sudo tee -a /etc/sysctl.conf
echo 'net.core.wmem_max = 16777216' | sudo tee -a /etc/sysctl.conf

# Optimize TCP settings
echo 'net.ipv4.tcp_rmem = 4096 65536 16777216' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_wmem = 4096 65536 16777216' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_congestion_control = bbr' | sudo tee -a /etc/sysctl.conf

# Apply changes
sudo sysctl -p
```

## Application Optimization

### 1. Rust Application Tuning

#### Compiler Optimizations
```toml
# Cargo.toml optimizations
[profile.release]
opt-level = 3
lto = true
codegen-units = 1
panic = "abort"
strip = true

[profile.release.package."*"]
opt-level = 3
lto = true
```

#### Runtime Optimizations
```rust
// Enable runtime optimizations
use std::sync::Arc;
use tokio::sync::RwLock;

// Use Arc for shared data
pub struct CleanroomEnvironment {
    config: Arc<CleanroomConfig>,
    metrics: Arc<RwLock<CleanroomMetrics>>,
    container_registry: Arc<RwLock<HashMap<String, String>>>,
}

// Use RwLock for read-heavy workloads
impl CleanroomEnvironment {
    pub async fn get_metrics(&self) -> CleanroomMetrics {
        self.metrics.read().await.clone()
    }
    
    pub async fn update_metrics(&self, metrics: CleanroomMetrics) {
        *self.metrics.write().await = metrics;
    }
}
```

#### Memory Management
```rust
// Optimize memory allocation
use std::alloc::{GlobalAlloc, Layout, System};
use std::sync::atomic::{AtomicUsize, Ordering};

struct CustomAllocator;

unsafe impl GlobalAlloc for CustomAllocator {
    unsafe fn alloc(&self, layout: Layout) -> *mut u8 {
        System.alloc(layout)
    }
    
    unsafe fn dealloc(&self, ptr: *mut u8, layout: Layout) {
        System.dealloc(ptr, layout);
    }
}

#[global_allocator]
static GLOBAL: CustomAllocator = CustomAllocator;

// Use object pools for frequent allocations
use object_pool::Pool;

pub struct ObjectPool<T> {
    pool: Pool<T>,
}

impl<T> ObjectPool<T> {
    pub fn new() -> Self {
        Self {
            pool: Pool::new(|| T::default()),
        }
    }
    
    pub fn get(&self) -> PooledObject<T> {
        self.pool.get()
    }
}
```

#### Async Optimization
```rust
// Optimize async runtime
use tokio::runtime::Runtime;

pub fn create_optimized_runtime() -> Runtime {
    tokio::runtime::Builder::new_multi_thread()
        .worker_threads(num_cpus::get())
        .max_blocking_threads(256)
        .thread_name("cleanroom-worker")
        .thread_stack_size(3 * 1024 * 1024)
        .build()
        .unwrap()
}

// Use async streams for large data processing
use futures::stream::{self, StreamExt};

pub async fn process_large_dataset(data: Vec<Data>) -> Result<Vec<ProcessedData>> {
    let results = stream::iter(data)
        .map(|item| async move {
            process_item(item).await
        })
        .buffer_unordered(10) // Process 10 items concurrently
        .collect::<Vec<_>>()
        .await;
    
    Ok(results)
}
```

### 2. Configuration Optimization

#### Performance Configuration
```toml
# config.toml optimizations
[cleanroom]
enable_singleton_containers = true
container_startup_timeout = 60
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

[cleanroom.performance]
worker_threads = 8
max_blocking_threads = 256
thread_stack_size = 3145728  # 3MB
connection_pool_size = 100
query_timeout = 30
cache_size = 1000
cache_ttl = 300
```

#### Caching Configuration
```rust
// Implement efficient caching
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::{Duration, Instant};

pub struct Cache<K, V> {
    data: Arc<RwLock<HashMap<K, CacheEntry<V>>>>,
    ttl: Duration,
}

struct CacheEntry<V> {
    value: V,
    expires_at: Instant,
}

impl<K, V> Cache<K, V>
where
    K: Clone + Eq + std::hash::Hash,
    V: Clone,
{
    pub fn new(ttl: Duration) -> Self {
        Self {
            data: Arc::new(RwLock::new(HashMap::new())),
            ttl,
        }
    }
    
    pub async fn get(&self, key: &K) -> Option<V> {
        let mut data = self.data.write().await;
        if let Some(entry) = data.get(key) {
            if entry.expires_at > Instant::now() {
                Some(entry.value.clone())
            } else {
                data.remove(key);
                None
            }
        } else {
            None
        }
    }
    
    pub async fn set(&self, key: K, value: V) {
        let mut data = self.data.write().await;
        data.insert(key, CacheEntry {
            value,
            expires_at: Instant::now() + self.ttl,
        });
    }
}
```

### 3. API Optimization

#### Request Handling
```rust
// Optimize request handling
use axum::{
    extract::{Path, Query},
    http::StatusCode,
    response::Json,
    routing::get,
    Router,
};
use serde::{Deserialize, Serialize};
use std::sync::Arc;

pub fn create_optimized_router() -> Router {
    Router::new()
        .route("/health", get(health_check))
        .route("/metrics", get(get_metrics))
        .route("/api/v1/tests", get(list_tests))
        .route("/api/v1/tests/:id", get(get_test))
        .layer(axum::middleware::from_fn(rate_limiting))
        .layer(axum::middleware::from_fn(caching))
}

// Implement rate limiting
async fn rate_limiting(
    req: axum::extract::Request,
    next: axum::middleware::Next,
) -> Result<axum::response::Response, StatusCode> {
    // Rate limiting logic
    next.run(req).await
}

// Implement response caching
async fn caching(
    req: axum::extract::Request,
    next: axum::middleware::Next,
) -> axum::response::Response {
    // Caching logic
    next.run(req).await
}
```

#### Response Optimization
```rust
// Optimize response serialization
use serde::{Deserialize, Serialize};
use serde_json::Value;

#[derive(Serialize, Deserialize)]
pub struct OptimizedResponse<T> {
    #[serde(skip_serializing_if = "Option::is_none")]
    data: Option<T>,
    #[serde(skip_serializing_if = "Option::is_none")]
    error: Option<String>,
    #[serde(skip_serializing_if = "Option::is_none")]
    metadata: Option<Value>,
}

impl<T> OptimizedResponse<T> {
    pub fn success(data: T) -> Self {
        Self {
            data: Some(data),
            error: None,
            metadata: None,
        }
    }
    
    pub fn error(error: String) -> Self {
        Self {
            data: None,
            error: Some(error),
            metadata: None,
        }
    }
}

// Use streaming for large responses
use axum::response::Stream;
use futures::stream::{self, StreamExt};

pub async fn stream_large_data() -> Stream<impl futures::Stream<Item = Result<axum::body::Bytes, axum::Error>>> {
    let data_stream = stream::iter(0..1000)
        .map(|i| Ok(axum::body::Bytes::from(format!("data: {}\n", i))));
    
    Stream::new(data_stream)
}
```

## Database Optimization

### 1. PostgreSQL Optimization

#### Configuration Optimization
```sql
-- PostgreSQL configuration optimizations
-- postgresql.conf

-- Memory settings
shared_buffers = 256MB
effective_cache_size = 1GB
work_mem = 4MB
maintenance_work_mem = 64MB

-- Connection settings
max_connections = 100
shared_preload_libraries = 'pg_stat_statements'

-- Query optimization
random_page_cost = 1.1
effective_io_concurrency = 200
seq_page_cost = 1.0

-- Logging
log_min_duration_statement = 1000
log_checkpoints = on
log_connections = on
log_disconnections = on
log_lock_waits = on

-- WAL settings
wal_buffers = 16MB
checkpoint_completion_target = 0.9
checkpoint_timeout = 5min
max_wal_size = 1GB
min_wal_size = 80MB
```

#### Index Optimization
```sql
-- Create optimized indexes
CREATE INDEX CONCURRENTLY idx_tests_status ON tests (status);
CREATE INDEX CONCURRENTLY idx_tests_created_at ON tests (created_at);
CREATE INDEX CONCURRENTLY idx_tests_user_id ON tests (user_id);
CREATE INDEX CONCURRENTLY idx_tests_status_created_at ON tests (status, created_at);

-- Partial indexes for common queries
CREATE INDEX CONCURRENTLY idx_tests_active ON tests (id) WHERE status = 'active';
CREATE INDEX CONCURRENTLY idx_tests_recent ON tests (created_at) WHERE created_at > NOW() - INTERVAL '7 days';

-- Composite indexes
CREATE INDEX CONCURRENTLY idx_tests_user_status ON tests (user_id, status);
CREATE INDEX CONCURRENTLY idx_tests_status_created_at ON tests (status, created_at DESC);

-- Analyze tables for statistics
ANALYZE tests;
ANALYZE test_results;
ANALYZE containers;
```

#### Query Optimization
```sql
-- Optimize common queries
EXPLAIN (ANALYZE, BUFFERS) SELECT * FROM tests WHERE status = 'active' ORDER BY created_at DESC LIMIT 10;

-- Use prepared statements
PREPARE get_active_tests AS SELECT * FROM tests WHERE status = $1 ORDER BY created_at DESC LIMIT $2;
EXECUTE get_active_tests('active', 10);

-- Use materialized views for complex queries
CREATE MATERIALIZED VIEW test_summary AS
SELECT 
    status,
    COUNT(*) as count,
    AVG(EXTRACT(EPOCH FROM (completed_at - created_at))) as avg_duration
FROM tests
WHERE created_at > NOW() - INTERVAL '30 days'
GROUP BY status;

-- Refresh materialized view
REFRESH MATERIALIZED VIEW CONCURRENTLY test_summary;
```

#### Connection Pooling
```rust
// Implement connection pooling
use sqlx::PgPool;
use sqlx::postgres::PgPoolOptions;
use std::time::Duration;

pub async fn create_optimized_pool(database_url: &str) -> Result<PgPool> {
    PgPoolOptions::new()
        .max_connections(100)
        .min_connections(10)
        .acquire_timeout(Duration::from_secs(30))
        .idle_timeout(Duration::from_secs(600))
        .max_lifetime(Duration::from_secs(1800))
        .connect(database_url)
        .await
}

// Use connection pooling in application
pub struct DatabaseManager {
    pool: PgPool,
}

impl DatabaseManager {
    pub async fn new(database_url: &str) -> Result<Self> {
        let pool = create_optimized_pool(database_url).await?;
        Ok(Self { pool })
    }
    
    pub async fn execute_query(&self, query: &str) -> Result<Vec<serde_json::Value>> {
        let rows = sqlx::query(query)
            .fetch_all(&self.pool)
            .await?;
        
        Ok(rows.iter().map(|row| {
            serde_json::Value::Object(
                row.columns()
                    .iter()
                    .enumerate()
                    .map(|(i, col)| {
                        (col.name().to_string(), serde_json::Value::String(
                            row.try_get::<String, _>(i).unwrap_or_default()
                        ))
                    })
                    .collect()
            )
        }).collect())
    }
}
```

### 2. Redis Optimization

#### Configuration Optimization
```bash
# Redis configuration optimizations
# redis.conf

# Memory settings
maxmemory 1gb
maxmemory-policy allkeys-lru

# Persistence settings
save 900 1
save 300 10
save 60 10000

# Network settings
tcp-keepalive 300
timeout 0

# Performance settings
tcp-backlog 511
databases 16

# Logging
loglevel notice
logfile /var/log/redis/redis-server.log
```

#### Redis Optimization
```rust
// Implement Redis optimization
use redis::{Client, Connection, RedisResult};
use std::time::Duration;

pub struct RedisManager {
    client: Client,
}

impl RedisManager {
    pub fn new(redis_url: &str) -> RedisResult<Self> {
        let client = Client::open(redis_url)?;
        Ok(Self { client })
    }
    
    pub async fn get_connection(&self) -> RedisResult<Connection> {
        self.client.get_connection()
    }
    
    // Implement efficient caching
    pub async fn cache_get<T>(&self, key: &str) -> RedisResult<Option<T>>
    where
        T: serde::de::DeserializeOwned,
    {
        let mut conn = self.get_connection()?;
        let result: Option<String> = redis::cmd("GET").arg(key).query(&mut conn)?;
        
        if let Some(data) = result {
            let value: T = serde_json::from_str(&data)?;
            Ok(Some(value))
        } else {
            Ok(None)
        }
    }
    
    pub async fn cache_set<T>(&self, key: &str, value: &T, ttl: Duration) -> RedisResult<()>
    where
        T: serde::Serialize,
    {
        let mut conn = self.get_connection()?;
        let data = serde_json::to_string(value)?;
        
        redis::cmd("SETEX")
            .arg(key)
            .arg(ttl.as_secs())
            .arg(data)
            .execute(&mut conn);
        
        Ok(())
    }
    
    // Implement pipeline operations
    pub async fn pipeline_operations(&self, operations: Vec<(&str, &str)>) -> RedisResult<Vec<String>> {
        let mut conn = self.get_connection()?;
        let mut pipe = redis::pipe();
        
        for (key, value) in operations {
            pipe.set(key, value);
        }
        
        let results: Vec<String> = pipe.query(&mut conn)?;
        Ok(results)
    }
}
```

## Container Optimization

### 1. Docker Optimization

#### Docker Configuration
```json
{
  "log-driver": "json-file",
  "log-opts": {
    "max-size": "10m",
    "max-file": "3"
  },
  "storage-driver": "overlay2",
  "storage-opts": [
    "overlay2.override_kernel_check=true"
  ],
  "default-ulimits": {
    "nofile": {
      "Name": "nofile",
      "Hard": 65536,
      "Soft": 65536
    }
  },
  "max-concurrent-downloads": 3,
  "max-concurrent-uploads": 5,
  "shutdown-timeout": 15
}
```

#### Container Optimization
```dockerfile
# Optimized Dockerfile
FROM rust:1.70-slim as builder

# Install build dependencies
RUN apt-get update && apt-get install -y \
    pkg-config \
    libssl-dev \
    && rm -rf /var/lib/apt/lists/*

# Set working directory
WORKDIR /app

# Copy Cargo files
COPY Cargo.toml Cargo.lock ./

# Build dependencies
RUN cargo build --release --dependencies-only

# Copy source code
COPY src ./src

# Build application
RUN cargo build --release

# Runtime stage
FROM debian:bullseye-slim

# Install runtime dependencies
RUN apt-get update && apt-get install -y \
    ca-certificates \
    && rm -rf /var/lib/apt/lists/*

# Create user
RUN useradd -r -s /bin/false cleanroom

# Copy binary
COPY --from=builder /app/target/release/cleanroom /usr/local/bin/

# Set permissions
RUN chown cleanroom:cleanroom /usr/local/bin/cleanroom

# Switch to user
USER cleanroom

# Expose port
EXPOSE 8080

# Health check
HEALTHCHECK --interval=30s --timeout=3s --start-period=5s --retries=3 \
    CMD curl -f http://localhost:8080/health || exit 1

# Run application
CMD ["cleanroom", "server"]
```

#### Container Resource Limits
```yaml
# docker-compose.yml optimizations
version: '3.8'

services:
  cleanroom:
    image: cleanroom:latest
    deploy:
      resources:
        limits:
          cpus: '2.0'
          memory: 2G
        reservations:
          cpus: '1.0'
          memory: 1G
    ulimits:
      nofile:
        soft: 65536
        hard: 65536
    sysctls:
      - net.core.somaxconn=65535
    environment:
      - RUST_LOG=info
      - CLEANROOM_WORKER_THREADS=8
      - CLEANROOM_MAX_BLOCKING_THREADS=256
```

### 2. Container Orchestration

#### Kubernetes Optimization
```yaml
# Kubernetes deployment optimizations
apiVersion: apps/v1
kind: Deployment
metadata:
  name: cleanroom
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
        resources:
          requests:
            memory: "1Gi"
            cpu: "1000m"
          limits:
            memory: "2Gi"
            cpu: "2000m"
        env:
        - name: RUST_LOG
          value: "info"
        - name: CLEANROOM_WORKER_THREADS
          value: "8"
        - name: CLEANROOM_MAX_BLOCKING_THREADS
          value: "256"
        livenessProbe:
          httpGet:
            path: /health
            port: 8080
          initialDelaySeconds: 30
          periodSeconds: 10
        readinessProbe:
          httpGet:
            path: /ready
            port: 8080
          initialDelaySeconds: 5
          periodSeconds: 5
        ports:
        - containerPort: 8080
          name: http
        - containerPort: 9090
          name: metrics
```

#### Horizontal Pod Autoscaler
```yaml
# HPA configuration
apiVersion: autoscaling/v2
kind: HorizontalPodAutoscaler
metadata:
  name: cleanroom-hpa
spec:
  scaleTargetRef:
    apiVersion: apps/v1
    kind: Deployment
    name: cleanroom
  minReplicas: 3
  maxReplicas: 10
  metrics:
  - type: Resource
    resource:
      name: cpu
      target:
        type: Utilization
        averageUtilization: 70
  - type: Resource
    resource:
      name: memory
      target:
        type: Utilization
        averageUtilization: 80
```

## Network Optimization

### 1. Network Configuration

#### TCP Optimization
```bash
# TCP optimization
echo 'net.ipv4.tcp_congestion_control = bbr' | sudo tee -a /etc/sysctl.conf
echo 'net.core.rmem_max = 16777216' | sudo tee -a /etc/sysctl.conf
echo 'net.core.wmem_max = 16777216' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_rmem = 4096 65536 16777216' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_wmem = 4096 65536 16777216' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_window_scaling = 1' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_timestamps = 1' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_sack = 1' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_no_metrics_save = 1' | sudo tee -a /etc/sysctl.conf
echo 'net.ipv4.tcp_moderate_rcvbuf = 1' | sudo tee -a /etc/sysctl.conf
```

#### Load Balancer Optimization
```nginx
# Nginx load balancer optimization
upstream cleanroom_backend {
    least_conn;
    server cleanroom-1:8080 max_fails=3 fail_timeout=30s;
    server cleanroom-2:8080 max_fails=3 fail_timeout=30s;
    server cleanroom-3:8080 max_fails=3 fail_timeout=30s;
    keepalive 32;
}

server {
    listen 80;
    server_name cleanroom.example.com;
    
    # Connection optimization
    keepalive_timeout 65;
    keepalive_requests 100;
    
    # Buffer optimization
    client_body_buffer_size 128k;
    client_max_body_size 10m;
    client_header_buffer_size 1k;
    large_client_header_buffers 4 4k;
    
    # Timeout optimization
    client_body_timeout 12;
    client_header_timeout 12;
    send_timeout 10;
    
    location / {
        proxy_pass http://cleanroom_backend;
        proxy_set_header Host $host;
        proxy_set_header X-Real-IP $remote_addr;
        proxy_set_header X-Forwarded-For $proxy_add_x_forwarded_for;
        proxy_set_header X-Forwarded-Proto $scheme;
        
        # Connection optimization
        proxy_http_version 1.1;
        proxy_set_header Connection "";
        
        # Buffer optimization
        proxy_buffering on;
        proxy_buffer_size 4k;
        proxy_buffers 8 4k;
        proxy_busy_buffers_size 8k;
        
        # Timeout optimization
        proxy_connect_timeout 5s;
        proxy_send_timeout 10s;
        proxy_read_timeout 10s;
    }
}
```

### 2. API Gateway Optimization

#### Rate Limiting
```rust
// Implement rate limiting
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;
use tokio::time::{Duration, Instant};

pub struct RateLimiter {
    requests: Arc<RwLock<HashMap<String, Vec<Instant>>>>,
    limit: usize,
    window: Duration,
}

impl RateLimiter {
    pub fn new(limit: usize, window: Duration) -> Self {
        Self {
            requests: Arc::new(RwLock::new(HashMap::new())),
            limit,
            window,
        }
    }
    
    pub async fn is_allowed(&self, key: &str) -> bool {
        let mut requests = self.requests.write().await;
        let now = Instant::now();
        let cutoff = now - self.window;
        
        // Clean old requests
        if let Some(timestamps) = requests.get_mut(key) {
            timestamps.retain(|&ts| ts > cutoff);
            
            if timestamps.len() < self.limit {
                timestamps.push(now);
                true
            } else {
                false
            }
        } else {
            requests.insert(key.to_string(), vec![now]);
            true
        }
    }
}
```

#### Connection Pooling
```rust
// Implement connection pooling
use std::sync::Arc;
use tokio::sync::Semaphore;

pub struct ConnectionPool {
    semaphore: Arc<Semaphore>,
    max_connections: usize,
}

impl ConnectionPool {
    pub fn new(max_connections: usize) -> Self {
        Self {
            semaphore: Arc::new(Semaphore::new(max_connections)),
            max_connections,
        }
    }
    
    pub async fn acquire(&self) -> Result<tokio::sync::SemaphorePermit> {
        self.semaphore.acquire().await.map_err(|_| {
            std::io::Error::new(std::io::ErrorKind::Other, "Failed to acquire connection")
        })
    }
    
    pub fn available_connections(&self) -> usize {
        self.semaphore.available_permits()
    }
}
```

## Monitoring and Profiling

### 1. Performance Monitoring

#### Application Metrics
```rust
// Implement performance metrics
use prometheus::{Counter, Histogram, Gauge, Registry, Encoder, TextEncoder};
use std::time::Instant;

pub struct PerformanceMetrics {
    pub request_counter: Counter,
    pub request_duration: Histogram,
    pub active_connections: Gauge,
    pub memory_usage: Gauge,
    pub cpu_usage: Gauge,
}

impl PerformanceMetrics {
    pub fn new() -> Self {
        Self {
            request_counter: Counter::new("cleanroom_requests_total", "Total requests").unwrap(),
            request_duration: Histogram::new("cleanroom_request_duration_seconds", "Request duration").unwrap(),
            active_connections: Gauge::new("cleanroom_active_connections", "Active connections").unwrap(),
            memory_usage: Gauge::new("cleanroom_memory_usage_bytes", "Memory usage").unwrap(),
            cpu_usage: Gauge::new("cleanroom_cpu_usage_percent", "CPU usage").unwrap(),
        }
    }
    
    pub fn register(&self, registry: &Registry) -> Result<()> {
        registry.register(Box::new(self.request_counter.clone()))?;
        registry.register(Box::new(self.request_duration.clone()))?;
        registry.register(Box::new(self.active_connections.clone()))?;
        registry.register(Box::new(self.memory_usage.clone()))?;
        registry.register(Box::new(self.cpu_usage.clone()))?;
        Ok(())
    }
    
    pub fn record_request(&self, duration: Duration) {
        self.request_counter.inc();
        self.request_duration.observe(duration.as_secs_f64());
    }
    
    pub fn update_metrics(&self) {
        // Update system metrics
        if let Ok(memory) = get_memory_usage() {
            self.memory_usage.set(memory as f64);
        }
        
        if let Ok(cpu) = get_cpu_usage() {
            self.cpu_usage.set(cpu);
        }
    }
}
```

#### Profiling
```rust
// Implement profiling
use std::time::Instant;
use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::RwLock;

pub struct Profiler {
    timings: Arc<RwLock<HashMap<String, Vec<Duration>>>>,
}

impl Profiler {
    pub fn new() -> Self {
        Self {
            timings: Arc::new(RwLock::new(HashMap::new())),
        }
    }
    
    pub async fn profile<F, R>(&self, name: &str, f: F) -> R
    where
        F: FnOnce() -> R,
    {
        let start = Instant::now();
        let result = f();
        let duration = start.elapsed();
        
        let mut timings = self.timings.write().await;
        timings.entry(name.to_string()).or_insert_with(Vec::new).push(duration);
        
        result
    }
    
    pub async fn get_stats(&self) -> HashMap<String, TimingStats> {
        let timings = self.timings.read().await;
        let mut stats = HashMap::new();
        
        for (name, durations) in timings.iter() {
            if !durations.is_empty() {
                let total: Duration = durations.iter().sum();
                let avg = total / durations.len() as u32;
                let min = durations.iter().min().unwrap();
                let max = durations.iter().max().unwrap();
                
                stats.insert(name.clone(), TimingStats {
                    count: durations.len(),
                    total,
                    average: avg,
                    min: *min,
                    max: *max,
                });
            }
        }
        
        stats
    }
}

pub struct TimingStats {
    pub count: usize,
    pub total: Duration,
    pub average: Duration,
    pub min: Duration,
    pub max: Duration,
}
```

### 2. Performance Analysis

#### Benchmarking
```rust
// Implement benchmarking
use criterion::{black_box, criterion_group, criterion_main, Criterion};
use std::time::Duration;

fn benchmark_request_handling(c: &mut Criterion) {
    let mut group = c.benchmark_group("request_handling");
    group.measurement_time(Duration::from_secs(10));
    group.sample_size(100);
    
    group.bench_function("health_check", |b| {
        b.iter(|| {
            // Benchmark health check
            black_box(health_check())
        })
    });
    
    group.bench_function("metrics_endpoint", |b| {
        b.iter(|| {
            // Benchmark metrics endpoint
            black_box(get_metrics())
        })
    });
    
    group.finish();
}

fn benchmark_database_operations(c: &mut Criterion) {
    let mut group = c.benchmark_group("database_operations");
    group.measurement_time(Duration::from_secs(10));
    group.sample_size(100);
    
    group.bench_function("simple_query", |b| {
        b.iter(|| {
            // Benchmark simple query
            black_box(execute_simple_query())
        })
    });
    
    group.bench_function("complex_query", |b| {
        b.iter(|| {
            // Benchmark complex query
            black_box(execute_complex_query())
        })
    });
    
    group.finish();
}

criterion_group!(benches, benchmark_request_handling, benchmark_database_operations);
criterion_main!(benches);
```

#### Load Testing
```rust
// Implement load testing
use std::sync::Arc;
use tokio::sync::Semaphore;
use tokio::time::{Duration, Instant};

pub struct LoadTester {
    client: reqwest::Client,
    semaphore: Arc<Semaphore>,
}

impl LoadTester {
    pub fn new(max_concurrent: usize) -> Self {
        Self {
            client: reqwest::Client::new(),
            semaphore: Arc::new(Semaphore::new(max_concurrent)),
        }
    }
    
    pub async fn run_load_test(&self, url: &str, duration: Duration) -> LoadTestResults {
        let start = Instant::now();
        let mut results = LoadTestResults::new();
        
        while start.elapsed() < duration {
            let permit = self.semaphore.acquire().await.unwrap();
            
            let client = self.client.clone();
            let url = url.to_string();
            
            tokio::spawn(async move {
                let request_start = Instant::now();
                
                match client.get(&url).send().await {
                    Ok(response) => {
                        let duration = request_start.elapsed();
                        results.record_success(duration);
                    }
                    Err(_) => {
                        let duration = request_start.elapsed();
                        results.record_failure(duration);
                    }
                }
                
                drop(permit);
            });
        }
        
        // Wait for all requests to complete
        tokio::time::sleep(Duration::from_secs(1)).await;
        
        results
    }
}

pub struct LoadTestResults {
    pub total_requests: usize,
    pub successful_requests: usize,
    pub failed_requests: usize,
    pub total_duration: Duration,
    pub average_response_time: Duration,
    pub min_response_time: Duration,
    pub max_response_time: Duration,
    pub requests_per_second: f64,
}

impl LoadTestResults {
    fn new() -> Self {
        Self {
            total_requests: 0,
            successful_requests: 0,
            failed_requests: 0,
            total_duration: Duration::from_secs(0),
            average_response_time: Duration::from_secs(0),
            min_response_time: Duration::from_secs(0),
            max_response_time: Duration::from_secs(0),
            requests_per_second: 0.0,
        }
    }
    
    fn record_success(&mut self, duration: Duration) {
        self.total_requests += 1;
        self.successful_requests += 1;
        self.update_timing_stats(duration);
    }
    
    fn record_failure(&mut self, duration: Duration) {
        self.total_requests += 1;
        self.failed_requests += 1;
        self.update_timing_stats(duration);
    }
    
    fn update_timing_stats(&mut self, duration: Duration) {
        if self.min_response_time == Duration::from_secs(0) || duration < self.min_response_time {
            self.min_response_time = duration;
        }
        
        if duration > self.max_response_time {
            self.max_response_time = duration;
        }
        
        self.total_duration += duration;
        self.average_response_time = self.total_duration / self.total_requests as u32;
        
        if self.total_duration.as_secs() > 0 {
            self.requests_per_second = self.total_requests as f64 / self.total_duration.as_secs_f64();
        }
    }
}
```

## Benchmarking

### 1. Performance Benchmarks

#### System Benchmarks
```bash
#!/bin/bash
# System performance benchmark script

echo "=== Cleanroom Performance Benchmark ==="
echo "Date: $(date)"
echo

# CPU benchmark
echo "=== CPU Performance ==="
sysbench cpu --cpu-max-prime=20000 --threads=8 run

# Memory benchmark
echo "=== Memory Performance ==="
sysbench memory --memory-block-size=1K --memory-total-size=10G --threads=8 run

# Disk I/O benchmark
echo "=== Disk I/O Performance ==="
sysbench fileio --file-total-size=1G --file-test-mode=rndrw --threads=8 run

# Network benchmark
echo "=== Network Performance ==="
iperf3 -s &
sleep 2
iperf3 -c localhost -t 30
pkill iperf3

# Database benchmark
echo "=== Database Performance ==="
sudo -u postgres psql -c "SELECT pg_stat_statements_reset();"
sleep 5
sudo -u postgres psql -c "SELECT query, mean_time, calls FROM pg_stat_statements ORDER BY mean_time DESC LIMIT 10;"
```

#### Application Benchmarks
```bash
#!/bin/bash
# Application performance benchmark script

echo "=== Cleanroom Application Benchmark ==="
echo "Date: $(date)"
echo

# Health check benchmark
echo "=== Health Check Performance ==="
ab -n 10000 -c 100 http://localhost:8080/health

# Metrics endpoint benchmark
echo "=== Metrics Endpoint Performance ==="
ab -n 10000 -c 100 http://localhost:8080/metrics

# API endpoint benchmark
echo "=== API Endpoint Performance ==="
ab -n 10000 -c 100 http://localhost:8080/api/v1/status

# Load test with wrk
echo "=== Load Test with wrk ==="
wrk -t12 -c400 -d30s http://localhost:8080/health

# Stress test
echo "=== Stress Test ==="
wrk -t12 -c400 -d60s --script=stress.lua http://localhost:8080/api/v1/tests
```

### 2. Container Benchmarks

#### Docker Performance
```bash
#!/bin/bash
# Docker performance benchmark script

echo "=== Docker Performance Benchmark ==="
echo "Date: $(date)"
echo

# Container startup time
echo "=== Container Startup Time ==="
for i in {1..10}; do
    time docker run --rm alpine:latest echo "test"
done

# Container resource usage
echo "=== Container Resource Usage ==="
docker run -d --name test-container alpine:latest sleep 60
docker stats --no-stream test-container
docker rm -f test-container

# Image pull time
echo "=== Image Pull Time ==="
docker rmi alpine:latest
time docker pull alpine:latest

# Container network performance
echo "=== Container Network Performance ==="
docker run -d --name test-container alpine:latest sleep 60
docker exec test-container ping -c 10 8.8.8.8
docker rm -f test-container
```

### 3. Database Benchmarks

#### PostgreSQL Performance
```sql
-- PostgreSQL performance benchmark
-- Create test table
CREATE TABLE IF NOT EXISTS test_table (
    id SERIAL PRIMARY KEY,
    data TEXT,
    created_at TIMESTAMP DEFAULT NOW()
);

-- Insert test data
INSERT INTO test_table (data) 
SELECT 'test_data_' || generate_series(1, 100000);

-- Benchmark queries
EXPLAIN (ANALYZE, BUFFERS) SELECT * FROM test_table WHERE id = 1;
EXPLAIN (ANALYZE, BUFFERS) SELECT * FROM test_table WHERE data LIKE 'test_data_1%';
EXPLAIN (ANALYZE, BUFFERS) SELECT COUNT(*) FROM test_table;
EXPLAIN (ANALYZE, BUFFERS) SELECT * FROM test_table ORDER BY created_at DESC LIMIT 10;

-- Benchmark concurrent connections
-- Run this in multiple terminals
SELECT pg_sleep(1);
```

#### Redis Performance
```bash
#!/bin/bash
# Redis performance benchmark script

echo "=== Redis Performance Benchmark ==="
echo "Date: $(date)"
echo

# Basic operations
echo "=== Basic Operations ==="
redis-benchmark -q -n 100000 -c 100

# String operations
echo "=== String Operations ==="
redis-benchmark -q -n 100000 -c 100 -t set,get

# Hash operations
echo "=== Hash Operations ==="
redis-benchmark -q -n 100000 -c 100 -t hset,hget

# List operations
echo "=== List Operations ==="
redis-benchmark -q -n 100000 -c 100 -t lpush,lpop

# Set operations
echo "=== Set Operations ==="
redis-benchmark -q -n 100000 -c 100 -t sadd,spop

# Sorted set operations
echo "=== Sorted Set Operations ==="
redis-benchmark -q -n 100000 -c 100 -t zadd,zrange
```

## Performance Testing

### 1. Load Testing

#### Apache Bench
```bash
#!/bin/bash
# Apache Bench load testing script

echo "=== Apache Bench Load Testing ==="
echo "Date: $(date)"
echo

# Basic load test
echo "=== Basic Load Test ==="
ab -n 10000 -c 100 http://localhost:8080/health

# Sustained load test
echo "=== Sustained Load Test ==="
ab -n 100000 -c 100 -t 60 http://localhost:8080/health

# Peak load test
echo "=== Peak Load Test ==="
ab -n 50000 -c 500 http://localhost:8080/health

# Stress test
echo "=== Stress Test ==="
ab -n 100000 -c 1000 http://localhost:8080/health
```

#### wrk Load Testing
```bash
#!/bin/bash
# wrk load testing script

echo "=== wrk Load Testing ==="
echo "Date: $(date)"
echo

# Basic load test
echo "=== Basic Load Test ==="
wrk -t12 -c400 -d30s http://localhost:8080/health

# Sustained load test
echo "=== Sustained Load Test ==="
wrk -t12 -c400 -d300s http://localhost:8080/health

# Peak load test
echo "=== Peak Load Test ==="
wrk -t24 -c800 -d60s http://localhost:8080/health

# Stress test
echo "=== Stress Test ==="
wrk -t24 -c1000 -d120s http://localhost:8080/health
```

### 2. Stress Testing

#### Stress Test Script
```lua
-- stress.lua - wrk stress test script
local counter = 1
local threads = {}

function setup(thread)
   thread:set("id", counter)
   table.insert(threads, thread)
   counter = counter + 1
end

function init(args)
   requests  = 0
   responses = 0
   local msg = "thread %d created"
   print(msg:format(id))
end

function request()
   requests = requests + 1
   return wrk.format("GET", "/health")
end

function response(status, headers, body)
   responses = responses + 1
   if status ~= 200 then
      print("Error: " .. status)
   end
end

function done(summary, latency, requests)
   local msg = "thread %d summary: %d requests, %d responses, %.2f requests/sec"
   print(msg:format(id, requests, responses, requests/summary.duration))
end
```

#### Stress Test Execution
```bash
#!/bin/bash
# Stress test execution script

echo "=== Stress Testing ==="
echo "Date: $(date)"
echo

# Run stress test
echo "=== Running Stress Test ==="
wrk -t12 -c400 -d60s --script=stress.lua http://localhost:8080/health

# Monitor system during stress test
echo "=== System Monitoring ==="
top -bn1 | head -20
free -h
df -h
```

### 3. Endurance Testing

#### Endurance Test Script
```bash
#!/bin/bash
# Endurance testing script

echo "=== Endurance Testing ==="
echo "Date: $(date)"
echo

# Run endurance test for 1 hour
echo "=== Running Endurance Test (1 hour) ==="
wrk -t12 -c400 -d3600s http://localhost:8080/health

# Monitor system during endurance test
echo "=== System Monitoring ==="
while true; do
    echo "=== $(date) ==="
    top -bn1 | head -20
    free -h
    df -h
    sleep 300  # Check every 5 minutes
done &
```

## Troubleshooting

### 1. Performance Issues

#### Common Performance Problems
```bash
# Check system performance
echo "=== System Performance Check ==="
top -bn1 | head -20
free -h
df -h
iostat -x 1 5

# Check application performance
echo "=== Application Performance Check ==="
ps aux --sort=-%cpu | head -10
ps aux --sort=-%mem | head -10
netstat -tulpn | grep cleanroom

# Check database performance
echo "=== Database Performance Check ==="
sudo -u postgres psql -c "SELECT * FROM pg_stat_activity;"
sudo -u postgres psql -c "SELECT query, mean_time, calls FROM pg_stat_statements ORDER BY mean_time DESC LIMIT 10;"

# Check container performance
echo "=== Container Performance Check ==="
docker stats --no-stream
docker ps -a
```

#### Performance Bottlenecks
```bash
# Identify CPU bottlenecks
echo "=== CPU Bottleneck Analysis ==="
top -bn1 | grep "Cpu(s)"
htop
perf top

# Identify memory bottlenecks
echo "=== Memory Bottleneck Analysis ==="
free -h
cat /proc/meminfo
vmstat 1 5

# Identify I/O bottlenecks
echo "=== I/O Bottleneck Analysis ==="
iostat -x 1 5
iotop
lsof | grep cleanroom

# Identify network bottlenecks
echo "=== Network Bottleneck Analysis ==="
netstat -i
iftop
nethogs
```

### 2. Optimization Recommendations

#### System Optimization
```bash
# System optimization recommendations
echo "=== System Optimization Recommendations ==="

# Check CPU usage
CPU_USAGE=$(top -bn1 | grep "Cpu(s)" | awk '{print $2}' | cut -d'%' -f1)
if (( $(echo "$CPU_USAGE > 80" | bc -l) )); then
    echo "⚠️  High CPU usage: $CPU_USAGE%"
    echo "   Recommendations:"
    echo "   - Increase CPU resources"
    echo "   - Optimize application code"
    echo "   - Use connection pooling"
    echo "   - Implement caching"
fi

# Check memory usage
MEMORY_USAGE=$(free | grep Mem | awk '{printf("%.1f", $3/$2 * 100.0)}')
if (( $(echo "$MEMORY_USAGE > 80" | bc -l) )); then
    echo "⚠️  High memory usage: $MEMORY_USAGE%"
    echo "   Recommendations:"
    echo "   - Increase memory resources"
    echo "   - Optimize memory usage"
    echo "   - Use memory-efficient data structures"
    echo "   - Implement garbage collection tuning"
fi

# Check disk usage
DISK_USAGE=$(df -h / | awk 'NR==2{print $5}' | sed 's/%//')
if (( $DISK_USAGE > 80 )); then
    echo "⚠️  High disk usage: $DISK_USAGE%"
    echo "   Recommendations:"
    echo "   - Clean up old files"
    echo "   - Increase disk space"
    echo "   - Implement log rotation"
    echo "   - Use compression"
fi
```

#### Application Optimization
```bash
# Application optimization recommendations
echo "=== Application Optimization Recommendations ==="

# Check response times
echo "=== Response Time Analysis ==="
curl -w "@curl-format.txt" -o /dev/null -s http://localhost:8080/health

# Check error rates
echo "=== Error Rate Analysis ==="
grep -c "ERROR" /var/log/cleanroom/*.log
grep -c "WARN" /var/log/cleanroom/*.log

# Check throughput
echo "=== Throughput Analysis ==="
ab -n 1000 -c 10 http://localhost:8080/health | grep "Requests per second"

# Recommendations
echo "=== Optimization Recommendations ==="
echo "1. Enable connection pooling"
echo "2. Implement response caching"
echo "3. Use async/await for I/O operations"
echo "4. Optimize database queries"
echo "5. Use compression for responses"
echo "6. Implement rate limiting"
echo "7. Use CDN for static content"
echo "8. Implement horizontal scaling"
```

This performance tuning guide provides comprehensive instructions for optimizing the Cleanroom Testing Framework. Regular performance monitoring and optimization are essential for maintaining optimal system performance.
