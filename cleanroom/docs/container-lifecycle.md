# Container Lifecycle Management

## Container Lifecycle Overview

The Cleanroom framework implements a sophisticated container lifecycle management system following core team best practices for performance, reliability, and resource efficiency.

## Lifecycle States

```mermaid
stateDiagram-v2
    [*] --> Created
    Created --> Starting
    Starting --> Ready
    Ready --> Running
    Running --> Stopping
    Stopping --> Stopped
    Stopped --> [*]
    
    Starting --> Failed
    Running --> Failed
    Failed --> [*]
    
    Running --> Restarting
    Restarting --> Starting
```

## Container Registry Pattern

```mermaid
graph TB
    subgraph "Container Registry"
        CR[ContainerRegistry]
        CM[ContainerMap]
        CS[ContainerState]
        CL[ContainerLocks]
    end
    
    subgraph "Singleton Management"
        SC[SingletonContainers]
        CC[ContainerCache]
        RC[ResourceCache]
        LC[LifecycleCache]
    end
    
    subgraph "Container Types"
        PC[PostgresContainer]
        RC2[RedisContainer]
        GC[GenericContainer]
    end
    
    CR --> CM
    CR --> CS
    CR --> CL
    
    SC --> CC
    SC --> RC
    SC --> LC
    
    CM --> PC
    CM --> RC2
    CM --> GC
    
    CC --> PC
    CC --> RC2
    CC --> GC
```

## Container Creation Flow

```mermaid
sequenceDiagram
    participant T as Test
    participant CE as CleanroomEnvironment
    participant CR as ContainerRegistry
    participant TC as TestcontainerBackend
    participant D as Docker
    participant C as Container
    
    T->>CE: Request Container
    CE->>CR: Get/Create Container
    CR->>CR: Check Singleton Cache
    
    alt Container Exists
        CR-->>CE: Return Existing Container
    else Container Not Found
        CR->>TC: Create Container
        TC->>D: Pull Image
        D-->>TC: Image Ready
        TC->>D: Start Container
        D-->>TC: Container Started
        TC->>C: Wait for Ready
        C-->>TC: Health Check Passed
        TC-->>CR: Container Reference
        CR->>CR: Cache Container
        CR-->>CE: Return Container
    end
    
    CE-->>T: Container Access
```

## Health Check System

```mermaid
graph TB
    subgraph "Health Check Components"
        HC[HealthCheck]
        HP[HealthProbe]
        HS[HealthStatus]
        HT[HealthTimeout]
    end
    
    subgraph "Check Types"
        TC[TcpCheck]
        HC2[HttpCheck]
        CC[CommandCheck]
        FC[FileCheck]
    end
    
    subgraph "Health States"
        HS2[Healthy]
        HS3[Unhealthy]
        HS4[Unknown]
        HS5[Starting]
    end
    
    HC --> HP
    HC --> HS
    HC --> HT
    
    HP --> TC
    HP --> HC2
    HP --> CC
    HP --> FC
    
    HS --> HS2
    HS --> HS3
    HS --> HS4
    HS --> HS5
```

## Resource Management

```mermaid
graph TB
    subgraph "Resource Monitoring"
        RM[ResourceMonitor]
        CM[CPUMonitor]
        MM[MemoryMonitor]
        DM[DiskMonitor]
        NM[NetworkMonitor]
    end
    
    subgraph "Resource Limits"
        RL[ResourceLimits]
        CL[CPULimits]
        ML[MemoryLimits]
        DL[DiskLimits]
        NL[NetworkLimits]
    end
    
    subgraph "Resource Actions"
        RA[ResourceActions]
        TA[ThrottleAction]
        KA[KillAction]
        RA2[RestartAction]
        AA[AlertAction]
    end
    
    RM --> CM
    RM --> MM
    RM --> DM
    RM --> NM
    
    RL --> CL
    RL --> ML
    RL --> DL
    RL --> NL
    
    RM --> RA
    RA --> TA
    RA --> KA
    RA --> RA2
    RA --> AA
```

## Container Customization

```mermaid
graph TB
    subgraph "Container Customizers"
        CC[ContainerCustomizer]
        EV[EnvironmentVariables]
        VM[VolumeMounts]
        PM[PortMappings]
        RL[ResourceLimits]
        HC[HealthCheckConfig]
        IC[InitCommands]
    end
    
    subgraph "Customization Types"
        CT[CustomTypes]
        IT[ImageTags]
        VT[VolumeTypes]
        PT[PortTypes]
        RT[ResourceTypes]
    end
    
    subgraph "Configuration Sources"
        CS[ConfigSources]
        FS[FileSource]
        ES[EnvSource]
        CLS[CLISource]
        DS[DefaultSource]
    end
    
    CC --> EV
    CC --> VM
    CC --> PM
    CC --> RL
    CC --> HC
    CC --> IC
    
    CT --> IT
    CT --> VT
    CT --> PT
    CT --> RT
    
    CS --> FS
    CS --> ES
    CS --> CLS
    CS --> DS
```

## Cleanup and Teardown

```mermaid
sequenceDiagram
    participant T as Test
    participant CE as CleanroomEnvironment
    participant CR as ContainerRegistry
    participant TC as TestcontainerBackend
    participant D as Docker
    participant C as Container
    
    T->>CE: Test Complete
    CE->>CR: Cleanup Containers
    CR->>CR: Check Container State
    
    alt Container Running
        CR->>TC: Stop Container
        TC->>C: Send Stop Signal
        C-->>TC: Container Stopped
        TC->>D: Remove Container
        D-->>TC: Container Removed
        TC-->>CR: Cleanup Complete
    end
    
    CR->>CR: Remove from Cache
    CR-->>CE: Cleanup Complete
    CE->>CE: Update Metrics
    CE-->>T: Environment Cleaned
```

## Performance Optimizations

### Singleton Pattern Implementation

```rust
// Containers are created once and reused across tests
let postgres = environment.get_or_create_container("postgres", || {
    PostgresContainer::new(&environment.docker_client, "testdb", "testuser", "testpass")
}).await.unwrap();
```

### Resource Caching

```mermaid
graph TB
    subgraph "Cache Layers"
        L1[L1: Container Cache]
        L2[L2: Image Cache]
        L3[L3: Network Cache]
        L4[L4: Volume Cache]
    end
    
    subgraph "Cache Strategies"
        LRU[LRU Eviction]
        TTL[TTL Expiration]
        LFU[LFU Eviction]
        MRU[MRU Eviction]
    end
    
    subgraph "Cache Metrics"
        CH[Cache Hits]
        CM[Cache Misses]
        CR[Cache Ratio]
        CS[Cache Size]
    end
    
    L1 --> LRU
    L2 --> TTL
    L3 --> LFU
    L4 --> MRU
    
    LRU --> CH
    TTL --> CM
    LFU --> CR
    MRU --> CS
```

## Error Handling in Lifecycle

```mermaid
graph TB
    subgraph "Lifecycle Errors"
        CE[CreationError]
        SE[StartError]
        HE[HealthError]
        RE[ResourceError]
        TE[TimeoutError]
    end
    
    subgraph "Error Recovery"
        ER[ErrorRecovery]
        AR[AutomaticRecovery]
        MR[ManualRecovery]
        RR[RetryRecovery]
    end
    
    subgraph "Recovery Actions"
        RA[RestartAction]
        CA[CleanupAction]
        FA[FallbackAction]
        AA[AlertAction]
    end
    
    CE --> ER
    SE --> ER
    HE --> ER
    RE --> ER
    TE --> ER
    
    ER --> AR
    ER --> MR
    ER --> RR
    
    AR --> RA
    AR --> CA
    MR --> FA
    RR --> AA
```

## Monitoring and Observability

### Container Metrics

```mermaid
graph TB
    subgraph "Container Metrics"
        CM[ContainerMetrics]
        CPU[CPU Usage]
        MEM[Memory Usage]
        NET[Network Usage]
        DISK[Disk Usage]
        UPTIME[Uptime]
    end
    
    subgraph "Performance Metrics"
        PM[PerformanceMetrics]
        RT[Response Time]
        TP[Throughput]
        LT[Latency]
        QPS[Queries Per Second]
    end
    
    subgraph "Health Metrics"
        HM[HealthMetrics]
        HC[Health Checks]
        HS[Health Status]
        HT[Health Time]
        HF[Health Failures]
    end
    
    CM --> CPU
    CM --> MEM
    CM --> NET
    CM --> DISK
    CM --> UPTIME
    
    PM --> RT
    PM --> TP
    PM --> LT
    PM --> QPS
    
    HM --> HC
    HM --> HS
    HM --> HT
    HM --> HF
```

## Best Practices

1. **Singleton Containers**: Start containers once per test suite for performance
2. **Health Checks**: Implement comprehensive health check system
3. **Resource Limits**: Enforce resource limits to prevent system overload
4. **Graceful Shutdown**: Implement proper cleanup and teardown procedures
5. **Error Recovery**: Implement automatic error recovery mechanisms
6. **Monitoring**: Track container metrics and performance
7. **Caching**: Implement multi-layer caching for performance
8. **Customization**: Support flexible container customization
9. **Isolation**: Ensure proper container isolation and security
10. **Determinism**: Maintain deterministic container lifecycle

