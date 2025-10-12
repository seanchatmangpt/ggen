# Cleanroom Architecture Overview

## System Architecture

The Cleanroom Testing Framework is designed as a comprehensive, production-ready testing environment using testcontainers with core team best practices. The architecture follows a modular design with clear separation of concerns.

### Core Components

```mermaid
graph TB
    subgraph "CleanroomEnvironment"
        CE[CleanroomEnvironment]
        CM[CleanroomMetrics]
        CR[ContainerRegistry]
        SM[ServiceManager]
        TC[TestcontainerBackend]
    end
    
    subgraph "Configuration Layer"
        CC[CleanroomConfig]
        SP[SecurityPolicy]
        RL[ResourceLimits]
        PM[PerformanceMonitoring]
    end
    
    subgraph "Execution Layer"
        RT[RuntimeManager]
        SC[Scenario]
        AE[AssertionEngine]
        AT[AttestationEngine]
    end
    
    subgraph "Container Layer"
        PC[PostgresContainer]
        RC[RedisContainer]
        GC[GenericContainer]
        CW[ContainerWrapper]
    end
    
    subgraph "Support Services"
        CT[CoverageTracker]
        ST[SnapshotManager]
        TR[TracingManager]
        DM[DeterminismEngine]
        AM[ArtifactManager]
    end
    
    CE --> CM
    CE --> CR
    CE --> SM
    CE --> TC
    
    CC --> SP
    CC --> RL
    CC --> PM
    
    RT --> SC
    SC --> AE
    SC --> AT
    
    CR --> PC
    CR --> RC
    CR --> GC
    PC --> CW
    RC --> CW
    GC --> CW
    
    CE --> CT
    CE --> ST
    CE --> TR
    CE --> DM
    CE --> AM
```

## Component Responsibilities

### CleanroomEnvironment
- **Purpose**: Central orchestrator for the testing environment
- **Responsibilities**:
  - Session management with unique UUIDs
  - Container lifecycle management
  - Metrics collection and reporting
  - Resource monitoring and limits enforcement
  - Test execution coordination

### Configuration Layer
- **CleanroomConfig**: Main configuration structure with validation
- **SecurityPolicy**: Network, filesystem, and process isolation policies
- **ResourceLimits**: CPU, memory, disk, and network constraints
- **PerformanceMonitoring**: Real-time performance tracking and alerting

### Execution Layer
- **RuntimeManager**: Command execution with timeout and policy enforcement
- **Scenario**: Multi-step test orchestration with deterministic execution
- **AssertionEngine**: Test result validation and verification
- **AttestationEngine**: Security and compliance verification

### Container Layer
- **PostgresContainer**: PostgreSQL database container with connection management
- **RedisContainer**: Redis cache container with key-value operations
- **GenericContainer**: Generic container for custom applications
- **ContainerWrapper**: Common interface for all container types

### Support Services
- **CoverageTracker**: Test coverage measurement and reporting
- **SnapshotManager**: Snapshot testing and validation
- **TracingManager**: Distributed tracing and observability
- **DeterminismEngine**: Deterministic execution with fixed seeds
- **ArtifactManager**: Test artifact collection and management

## Data Flow

```mermaid
sequenceDiagram
    participant T as Test
    participant CE as CleanroomEnvironment
    participant CR as ContainerRegistry
    participant SM as ServiceManager
    participant TC as TestcontainerBackend
    participant C as Container
    
    T->>CE: Create Environment
    CE->>CR: Initialize Registry
    CE->>SM: Initialize Services
    CE->>TC: Initialize Backend
    
    T->>CE: Execute Test
    CE->>CR: Get/Create Container
    CR->>TC: Create Container
    TC->>C: Start Container
    C-->>TC: Container Ready
    TC-->>CR: Container Reference
    CR-->>CE: Container Access
    CE->>SM: Start Services
    SM->>C: Service Operations
    C-->>SM: Service Ready
    SM-->>CE: Services Ready
    CE-->>T: Test Execution Complete
```

## Security Architecture

```mermaid
graph TB
    subgraph "Security Boundaries"
        NI[Network Isolation]
        FI[Filesystem Isolation]
        PI[Process Isolation]
        DR[Data Redaction]
    end
    
    subgraph "Policy Enforcement"
        SP[SecurityPolicy]
        RL[ResourceLimits]
        PE[PolicyEngine]
    end
    
    subgraph "Compliance"
        AT[AttestationEngine]
        AL[AuditLogging]
        CV[ComplianceValidation]
    end
    
    SP --> NI
    SP --> FI
    SP --> PI
    SP --> DR
    
    PE --> SP
    PE --> RL
    
    AT --> PE
    AL --> PE
    CV --> AT
```

## Performance Architecture

```mermaid
graph TB
    subgraph "Performance Monitoring"
        PM[PerformanceMonitoring]
        MT[MetricsTracker]
        RT[ResourceTracker]
        PT[ProfilingTracker]
    end
    
    subgraph "Optimization"
        SC[SingletonContainers]
        RC[ResourceCaching]
        DC[DeterministicExecution]
        PC[ParallelExecution]
    end
    
    subgraph "SLOs"
        FT[FirstBuild ≤ 15s]
        IT[Incremental ≤ 2s]
        RT[RDF Processing ≤ 5s]
        GM[Generation Memory ≤ 100MB]
        CS[CLI Scaffolding ≤ 3s]
    end
    
    PM --> MT
    PM --> RT
    PM --> PT
    
    SC --> RC
    DC --> PC
    
    MT --> FT
    RT --> IT
    PT --> RT
    RC --> GM
    PC --> CS
```

## Error Handling Architecture

```mermaid
graph TB
    subgraph "Error Types"
        CE[CleanroomError]
        BE[BackendError]
        VE[ValidationError]
        RE[ResourceError]
        TE[TimeoutError]
    end
    
    subgraph "Error Handling"
        EH[ErrorHandler]
        RL[RecoveryLogic]
        FL[FallbackLogic]
        EL[ErrorLogging]
    end
    
    subgraph "Error Recovery"
        AR[AutomaticRecovery]
        MR[ManualRecovery]
        CR[ContainerRecovery]
        SR[ServiceRecovery]
    end
    
    CE --> EH
    BE --> EH
    VE --> EH
    RE --> EH
    TE --> EH
    
    EH --> RL
    EH --> FL
    EH --> EL
    
    RL --> AR
    RL --> MR
    AR --> CR
    AR --> SR
```

## Integration Points

### Testcontainers Integration
- Uses testcontainers-rs version 0.22
- Supports PostgreSQL, Redis, and generic containers
- Implements singleton pattern for performance
- Provides health checks and readiness probes

### Docker Integration
- Requires Docker daemon for container execution
- Supports container customization and configuration
- Implements proper cleanup and resource management
- Provides network isolation and port mapping

### Rust Integration
- Built with Rust stable toolchain
- Uses tokio for async runtime
- Implements RAII for resource management
- Provides type-safe configuration and error handling

## Best Practices Implemented

1. **Singleton Containers**: Start containers once per test suite for performance
2. **Resource Monitoring**: Track CPU, memory, disk, and network usage
3. **Security Isolation**: Network, filesystem, and process isolation
4. **Deterministic Execution**: Fixed seeds for reproducible tests
5. **Coverage Tracking**: Track test coverage and execution paths
6. **Snapshot Testing**: Capture and compare test outputs
7. **Tracing & Observability**: Detailed tracing and metrics collection
8. **Error Handling**: Comprehensive error handling and recovery
9. **Performance Monitoring**: Real-time performance monitoring and alerting
10. **RAII Management**: Automatic resource cleanup and lifecycle management
