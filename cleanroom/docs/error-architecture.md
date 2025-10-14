# Error Architecture

## Error System Overview

The Cleanroom framework implements a comprehensive error handling system following core team best practices with structured error types, context preservation, and user-friendly error messages.

## Error Hierarchy

```mermaid
graph TB
    subgraph "Core Error Types"
        CE[CleanroomError]
        EK[ErrorKind]
        BE[BackendError]
        PE[PolicyError]
        SE[ScenarioError]
        SVE[ServiceError]
        CFE[ConfigError]
    end
    
    subgraph "ErrorKind Variants"
        EK2[ContainerError]
        EK3[NetworkError]
        EK4[ResourceLimitExceeded]
        EK5[Timeout]
        EK6[ConfigurationError]
        EK7[PolicyViolation]
        EK8[DeterministicError]
        EK9[CoverageError]
        EK10[SnapshotError]
        EK11[TracingError]
        EK12[RedactionError]
        EK13[ReportError]
        EK14[IoError]
        EK15[SerializationError]
        EK16[ValidationError]
        EK17[InternalError]
    end
    
    subgraph "Specialized Error Types"
        BE2[BackendError Variants]
        PE2[PolicyError Variants]
        SE2[ScenarioError Variants]
        SVE2[ServiceError Variants]
        CFE2[ConfigError Variants]
    end
    
    CE --> EK
    CE --> BE
    CE --> PE
    CE --> SE
    CE --> SVE
    CE --> CFE
    
    EK --> EK2
    EK --> EK3
    EK --> EK4
    EK --> EK5
    EK --> EK6
    EK --> EK7
    EK --> EK8
    EK --> EK9
    EK --> EK10
    EK --> EK11
    EK --> EK12
    EK --> EK13
    EK --> EK14
    EK --> EK15
    EK --> EK16
    EK --> EK17
    
    BE --> BE2
    PE --> PE2
    SE --> SE2
    SVE --> SVE2
    CFE --> CFE2
```

## CleanroomError Structure

```mermaid
graph TB
    subgraph "CleanroomError Fields"
        CE[CleanroomError]
        EK[ErrorKind]
        EM[Error Message]
        EC[Error Context]
        ES[Error Source]
        ET[Error Timestamp]
    end
    
    subgraph "Error Context"
        EC2[Context Information]
        EC3[Additional Details]
        EC4[Debug Information]
        EC5[User-Friendly Message]
    end
    
    subgraph "Error Source"
        ES2[Source Error]
        ES3[Error Chain]
        ES4[Original Error]
        ES5[Error Propagation]
    end
    
    CE --> EK
    CE --> EM
    CE --> EC
    CE --> ES
    CE --> ET
    
    EC --> EC2
    EC --> EC3
    EC --> EC4
    EC --> EC5
    
    ES --> ES2
    ES --> ES3
    ES --> ES4
    ES --> ES5
```

## Error Creation Flow

```mermaid
sequenceDiagram
    participant T as Test
    participant CE as CleanroomError
    participant EK as ErrorKind
    participant EM as ErrorMessage
    participant EC as ErrorContext
    participant ES as ErrorSource
    
    T->>CE: Create Error
    CE->>EK: Set Error Kind
    CE->>EM: Set Error Message
    CE->>EC: Add Context (Optional)
    CE->>ES: Add Source (Optional)
    CE->>CE: Set Timestamp
    
    CE-->>T: Error Created
```

## ErrorKind Categories

```mermaid
graph TB
    subgraph "Infrastructure Errors"
        IE[Infrastructure Errors]
        CE[ContainerError]
        NE[NetworkError]
        RE[ResourceLimitExceeded]
        TE[Timeout]
    end
    
    subgraph "Configuration Errors"
        CFE[Configuration Errors]
        CFE2[ConfigurationError]
        VE[ValidationError]
        IE2[InternalError]
    end
    
    subgraph "Policy Errors"
        PE[Policy Errors]
        PE2[PolicyViolation]
        DE[DeterministicError]
    end
    
    subgraph "Feature Errors"
        FE[Feature Errors]
        COE[CoverageError]
        SE[SnapshotError]
        TRE[TracingError]
        RDE[RedactionError]
        RPE[ReportError]
    end
    
    subgraph "System Errors"
        SYE[System Errors]
        IOE[IoError]
        SER[SerializationError]
    end
    
    IE --> CE
    IE --> NE
    IE --> RE
    IE --> TE
    
    CFE --> CFE2
    CFE --> VE
    CFE --> IE2
    
    PE --> PE2
    PE --> DE
    
    FE --> COE
    FE --> SE
    FE --> TRE
    FE --> RDE
    FE --> RPE
    
    SYE --> IOE
    SYE --> SER
```

## Specialized Error Types

### BackendError Variants

```mermaid
graph TB
    subgraph "BackendError"
        BE[BackendError]
        RE[Runtime Error]
        CEE[Command Execution Error]
        CSE[Container Startup Error]
        CCE[Container Communication Error]
        IPE[Image Pull Error]
        IBE[Image Build Error]
        UFE[Unsupported Feature Error]
    end
    
    BE --> RE
    BE --> CEE
    BE --> CSE
    BE --> CCE
    BE --> IPE
    BE --> IBE
    BE --> UFE
```

### PolicyError Variants

```mermaid
graph TB
    subgraph "PolicyError"
        PE[PolicyError]
        IPE[Invalid Policy Error]
        PVE[Policy Violation Error]
        UFE[Unsupported Feature Error]
    end
    
    PE --> IPE
    PE --> PVE
    PE --> UFE
```

### ScenarioError Variants

```mermaid
graph TB
    subgraph "ScenarioError"
        SE[ScenarioError]
        ISE[Invalid Scenario Error]
        SEFE[Step Execution Failed Error]
        STE[Scenario Timeout Error]
        CEE[Concurrent Execution Error]
    end
    
    SE --> ISE
    SE --> SEFE
    SE --> STE
    SE --> CEE
```

### ServiceError Variants

```mermaid
graph TB
    subgraph "ServiceError"
        SVE[ServiceError]
        CFE[Connection Failed Error]
        SFE[Startup Failed Error]
        HCFE[Health Check Failed Error]
        CE[Configuration Error]
        UOE[Unsupported Operation Error]
    end
    
    SVE --> CFE
    SVE --> SFE
    SVE --> HCFE
    SVE --> CE
    SVE --> UOE
```

### ConfigError Variants

```mermaid
graph TB
    subgraph "ConfigError"
        CFE[ConfigError]
        IFE[Invalid File Error]
        MVE[Missing Value Error]
        IVE[Invalid Value Error]
        IPE[Invalid Pattern Error]
    end
    
    CFE --> IFE
    CFE --> MVE
    CFE --> IVE
    CFE --> IPE
```

## Error Conversion and Propagation

```mermaid
graph TB
    subgraph "External Error Sources"
        EES[External Error Sources]
        IOE[std::io::Error]
        JSE[serde_json::Error]
        TCE[testcontainers::TestcontainersError]
    end
    
    subgraph "Error Conversion"
        EC[Error Conversion]
        FIO[From IO Error]
        FJS[From JSON Error]
        FTC[From Testcontainers Error]
    end
    
    subgraph "CleanroomError"
        CE[CleanroomError]
        EK[ErrorKind]
        EM[Error Message]
    end
    
    EES --> IOE
    EES --> JSE
    EES --> TCE
    
    EC --> FIO
    EC --> FJS
    EC --> FTC
    
    FIO --> CE
    FJS --> CE
    FTC --> CE
    
    CE --> EK
    CE --> EM
```

## Error Handling Patterns

```mermaid
graph TB
    subgraph "Error Handling Patterns"
        EHP[Error Handling Patterns]
        EH[Error Handling]
        ER[Error Recovery]
        EL[Error Logging]
        EN[Error Notification]
    end
    
    subgraph "Error Recovery Strategies"
        ERS[Error Recovery Strategies]
        AR[Automatic Recovery]
        MR[Manual Recovery]
        RR[Retry Recovery]
        FR[Fallback Recovery]
    end
    
    subgraph "Error Response Actions"
        ERA[Error Response Actions]
        RA[Retry Action]
        CA[Cleanup Action]
        FA[Fallback Action]
        AA[Alert Action]
    end
    
    EHP --> EH
    EHP --> ER
    EHP --> EL
    EHP --> EN
    
    ERS --> AR
    ERS --> MR
    ERS --> RR
    ERS --> FR
    
    ERA --> RA
    ERA --> CA
    ERA --> FA
    ERA --> AA
```

## Error Context and Metadata

```mermaid
graph TB
    subgraph "Error Metadata"
        EM[Error Metadata]
        ET[Error Timestamp]
        EC[Error Context]
        ES[Error Source]
        EL[Error Location]
        EU[Error User]
    end
    
    subgraph "Context Information"
        CI[Context Information]
        CF[Configuration Context]
        RC[Runtime Context]
        TC[Test Context]
        SC[System Context]
    end
    
    subgraph "Source Information"
        SI[Source Information]
        SE[Source Error]
        SC2[Source Component]
        SF[Source Function]
        SL[Source Line]
    end
    
    EM --> ET
    EM --> EC
    EM --> ES
    EM --> EL
    EM --> EU
    
    CI --> CF
    CI --> RC
    CI --> TC
    CI --> SC
    
    SI --> SE
    SI --> SC2
    SI --> SF
    SI --> SL
```

## Error Display and Formatting

```mermaid
graph TB
    subgraph "Error Display"
        ED[Error Display]
        DF[Display Format]
        UF[User-Friendly Format]
        DF2[Debug Format]
        JF[JSON Format]
    end
    
    subgraph "Error Information"
        EI[Error Information]
        EK[Error Kind]
        EM[Error Message]
        EC[Error Context]
        ES[Error Source]
    end
    
    subgraph "Error Output"
        EO[Error Output]
        CO[Console Output]
        FO[File Output]
        AO[API Output]
        LO[Log Output]
    end
    
    ED --> DF
    ED --> UF
    ED --> DF2
    ED --> JF
    
    EI --> EK
    EI --> EM
    EI --> EC
    EI --> ES
    
    EO --> CO
    EO --> FO
    EO --> AO
    EO --> LO
```

## Error Testing and Validation

```mermaid
graph TB
    subgraph "Error Testing"
        ET[Error Testing]
        ECT[Error Creation Tests]
        EDT[Error Display Tests]
        ECT2[Error Conversion Tests]
        EHT[Error Handling Tests]
    end
    
    subgraph "Error Validation"
        EV[Error Validation]
        EV2[Error Kind Validation]
        EVM[Error Message Validation]
        EVC[Error Context Validation]
        EVS[Error Source Validation]
    end
    
    subgraph "Error Scenarios"
        ES[Error Scenarios]
        CES[Container Error Scenarios]
        NES[Network Error Scenarios]
        RES[Resource Error Scenarios]
        TES[Timeout Error Scenarios]
    end
    
    ET --> ECT
    ET --> EDT
    ET --> ECT2
    ET --> EHT
    
    EV --> EV2
    EV --> EVM
    EV --> EVC
    EV --> EVS
    
    ES --> CES
    ES --> NES
    ES --> RES
    ES --> TES
```

## Error Best Practices

### 1. Error Creation
```rust
// Good: Use specific error constructors
let error = CleanroomError::container_error("Container failed to start")
    .with_context("PostgreSQL container")
    .with_source("testcontainers");

// Avoid: Generic error creation
let error = CleanroomError::new(ErrorKind::ContainerError, "Container failed");
```

### 2. Error Context
```rust
// Good: Add meaningful context
let error = CleanroomError::timeout_error("Operation timed out")
    .with_context("Database connection attempt")
    .with_source("PostgreSQL service");

// Avoid: Minimal context
let error = CleanroomError::timeout_error("Timeout");
```

### 3. Error Conversion
```rust
// Good: Convert external errors properly
let result: Result<String> = std::fs::read_to_string("config.toml")
    .map_err(|e| CleanroomError::io_error("Failed to read config file")
        .with_source(e.to_string()))?;

// Avoid: Losing error information
let result: Result<String> = std::fs::read_to_string("config.toml")
    .map_err(|_| CleanroomError::io_error("File read failed"))?;
```

### 4. Error Handling
```rust
// Good: Handle errors with specific types
match result {
    Ok(value) => println!("Success: {:?}", value),
    Err(CleanroomError { kind: ErrorKind::ContainerError, .. }) => {
        eprintln!("Container error occurred");
        // Handle container-specific error
    },
    Err(CleanroomError { kind: ErrorKind::Timeout, .. }) => {
        eprintln!("Timeout occurred");
        // Handle timeout-specific error
    },
    Err(e) => {
        eprintln!("Unexpected error: {}", e);
        // Handle unexpected error
    },
}

// Avoid: Generic error handling
match result {
    Ok(value) => println!("Success: {:?}", value),
    Err(e) => eprintln!("Error: {}", e),
}
```

### 5. Error Propagation
```rust
// Good: Propagate errors with context
fn process_data() -> Result<String> {
    let data = read_file("data.txt")?; // ? operator propagates error
    let processed = transform_data(data)?;
    Ok(processed)
}

// Avoid: Swallowing errors
fn process_data() -> Result<String> {
    let data = match read_file("data.txt") {
        Ok(d) => d,
        Err(_) => return Err(CleanroomError::io_error("File read failed")),
    };
    Ok(data)
}
```

## Error Monitoring and Alerting

### Error Metrics
- Error frequency by type
- Error rate trends
- Error recovery success rate
- Error impact assessment
- Error resolution time

### Error Alerts
- Critical error alerts
- Error threshold breaches
- Error pattern anomalies
- Error escalation procedures
- Error notification channels

### Error Reporting
- Daily error summary
- Weekly error trends
- Monthly error analysis
- Quarterly error review
- Annual error assessment

