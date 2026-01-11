# ggen v3 Sector Bundles Catalog

**Status**: DETAILED CATALOG
**Version**: 3.0.0-alpha
**Purpose**: Complete specification of all sector bundles with ontologies, guards, and dark matter targets

---

## Table of Contents

1. [Sector Bundles Overview](#overview)
2. [Healthcare Bundle](#healthcare)
3. [Microservices Bundle](#microservices)
4. [Observability Bundle](#observability)
5. [Financial Services Bundle](#financial)
6. [SaaS & Multi-Tenancy Bundle](#saas)
7. [Academic & Publishing Bundle](#academic)
8. [IoT & Edge Computing Bundle](#iot)
9. [Real-Time Communications Bundle](#realtime)
10. [Data & Analytics Bundle](#data-analytics)
11. [Bundle Composition & Integration](#composition)

---

## Sector Bundles Overview

**Sector bundles are pre-composed vertical stacks** that provide 80% of a working system for specific industries/use cases.

### Bundle Structure

```
sector-{industry}-8020/
├── package.toml                  # Metadata, is_8020 flag
├── ggen.toml                     # Lifecycle configuration
├── README.md                     # Getting started guide
├── ontologies/
│   ├── {industry}_base.ttl      # Core domain model
│   ├── {industry}_patterns.ttl  # Common patterns
│   └── {industry}_compliance.ttl # Industry-specific rules
├── templates/
│   ├── π_core/                  # Crate scaffolding
│   ├── π_domain/                # Type definitions
│   ├── π_cli/                   # CLI commands
│   └── π_deployment/            # Deployment configs
├── guards/
│   ├── guard_{concern}_1.rs     # Guard implementations
│   └── guard_{concern}_2.rs
├── examples/
│   ├── example-1/               # Complete working examples
│   ├── example-2/
│   └── README.md
└── tests/
    ├── unit/
    ├── integration/
    └── compliance/
```

### Bundle Characteristics

| Aspect | Specification |
|--------|---------------|
| **Ontology Entities** | 50-150 domain-specific entities |
| **Template Coverage** | 70-90% of typical project |
| **Test Coverage** | 80-100% of bundle code |
| **Documentation** | Complete API ref + tutorials |
| **Dark Matter Target** | 60-80% work elimination |
| **Production Readiness** | 85-100 points |
| **Maturity Level** | Production minimum |

---

## Healthcare Bundle

### Bundle: `sector-healthcare-8020`

**Target**: Healthcare applications (patient records, compliance, integrations)

**Dark Matter Targets**:
- FHIR schema translation: 80% reduction (from 16 hours to 3 hours per resource)
- Compliance checklist automation: 90% reduction (from 4 hours to 20 min)
- HL7 integration: 70% reduction (from 2 days to 6 hours per integration)
- Audit trail implementation: 95% reduction (from 8 hours to 30 min)
- Encryption setup: 80% reduction

### Core Ontology Entities

```sparql
# healthcare_base.ttl - FHIR resources

PREFIX healthcare: <http://ggen.io/ontology/v3/healthcare#>
PREFIX fhir: <http://hl7.org/fhir#>

# FHIR Resource Types
healthcare:Patient a rdfs:Class ;
    rdfs:subClassOf fhir:Resource ;
    healthcare:fhirUrl "https://www.hl7.org/fhir/patient.html" ;
    healthcare:fields [
        healthcare:firstName (xsd:string, 1..1, required) ,
        healthcare:lastName (xsd:string, 1..1, required) ,
        healthcare:dateOfBirth (xsd:date, 1..1, required) ,
        healthcare:mrn (xsd:string, 1..1, unique, unique="patient_mrn") ,
        healthcare:addresses (healthcare:Address, 0..*) ,
        healthcare:telecom (healthcare:Telecom, 0..*)
    ] ;
    healthcare:constraints [
        healthcare:MrnFormatConstraint ,
        healthcare:MinimumAgeConstraint ,
        healthcare:NameNotEmptyConstraint
    ] .

healthcare:Address a rdfs:Class ;
    healthcare:fields [
        healthcare:street (xsd:string, 0..1) ,
        healthcare:city (xsd:string, 0..1) ,
        healthcare:state (xsd:string, 0..1, pattern="[A-Z]{2}") ,
        healthcare:zip (xsd:string, 0..1, pattern="\\d{5}(-\\d{4})?"
    ] .

healthcare:Telecom a rdfs:Class ;
    healthcare:fields [
        healthcare:system (healthcare:TelecomSystem, 1..1) ,
        healthcare:value (xsd:string, 1..1) ,
        healthcare:use (healthcare:TelecomUse, 0..1)
    ] .

# Observation resource
healthcare:Observation a rdfs:Class ;
    rdfs:subClassOf fhir:Resource ;
    healthcare:fields [
        healthcare:subject (healthcare:Reference, 1..1) ,
        healthcare:effectiveTime (xsd:dateTime, 1..1) ,
        healthcare:code (healthcare:CodeableConcept, 1..1) ,
        healthcare:value (xsd:string, 0..1) ,  # Can be various types
        healthcare:status (healthcare:ObservationStatus, 1..1)
    ] .

# Medication resource
healthcare:Medication a rdfs:Class ;
    rdfs:subClassOf fhir:Resource ;
    healthcare:fields [
        healthcare:code (healthcare:CodeableConcept, 1..1) ,
        healthcare:form (healthcare:CodeableConcept, 0..1) ,
        healthcare:ingredient (healthcare:MedicationIngredient, 0..*)
    ] .

healthcare:MedicationIngredient a rdfs:Class ;
    healthcare:fields [
        healthcare:item (healthcare:CodeableReference, 1..1) ,
        healthcare:strength (healthcare:Ratio, 0..1)
    ] .
```

### Key Guards

#### Guard: HIPAACompliance

```rust
pub struct HIPAACompliance;

impl Guard for HIPAACompliance {
    fn check(&self, package: &Package) -> Result<ValidationReceipt, GuardError> {
        let mut receipt = ValidationReceipt::new();

        // 1. Encryption at rest
        self.check_encryption_at_rest(package)?;

        // 2. Encryption in transit (TLS 1.2+)
        self.check_tls_configuration(package)?;

        // 3. Access logs
        self.check_audit_trails(package)?;

        // 4. De-identification rules
        self.check_deidentification_logic(package)?;

        // 5. Data retention policies
        self.check_retention_policies(package)?;

        // 6. Breach notification procedures
        self.check_breach_procedures(package)?;

        Ok(receipt)
    }
}
```

#### Guard: FHIRConformance

```rust
pub struct FHIRConformance;

impl Guard for FHIRConformance {
    fn check(&self, package: &Package) -> Result<ValidationReceipt, GuardError> {
        // 1. Verify all FHIR resources use correct structure
        // 2. Validate JSON schema against FHIR spec
        // 3. Check cardinality of fields (1..1, 0..1, 0..*, 1..*)
        // 4. Ensure required fields are not nullable
        // 5. Verify constraint definitions match FHIR
        Ok(receipt)
    }
}
```

### Generated Code Example

From `healthcare:Patient` entity, generate:

**Rust**:
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Patient {
    pub id: String,
    pub first_name: String,
    pub last_name: String,
    pub date_of_birth: NaiveDate,
    pub mrn: String,  // Unique, validated
    pub addresses: Vec<Address>,
    pub telecom: Vec<Telecom>,
}

impl Patient {
    pub fn validate(&self) -> Result<(), PatientError> {
        // MRN format validation
        if !self.mrn.chars().all(|c| c.is_ascii_alphanumeric()) {
            return Err(PatientError::InvalidMrn);
        }

        // Age validation (minimum age constraint)
        let age = Local::now().date_naive().years_since(self.date_of_birth)
            .ok_or(PatientError::InvalidDateOfBirth)?;
        if age < 0 {
            return Err(PatientError::InvalidDateOfBirth);
        }

        Ok(())
    }
}

// Encrypted field handling
impl Patient {
    pub fn encrypt_pii(&mut self, key: &EncryptionKey) -> Result<(), EncryptionError> {
        self.first_name = encrypt(&self.first_name, key)?;
        self.last_name = encrypt(&self.last_name, key)?;
        self.date_of_birth = encrypt(&self.date_of_birth.to_string(), key)?;
        Ok(())
    }
}
```

**TypeScript**:
```typescript
export interface Patient {
    id: string;
    firstName: string;
    lastName: string;
    dateOfBirth: string;  // ISO 8601
    mrn: string;
    addresses: Address[];
    telecom: Telecom[];
}

export class PatientValidator {
    static validate(patient: Patient): ValidationError[] {
        const errors: ValidationError[] = [];

        // MRN validation
        if (!/^[A-Z0-9]+$/.test(patient.mrn)) {
            errors.push({ field: 'mrn', message: 'Invalid MRN format' });
        }

        // Date validation
        const dob = new Date(patient.dateOfBirth);
        if (isNaN(dob.getTime())) {
            errors.push({ field: 'dateOfBirth', message: 'Invalid date' });
        }

        return errors;
    }
}
```

### Dark Matter Reduction: Compliance Automation

**Before (Manual)**:
```
1. Review HIPAA requirements checklist (30 min)
2. Identify required fields for audit logs (20 min)
3. Write encryption wrapper classes (90 min)
4. Implement access log middleware (120 min)
5. Create breach notification logic (60 min)
6. Write compliance tests (120 min)
Total: ~8.5 hours
```

**After (ggen Bundle)**:
```
1. Apply HIPAACompliance guard (5 min)
   ✓ Audit log structure generated
   ✓ Encryption templates provided
   ✓ Breach notification scaffolds created
2. Customize compliance logic (20 min)
3. Run compliance tests (5 min)
   ✓ All generated, just verify
Total: ~30 minutes
```

**Reduction**: 94% (8.5 hours → 30 min)

---

## Microservices Bundle

### Bundle: `sector-rust-microservice-8020`

**Target**: Rust microservices with production patterns

**Dark Matter Targets**:
- Error handling: 90% reduction (6 hours → 30 min)
- Observability setup: 85% reduction (8 hours → 1 hour)
- Health checks: 95% reduction (2 hours → 5 min)
- Graceful shutdown: 90% reduction (3 hours → 20 min)
- API versioning: 80% reduction (4 hours → 50 min)

### Core Ontology Entities

```sparql
PREFIX microservice: <http://ggen.io/ontology/v3/microservice#>

# Service definition
microservice:Service a rdfs:Class ;
    microservice:fields [
        microservice:name (xsd:string, 1..1) ,
        microservice:port (xsd:unsignedInt, 1..1, min=1024, max=65535) ,
        microservice:version (xsd:string, 1..1) ,
        microservice:dependencies (microservice:Service, 0..*) ,
        microservice:endpoints (microservice:Endpoint, 1..*)
    ] .

# REST endpoint
microservice:Endpoint a rdfs:Class ;
    microservice:fields [
        microservice:method (microservice:HttpMethod, 1..1) ,  # GET, POST, etc.
        microservice:path (xsd:string, 1..1, pattern="/[a-z/{}]*") ,
        microservice:handler (xsd:string, 1..1) ,  # Function name
        microservice:requestBody (microservice:RequestBody, 0..1) ,
        microservice:responseBody (microservice:ResponseBody, 1..1) ,
        microservice:errors (microservice:ErrorResponse, 0..*)
    ] .

# Error handling
microservice:ErrorResponse a rdfs:Class ;
    microservice:fields [
        microservice:statusCode (xsd:unsignedInt, 1..1) ,
        microservice:errorType (xsd:string, 1..1) ,
        microservice:message (xsd:string, 1..1) ,
        microservice:details (xsd:string, 0..1)
    ] .

# Observability
microservice:ObservabilityConfig a rdfs:Class ;
    microservice:fields [
        microservice:logLevel (microservice:LogLevel, 1..1) ,
        microservice:metrics (xsd:boolean, 1..1) ,
        microservice:traces (xsd:boolean, 1..1) ,
        microservice:healthCheckPath (xsd:string, 1..1) ,
        microservice:readinessCheckPath (xsd:string, 0..1)
    ] .

# Deployment
microservice:DeploymentTarget a rdfs:Class ;
    microservice:fields [
        microservice:targetType (microservice:TargetType, 1..1) ,  # Docker, K8s, Lambda
        microservice:environment (xsd:string, 1..1) ,  # prod, staging, dev
        microservice:replicas (xsd:unsignedInt, 1..1, min=1) ,
        microservice:resources (microservice:ResourceLimits, 0..1)
    ] .

microservice:ResourceLimits a rdfs:Class ;
    microservice:fields [
        microservice:cpuRequest (xsd:string, 0..1) ,  # "500m"
        microservice:cpuLimit (xsd:string, 0..1) ,
        microservice:memoryRequest (xsd:string, 0..1) ,  # "512Mi"
        microservice:memoryLimit (xsd:string, 0..1)
    ] .
```

### Key Guards

#### Guard: ChatmanCompliant

```rust
pub struct ChatmanCompliant;

impl Guard for ChatmanCompliant {
    fn check(&self, package: &Package) -> Result<ValidationReceipt, GuardError> {
        let mut receipt = ValidationReceipt::new();

        // 1. No unwrap/expect in production code
        self.check_no_unwrap(package)?;

        // 2. No panic in production code
        self.check_no_panic(package)?;

        // 3. AAA (Arrange-Act-Assert) enforced in tests
        self.check_test_aaa_pattern(package)?;

        // 4. All errors handled
        self.check_error_handling(package)?;

        // 5. Trait bounds documented
        self.check_generic_bounds(package)?;

        Ok(receipt)
    }
}
```

#### Guard: ProductionReady

```rust
pub struct ProductionReady;

impl Guard for ProductionReady {
    fn check(&self, package: &Package) -> Result<ValidationReceipt, GuardError> {
        // 1. Health check endpoints
        self.check_health_endpoints(package)?;

        // 2. Graceful shutdown logic
        self.check_graceful_shutdown(package)?;

        // 3. Error recovery
        self.check_error_recovery(package)?;

        // 4. Logging at all levels
        self.check_logging_coverage(package)?;

        // 5. Metrics instrumentation
        self.check_metrics_present(package)?;

        Ok(receipt)
    }
}
```

### Generated Code Example

**Error Handling Framework**:
```rust
// Generated error type
#[derive(Debug)]
pub enum ServiceError {
    NotFound(String),
    BadRequest(String),
    Internal(String),
    Unauthorized,
    Forbidden,
    Conflict(String),
}

impl From<ServiceError> for HttpResponse {
    fn from(err: ServiceError) -> Self {
        match err {
            ServiceError::NotFound(msg) => {
                HttpResponse::NotFound().json(ErrorResponse {
                    error: "NOT_FOUND",
                    message: msg,
                })
            }
            ServiceError::BadRequest(msg) => {
                HttpResponse::BadRequest().json(ErrorResponse {
                    error: "BAD_REQUEST",
                    message: msg,
                })
            }
            // ... other cases
        }
    }
}

// Generated endpoint with proper error handling
#[post("/users")]
pub async fn create_user(
    body: web::Json<CreateUserRequest>,
    db: web::Data<Database>,
) -> Result<HttpResponse, ServiceError> {
    // Arrange
    let user_data = body.into_inner();

    // Validate
    validate_user(&user_data)?;

    // Act
    let user = db.insert_user(&user_data)
        .await
        .map_err(|e| ServiceError::Internal(e.to_string()))?;

    // Assert & Return
    Ok(HttpResponse::Created().json(user))
}

// Generated health check
#[get("/health")]
pub async fn health_check(
    db: web::Data<Database>,
) -> Result<HttpResponse, ServiceError> {
    let is_healthy = db.ping()
        .await
        .is_ok();

    if is_healthy {
        Ok(HttpResponse::Ok().json(json!({"status": "healthy"})))
    } else {
        Err(ServiceError::Internal("Database unreachable".into()))
    }
}
```

---

## Observability Bundle

### Bundle: `sector-observability-8020`

**Target**: Complete observability infrastructure (metrics, logs, traces, SLOs)

**Dark Matter Targets**:
- OTEL instrumentation: 85% reduction (8 hours → 1 hour)
- SLO definition: 90% reduction (3 hours → 20 min)
- Dashboard setup: 80% reduction (6 hours → 1 hour)
- Alerting rules: 85% reduction (4 hours → 30 min)

### Core Ontology

```sparql
PREFIX obs: <http://ggen.io/ontology/v3/observability#>

# Metrics definition
obs:Metric a rdfs:Class ;
    obs:fields [
        obs:name (xsd:string, 1..1) ,
        obs:type (obs:MetricType, 1..1) ,  # Counter, Gauge, Histogram
        obs:unit (xsd:string, 0..1) ,
        obs:description (xsd:string, 1..1)
    ] .

# Service Level Objectives
obs:SLO a rdfs:Class ;
    obs:fields [
        obs:name (xsd:string, 1..1) ,
        obs:indicator (obs:SLI, 1..1) ,
        obs:target (xsd:decimal, 1..1) ,  # 99.9%
        obs:window (xsd:duration, 1..1) ,  # 30 days
        obs:errorBudget (xsd:decimal, 0..1)
    ] .

# Service Level Indicators
obs:SLI a rdfs:Class ;
    obs:fields [
        obs:name (xsd:string, 1..1) ,
        obs:query (xsd:string, 1..1) ,  # SPARQL or PromQL
        obs:description (xsd:string, 1..1)
    ] .

# Alert rule
obs:AlertRule a rdfs:Class ;
    obs:fields [
        obs:name (xsd:string, 1..1) ,
        obs:condition (xsd:string, 1..1) ,  # PromQL condition
        obs:severity (obs:Severity, 1..1) ,  # critical, warning, info
        obs:message (xsd:string, 1..1) ,
        obs:runbookUrl (xsd:anyURI, 0..1)
    ] .
```

### Generated Observability Stack

**Metrics Registration** (Prometheus):
```rust
// Generated from ontology
pub struct MetricsRegistry;

impl MetricsRegistry {
    pub fn http_request_duration() -> HistogramVec {
        register_histogram_vec!(
            "http_request_duration_seconds",
            "HTTP request duration",
            &["method", "path", "status"]
        ).unwrap()
    }

    pub fn database_queries() -> IntCounterVec {
        register_int_counter_vec!(
            "database_queries_total",
            "Total database queries",
            &["operation", "table"]
        ).unwrap()
    }

    pub fn cache_hits() -> IntCounterVec {
        register_int_counter_vec!(
            "cache_hits_total",
            "Total cache hits",
            &["cache_name"]
        ).unwrap()
    }
}
```

**SLO Definition** (Grafana dashboard):
```json
{
  "dashboard": {
    "title": "Service SLOs",
    "panels": [
      {
        "title": "API Availability SLO",
        "targets": [
          {
            "expr": "sum(rate(http_requests_total{status=~'2..'}[5m])) / sum(rate(http_requests_total[5m]))",
            "legendFormat": "Availability"
          }
        ],
        "alert": {
          "name": "Low API Availability",
          "condition": "{{value}} < 0.999",
          "message": "API availability below 99.9% SLO"
        }
      }
    ]
  }
}
```

---

## Financial Services Bundle

### Bundle: `sector-finance-8020`

**Target**: Financial applications (payments, transactions, compliance)

**Dark Matter Targets**:
- PCI-DSS compliance: 80% reduction
- Payment processing: 75% reduction
- Regulatory reporting: 85% reduction
- Audit trail: 90% reduction

### Key Entities

```sparql
PREFIX finance: <http://ggen.io/ontology/v3/finance#>

finance:Account a rdfs:Class ;
    finance:fields [
        finance:accountId (xsd:string, 1..1, unique) ,
        finance:balance (xsd:decimal, 1..1, min=0) ,
        finance:currency (xsd:string, 1..1, pattern="[A-Z]{3}") ,
        finance:accountType (finance:AccountType, 1..1) ,
        finance:createdAt (xsd:dateTime, 1..1) ,
        finance:encryptedPan (xsd:hexBinary, 0..1)  # PCI-DSS encrypted
    ] .

finance:Transaction a rdfs:Class ;
    finance:fields [
        finance:id (xsd:string, 1..1, unique) ,
        finance:fromAccount (xsd:string, 1..1) ,
        finance:toAccount (xsd:string, 1..1) ,
        finance:amount (xsd:decimal, 1..1, min=0.01) ,
        finance:currency (xsd:string, 1..1) ,
        finance:status (finance:TransactionStatus, 1..1) ,
        finance:timestamp (xsd:dateTime, 1..1) ,
        finance:auditTrail (xsd:string, 1..1)  # Immutable log
    ] .
```

---

## SaaS & Multi-Tenancy Bundle

### Bundle: `sector-saas-8020`

**Target**: SaaS applications with multi-tenancy, billing, auth

**Dark Matter Targets**:
- Tenant isolation: 90% reduction
- Billing integration: 85% reduction
- Auth/RBAC: 80% reduction

### Key Features

1. **Tenant Isolation**:
   - Row-level security (RLS)
   - Data segregation
   - Resource quotas

2. **Billing System**:
   - Usage tracking
   - Invoice generation
   - Payment processing
   - Metering

3. **Authentication**:
   - OAuth2/OIDC integration
   - SAML support
   - API key management
   - MFA

---

## Academic & Publishing Bundle

### Bundle: `sector-academic-8020`

**Target**: Academic paper submission, peer review, publishing

**Dark Matter Targets**:
- Formatting: 85% reduction
- Submission checklist: 90% reduction
- Review workflow: 80% reduction

### Key Entities

```sparql
PREFIX academic: <http://ggen.io/ontology/v3/academic#>

academic:Paper a rdfs:Class ;
    academic:fields [
        academic:id (xsd:string, 1..1) ,
        academic:title (xsd:string, 1..1, maxLength=200) ,
        academic:authors (academic:Author, 1..*) ,
        academic:abstract (xsd:string, 1..1) ,
        academic:keywords (xsd:string, 1..*) ,
        academic:content (xsd:string, 1..1) ,
        academic:venue (xsd:string, 1..1) ,
        academic:status (academic:PaperStatus, 1..1) ,
        academic:submittedAt (xsd:dateTime, 0..1) ,
        academic:reviews (academic:Review, 0..*)
    ] .

academic:Author a rdfs:Class ;
    academic:fields [
        academic:name (xsd:string, 1..1) ,
        academic:email (xsd:string, 1..1) ,
        academic:affiliation (xsd:string, 0..1) ,
        academic:orcid (xsd:string, 0..1)
    ] .

academic:Review a rdfs:Class ;
    academic:fields [
        academic:reviewer (xsd:string, 1..1) ,
        academic:recommendation (academic:Recommendation, 1..1) ,
        academic:comments (xsd:string, 1..1) ,
        academic:submittedAt (xsd:dateTime, 1..1) ,
        academic:confidential (xsd:boolean, 1..1)
    ] .
```

---

## IoT & Edge Computing Bundle

### Bundle: `sector-iot-edge-8020`

**Target**: IoT devices, edge computing, sensor networks

**Dark Matter Targets**:
- Device management: 85% reduction
- Message routing: 80% reduction
- Time series storage: 75% reduction

### Key Entities

```sparql
PREFIX iot: <http://ggen.io/ontology/v3/iot#>

iot:Device a rdfs:Class ;
    iot:fields [
        iot:deviceId (xsd:string, 1..1, unique) ,
        iot:deviceType (iot:DeviceType, 1..1) ,
        iot:firmwareVersion (xsd:string, 1..1) ,
        iot:status (iot:DeviceStatus, 1..1) ,
        iot:lastSeen (xsd:dateTime, 1..1) ,
        iot:sensors (iot:Sensor, 0..*)
    ] .

iot:Sensor a rdfs:Class ;
    iot:fields [
        iot:name (xsd:string, 1..1) ,
        iot:type (iot:SensorType, 1..1) ,
        iot:unit (xsd:string, 1..1) ,
        iot:lastValue (xsd:decimal, 0..1) ,
        iot:lastUpdated (xsd:dateTime, 0..1)
    ] .

iot:TimeSeries a rdfs:Class ;
    iot:fields [
        iot:deviceId (xsd:string, 1..1) ,
        iot:sensorId (xsd:string, 1..1) ,
        iot:timestamp (xsd:dateTime, 1..1) ,
        iot:value (xsd:decimal, 1..1) ,
        iot:quality (iot:DataQuality, 1..1)
    ] .
```

---

## Real-Time Communications Bundle

### Bundle: `sector-realtime-8020`

**Target**: Real-time messaging (WebSockets, MQTT, gRPC)

**Dark Matter Targets**:
- Connection management: 85% reduction
- Message routing: 80% reduction
- Presence tracking: 75% reduction

---

## Data & Analytics Bundle

### Bundle: `sector-analytics-8020`

**Target**: Data pipelines, warehousing, BI

**Dark Matter Targets**:
- Schema definition: 80% reduction
- Pipeline orchestration: 75% reduction
- Transformation logic: 70% reduction

---

## Bundle Composition & Integration

### Cross-Bundle Dependencies

```
sector-microservice-8020
  ├─ Requires: sector-observability-8020
  ├─ Optional: sector-finance-8020 (payment service)
  └─ Optional: sector-saas-8020 (if multi-tenant)

sector-healthcare-8020
  ├─ Requires: sector-observability-8020
  ├─ Requires: compliance guards
  └─ Optional: sector-analytics-8020 (analytics)

sector-saas-8020
  ├─ Requires: sector-microservice-8020
  ├─ Requires: sector-auth-8020
  └─ Optional: sector-analytics-8020 (usage tracking)
```

### Installation Pattern

```bash
# Install base microservice bundle
ggen marketplace install sector-rust-microservice-8020

# Add observability
ggen marketplace install sector-observability-8020 \
  --dependency sector-rust-microservice-8020

# Merge ontologies
ggen ontology merge \
  ./ontologies/microservice_base.ttl \
  ./ontologies/observability_base.ttl \
  --output ./ontologies/combined.ttl

# Generate everything
ggen project gen . --ontology ./ontologies/combined.ttl
```

---

## Bundle Selection Matrix

| Use Case | Bundle | Dark Matter Reduction | Setup Time |
|----------|--------|---------------------|-----------|
| REST API | microservice-8020 | 90% | 2 hours |
| Patient Records | healthcare-8020 | 80% | 3 hours |
| Observability | observability-8020 | 85% | 1 hour |
| Payments | finance-8020 | 80% | 4 hours |
| Multi-Tenant SaaS | saas-8020 | 85% | 5 hours |
| Academic Submission | academic-8020 | 85% | 2 hours |
| IoT Platform | iot-edge-8020 | 85% | 3 hours |
| Real-Time Chat | realtime-8020 | 80% | 2 hours |
| Data Pipeline | analytics-8020 | 75% | 4 hours |

---

## Conclusion

Sector bundles enable users to start with **80% of a working system** instead of starting from scratch. Each bundle represents distilled domain knowledge from hundreds of projects, pre-composed as:

- ✅ Complete ontologies
- ✅ Validated templates
- ✅ Industry-specific guards
- ✅ Working examples
- ✅ Comprehensive documentation

Result: **From days of setup to hours of customization.**

---

**Document Version**: 1.0
**Created**: November 17, 2025
**Branch**: `claude/plan-ggen-v3-rewrite-01PyJAjvvvwdVWwD6wickodF`
**Next**: Security & Threat Model Analysis
