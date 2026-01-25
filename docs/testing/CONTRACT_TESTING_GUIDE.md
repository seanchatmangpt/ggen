<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Contract Testing Between Services (gRPC)](#contract-testing-between-services-grpc)
  - [Overview](#overview)
  - [Contract Definition](#contract-definition)
    - [Payment Service Contract](#payment-service-contract)
    - [Contract Tests](#contract-tests)
      - [Test 1: Request Serialization](#test-1-request-serialization)
      - [Test 2: Response Structure](#test-2-response-structure)
      - [Test 3: Error Response](#test-3-error-response)
      - [Test 4: API Versioning](#test-4-api-versioning)
  - [Service Contract Testing](#service-contract-testing)
    - [Deployment Service Contract](#deployment-service-contract)
    - [Monitoring Service Contract](#monitoring-service-contract)
  - [Contract Breaking Change Detection](#contract-breaking-change-detection)
    - [Version Compatibility Matrix](#version-compatibility-matrix)
  - [Request/Response Validation](#requestresponse-validation)
    - [Payload Validation](#payload-validation)
  - [Mock Service for Contract Testing](#mock-service-for-contract-testing)
    - [Mock Payment Service](#mock-payment-service)
  - [Consumer-Driven Contracts](#consumer-driven-contracts)
  - [Contract Test Execution](#contract-test-execution)
    - [Running Contract Tests](#running-contract-tests)
    - [CI/CD Integration](#cicd-integration)
  - [Contract Registry](#contract-registry)
    - [Contract Document Template](#contract-document-template)
  - [Best Practices](#best-practices)
  - [Troubleshooting](#troubleshooting)
    - [Contract Test Failures](#contract-test-failures)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Contract Testing Between Services (gRPC)

Guide for testing service-to-service contracts in ggen's microservice architecture.

## Overview

Contract testing verifies that services can communicate correctly without full integration testing.

**Benefits**:
- Detect API incompatibilities early
- Test independently from other services
- Fail fast on breaking changes
- Document service contracts

## Contract Definition

### Payment Service Contract

```protobuf
service PaymentService {
  rpc ProcessPayment(PaymentRequest) returns (PaymentResponse);
  rpc RefundPayment(RefundRequest) returns (RefundResponse);
}

message PaymentRequest {
  double amount = 1;
  string customer_id = 2;
  string order_id = 3;
}

message PaymentResponse {
  string transaction_id = 1;
  string status = 2;  // SUCCESS, FAILED, PENDING
}
```

### Contract Tests

#### Test 1: Request Serialization
```rust
#[test]
fn contract_payment_request_serialization() {
    // Arrange
    let request = PaymentRequest {
        amount: 99.99,
        customer_id: "cust-123".to_string(),
        order_id: "order-456".to_string(),
    };

    // Act
    let bytes = request.encode_to_vec();

    // Assert: Should be deterministic binary
    assert!(!bytes.is_empty());

    // Deserialize and verify round-trip
    let decoded = PaymentRequest::decode(&bytes[..]).unwrap();
    assert_eq!(decoded.amount, request.amount);
}
```

#### Test 2: Response Structure
```rust
#[test]
fn contract_payment_response_structure() {
    // Arrange
    let response = PaymentResponse {
        transaction_id: "txn-123".to_string(),
        status: "SUCCESS".to_string(),
    };

    // Act
    let bytes = response.encode_to_vec();
    let decoded = PaymentResponse::decode(&bytes[..]).unwrap();

    // Assert
    assert_eq!(decoded.status, "SUCCESS");
    assert!(!decoded.transaction_id.is_empty());
}
```

#### Test 3: Error Response
```rust
#[test]
fn contract_payment_error_response() {
    // Arrange: Invalid amount
    let request = PaymentRequest {
        amount: -99.99,  // Invalid
        customer_id: "cust-123".to_string(),
        order_id: "order-456".to_string(),
    };

    // Act
    let response = validate_payment_request(&request);

    // Assert: Should return structured error
    assert!(response.is_err());
}
```

#### Test 4: API Versioning
```rust
#[test]
fn contract_payment_api_compatibility() {
    // Verify backward compatibility
    let v1_request = PaymentRequestV1 {
        amount: 99.99,
        customer_id: "cust-123".to_string(),
    };

    let v2_request = PaymentRequestV2::from_v1(v1_request);

    // Assert: V2 should contain all V1 fields
    assert_eq!(v2_request.amount, 99.99);
    assert_eq!(v2_request.customer_id, "cust-123");
}
```

## Service Contract Testing

### Deployment Service Contract

```rust
#[tokio::test]
async fn contract_deployment_service_readiness() {
    // Arrange: Verify service is ready
    let client = DeploymentServiceClient::connect("http://localhost:50051")
        .await
        .expect("Service should be running");

    // Act
    let request = HealthCheckRequest::default();
    let response = client.health_check(request).await;

    // Assert
    assert!(response.is_ok());
    assert_eq!(response.unwrap().status, HealthStatus::Serving);
}

#[tokio::test]
async fn contract_deployment_streaming_response() {
    // Arrange
    let mut client = DeploymentServiceClient::connect("http://localhost:50051")
        .await
        .unwrap();

    // Act: Deploy and stream status
    let request = DeploymentRequest {
        service: "payment-api".to_string(),
        version: "2.0.0".to_string(),
    };

    let mut stream = client
        .deploy_streaming(request)
        .await
        .unwrap()
        .into_inner();

    // Assert: Receive multiple status updates
    let mut statuses = Vec::new();
    while let Some(response) = stream.message().await.unwrap() {
        statuses.push(response.status);
    }

    assert!(!statuses.is_empty());
    assert!(statuses.iter().all(|s| !s.is_empty()));
}
```

### Monitoring Service Contract

```rust
#[tokio::test]
async fn contract_monitoring_metrics_endpoint() {
    // Arrange
    let client = reqwest::Client::new();

    // Act
    let response = client
        .get("http://localhost:9090/metrics")
        .send()
        .await;

    // Assert: Should return Prometheus-formatted metrics
    assert!(response.is_ok());
    let body = response.unwrap().text().await.unwrap();
    assert!(body.contains("# HELP"));  // Prometheus format
    assert!(body.contains("# TYPE"));  // Prometheus format
}

#[tokio::test]
async fn contract_monitoring_alert_webhook() {
    // Arrange
    let client = reqwest::Client::new();
    let alert = AlertWebhookRequest {
        level: "CRITICAL".to_string(),
        message: "Queue overflow".to_string(),
    };

    // Act
    let response = client
        .post("http://localhost:3000/webhook/alerts")
        .json(&alert)
        .send()
        .await;

    // Assert: Should accept webhook
    assert!(response.is_ok());
    assert_eq!(response.unwrap().status(), 200);
}
```

## Contract Breaking Change Detection

### Version Compatibility Matrix

```rust
#[derive(Debug)]
struct ContractMatrix {
    service_name: String,
    current_version: String,
    compatible_versions: Vec<String>,
}

#[test]
fn contract_version_compatibility_matrix() {
    // Arrange
    let matrix = ContractMatrix {
        service_name: "payment-service".to_string(),
        current_version: "2.1.0".to_string(),
        compatible_versions: vec![
            "2.1.0".to_string(),
            "2.0.0".to_string(),
            // "1.9.0" - BREAKING CHANGE REMOVED
        ],
    };

    // Act: Verify all compatible versions
    for version in &matrix.compatible_versions {
        let client = ServiceClient::with_version("payment-service", version)
            .expect("Should initialize client");
        let result = client.test_connection().await;

        // Assert
        assert!(result.is_ok(),
            "Version {} should be compatible", version);
    }
}
```

## Request/Response Validation

### Payload Validation

```rust
#[test]
fn contract_payment_request_validation() {
    // Required fields
    let invalid = PaymentRequest {
        amount: 0.0,  // Invalid: must be > 0
        customer_id: "".to_string(),  // Invalid: required
        order_id: "".to_string(),  // Invalid: required
    };

    let result = validate_payment_request(&invalid);
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .iter()
        .any(|e| e.contains("amount must be positive")));
}

#[test]
fn contract_payment_request_edge_cases() {
    // Test edge case amounts
    let test_cases = vec![
        (0.01, true),           // Minimum
        (9999999.99, true),     // Maximum
        (0.00, false),          // Too small
        (-99.99, false),        // Negative
        (f64::NAN, false),      // NaN
        (f64::INFINITY, false), // Infinity
    ];

    for (amount, should_valid) in test_cases {
        let request = PaymentRequest {
            amount,
            customer_id: "cust-123".to_string(),
            order_id: "order-456".to_string(),
        };

        let result = validate_payment_request(&request);
        assert_eq!(
            result.is_ok(),
            should_valid,
            "Amount {} validation mismatch",
            amount
        );
    }
}
```

## Mock Service for Contract Testing

### Mock Payment Service

```rust
use mockall::predicate::*;
use mockall::mock;

mock! {
    PaymentService {}

    #[async_trait]
    impl PaymentServiceTrait for PaymentService {
        async fn process_payment(
            &self,
            request: PaymentRequest
        ) -> Result<PaymentResponse, String>;
    }
}

#[tokio::test]
async fn contract_payment_service_mock() {
    // Arrange
    let mut mock = MockPaymentService::new();
    mock.expect_process_payment()
        .with(eq(PaymentRequest {
            amount: 99.99,
            customer_id: "cust-123".to_string(),
            order_id: "order-456".to_string(),
        }))
        .times(1)
        .returning(|_| Ok(PaymentResponse {
            transaction_id: "txn-123".to_string(),
            status: "SUCCESS".to_string(),
        }));

    // Act
    let request = PaymentRequest {
        amount: 99.99,
        customer_id: "cust-123".to_string(),
        order_id: "order-456".to_string(),
    };

    let response = mock.process_payment(request).await;

    // Assert
    assert!(response.is_ok());
    assert_eq!(response.unwrap().status, "SUCCESS");
}
```

## Consumer-Driven Contracts

Define what consumers expect from providers:

```rust
#[test]
fn consumer_expects_payment_response_format() {
    // Consumer: Order Service
    // Provider: Payment Service

    // Assert expected response format
    let response = PaymentResponse {
        transaction_id: "txn-123".to_string(),
        status: "SUCCESS".to_string(),
    };

    // Must have these fields
    assert!(!response.transaction_id.is_empty());
    assert!(!response.status.is_empty());

    // Status must be one of valid values
    assert!(vec!["SUCCESS", "FAILED", "PENDING"]
        .contains(&response.status.as_str()));
}

#[test]
fn consumer_expects_error_response_format() {
    // Error responses must follow standard format
    let error = ErrorResponse {
        code: "INVALID_AMOUNT".to_string(),
        message: "Amount must be positive".to_string(),
    };

    assert!(!error.code.is_empty());
    assert!(!error.message.is_empty());
    assert!(error.code.chars().all(|c| c.is_uppercase() || c == '_'));
}
```

## Contract Test Execution

### Running Contract Tests
```bash
# All contract tests
cargo test contract_

# Specific service contract
cargo test contract_payment

# With logging
RUST_LOG=debug cargo test contract_ -- --nocapture
```

### CI/CD Integration
```bash
# Pre-commit validation
cargo make pre-commit

# Pull request validation
cargo make ci

# Production deployment gate
cargo make release-validate
```

## Contract Registry

Document service contracts for team reference:

```
docs/
├── contracts/
│   ├── payment-service/
│   │   ├── v2.1.0.md        # Current version
│   │   ├── v2.0.0.md
│   │   └── v1.9.0.md        # Deprecated
│   ├── deployment-service/
│   ├── monitoring-service/
│   └── README.md            # Contract registry
```

### Contract Document Template

```markdown
# Payment Service Contract (v2.1.0)

## Service Details
- **Service**: payment-service
- **Version**: 2.1.0
- **Protocol**: gRPC
- **Port**: 50051

## Methods

### ProcessPayment
Request: `PaymentRequest`
- `amount`: double (required, > 0)
- `customer_id`: string (required)
- `order_id`: string (required)

Response: `PaymentResponse`
- `transaction_id`: string
- `status`: enum (SUCCESS|FAILED|PENDING)

## Changes from v2.0.0
- Added transaction_id response field
- No breaking changes

## Compatibility
- Backward compatible with v2.0.0
- Not compatible with v1.9.0
```

## Best Practices

1. **Define contracts explicitly** - Use .proto files or OpenAPI specs
2. **Test contracts early** - Contract tests run before integration tests
3. **Version carefully** - Semantic versioning for service APIs
4. **Document changes** - Maintain changelog of API modifications
5. **Test compatibility** - Verify backward/forward compatibility
6. **Use standards** - Follow gRPC, OpenAPI, or REST conventions
7. **Automated checks** - CI/CD gates for contract compliance

## Troubleshooting

### Contract Test Failures

**Issue**: Service schema changed
**Solution**: Update contract definition and tests

**Issue**: Version incompatibility
**Solution**: Check compatibility matrix, verify clients updated

**Issue**: Serialization errors
**Solution**: Ensure proto definitions match between services

## References

- gRPC Documentation: https://grpc.io/docs/
- Protocol Buffers: https://developers.google.com/protocol-buffers
- Consumer-Driven Contracts: https://martinfowler.com/articles/consumerDrivenContracts.html
