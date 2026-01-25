<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [ADR-001: gRPC vs HTTP API Architecture](#adr-001-grpc-vs-http-api-architecture)
  - [Problem Statement](#problem-statement)
  - [Decision](#decision)
  - [Rationale](#rationale)
    - [gRPC Benefits (Internal)](#grpc-benefits-internal)
    - [HTTP/REST Benefits (Public)](#httprest-benefits-public)
    - [Hybrid Approach Advantages](#hybrid-approach-advantages)
  - [Implementation](#implementation)
    - [Service Definitions (tai.proto)](#service-definitions-taiproto)
    - [HTTP Gateway (REST API)](#http-gateway-rest-api)
    - [Client Implementation](#client-implementation)
  - [Trade-offs](#trade-offs)
  - [Consequences](#consequences)
    - [Positive](#positive)
    - [Negative](#negative)
  - [Migration Path](#migration-path)
  - [Monitoring and Observability](#monitoring-and-observability)
  - [References](#references)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# ADR-001: gRPC vs HTTP API Architecture

**Status:** Accepted
**Date:** 2026-01-25
**Context:** Choosing between gRPC and HTTP/REST for inter-service communication
**Deciders:** Architecture Review Board

## Problem Statement

TAI system requires efficient, reliable communication between multiple microservices (Governor, Coordinator, Scheduler) and external clients. Need to evaluate trade-offs between:
- **gRPC**: Binary protocol, HTTP/2, strongly-typed services
- **HTTP/REST**: Universal, widely-supported, simpler integration
- **Hybrid approach**: Use gRPC internally, HTTP for public APIs

## Decision

**Hybrid architecture:**
- **Internal service-to-service:** gRPC (high performance, low latency, streaming)
- **External/Public APIs:** HTTP/REST over gRPC with JSON transcoding
- **Client libraries:** Support both gRPC and HTTP clients

## Rationale

### gRPC Benefits (Internal)
1. **Performance:**
   - Binary serialization (Protocol Buffers) vs JSON
   - HTTP/2 multiplexing for concurrent requests
   - Reduced payload size (estimated 3-7x smaller than JSON)
   - Lower latency (<10ms vs 50-100ms for REST)

2. **Features:**
   - Built-in streaming (bidirectional)
   - Server push capabilities
   - Type safety through .proto files
   - Automatic client/server generation

3. **Reliability:**
   - Strong contract enforcement
   - Semantic versioning through proto definitions
   - Built-in health checks (grpc.health.v1)

### HTTP/REST Benefits (Public)
1. **Accessibility:**
   - Works across all firewalls and proxies
   - Easy debugging (curl, Postman, browser)
   - Natural fit for web applications
   - CDN-friendly caching

2. **Ecosystem:**
   - Mature observability tools
   - Standard load balancing
   - Reverse proxy support
   - Wide library availability

### Hybrid Approach Advantages
- Services communicate efficiently internally (gRPC)
- External clients have familiar HTTP interface
- gRPC web protocol for browser-based clients
- Reverse proxy handles protocol translation

## Implementation

### Service Definitions (tai.proto)
```proto
service Governor {
  rpc ProposePolicy(Policy) returns (Receipt);
  rpc EnforcePolicy(Policy) returns (Receipt);
  rpc GetPolicies(GetPoliciesRequest) returns (GetPoliciesResponse);
  rpc StreamPolicies(StreamPoliciesRequest) returns (stream Policy);
}

service Coordinator {
  rpc SubmitSignal(Signal) returns (Receipt);
  rpc RequestAction(Action) returns (Receipt);
  rpc StreamActions(StreamActionsRequest) returns (stream Action);
}

service Scheduler {
  rpc SubmitTask(ScheduleTaskRequest) returns (Receipt);
  rpc GetTaskStatus(GetTaskStatusRequest) returns (TaskStatus);
  rpc StreamTaskUpdates(StreamTaskUpdatesRequest) returns (stream TaskUpdate);
}
```

### HTTP Gateway (REST API)
- Envoy proxy with gRPC-JSON transcoding
- Automatic conversion: HTTP/JSON ↔ gRPC/Protobuf
- OpenAPI/Swagger generation from proto definitions
- Standard HTTP status codes mapping to gRPC codes

### Client Implementation
```rust
// Internal service-to-service (gRPC)
let governor_client = GovernorClient::connect("http://governor:50051").await?;
let receipt = governor_client.propose_policy(policy).await?;

// External client (HTTP)
let client = Client::new("https://api.example.com/v1");
let receipt = client.post("/policies/propose", policy).await?;
```

## Trade-offs

| Aspect | gRPC | REST | Decision |
|--------|------|------|----------|
| Performance | Excellent (binary, multiplexed) | Good (JSON, simple) | gRPC internal |
| Accessibility | Limited (special clients) | Excellent (ubiquitous) | REST external |
| Debugging | Hard (binary protocol) | Easy (text-based) | Both supported |
| Tooling | Maturing | Mature | REST for debugging |
| Streaming | Native bidirectional | Requires websockets | gRPC for streaming |
| Latency | <10ms | 50-100ms | gRPC critical path |

## Consequences

### Positive
- High-performance inter-service communication
- Type-safe service contracts
- Built-in streaming capabilities
- Familiar REST API for external clients
- Better resource utilization (HTTP/2, compression)

### Negative
- Operational complexity (maintain dual protocols)
- gRPC debugging requires specialized tools
- Additional layer (proxy) for HTTP translation
- Team training needed for proto-based development

## Migration Path

1. **Phase 1:** Deploy gRPC services internally, HTTP gateway at perimeter
2. **Phase 2:** Migrate internal clients from HTTP to gRPC
3. **Phase 3:** Add gRPC-Web for browser clients
4. **Phase 4:** Deprecate legacy REST APIs (if any)

## Monitoring and Observability

- gRPC service metrics: request count, latency, error rate
- HTTP gateway metrics: translation overhead, proxy performance
- Health checks: grpc.health.v1 for service discovery
- Distributed tracing: trace gRPC calls across services

## References
- [gRPC Documentation](https://grpc.io/docs/)
- [Protocol Buffers Guide](https://developers.google.com/protocol-buffers)
- [gRPC Performance Best Practices](https://grpc.io/docs/guides/performance/)
- tai.proto service definitions in crates/tai-grpc/proto/
