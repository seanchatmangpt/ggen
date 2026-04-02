comprehensive infrastructure for the A2A (Agent-to-Agent) and TPS (Toyota Production System) implementation, including new crates for core functionality, quality control mechanisms, and extensive benchmark suites.

Key Changes
New Crates Added
ggen-testing: Chicago TDD test infrastructure
ggen-poka-yoke: Poka-yoke error proofing mechanisms
ggen-workflow-43: van der Aalst's 43 workflow patterns implementation
ggen-backpressure: Backpressure and admission control (λ ≤ μ)
ggen-heijunka: Heijunka load leveling and smooth workload distribution
ggen-osiris: OSIRIS zero cognitive load architecture
ggen-canonical: Deterministic canonicalization (Rust/JSON/RDF) with hash verification
ggen-jidoka: Jidoka gates and andon signal system for quality control
ggen-kaizen: Kaizen continuous improvement tracking with PDCA cycles
ggen-prompt-mfg: Prompt manufacturing via CONSTRUCT - deterministic compilation from RDF
ggen-packet: Packet discipline and work order types
ggen-receipt: Cryptographic receipt system with Ed25519 signatures
ggen-consensus: Byzantine consensus for receipt verification
ggen-a2a: A2A task state machine protocol
ggen-cli-tps: CLI commands for A2A/TPS operations
ggen-metrics-tps: A2A/TPS metrics collection and telemetry
ggen-marketplace-tps: TPS marketplace/workspace economy integration
ggen-integration: Integration layer connecting all A2A/TPS systems
ggen-transport: MCP/A2A transport layer with streaming support
ggen-e2e-tps: End-to-end integration tests for A2A/TPS pipeline
Benchmark Suites
Added comprehensive criterion benchmarks for:

a2a_bench.rs: Task creation, state transitions, serialization, and lifecycle benchmarks
backpressure_bench.rs: Token pool operations, concurrent acquisition, and admission control
canonical_bench.rs: JSON canonicalization, hashing, and determinism verification
jidoka_bench.rs: Andon signal checks, gate operations, and batch signal processing
packet_bench.rs: Work order creation, validation, and state transitions
Workspace Configuration
Updated Cargo.toml with new workspace members and dependencies
Added version 6.0.0 for core A2A/TPS crates (ggen-backpressure, ggen-a2a, ggen-heijunka, ggen-metrics-tps, ggen-transport)
Temporarily excluded ggen-execution and marketplace/packages/rig-mcp pending resolution of compilation issues
Added benchmark harness configurations for all new benchmark suites
Implementation Details
Quality Control (Jidoka)
The jidoka system implements andon signal mechanisms (Green/Yellow/Red) with gates for:

Compiler validation
Test execution
Lint checking
Production line monitoring
Backpressure & Admission Control
Token-based WIP (Work In Progress) limiting system ensuring λ ≤ μ (arrival rate ≤ service rate) with:

Async token acquisition
Concurrent access patterns
Utilization tracking and metrics
Canonical Determinization
Deterministic canonicalization for Rust/JSON/RDF with:

Consistent ordering guarantees
Hash verification
Batch processing support
A2A Task State Machine
Complete task lifecycle management with:

State validation
Transition rules
Artifact handling
Serialization support
Testing & Benchmarking
All new crates include comprehensive benchmarks covering:

Creation and initialization overhead
State transition performance
Concurrent operation scaling
Batch processing throughput
Serialization/deserialization costs