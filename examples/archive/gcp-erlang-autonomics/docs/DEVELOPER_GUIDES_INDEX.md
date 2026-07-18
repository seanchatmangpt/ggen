# Erlang Developer Guides - Complete Index

## Overview

This directory contains five comprehensive developer guides for building and maintaining autonomic governors using Erlang on GCP. Each guide is production-ready with runnable code examples.

---

## Guide Navigation

### 1. **ERLANG_QUICK_START.md** (19 KB)
**For**: First-time Erlang developers, newcomers to the project
**Key Topics**:
- ✓ Setup Erlang OTP 25+ (asdf, Homebrew, apt)
- ✓ Compile governors with rebar3
- ✓ Run individual FSMs interactively
- ✓ Connect to single-node and multi-node clusters
- ✓ Real examples: subscription lifecycle, billing with quota, multi-tenant entitlements
- ✓ Error handling and troubleshooting

**Time to Read**: 30-40 minutes
**Prerequisites**: Basic shell/terminal knowledge
**Next Step**: Read GEN_STATEM_PATTERNS.md for deeper understanding

---

### 2. **GEN_STATEM_PATTERNS.md** (36 KB)
**For**: FSM architects, state machine designers
**Key Topics**:
- ✓ When to use gen_statem vs gen_server
- ✓ State machine design principles (explicit states, transition tables)
- ✓ Avoiding circular waits and deadlocks
- ✓ Timeout strategies (fixed, escalation chains, event-driven reset, exponential backoff)
- ✓ Event handling: call vs cast patterns
- ✓ Real governors: Billing FSM (8 states), Quota SLA FSM
- ✓ Advanced patterns: composition, self-healing, anti-patterns to avoid

**Time to Read**: 45-60 minutes
**Prerequisites**: ERLANG_QUICK_START.md
**Next Step**: ERLANG_DISTRIBUTION.md for clustering, or PERFORMANCE_TUNING.md for optimization

---

### 3. **ERLANG_DISTRIBUTION.md** (26 KB)
**For**: Distributed systems engineers, SRE/DevOps teams
**Key Topics**:
- ✓ Erlang distribution basics (nodes, clustering, RPC)
- ✓ RPC patterns: synchronous/asynchronous, timeouts, batch calls
- ✓ Global process registry with gproc (registration, lookup, pub/sub)
- ✓ Distributed transactions with mnesia
- ✓ Node failures and reconnection strategies
- ✓ Split-brain detection and healing
- ✓ Cluster debugging and monitoring
- ✓ Production patterns: high availability, load balancing, eventual consistency

**Time to Read**: 40-50 minutes
**Prerequisites**: GEN_STATEM_PATTERNS.md
**Next Step**: Production deployment (see MARKETPLACE_OPERATIONS.md)

---

### 4. **ATOMVM_DEVELOPMENT.md** (33 KB)
**For**: IoT/edge device developers, embedded systems engineers
**Key Topics**:
- ✓ AtomVM vs BEAM differences (memory, startup, capabilities)
- ✓ Memory constraints on 256MB devices (tracking, optimization, cache limits)
- ✓ Offline operation and cache management
- ✓ Bidirectional sync protocols with cloud
- ✓ Incremental sync for bandwidth-constrained networks
- ✓ Debugging edge issues (remote observation, memory leak detection, battery optimization)
- ✓ Self-healing devices and battery-aware operation

**Time to Read**: 40-50 minutes
**Prerequisites**: ERLANG_QUICK_START.md
**Next Step**: PERFORMANCE_TUNING.md for edge device optimization

---

### 5. **PERFORMANCE_TUNING.md** (27 KB)
**For**: Performance engineers, SRE teams optimizing for throughput/latency
**Key Topics**:
- ✓ Message queue optimization (backpressure, batching, load balancing, priority queues)
- ✓ Memory profiling (leak detection, per-process analysis, ETS tuning)
- ✓ CPU hotspots with eprof
- ✓ Latency analysis with fprof
- ✓ FSM benchmarking (state transitions, ETS operations, comparative analysis)
- ✓ Production optimization: caching, async processing, connection pooling
- ✓ SLO monitoring and compliance

**Time to Read**: 45-60 minutes
**Prerequisites**: ERLANG_QUICK_START.md + GEN_STATEM_PATTERNS.md
**Next Step**: Implement optimizations based on findings

---

## Learning Paths

### Path 1: Get Started (Beginner → Intermediate)
1. **ERLANG_QUICK_START.md** - Learn basics and set up environment
2. **GEN_STATEM_PATTERNS.md** - Understand FSM design
3. Implement a simple governor following patterns from guide 2
4. **ERLANG_DISTRIBUTION.md** - Add clustering (optional)

**Estimated Time**: 3-4 hours + hands-on practice

### Path 2: Build Distributed System (Intermediate → Advanced)
1. **ERLANG_QUICK_START.md** - Ensure solid foundation
2. **GEN_STATEM_PATTERNS.md** - Master FSM design
3. **ERLANG_DISTRIBUTION.md** - Deep dive into clustering and RPC
4. Implement distributed governors with failover
5. **PERFORMANCE_TUNING.md** - Optimize for production

**Estimated Time**: 5-6 hours + hands-on practice

### Path 3: Edge Deployment (Beginner → Advanced)
1. **ERLANG_QUICK_START.md** - Learn Erlang basics
2. **GEN_STATEM_PATTERNS.md** - Understand FSM design
3. **ATOMVM_DEVELOPMENT.md** - Learn AtomVM constraints and patterns
4. Implement offline-capable governors with sync
5. **PERFORMANCE_TUNING.md** - Optimize for edge devices

**Estimated Time**: 4-5 hours + hands-on practice

### Path 4: Production Optimization (Advanced)
1. Review all guides (2-3 hours)
2. **PERFORMANCE_TUNING.md** - Deep dive into profiling and optimization
3. Run benchmarks and profilers on current code
4. Implement optimizations based on findings
5. Monitor SLOs and iterate

**Estimated Time**: 6-8 hours + ongoing monitoring

---

## Code Examples by Topic

### Subscription Management
- ERLANG_QUICK_START.md → "Example 1: Complete Subscription Lifecycle"
- GEN_STATEM_PATTERNS.md → Real Governor Examples (Billing Governor)

### Offline Operation
- ATOMVM_DEVELOPMENT.md → "Offline Operation and Cache Management"
- ATOMVM_DEVELOPMENT.md → "Sync with Cloud"

### Distributed Transactions
- ERLANG_DISTRIBUTION.md → "Distributed Transactions with mnesia"
- ERLANG_DISTRIBUTION.md → "Production Patterns"

### Performance Optimization
- PERFORMANCE_TUNING.md → All sections
- ATOMVM_DEVELOPMENT.md → Battery-aware operation

### Error Recovery
- ERLANG_DISTRIBUTION.md → "Node Failures and Reconnection"
- ATOMVM_DEVELOPMENT.md → "Self-Healing Device"
- GEN_STATEM_PATTERNS.md → Timeout strategies

---

## Quick Reference

### Common Tasks

**"How do I start an FSM?"**
→ ERLANG_QUICK_START.md → "Running Individual FSMs"

**"How do I design a state machine?"**
→ GEN_STATEM_PATTERNS.md → "State Machine Design Principles"

**"How do I handle timeouts?"**
→ GEN_STATEM_PATTERNS.md → "Timeout Strategies and Escalation"

**"How do I cluster nodes?"**
→ ERLANG_DISTRIBUTION.md → "Distribution Basics"

**"How do I make RPC calls?"**
→ ERLANG_DISTRIBUTION.md → "RPC Calls Between Nodes"

**"How do I sync data offline?"**
→ ATOMVM_DEVELOPMENT.md → "Sync with Cloud"

**"How do I find performance bottlenecks?"**
→ PERFORMANCE_TUNING.md → "CPU Hotspots and eprof"

**"How do I detect memory leaks?"**
→ PERFORMANCE_TUNING.md → "Memory Profiling"

**"How do I benchmark my FSM?"**
→ PERFORMANCE_TUNING.md → "Benchmarking FSM Operations"

---

## Code Runnable Examples

All guides include **complete, runnable code examples**:

### FSM Examples
- Entitlement Governor (8-state FSM)
- Billing Governor (6-state FSM with payment retry)
- Quota SLA Governor (quota enforcement with escalation)
- Subscription Governor (lifecycle management)
- Customer Account Governor (multi-FSM coordination)

### Cluster Examples
- Multi-node cluster setup
- RPC calls between nodes
- Global process registry (gproc)
- Distributed transactions (mnesia)
- Failover and recovery

### Edge Examples
- Offline operation with local caching
- Bidirectional cloud sync
- Conflict resolution
- Self-healing device
- Battery-aware operation

### Benchmark Examples
- Grant entitlement latency measurement
- State transition performance
- ETS operation benchmarking
- Comparative implementation analysis

---

## Erlang OTP Reference

All guides target **OTP 25+** with examples that work on:
- OTP 25 (tested)
- OTP 26 (recommended)

Key modules used:
- `gen_statem` - State machine behavior
- `supervisor` - Process supervision
- `ets` - In-memory tables
- `mnesia` - Distributed database
- `rpc` - Remote procedure calls
- `gproc` - Global process registry
- `eprof` / `fprof` - Performance profiling

---

## Integration with Project

These guides document the Erlang implementation in:
```
gcp-erlang-autonomics/
├── erlang_src/                    # Erlang source code
│   ├── autonomics_sup.erl         # Root supervisor
│   ├── entitlement_governor.erl   # 8-state FSM
│   ├── billing_governor.erl       # Payment management
│   └── ... (other supervisors and governors)
├── docs/                          # This directory
│   ├── ERLANG_QUICK_START.md
│   ├── GEN_STATEM_PATTERNS.md
│   ├── ERLANG_DISTRIBUTION.md
│   ├── ATOMVM_DEVELOPMENT.md
│   ├── PERFORMANCE_TUNING.md
│   └── DEVELOPER_GUIDES_INDEX.md  (this file)
└── tests/                         # Test suites
    └── ... (eunit and common_test suites)
```

---

## External Resources

### Official Erlang Documentation
- [Erlang OTP Official Docs](http://erlang.org/doc/)
- [gen_statem](http://erlang.org/doc/man/gen_statem.html)
- [supervisor](http://erlang.org/doc/man/supervisor.html)
- [ETS](http://erlang.org/doc/man/ets.html)
- [mnesia](http://erlang.org/doc/man/mnesia.html)

### Recommended Books
- "Programming Erlang" - Joe Armstrong
- "Erlang and OTP in Action" - Martin & Cassandra

### Community
- [Erlang Forums](https://erlangforums.com/)
- [Stack Overflow: erlang tag](https://stackoverflow.com/questions/tagged/erlang)

---

## Contribute

To improve these guides:
1. Open an issue on GitHub describing what's missing
2. Submit a PR with examples or corrections
3. Test all code examples locally before submitting

---

## FAQ

**Q: Which guide should I read first?**
A: Start with ERLANG_QUICK_START.md to set up your environment and run simple examples.

**Q: Do I need to read all guides?**
A: No. Choose based on your goals (see Learning Paths above).

**Q: Are the code examples production-ready?**
A: Yes, all examples follow production-quality patterns. Most are based on actual code in the autonomics governors.

**Q: What Erlang version do I need?**
A: OTP 25 or later. OTP 26 recommended for best performance.

**Q: Can I use these examples on Windows?**
A: Yes, Erlang works on Windows. All code examples are platform-agnostic.

**Q: Is there a video walkthrough?**
A: Not yet, but the guides are detailed enough to learn from text + code examples.

---

**Last Updated**: January 2026
**Total Content**: ~140 KB across 5 guides
**Total Time to Learn All**: 5-8 hours (reading + hands-on practice)
**Status**: Production-Ready
