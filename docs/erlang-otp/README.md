# Erlang/OTP Documentation

**Diataxis-Structured Documentation for Building Fault-Tolerant Systems**

This documentation follows the [Diataxis framework](https://diataxis.fr/) to provide comprehensive coverage of Erlang/OTP concepts and practices.

## Documentation Map

```
┌─────────────────────────────────────────────────────┐
│                                                     │
│  LEARNING          │          DOING                │
│  (Tutorials)       │       (How-To Guides)         │
│                    │                               │
│  Getting started   │    Solving specific problems  │
│  Step-by-step      │    Goal-oriented              │
│                    │                               │
├────────────────────┼───────────────────────────────┤
│                    │                               │
│  UNDERSTANDING     │       INFORMATION             │
│  (Explanation)     │       (Reference)             │
│                    │                               │
│  Background theory │    Technical specifications   │
│  Design philosophy │    API documentation          │
│                    │                               │
└─────────────────────────────────────────────────────┘
```

## 📚 Tutorials (Learning-Oriented)

**Goal**: Learn by doing. Build working systems step-by-step.

- [01. Your First OTP Application](tutorials/01-first-otp-app.md) - Build a key-value store with supervision (30 minutes)
- [02. Message Passing Basics](tutorials/02-message-passing-basics.md) - Master process communication patterns (45 minutes)
- [03. Building Supervision Trees](tutorials/03-supervision-trees.md) - Implement fault-tolerant architectures (60 minutes)

## 🔧 How-To Guides (Task-Oriented)

**Goal**: Solve specific problems. Practical recipes for common tasks.

- [Handle Process Crashes](how-to/handle-process-crashes.md) - Implement layered supervision with backoff
- [Optimize Message Passing](how-to/optimize-message-passing.md) - ETS, batching, and performance tuning
- [Hot Code Reloading](how-to/hot-code-reloading.md) - Deploy updates without downtime

## 📖 Reference (Information-Oriented)

**Goal**: Look up technical details. Comprehensive API documentation.

- [gen_server Behavior API](reference/gen-server-api.md) - Complete callback reference
- [Supervisor API](reference/supervisor-api.md) - Strategies, child specs, restart policies
- [gen_statem Behavior API](reference/gen-statem-api.md) - State machine implementation

## 💡 Explanation (Understanding-Oriented)

**Goal**: Understand why. Design philosophy and architectural principles.

- [The "Let It Crash" Philosophy](explanation/let-it-crash-philosophy.md) - Joe Armstrong's key insight
- [Actor Model and Concurrency](explanation/actor-model-concurrency.md) - Theoretical foundations
- [BEAM VM Architecture](explanation/beam-vm-architecture.md) - Runtime internals and optimizations

## Quick Start

**New to Erlang/OTP?** Start here:

1. Read [The "Let It Crash" Philosophy](explanation/let-it-crash-philosophy.md) to understand the mindset
2. Follow [Your First OTP Application](tutorials/01-first-otp-app.md) to build something
3. Consult [How-To Guides](how-to/) when you encounter specific problems
4. Reference [API Documentation](reference/) for technical details

## Learning Path

```
┌─────────────────────────────────────────────────────────┐
│ 1. Read "Let It Crash" Philosophy                      │
│    ↓                                                    │
│ 2. Build First OTP App (gen_server + supervisor)       │
│    ↓                                                    │
│ 3. Learn Message Passing Patterns                      │
│    ↓                                                    │
│ 4. Study Actor Model Theory                            │
│    ↓                                                    │
│ 5. Build Supervision Trees (fault tolerance)           │
│    ↓                                                    │
│ 6. Optimize with ETS and batching                      │
│    ↓                                                    │
│ 7. Implement Hot Code Reloading                        │
│    ↓                                                    │
│ 8. Study BEAM VM Architecture (advanced)               │
└─────────────────────────────────────────────────────────┘
```

## Design Philosophy

These docs follow **Joe Armstrong's teaching principles**:

- **Simple**: Clear examples, minimal jargon
- **Elegant**: Show the beauty of the actor model
- **Practical**: Every concept backed by working code

## Prerequisites

- Erlang/OTP 26 or later
- rebar3 build tool
- Basic functional programming knowledge (pattern matching, recursion)

## AGI Connection

Erlang's actor model provides a blueprint for multi-agent AI systems:

- **Process isolation** → Independent AI agents
- **Message passing** → Agent communication without shared state
- **Supervision trees** → Fault tolerance in agent networks
- **Massive parallelism** → Millions of concurrent agents

See [Actor Model and Concurrency](explanation/actor-model-concurrency.md) for deeper analysis.

## Contributing

This documentation is part of the ggen project. See [CONTRIBUTING.md](/CONTRIBUTING.md) for guidelines.

## Resources

- [Erlang Official Documentation](https://www.erlang.org/doc/)
- [Learn You Some Erlang](https://learnyousomeerlang.com/)
- [Programming Erlang (Joe Armstrong)](https://pragprog.com/titles/jaerlang2/)
- [Designing for Scalability with Erlang/OTP](https://www.oreilly.com/library/view/designing-for-scalability/9781449361556/)

---

**Last Updated**: 2026-01-29
**Version**: 1.0.0
**Maintained by**: ggen project
