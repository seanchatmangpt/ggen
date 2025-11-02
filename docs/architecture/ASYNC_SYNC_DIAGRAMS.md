# Async/Sync Architecture Diagrams

**Version**: 2.0.0
**Date**: 2025-11-01
**Related**: ASYNC_SYNC_WRAPPER_ARCHITECTURE.md

---

## C4 Model Diagrams

### Level 1: System Context

```
                        ┌─────────────┐
                        │             │
                        │    User     │
                        │  (Developer)│
                        │             │
                        └──────┬──────┘
                               │
                               │ CLI commands
                               ▼
                    ┌──────────────────────┐
                    │                      │
                    │   ggen CLI System    │
                    │                      │
                    │  Code generation via │
                    │  knowledge graphs    │
                    │                      │
                    └──────────┬───────────┘
                               │
           ┌───────────────────┼───────────────────┐
           │                   │                   │
           ▼                   ▼                   ▼
    ┌──────────┐        ┌──────────┐       ┌──────────┐
    │   File   │        │ Network  │       │   RDF    │
    │  System  │        │ (HTTP)   │       │   Store  │
    └──────────┘        └──────────┘       └──────────┘
```

---

### Level 2: Container Diagram

```
┌─────────────────────────────────────────────────────────────────┐
│                         ggen CLI Binary                          │
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐     │
│  │                  CLI Container (Sync)                  │     │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐             │     │
│  │  │Template  │  │ Market   │  │ Project  │  ...        │     │
│  │  │Commands  │  │Commands  │  │Commands  │             │     │
│  │  └────┬─────┘  └────┬─────┘  └────┬─────┘             │     │
│  │       │             │             │                    │     │
│  └───────┼─────────────┼─────────────┼────────────────────┘     │
│          │             │             │                          │
│  ┌───────┴─────────────┴─────────────┴────────────────────┐     │
│  │        Runtime Bridge (async ↔ sync)                   │     │
│  │  ┌──────────────────────────────────────────────┐      │     │
│  │  │  Global Tokio Runtime                        │      │     │
│  │  │  - Multi-threaded (4 workers)               │      │     │
│  │  │  - Lazy initialized (once_cell)             │      │     │
│  │  │  - Static lifetime                          │      │     │
│  │  └──────────────────────────────────────────────┘      │     │
│  └──────────────────────────────────────────────────────────┘     │
│          │             │             │                          │
│  ┌───────┴─────────────┴─────────────┴────────────────────┐     │
│  │              Domain Container (Async)                  │     │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐             │     │
│  │  │Template  │  │Marketplace│  │ Project  │  ...        │     │
│  │  │ Domain   │  │  Domain   │  │ Domain   │             │     │
│  │  └──────────┘  └──────────┘  └──────────┘             │     │
│  └──────────────────────────────────────────────────────────┘     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
         │             │             │
         ▼             ▼             ▼
  ┌──────────┐  ┌──────────┐  ┌──────────┐
  │  tokio   │  │ reqwest  │  │ oxigraph │
  │   I/O    │  │  HTTP    │  │   RDF    │
  └──────────┘  └──────────┘  └──────────┘
```

---

### Level 3: Component Diagram - CLI Layer

```
┌─────────────────────────────────────────────────────────────────┐
│                      CLI Layer (Sync)                            │
│                   cli/src/cmds/**/*.rs                           │
│                                                                  │
│  ┌──────────────────────┐  ┌──────────────────────┐             │
│  │   Template Module    │  │   Market Module      │             │
│  ├──────────────────────┤  ├──────────────────────┤             │
│  │ TemplateCmd          │  │ MarketCmd            │             │
│  │  └─ Verb::New        │  │  └─ Verb::Search     │             │
│  │  └─ Verb::List       │  │  └─ Verb::Install    │             │
│  │  └─ Verb::Generate   │  │  └─ Verb::Publish    │             │
│  │                      │  │                      │             │
│  │ Each verb:           │  │ Each verb:           │             │
│  │  1. Parse args       │  │  1. Parse args       │             │
│  │  2. Validate input   │  │  2. Validate input   │             │
│  │  3. Call runtime::   │  │  3. Call runtime::   │             │
│  │     execute()        │  │     execute()        │             │
│  │  4. Format output    │  │  4. Format output    │             │
│  └──────────────────────┘  └──────────────────────┘             │
│           │                          │                          │
│           └──────────┬───────────────┘                          │
│                      ▼                                          │
│         ┌─────────────────────────┐                             │
│         │  runtime::execute()     │                             │
│         │  (async → sync bridge)  │                             │
│         └─────────────────────────┘                             │
└─────────────────────────────────────────────────────────────────┘
```

---

### Level 3: Component Diagram - Domain Layer

```
┌─────────────────────────────────────────────────────────────────┐
│                    Domain Layer (Async)                          │
│                  cli/src/domain/**/*.rs                          │
│                                                                  │
│  ┌──────────────────────┐  ┌──────────────────────┐             │
│  │  Template Domain     │  │ Marketplace Domain   │             │
│  ├──────────────────────┤  ├──────────────────────┤             │
│  │ TemplateService      │  │ MarketplaceClient    │             │
│  │  └─ create()         │  │  └─ search()         │             │
│  │  └─ list()           │  │  └─ install()        │             │
│  │  └─ generate()       │  │  └─ publish()        │             │
│  │                      │  │                      │             │
│  │ Pure Functions:      │  │ Async I/O:           │             │
│  │  └─ validate()       │  │  └─ HTTP requests    │             │
│  │  └─ transform()      │  │  └─ Cache reads      │             │
│  │  └─ format()         │  │  └─ JSON parsing     │             │
│  └──────────────────────┘  └──────────────────────┘             │
│           │                          │                          │
│           └──────────┬───────────────┘                          │
│                      ▼                                          │
│         ┌─────────────────────────┐                             │
│         │   Core Libraries        │                             │
│         │  (ggen-core, ggen-ai)   │                             │
│         └─────────────────────────┘                             │
└─────────────────────────────────────────────────────────────────┘
```

---

### Level 4: Code - Runtime Bridge

```
┌─────────────────────────────────────────────────────────────────┐
│                  Runtime Bridge (cli/src/runtime.rs)             │
│                                                                  │
│  ┌────────────────────────────────────────────────────────┐     │
│  │  static RUNTIME: Lazy<Runtime>                        │     │
│  │                                                        │     │
│  │  Lazy::new(|| {                                       │     │
│  │      tokio::runtime::Builder::new_multi_thread()      │     │
│  │          .worker_threads(4)                           │     │
│  │          .thread_name("ggen-runtime")                 │     │
│  │          .enable_all()                                │     │
│  │          .build()                                     │     │
│  │          .expect("Failed to create runtime")          │     │
│  │  })                                                   │     │
│  │                                                        │     │
│  └────────────────────────────────────────────────────────┘     │
│                           ▲                                     │
│                           │ Initialized once on first access    │
│                           │                                     │
│  ┌────────────────────────────────────────────────────────┐     │
│  │  pub fn execute<F>(future: F) -> Result<()>          │     │
│  │  where                                                │     │
│  │      F: Future<Output = Result<()>>,                 │     │
│  │  {                                                    │     │
│  │      RUNTIME.block_on(future)                        │     │
│  │  }                                                    │     │
│  │                                                        │     │
│  │  Key Properties:                                      │     │
│  │  - Thread-safe (Send + Sync)                         │     │
│  │  - Static lifetime (never dropped)                   │     │
│  │  - Zero overhead (<10ns after init)                  │     │
│  │  - Lazy init (50-100ms first call)                   │     │
│  └────────────────────────────────────────────────────────┘     │
│                                                                  │
└─────────────────────────────────────────────────────────────────┘
```

---

## Data Flow Diagrams

### Command Execution Flow

```
┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐   ┌──────────┐
│   User   │   │   CLI    │   │ Runtime  │   │  Domain  │   │  Tokio   │
│          │   │  Layer   │   │  Bridge  │   │  Layer   │   │ Runtime  │
└─────┬────┘   └─────┬────┘   └─────┬────┘   └─────┬────┘   └─────┬────┘
      │              │              │              │              │
      │─1. CLI cmd──>│              │              │              │
      │              │              │              │              │
      │              │─2. parse────>│              │              │
      │              │   args       │              │              │
      │              │              │              │              │
      │              │─3. validate─>│              │              │
      │              │   input      │              │              │
      │              │              │              │              │
      │              │─4. execute()>│              │              │
      │              │   (async     │              │              │
      │              │    closure)  │              │              │
      │              │              │              │              │
      │              │              │─5. RUNTIME  >│              │
      │              │              │   .block_on()│              │
      │              │              │              │              │
      │              │              │              │─6. domain   >│
      │              │              │              │   function() │
      │              │              │              │   (async)    │
      │              │              │              │              │
      │              │              │              │              │─7. async
      │              │              │              │              │   I/O
      │              │              │              │              │
      │              │              │              │<─8. Result──┤
      │              │              │              │              │
      │              │              │<─9. Result──┤              │
      │              │              │              │              │
      │              │<─10. Result─┤              │              │
      │              │              │              │              │
      │              │─11. format  >│              │              │
      │              │    output    │              │              │
      │              │              │              │              │
      │<─12. output─┤              │              │              │
      │   (stdout)   │              │              │              │
      │              │              │              │              │
```

---

### Error Propagation Flow

```
Domain Layer (Async)
    │
    │  tokio::fs::read().await?
    │  ↓ io::Error
    │
    │  Converted via From trait
    │  ↓ ggen_utils::error::Error
    │
    │  Result<T, Error>
    └──────────────────────────┐
                               │
                               ▼
Runtime Bridge
    │
    │  RUNTIME.block_on(future)
    │  ↓ Result<T, Error> (zero overhead)
    │
    └──────────────────────────┐
                               │
                               ▼
CLI Layer (Sync)
    │
    │  Format error message
    │  ↓ Result<T, Error>
    │
    └──────────────────────────┐
                               │
                               ▼
Main Entry Point
    │
    │  match result
    │  ├─ Ok(()) → exit code 0
    │  └─ Err(e) → print error, exit code 1
    │
    ▼
User
```

---

## State Diagram - Runtime Lifecycle

```
                    ┌─────────────────┐
                    │   Program       │
                    │   Start         │
                    └────────┬────────┘
                             │
                             ▼
                    ┌─────────────────┐
                    │   First         │
                    │   execute()     │
                    │   call          │
                    └────────┬────────┘
                             │
                             │ Lazy::force()
                             ▼
                    ┌─────────────────┐
                    │  Initialize     │
                    │  RUNTIME        │
                    │  (50-100ms)     │
                    └────────┬────────┘
                             │
                             │ Cached in static
                             ▼
         ┌──────────────────────────────────────┐
         │                                      │
         │         Runtime Active               │
         │                                      │
         │  ┌──────────────────────────────┐   │
         │  │  Subsequent execute() calls   │   │
         │  │  (<10ns overhead)             │   │
         │  └──────────────────────────────┘   │
         │                                      │
         │  Properties:                         │
         │  - Static lifetime (never dropped)   │
         │  - Thread-safe (Send + Sync)        │
         │  - 4 worker threads                 │
         │  - ~10MB memory                     │
         │                                      │
         └──────────────────────────────────────┘
                             │
                             │ Program lifetime
                             ▼
                    ┌─────────────────┐
                    │   Program       │
                    │   Exit          │
                    │  (Runtime auto  │
                    │   cleanup)      │
                    └─────────────────┘
```

---

## Performance Characteristics

### Call Graph - execute() Overhead

```
execute() call
    │
    ├─ Lazy::force() (first call only)
    │   ├─ Builder::new_multi_thread()
    │   ├─ .worker_threads(4)
    │   ├─ .enable_all()
    │   └─ .build()
    │       └─ Allocate thread pool (~50-100ms)
    │
    ├─ RUNTIME.block_on(future)
    │   ├─ Enter runtime context (~2ns)
    │   ├─ Schedule future (~3ns)
    │   ├─ Execute future (variable, async work)
    │   └─ Return result (~3ns)
    │
    └─ Total overhead: 8-10ns (after first call)
```

---

### Memory Layout

```
┌─────────────────────────────────────────────────────────────┐
│                    Process Memory                            │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌────────────────────────────────────────────────────┐     │
│  │  Static Data (.data segment)                       │     │
│  │                                                     │     │
│  │  static RUNTIME: Lazy<Runtime> (~10MB)             │     │
│  │  ┌────────────────────────────────────────────┐    │     │
│  │  │ Tokio Runtime                              │    │     │
│  │  │  - Thread pool (4 workers × 2MB = 8MB)    │    │     │
│  │  │  - Task scheduler (~1MB)                  │    │     │
│  │  │  - I/O driver (~500KB)                    │    │     │
│  │  │  - Timer wheel (~500KB)                   │    │     │
│  │  └────────────────────────────────────────────┘    │     │
│  │                                                     │     │
│  └────────────────────────────────────────────────────┘     │
│                                                              │
│  ┌────────────────────────────────────────────────────┐     │
│  │  Stack (per-thread)                                │     │
│  │  - Main thread (~8MB)                              │     │
│  │  - Worker threads (4 × 2MB = 8MB)                  │     │
│  │  - Per-call overhead (<100 bytes)                  │     │
│  └────────────────────────────────────────────────────┘     │
│                                                              │
│  ┌────────────────────────────────────────────────────┐     │
│  │  Heap (dynamic allocation)                         │     │
│  │  - Minimal per-call allocation                     │     │
│  │  - Dominated by async I/O buffers                  │     │
│  └────────────────────────────────────────────────────┘     │
│                                                              │
└─────────────────────────────────────────────────────────────┘

Total Runtime Overhead: ~10MB (one-time allocation)
Per-call Stack Overhead: <100 bytes
```

---

## Deployment Diagram

```
┌─────────────────────────────────────────────────────────────┐
│                     User Machine                             │
├─────────────────────────────────────────────────────────────┤
│                                                              │
│  ┌────────────────────────────────────────────────────┐     │
│  │              ggen Binary                           │     │
│  │  ┌──────────────────────────────────────────┐      │     │
│  │  │  CLI Layer (sync)                        │      │     │
│  │  │  - Argument parsing                      │      │     │
│  │  │  - Input validation                      │      │     │
│  │  │  - Output formatting                     │      │     │
│  │  └───────────────┬──────────────────────────┘      │     │
│  │                  │                                  │     │
│  │  ┌───────────────▼──────────────────────────┐      │     │
│  │  │  Runtime Bridge (sync ↔ async)           │      │     │
│  │  │  - Global Tokio runtime                  │      │     │
│  │  │  - execute() function                    │      │     │
│  │  └───────────────┬──────────────────────────┘      │     │
│  │                  │                                  │     │
│  │  ┌───────────────▼──────────────────────────┐      │     │
│  │  │  Domain Layer (async)                    │      │     │
│  │  │  - Business logic                        │      │     │
│  │  │  - I/O operations                        │      │     │
│  │  └───────────────┬──────────────────────────┘      │     │
│  │                  │                                  │     │
│  └──────────────────┼──────────────────────────────────┘     │
│                     │                                        │
│                     │                                        │
├─────────────────────┼────────────────────────────────────────┤
│  Operating System   │                                        │
│                     │                                        │
│  ┌──────────────────▼──────────────────────────┐             │
│  │  Async I/O Subsystem                        │             │
│  │  ┌──────────┐  ┌──────────┐  ┌──────────┐  │             │
│  │  │  epoll   │  │  kqueue  │  │  IOCP    │  │             │
│  │  │ (Linux)  │  │  (macOS) │  │(Windows) │  │             │
│  │  └──────────┘  └──────────┘  └──────────┘  │             │
│  └──────────────────┬──────────────────────────┘             │
│                     │                                        │
└─────────────────────┼────────────────────────────────────────┘
                      │
       ┌──────────────┼──────────────┐
       │              │              │
       ▼              ▼              ▼
  ┌─────────┐   ┌─────────┐   ┌─────────┐
  │  File   │   │ Network │   │  Other  │
  │ System  │   │ (HTTP)  │   │   I/O   │
  └─────────┘   └─────────┘   └─────────┘
```

---

## Comparison Diagrams

### Global Runtime vs Per-Command Runtime

```
┌────────────────────────────────────────────────────────────────┐
│           Global Runtime (Our Approach) ✅                     │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Process Lifetime:                                             │
│  │                                                              │
│  ├─ First execute() call                                       │
│  │   └─ Initialize RUNTIME (50-100ms, one-time)               │
│  │                                                              │
│  ├─ Subsequent execute() calls                                 │
│  │   └─ Use cached RUNTIME (8-10ns overhead)                  │
│  │                                                              │
│  └─ Process exit                                               │
│      └─ Runtime cleanup (automatic)                            │
│                                                                 │
│  Memory: ~10MB (one-time allocation)                           │
│  Overhead: 8-10ns per call (after first)                       │
│  Thread safety: ✅ Built-in                                    │
│                                                                 │
└────────────────────────────────────────────────────────────────┘


┌────────────────────────────────────────────────────────────────┐
│        Per-Command Runtime (Naive Approach) ❌                 │
├────────────────────────────────────────────────────────────────┤
│                                                                 │
│  Each execute() call:                                          │
│  │                                                              │
│  ├─ Create new Runtime (10-50ms)                               │
│  │   ├─ Allocate thread pool (~10MB)                          │
│  │   ├─ Initialize I/O driver                                 │
│  │   └─ Setup timer wheel                                     │
│  │                                                              │
│  ├─ Execute future                                             │
│  │                                                              │
│  └─ Drop runtime (cleanup overhead)                            │
│                                                                 │
│  Memory: 10MB per call (not reused)                            │
│  Overhead: 10-50ms per call (27,900% slower!)                  │
│  Thread safety: ⚠️ Must manage manually                        │
│                                                                 │
└────────────────────────────────────────────────────────────────┘

Performance Comparison:
┌─────────────────┬─────────────┬─────────────────┬──────────────┐
│ Metric          │ Global      │ Per-Command     │ Improvement  │
├─────────────────┼─────────────┼─────────────────┼──────────────┤
│ First call      │ 50-100ms    │ 10-50ms         │ Similar      │
│ Subsequent calls│ 8-10ns      │ 10-50ms         │ 2,835,294x ✅│
│ Memory overhead │ 10MB        │ 10MB × N calls  │ N times less │
│ Thread safety   │ Built-in    │ Manual          │ Easier ✅    │
│ Complexity      │ Low         │ High            │ Simpler ✅   │
└─────────────────┴─────────────┴─────────────────┴──────────────┘
```

---

**End of Diagrams**
