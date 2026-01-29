# BEAM VM Architecture

**BEAM** (Bogdan/Björn's Erlang Abstract Machine) - The runtime that makes Erlang's concurrency model possible.

## Overview

BEAM is a virtual machine designed for:
- Massive concurrency (millions of processes)
- Fault tolerance (process isolation)
- Soft real-time (predictable latency)
- Hot code swapping (zero downtime upgrades)
- Distribution (transparent remote messaging)

## Architecture Components

### High-Level View

```
┌─────────────────────────────────────────────────┐
│               BEAM Virtual Machine               │
├─────────────────────────────────────────────────┤
│  ┌─────────────────────────────────────────┐   │
│  │          Process Scheduler              │   │
│  │  ┌────┐ ┌────┐ ┌────┐ ┌────┐          │   │
│  │  │Sch1│ │Sch2│ │Sch3│ │Sch4│  (per CPU)│   │
│  │  └────┘ └────┘ └────┘ └────┘          │   │
│  └─────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────┐   │
│  │          Process Heap Manager           │   │
│  │  ┌──────┐  ┌──────┐  ┌──────┐          │   │
│  │  │Heap1 │  │Heap2 │  │Heap3 │  (per proc)│   │
│  │  └──────┘  └──────┘  └──────┘          │   │
│  └─────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────┐   │
│  │       Garbage Collector (per-process)   │   │
│  └─────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────┐   │
│  │       Message Passing Infrastructure    │   │
│  └─────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────┐   │
│  │       ETS (Erlang Term Storage)         │   │
│  └─────────────────────────────────────────┘   │
│  ┌─────────────────────────────────────────┐   │
│  │       Distribution Layer                │   │
│  └─────────────────────────────────────────┘   │
└─────────────────────────────────────────────────┘
```

## Process Model

### Lightweight Processes

**Not OS threads!** Erlang processes are green threads scheduled by BEAM.

**Memory layout**:

```
┌──────────────────────────────────────┐
│        Erlang Process (~2KB)         │
├──────────────────────────────────────┤
│ Process Control Block (PCB)          │
│  - Process ID (PID)                  │
│  - State (running/waiting)           │
│  - Reduction count                   │
│  - Trap exit flag                    │
│  - Links and monitors                │
│  - Current function                  │
├──────────────────────────────────────┤
│ Heap (stack + heap combined)         │
│  - Initial: 233 words (~1.8KB)       │
│  - Grows dynamically                 │
│  - Per-process garbage collection    │
├──────────────────────────────────────┤
│ Mailbox (message queue)              │
│  - Unbounded FIFO queue              │
│  - Messages copied from sender       │
└──────────────────────────────────────┘
```

**Cost comparison**:

| Type | Creation time | Memory | Max per machine |
|------|---------------|--------|-----------------|
| OS Thread | ~1ms | ~1MB | ~10,000 |
| Erlang Process | ~1μs | ~2KB | ~10,000,000 |

**Why so lightweight?**
- No OS context switch (all in userspace)
- Small initial allocation
- Per-process heap (no lock contention)
- Lazy copying garbage collection

### Process Creation

```erlang
Pid = spawn(fun() -> worker_loop() end).
```

**BEAM operations**:
1. Allocate PCB (~300 bytes)
2. Allocate initial heap (233 words)
3. Initialize mailbox (empty queue)
4. Add to scheduler run queue
5. Return PID to caller

**Total time**: ~1 microsecond

## Scheduler

### Preemptive Scheduling

**One scheduler thread per CPU core**:

```
CPU 1: [Scheduler 1] → Process queue 1
CPU 2: [Scheduler 2] → Process queue 2
CPU 3: [Scheduler 3] → Process queue 3
CPU 4: [Scheduler 4] → Process queue 4
```

**Reduction counting**:

```erlang
% Each function call consumes 1 reduction
func1() ->
    func2(),     % 1 reduction
    func3(),     % 1 reduction
    func4().     % 1 reduction
```

**Preemption after 2000 reductions**:

```
Process A executes 2000 reductions → Preempted
Process B executes 2000 reductions → Preempted
Process C executes 2000 reductions → Preempted
Process A resumes...
```

**Why 2000?**
- Balance between fairness and overhead
- ~1ms of work on modern CPU
- Prevents one process from monopolizing scheduler

### Work Stealing

```
Scheduler 1: [P1, P2, P3]  ← Busy
Scheduler 2: []            ← Idle

Scheduler 2 steals half of Scheduler 1's queue:

Scheduler 1: [P1]
Scheduler 2: [P2, P3]
```

**Load balancing** happens automatically, no manual intervention needed.

### Priority Levels

```erlang
% Normal priority (default)
spawn(fun() -> work() end).

% High priority
spawn_opt(fun() -> critical_work() end, [priority, high]).

% Low priority
spawn_opt(fun() -> background_work() end, [priority, low]).
```

**Scheduling order**:
1. Max (internal use only)
2. High
3. Normal (default)
4. Low

## Memory Management

### Per-Process Heap

**Each process has isolated heap**:

```
Process A Heap: [Data1, Data2, Data3]
Process B Heap: [Data4, Data5, Data6]
Process C Heap: [Data7, Data8, Data9]

No locks needed!
No shared memory!
No race conditions!
```

**Heap structure (generational)**:

```
┌──────────────────────────────────────┐
│         Young Heap (nursery)         │
│  - New allocations go here           │
│  - Most objects die young            │
│  - Fast GC (minor collection)        │
├──────────────────────────────────────┤
│         Old Heap                     │
│  - Survivors promoted here           │
│  - Slower GC (major collection)      │
│  - Less frequent                     │
└──────────────────────────────────────┘
```

### Garbage Collection

**Per-process, generational GC**:

```erlang
% Process A allocates
Data = lists:seq(1, 1000000).  % Young heap fills up

% GC triggered (only for Process A!)
% Other processes continue running
% No "stop the world" pauses

% Survivors promoted to old heap
% Young heap cleared
```

**GC triggers**:
1. Heap exhausted
2. Explicit `erlang:garbage_collect()`
3. Process receives many messages

**Benefits**:
- No global GC pauses
- Predictable latency (soft real-time)
- Scales to millions of processes

**Drawback**:
- More overall memory use than global GC
- Shared data must be copied

### Binary Optimization

**Special handling for binaries**:

```erlang
% Small binary (<64 bytes): Copied to process heap
SmallBin = <<1,2,3>>.

% Large binary (≥64 bytes): Reference counted, shared
LargeBin = <<1:1000000>>.

% Sending large binary: Only reference copied!
Pid ! LargeBin.  % Fast! No data copied
```

**Reference counting**:

```
Process A creates binary: RefCount = 1
Process A sends to Process B: RefCount = 2
Process A garbage collects: RefCount = 1
Process B garbage collects: RefCount = 0 → Free memory
```

## Message Passing

### Message Send

```erlang
Pid ! Message.
```

**BEAM operations**:

1. **Copy message** from sender heap to receiver mailbox
   ```
   Sender Heap: [Message] → copy → Receiver Mailbox: [Message]
   ```

2. **Append to mailbox** (lock-free for single writer)
   ```
   Mailbox: [Msg1, Msg2] → [Msg1, Msg2, NewMsg]
   ```

3. **Signal receiver** scheduler
   ```
   If receiver waiting: Wake up
   If receiver running: Continue (will check mailbox later)
   ```

4. **Return immediately** (asynchronous!)

**Cost**: ~100ns for small message

### Message Receive

```erlang
receive
    {tag, Value} -> process(Value)
after 5000 ->
    timeout
end.
```

**BEAM operations**:

1. **Scan mailbox** for matching message
   ```
   Mailbox: [Msg1, Msg2, Msg3, {tag, 42}, Msg4]
                           ↑ Match!
   ```

2. **Remove matched message** from mailbox
   ```
   Mailbox: [Msg1, Msg2, Msg3, Msg4]
   ```

3. **Update receive pointer** (optimization for selective receive)

4. **Execute handler**

**Selective receive cost**: O(n) where n = mailbox size

**Optimization**: Use `receive` without pattern to process in order (O(1))

```erlang
% Fast: Process in order
receive
    Message -> handle(Message)
end.

% Slow: Selective receive
receive
    {priority, Msg} -> handle(Msg)
end.
```

## ETS (Erlang Term Storage)

### Architecture

**Shared memory tables** (exception to isolated heaps):

```
┌──────────────────────────────────────┐
│            ETS Table                 │
│  (shared between processes)          │
├──────────────────────────────────────┤
│ Type: set | ordered_set | bag        │
│ Access: public | protected | private │
│ Concurrency: read/write locks        │
└──────────────────────────────────────┘
         ↑           ↑           ↑
    Process A   Process B   Process C
```

### Concurrency Options

```erlang
% Read concurrency (multiple readers)
ets:new(my_table, [set, public, {read_concurrency, true}]).

% Write concurrency (partitioned locks)
ets:new(my_table, [set, public, {write_concurrency, true}]).

% Both (maximum parallelism)
ets:new(my_table, [set, public,
                  {read_concurrency, true},
                  {write_concurrency, true}]).
```

**Performance**:

| Operation | Without concurrency | With read_concurrency |
|-----------|---------------------|------------------------|
| Read (1 process) | 10M ops/sec | 10M ops/sec |
| Read (8 processes) | 10M ops/sec | 80M ops/sec |

### Memory Model

**ETS data is NOT in process heap**:

```
Process Heap: [Ref1, Ref2, Ref3]  ← Only references
                ↓      ↓      ↓
ETS Table:    [Data1, Data2, Data3]  ← Actual data

Lookup: Copy from ETS to process heap
Insert: Copy from process heap to ETS
```

## Distribution

### Node Communication

**Transparent remote messaging**:

```erlang
% Local send
Pid ! Message.

% Remote send (same syntax!)
{registered_name, 'node@remote'} ! Message.
```

**BEAM operations** (remote send):

1. **Serialize message** (external term format)
2. **Send over TCP** to remote node
3. **Deserialize** on remote node
4. **Deliver to mailbox**

**Distribution protocol**:
- TCP connections between nodes
- Heartbeat for failure detection
- Cookie-based authentication
- Encrypted connections (optional)

### Process Monitoring

```erlang
% Monitor remote process
MonitorRef = monitor(process, {registered_name, 'node@remote'}).

% If remote process dies or node disconnects:
receive
    {'DOWN', MonitorRef, process, Pid, Reason} ->
        % Handle failure
end.
```

**BEAM tracks**:
- Node connections
- Process lifecycles
- Link/monitor relationships
- Automatic cleanup on disconnect

## Hot Code Loading

### Code Server

**Two versions of each module in memory**:

```
Module: my_module
  Current version: #1 (running processes use this)
  Old version: #0 (deprecated, will be purged)

Load new version:
  New version: #2 (loaded but not active)
  Current → Old (#1 becomes old)
  New → Current (#2 becomes current)

Fully qualified calls use new version immediately:
  my_module:func()  → Uses #2

Local calls continue using old version:
  func()  → Uses #1 (until code_change callback)
```

### Code Replacement

```erlang
% Load new version
code:load_file(my_module).

% Processes using old version:
% - Fully qualified calls: Use new version
% - Local calls: Use old version until code_change

% Trigger code change in gen_server:
sys:suspend(Pid).
sys:change_code(Pid, my_module, OldVsn, []).
sys:resume(Pid).
```

## Performance Characteristics

### Latency

**Typical operations** (modern CPU, nanoseconds):

| Operation | Time |
|-----------|------|
| Spawn process | 1,000ns (1μs) |
| Send message (small) | 100ns |
| Receive message | 100ns |
| ETS lookup | 50ns |
| Function call | 10ns |
| Pattern match | 10ns |

### Throughput

**Message passing**:
- 10-40 million messages/sec (single node)
- 1-5 million messages/sec (distributed)

**Process creation**:
- 1-2 million processes/sec

**ETS operations**:
- 100 million reads/sec (read_concurrency)
- 10 million writes/sec

## Tuning and Monitoring

### Scheduler Utilization

```erlang
% Check scheduler usage
erlang:statistics(scheduler_wall_time).
%% [{1, ActiveTime1, TotalTime1},
%%  {2, ActiveTime2, TotalTime2},
%%  ...]

% High utilization (>80%): Good
% Low utilization (<50%): Underutilized or blocking
```

### Process Info

```erlang
% Inspect process
process_info(Pid).
%% [{heap_size, 987},
%%  {message_queue_len, 42},
%%  {reductions, 15234},
%%  {current_function, {module, func, 2}},
%%  ...]

% Monitor mailbox growth
{message_queue_len, Len} = process_info(Pid, message_queue_len).
```

### Observer Tool

```erlang
observer:start().
% Visual monitoring:
% - Process tree
% - Memory usage
% - CPU usage
% - ETS tables
% - Application tree
```

## Summary

**BEAM provides**:
- ✅ Lightweight processes (2KB, 1μs spawn)
- ✅ Preemptive scheduling (fairness)
- ✅ Per-process GC (no stop-the-world)
- ✅ Efficient message passing (100ns)
- ✅ Shared memory tables (ETS)
- ✅ Transparent distribution
- ✅ Hot code loading

**Design trade-offs**:
- ✅ Latency over throughput (soft real-time)
- ✅ Isolation over shared memory (fault tolerance)
- ✅ Simplicity over maximum performance (maintainability)

**Best for**:
- Concurrent systems (millions of connections)
- Distributed systems (multi-node clusters)
- Fault-tolerant systems (telecom, databases)
- Soft real-time systems (predictable latency)

## Further Reading

- [Tutorial: Your First OTP Application](../tutorials/01-first-otp-app.md) - Use BEAM processes
- [How-To: Optimize Message Passing](../how-to/optimize-message-passing.md) - Performance tuning
- [Actor Model Concurrency](actor-model-concurrency.md) - Theoretical foundation

## References

- Happi (Erik Stenman) - "The Erlang Runtime System" (BEAM book)
- Joe Armstrong - "Making reliable distributed systems in the presence of software errors"
- Erlang Efficiency Guide - https://www.erlang.org/doc/efficiency_guide/processes.html

---

**Key Insight**: "BEAM isn't just a VM for Erlang—it's a concurrent operating system in its own right." - Robert Virding
