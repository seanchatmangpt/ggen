# Erlang/OTP Quick Reference for AI Agent Development

**Last Updated**: 2026-01-29
**Purpose**: Fast lookup for Erlang/OTP concepts in AGI-relevant context

---

## Core Concepts (30-Second Overview)

### "Let It Crash" Philosophy
**What**: Don't write defensive error checks; let processes crash and supervisors restart them.
**Why**: Simpler code + automatic recovery = higher reliability.
**Joe Armstrong**: "Just let it crash" — isolation prevents cascade failures.

### Supervision Trees
**What**: Hierarchical process monitors that automatically restart failed children.
**Strategies**: `one_for_one` (restart crashed only), `one_for_all` (restart all), `rest_for_one` (restart crashed + subsequent).
**Result**: Self-healing systems with 99.999%+ uptime.

### OTP Behaviors (Design Patterns)
- **gen_server**: Client-server pattern (stateful services)
- **gen_statem**: State machine pattern (workflows, protocols)
- **gen_event**: Event handling pattern (pub-sub, logging)
- **supervisor**: Process monitoring pattern (fault tolerance)
- **application**: Deployment unit pattern (OTP releases)

---

## OTP Behavior Cheat Sheet

### gen_server (Generic Server)
**Template**:
```erlang
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

init(_Args) -> {ok, State}.
handle_call(Request, From, State) -> {reply, Reply, NewState}.
handle_cast(Msg, State) -> {noreply, NewState}.
handle_info(Info, State) -> {noreply, NewState}.
terminate(Reason, State) -> ok.
code_change(OldVsn, State, Extra) -> {ok, NewState}.
```
**Use For**: AI agent processes, resource managers, stateful services.

### gen_statem (Generic State Machine)
**Template**:
```erlang
-behaviour(gen_statem).
-export([callback_mode/0, init/1, state1/3, state2/3, terminate/3, code_change/4]).

callback_mode() -> state_functions.
init(_Args) -> {ok, state1, Data}.
state1(EventType, EventContent, Data) -> {next_state, state2, NewData}.
state2(EventType, EventContent, Data) -> {keep_state, NewData}.
```
**Use For**: Workflow orchestrators, protocol handlers, multi-step agent tasks.

### supervisor (Process Supervision)
**Template**:
```erlang
-behaviour(supervisor).
-export([init/1]).

init(_Args) ->
    SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
    ChildSpecs = [
        #{id => worker1, start => {module, start_link, [Args]},
          restart => permanent, shutdown => 5000, type => worker}
    ],
    {ok, {SupFlags, ChildSpecs}}.
```
**Use For**: Agent pool managers, fault-tolerant service containers.

---

## Carrier-Grade Requirements

### Five Nines (99.999%)
- **Downtime**: 5.26 minutes/year
- **Mechanisms**: Redundancy + fault isolation + auto-recovery + monitoring
- **Erlang/OTP**: Designed for this (Ericsson AXD301 achieved **nine nines**)

### Scalability
- **Requirement**: Millions of concurrent connections
- **Erlang**: 2KB/process overhead → 1M+ processes per node
- **Distribution**: Add nodes for horizontal scaling

### Soft Real-Time
- **Requirement**: Predictable latency (<50ms for telecom)
- **Erlang**: Preemptive scheduling, per-process GC (no global pauses)
- **Note**: NOT hard real-time (unsuitable for life-critical systems)

---

## AGI Agent Patterns (2026 State-of-Art)

### Actor Model = Erlang Processes
```
AI Agent     → Erlang Process (gen_server)
Agent State  → Process State
Agent Action → handle_call/handle_cast
Perception   → receive message
Environment  → Other processes + external systems
```

### Multi-Agent Orchestration (1,445% surge in demand, 2026)
**Pattern 1**: Manager-Worker (single coordinator, N workers)
**Pattern 2**: Pipeline (gen_stage/Broadway for data flow)
**Pattern 3**: Mesh (peer-to-peer agent communication)
**Pattern 4**: Pub-Sub (gen_event for shared awareness)

### Agent Architecture (Hybrid Symbolic-Neural)
```
[Supervisor] (symbolic coordination)
├── [LLM Agent 1] (neural execution)
├── [LLM Agent 2] (neural execution)
└── [LLM Agent N] (neural execution)
```
**Why**: Erlang/OTP for fault tolerance + supervision; LLMs for intelligence.

---

## Diataxis Documentation Framework

### Four Content Types

| Type        | Purpose         | Audience       | Example                                  |
|-------------|-----------------|----------------|------------------------------------------|
| **Tutorial**    | Learning        | Beginners      | "Build Your First Agent Pool"            |
| **How-to**      | Task completion | Practitioners  | "How to Debug Distributed Agents"        |
| **Reference**   | Information     | All            | "gen_server API Documentation"           |
| **Explanation** | Understanding   | Deep learners  | "Why Erlang for Multi-Agent Systems?"    |

**Key Insight**: Separate these content types; mixing them confuses users.

---

## Project Template Structure

```
erlang_otp_ai_platform/
├── apps/
│   ├── agent_core/           # Agent runtime (gen_server)
│   ├── agent_orchestrator/   # Workflow (gen_statem)
│   ├── agent_knowledge/      # Shared state (Mnesia/CRDT)
│   └── agent_telemetry/      # Observability (OTP + OpenTelemetry)
├── docs/
│   ├── tutorials/            # Diataxis: Learning
│   ├── how_to/               # Diataxis: Task
│   ├── reference/            # Diataxis: Information
│   └── explanation/          # Diataxis: Understanding
├── test/
│   ├── unit/                 # EUnit tests
│   ├── integration/          # Common Test
│   └── property/             # PropEr property-based tests
├── config/
│   ├── sys.config            # Runtime config
│   └── vm.args               # VM tuning
└── rebar.config              # Build config
```

---

## Common Pitfalls and Solutions

| Pitfall                     | Problem                          | Solution                              |
|-----------------------------|----------------------------------|---------------------------------------|
| Large messages              | Expensive copying                | Use ETS, references, external storage |
| Synchronous bottlenecks     | gen_server:call blocks caller    | Use cast, async pools (poolboy)       |
| Distributed Erlang at scale | O(N²) connections                | Use Partisan, Riak Core               |
| Ignoring backpressure       | Fast producers overwhelm consumers | gen_stage, Broadway, flow control     |
| Inadequate testing          | Subtle concurrent bugs           | PropEr, fault injection, chaos        |

---

## Essential Commands

### Development
```bash
rebar3 new release myapp           # Create OTP application
rebar3 compile                     # Compile project
rebar3 shell                       # Start interactive shell
rebar3 ct                          # Run Common Test suites
rebar3 dialyzer                    # Type checking
```

### Production
```bash
rebar3 release                     # Build OTP release
rebar3 tar                         # Package for deployment
_build/default/rel/myapp/bin/myapp console  # Start release
```

### Debugging
```erlang
observer:start().                  % GUI monitoring
recon:proc_count(memory, 10).     % Top 10 memory hogs
dbg:tracer(), dbg:p(all, c).      % Trace all processes
```

---

## Performance Tuning

### VM Arguments (vm.args)
```
+P 1000000           # Max processes (default 262144)
+Q 1000000           # Max ports (default 65536)
+A 128               # Async thread pool size
+K true              # Kernel poll (epoll/kqueue)
+W w                 # Warning mapping (treat as warnings)
```

### Runtime Config (sys.config)
```erlang
[
  {agent_core, [
    {pool_size, 100},
    {max_queue_length, 10000}
  ]},
  {kernel, [
    {logger_level, info}
  ]}
].
```

---

## Safety and Observability for AI Agents

### AGI Safety Mechanisms
- **Resource Limits**: Cap CPU, memory, message queue per agent
- **Timeout Enforcement**: Kill agents exceeding time budgets
- **Input Validation**: Schema checks before LLM calls
- **Output Sanitization**: Parse/validate agent responses
- **Supervisor Guardrails**: Enforce behavior constraints at supervision level

### Telemetry Events (instrument everything)
```erlang
:telemetry.execute(
  [:agent, :inference, :complete],
  #{duration => Duration, tokens => Tokens},
  #{agent_id => AgentId, model => Model}
).
```

### Observability Stack
- **Metrics**: Prometheus + Grafana
- **Traces**: OpenTelemetry + Jaeger
- **Logs**: Logger + Lager
- **Live Debugging**: observer, recon, remote shell

---

## Key Design Decisions Summary

1. **Process-Per-Agent**: Each AI agent = separate Erlang process
2. **Supervision as Guardrails**: Supervisors enforce agent constraints
3. **Message Passing**: No shared state; all coordination via messages
4. **Telemetry First**: Instrument from day one
5. **Fail-Safe Defaults**: Design for failure; recovery is automatic
6. **Horizontal Scaling**: Add nodes to cluster for more capacity

---

## Success Metrics (Carrier-Grade + AGI)

**Technical**:
- ☑ 99.999% uptime (5.26 min/year downtime)
- ☑ 1M+ concurrent agent processes
- ☑ <100ms p99 latency for agent communication
- ☑ Zero data loss during node failures

**Documentation** (Diataxis):
- ☑ Runnable tutorial (<60 min to success)
- ☑ 10+ how-to guides for common tasks
- ☑ Complete API reference
- ☑ Deep explanations of design decisions

**Adoption**:
- ☑ Production deployments
- ☑ Community contributions
- ☑ Academic citations

---

## Further Reading

**Essential Resources**:
- Joe Armstrong's Thesis (2003): [Making Reliable Distributed Systems](https://erlang.org/download/armstrong_thesis_2003.pdf)
- Erlang Design Principles: https://www.erlang.org/doc/design_principles/des_princ
- Diataxis Framework: https://diataxis.fr
- Multi-Agent AI Survey (2026): [Agentic AI Survey - arXiv](https://arxiv.org/html/2510.25445v1)

**Books**:
- *Designing for Scalability with Erlang/OTP* (Francesco Cesarini, Steve Vinoski)
- *Learn You Some Erlang for Great Good!* (Fred Hébert)
- *Erlang and OTP in Action* (Martin Logan, Eric Merritt, Richard Carlsson)

**Community**:
- Erlang Forums: https://erlangforums.com
- Elixir Forum (BEAM ecosystem): https://elixirforum.com
- #erlang on Libera.Chat IRC

---

**Compiled By**: Research Agent
**For**: Quick reference during ggen Erlang/OTP example project development
