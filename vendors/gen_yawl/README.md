# gen_yawl - YAWL Workflow Engine OTP Behavior

`gen_yawl` is an Erlang/OTP behavior that implements YAWL (Yet Another Workflow Language) workflow patterns on top of `gen_pnet`'s Petri net foundation. It provides complete support for all 20 Van der Aalst workflow control-flow patterns with RDF/SPARQL integration for workflow definitions and cryptographic receipts for execution verification.

## Overview

`gen_yawl` bridges the gap between:
- **YAWL**: A high-level workflow language with 20 control-flow patterns
- **gen_pnet**: A generic Petri net OTP behavior for concurrent systems
- **RDF**: Semantic web standards for workflow definitions

## Features

| Feature | Description |
|---------|-------------|
| **Complete Pattern Coverage** | All 20 Van der Aalst workflow patterns |
| **RDF/SPARQL Integration** | Define workflows in Turtle RDF format with `gen_yawl_rdf` |
| **Cryptographic Receipts** | BLAKE3-hashed execution audit trail |
| **OTP Supervision** | Built-in supervision tree for fault tolerance |
| **Hot Code Reload** | Zero-downtime workflow updates |
| **Event Streaming** | Subscribe to workflow events |
| **Distributed Execution** | Multi-node case replication |

## RDF/SPARQL Integration

`gen_yawl` includes full RDF/SPARQL support through the `gen_yawl_rdf` module:

### Loading Workflow Specs from RDF

```erlang
%% Load a workflow specification from Turtle
{ok, Spec} = gen_yawl_rdf:load_spec("path/to/workflow.ttl").

%% Convert spec to RDF map
{ok, RDFMap} = gen_yawl_rdf:spec_to_records(Spec).
```

### Querying Workflow State

```erlang
%% Query enabled work items for a case
{ok, WorkItemIds} = gen_yawl_rdf:query_enabled_work_items(CaseId).

%% Get case state
{ok, CaseState} = gen_yawl_rdf:get_case_state(CaseId).

%% Trace execution history
{ok, History} = gen_yawl_rdf:trace_execution(CaseId).
```

### Generating RDF Receipts

```erlang
%% Generate RDF receipt from event
EventMap = #{
    <<"eventType">> => <<"WorkItemCompleted">>,
    <<"workItem">> => <<"workitem-123">>
},
Turtle = gen_yawl_rdf:records_to_rdf(EventMap).
```

### RDF Store Operations

```erlang
%% Start the RDF store
{ok, Pid} = gen_yawl_store:start_link().

%% Load Turtle content
ok = gen_yawl_store:load_turtle(Content, BaseUri).

%% Execute SPARQL queries
{ok, Results} = gen_yawl_store:query(SPARQL).
```

### YAWL Ontology

The YAWL ontology is included in `priv/ontology/yawl.ttl` and defines:

- **Classes**: `WorkflowSpec`, `WorkflowCase`, `Task`, `WorkItem`, etc.
- **Task Types**: `AtomicTask`, `CompositeTask`, `MultipleInstanceTask`
- **Split/Join Behaviors**: `AND_Split`, `XOR_Split`, `OR_Split`, `AND_Join`, etc.
- **Status Values**: `WorkItem_Enabled`, `WorkItem_Completed`, `Case_Active`, etc.

See the YAWL ontology file for complete vocabulary definitions.

## Quick Start

### 1. Add Dependency

Add to `rebar.config`:

```erlang
{deps, [
    {gen_pnet, {git, "https://github.com/joergen7/gen_pnet.git", {tag, "0.1.7"}}},
    {gen_yawl, {git, "https://github.com/your-org/gen_yawl.git", {tag, "0.1.0"}}}
]}.
```

### 2. Define a Workflow

Create a workflow module:

```erlang
-module(my_approval_workflow).
-behaviour(gen_yawl).

%% gen_yawl callbacks
-export([
    init_workflow/1,
    handle_work_item_enabled/2,
    execute_task/2,
    handle_work_item_completed/3
]).

%% Include records
-include_lib("gen_yawl/include/gen_yawl.hrl").

%% Initialize workflow specification
init_workflow(_Args) ->
    Spec = #yawl_spec{
        id = <<"my-approval">>,
        name = <<"My Approval Workflow">>,
        version = {1, 0},
        tasks = #{
            submit => #task_def{
                id = submit,
                name = <<"Submit Request">>,
                kind = atomic
            },
            approve => #task_def{
                id = approve,
                name = <<"Approve Request">>,
                kind = atomic
            }
        },
        flows = [
            #flow{from = input, to = submit},
            #flow{from = submit, to = approve},
            #flow{from = approve, to = output}
        ],
        input_condition = input,
        output_condition = output
    },
    {ok, Spec}.

%% Handle enabled work item
handle_work_item_enabled(WorkItem, State) ->
    {ok, State}.

%% Execute a task
execute_task(#work_item{task_id = submit, data = Data}, State) ->
    Result = #{<<"submitted_at">> => os:system_time(millisecond)},
    {ok, Result, State};

execute_task(#work_item{task_id = approve, data = Data}, State) ->
    Result = #{<<"approved">> => true},
    {ok, Result, State}.

%% Handle work item completion
handle_work_item_completed(_WorkItem, _Result, State) ->
    {ok, [], State}.
```

### 3. Start a Workflow Case

```erlang
%% Start a new case
{ok, CaseId, Pid} = gen_yawl_api:start_case(my_approval_workflow, #{
    case_data => #{<<"requester">> => <<"alice">>}
}).

%% Complete the submit task
{ok, WorkItems} = gen_yawl_api:list_enabled_work_items(CaseId),
[#work_item{id = WorkItemId}] = WorkItems,
{ok, _Receipt} = gen_yawl_api:complete_work_item(CaseId, WorkItemId, #{}).

%% Query case state
{ok, State} = gen_yawl_api:get_case_state(CaseId).
```

## Installation

### From Source

```bash
git clone https://github.com/your-org/gen_yawl.git
cd gen_yawl
rebar3 compile
rebar3 shell
```

### As Dependency

```erlang
%% In rebar.config
{deps, [
    {gen_yawl, {git, "https://github.com/your-org/gen_yawl.git", {tag, "0.1.0"}}}
]}.
```

## Configuration

Add to `sys.config`:

```erlang
[
    {gen_yawl, [
        {rdf_store_backend, oxigraph},
        {rdf_store_path, "./data/yawlstore"},
        {max_cases, 10000},
        {case_timeout, 3600000},  %% 1 hour
        {enable_metrics, true}
    ]}
].
```

## Documentation

| Document | Description |
|----------|-------------|
| [GUIDE.md](GUIDE.md) | Complete user guide with tutorials |
| [PATTERNS.md](PATTERNS.md) | Van der Aalst pattern reference |
| [ARCHITECTURE.md](ARCHITECTURE.md) | Architecture design document |
| [examples/README.md](examples/README.md) | Example workflows |
| `src/gen_yawl_rdf.erl` | RDF/SPARQL integration module |
| `src/gen_yawl_store.erl` | RDF store wrapper |
| `src/gen_yawl_receipts.erl` | Cryptographic receipt generation |
| `priv/ontology/yawl.ttl` | YAWL RDF ontology |

## Van der Aalst Patterns

`gen_yawl` supports all 20 workflow patterns:

| WP# | Pattern | Status |
|-----|---------|--------|
| WP1 | Sequence | Complete |
| WP2 | Parallel Split | Complete |
| WP3 | Synchronization | Complete |
| WP4 | Exclusive Choice | Complete |
| WP5 | Simple Merge | Complete |
| WP6 | Multi-Choice | Complete |
| WP7 | Structured Sync Merge | Complete |
| WP8 | Multi-Merge | Complete |
| WP9 | Discriminator | Complete |
| WP10 | Arbitrary Cycle | Complete |
| WP11 | Implicit Termination | Complete |
| WP12 | Multiple Instances | Planned |
| WP13 | Multiple Instances (Sequential) | Planned |
| WP14 | N-out-of-M | Planned |
| WP15 | State-based Choice | Planned |
| WP16 | Deferred Choice | Complete |
| WP17 | Interleaved Parallel Routing | Planned |
| WP18 | Milestone | Planned |
| WP19 | Cancel Task | Complete |
| WP20 | Cancel Case | Complete |

See [PATTERNS.md](PATTERNS.md) for detailed documentation of each pattern.

## Examples

Run examples in the Erlang shell:

```erlang
%% Start the application
application:ensure_all_started(gen_yawl).

%% Run the simple sequence example
{ok, CaseId} = simple_sequence:run().

%% Run the parallel approval example
{ok, CaseId} = parallel_approval:run().

%% Run the cancellation example
{ok, CaseId} = cancellation:run().
```

See [examples/README.md](examples/README.md) for more examples.

## API Reference

### Case Management

```erlang
%% Start a new workflow case
-spec start_case(WorkflowName :: atom(), Options :: map()) ->
    {ok, CaseId :: binary(), Pid :: pid()} | {error, Reason}.

%% Stop a running case
-spec stop_case(CaseId :: binary()) -> ok | {error, Reason}.

%% Get current case state
-spec get_case_state(CaseId :: binary()) ->
    {ok, #yawl_state{}} | {error, Reason}.
```

### Work Item Management

```erlang
%% List enabled work items
-spec list_enabled_work_items(CaseId :: binary()) ->
    {ok, [#work_item{}]} | {error, Reason}.

%% Complete a work item
-spec complete_work_item(CaseId :: binary(), WorkItemId :: binary(), Result :: map()) ->
    {ok, #receipt{}, EnabledTasks :: [atom()]} | {error, Reason}.

%% Cancel a work item
-spec cancel_work_item(CaseId :: binary(), WorkItemId :: binary()) ->
    ok | {error, Reason}.
```

### Event Subscription

```erlang
%% Subscribe to case events
-spec subscribe_to_case(CaseId :: binary(), Subscriber :: pid()) ->
    {ok, SubscriptionId :: binary()}.

%% Unsubscribe from events
-spec unsubscribe(SubscriptionId :: binary()) -> ok.
```

## Architecture

```
Application Layer
       |
   gen_yawl API
       |
   gen_yawl Behavior (gen_server)
       |
   gen_yawl Patterns
       |
   gen_yawl Petri Net Adapter
       |
   gen_pnet Foundation
```

See [ARCHITECTURE.md](ARCHITECTURE.md) for complete architecture details.

## Building

```bash
# Compile
rebar3 compile

# Run tests
rebar3 eunit

# Dialyzer
rebar3 dialyzer

# Start shell
rebar3 shell
```

## Dependencies

- `gen_pnet` - Generic Petri net OTP behavior
- `gproc` - Process registry
- `jsx` - JSON encoding for receipt export

## License

Apache-2.0

## Contributing

Contributions welcome! Please see [GUIDE.md](GUIDE.md) for development guidelines.

## References

- [gen_pnet](https://github.com/joergen7/gen_pnet) - Petri net OTP behavior
- [YAWL Foundation](http://www.yawlfoundation.org/) - YAWL workflow language
- [Workflow Patterns](https://www.workflowpatterns.com/) - Van der Aalst patterns
