# Actor Model and Concurrency

**Core Idea**: Computation through independent actors communicating via asynchronous messages.

## Theoretical Foundation

### The Actor Model (Carl Hewitt, 1973)

**Definition**: An actor is a computational entity that, in response to a message:
1. **Sends** messages to other actors
2. **Creates** new actors
3. **Designates** behavior for next message

**Key properties**:
- **No shared state** - Actors have private memory
- **Asynchronous communication** - Send never blocks
- **Location transparency** - Actors can be local or remote

### Erlang's Implementation

Erlang processes implement actors:

```erlang
% Actor = Erlang Process
Actor = spawn(fun() ->
    receive
        {From, Message} ->
            % Process message
            From ! {self(), "Reply"},
            actor_loop()  % Designate next behavior
    end
end).

% Send message (asynchronous)
Actor ! {self(), "Hello"}.

% Create new actor
NewActor = spawn(fun() -> worker_loop() end).
```

## Comparison with Other Models

### Shared Memory (Threads)

```java
// Java: Shared memory
class Counter {
    private int count = 0;

    synchronized void increment() {
        count++;  // Multiple threads access same memory
    }

    synchronized int get() {
        return count;
    }
}

// Problems:
// - Race conditions
// - Deadlocks
// - Hard to reason about
// - Doesn't scale to distributed systems
```

### Message Passing (Actors)

```erlang
% Erlang: Message passing
-module(counter).

start() ->
    spawn(fun() -> loop(0) end).

loop(Count) ->
    receive
        {From, increment} ->
            From ! {ok, Count + 1},
            loop(Count + 1);
        {From, get} ->
            From ! {ok, Count},
            loop(Count)
    end.

% Benefits:
% - No shared state (no race conditions)
% - No locks (no deadlocks)
% - Clear semantics
% - Works locally and distributed
```

### CSP (Communicating Sequential Processes)

```go
// Go: CSP with channels
ch := make(chan int)

// Send blocks until receiver ready
ch <- 42  // BLOCKS!

// Receive blocks until sender ready
value := <-ch  // BLOCKS!
```

```erlang
% Erlang: Actor model
Pid ! 42.  % NEVER blocks

receive
    Value -> Value  % Blocks only receiver
after 5000 ->
    timeout
end.
```

**Key difference**: CSP synchronizes sender and receiver. Actors decouple them.

## Actor Properties

### 1. Isolation

**Each actor has private state**:

```erlang
% Actor A
State = #{secret => "password123"},
process_message(Msg, State).

% Actor B cannot access State
% Only way to interact: Send message
```

**Benefits**:
- No race conditions
- Clear ownership
- Easy to reason about
- Safe parallelism

### 2. Asynchronous Messages

**Sending never blocks**:

```erlang
% Send to 1 million actors
lists:foreach(
    fun(Actor) -> Actor ! Message end,
    Actors
).
% Returns immediately!
% No waiting for receivers
```

**Message semantics**:
- **At-most-once delivery** - Messages may be lost
- **FIFO order** - Per sender-receiver pair
- **No global ordering** - Messages from different senders can interleave

### 3. Location Transparency

**Same code works locally and distributed**:

```erlang
% Local process
Pid = spawn(fun() -> worker_loop() end),
Pid ! {work, Data}.

% Remote process (different machine!)
{ok, Node} = net_kernel:connect_node('worker@remote'),
Pid = spawn(Node, fun() -> worker_loop() end),
Pid ! {work, Data}.  % Same syntax!
```

**Enables**:
- Transparent distribution
- Load balancing
- Fault tolerance across machines

### 4. Selective Receive

**Process messages in any order**:

```erlang
loop(State) ->
    receive
        {priority, Msg} ->
            % Process priority first
            handle_priority(Msg);
        {normal, Msg} ->
            % Process normal second
            handle_normal(Msg)
    end,
    loop(State).

% Even if normal arrives first,
% priority will be processed first!
```

## Actor Patterns

### Request-Reply

```erlang
% Synchronous semantics using async messages
call(Actor, Request) ->
    Actor ! {self(), Request},
    receive
        {Actor, Reply} -> Reply
    after 5000 ->
        {error, timeout}
    end.

% Actor implementation
loop(State) ->
    receive
        {From, Request} ->
            Reply = process(Request, State),
            From ! {self(), Reply},
            loop(State)
    end.
```

### Publish-Subscribe

```erlang
% Publisher
publish(Topic, Message) ->
    Subscribers = ets:lookup(topics, Topic),
    [Pid ! {Topic, Message} || {_, Pid} <- Subscribers].

% Subscriber
subscribe(Topic) ->
    registry ! {subscribe, Topic, self()}.

% Registry actor
registry_loop(Subscriptions) ->
    receive
        {subscribe, Topic, Pid} ->
            NewSubs = [{Topic, Pid} | Subscriptions],
            registry_loop(NewSubs)
    end.
```

### Pipeline

```erlang
% Stage 1: Parser
parser_loop() ->
    receive
        {raw_data, Data} ->
            Parsed = parse(Data),
            validator ! {parsed, Parsed}
    end,
    parser_loop().

% Stage 2: Validator
validator_loop() ->
    receive
        {parsed, Data} ->
            Validated = validate(Data),
            processor ! {validated, Validated}
    end,
    validator_loop().

% Stage 3: Processor
processor_loop() ->
    receive
        {validated, Data} ->
            Result = process(Data),
            storage ! {result, Result}
    end,
    processor_loop().

% Chain: raw_data → parser → validator → processor → storage
```

### Supervision (Unique to Erlang/OTP)

```erlang
% Supervisor actor monitors worker actors
supervisor_loop(Workers) ->
    receive
        {'DOWN', _Ref, process, Pid, Reason} ->
            % Worker crashed - restart it
            NewPid = spawn_link(fun() -> worker_loop() end),
            NewWorkers = replace_worker(Pid, NewPid, Workers),
            supervisor_loop(NewWorkers)
    end.

% Extension of actor model with failure handling
```

## Scalability

### Lightweight Processes

```erlang
% Spawn 1 million processes
Processes = [spawn(fun() -> actor_loop() end) || _ <- lists:seq(1, 1000000)].

% Memory per process: ~2KB
% Total memory: ~2GB (manageable!)
```

**Comparison**:

| Implementation | Memory per unit | Max per machine |
|----------------|-----------------|-----------------|
| OS Thread | ~1MB stack | ~10,000 |
| Go Goroutine | ~2KB | ~1,000,000 |
| Erlang Process | ~2KB | ~10,000,000 |
| Java Thread | ~1MB | ~10,000 |

### Parallel Execution

```erlang
% Process 1 million items in parallel
Results = pmap(
    fun(Item) ->
        % Each item processed in separate actor
        expensive_computation(Item)
    end,
    Items
).

pmap(F, List) ->
    Parent = self(),
    Pids = [spawn(fun() -> Parent ! {self(), F(X)} end) || X <- List],
    [receive {Pid, Result} -> Result end || Pid <- Pids].
```

### Distribution

```erlang
% Distribute work across cluster
distribute_work(Work) ->
    Nodes = nodes(),
    lists:foreach(
        fun({Task, Node}) ->
            % Spawn actor on specific node
            Pid = spawn(Node, fun() -> process_task(Task) end),
            Pid ! {start, Task}
        end,
        lists:zip(Work, cycle(Nodes))
    ).

% Same actor model, now distributed!
```

## Message Passing Semantics

### Delivery Guarantees

**At-most-once delivery**:
```erlang
Pid ! Message.
% Message may arrive: 0 or 1 times
% NEVER duplicated
```

**Why no guaranteed delivery?**
- In distributed systems, you can't distinguish "slow" from "dead"
- Forcing guarantees would require blocking (defeats async benefits)
- Applications implement retries/acks as needed

### Message Ordering

**FIFO per sender-receiver pair**:

```erlang
% Actor A sends to Actor B
B ! msg1,
B ! msg2,
B ! msg3.

% Actor B receives in order: msg1, msg2, msg3
```

**No global ordering**:

```erlang
% Actor A sends to C
C ! a1,
C ! a2.

% Actor B sends to C
C ! b1,
C ! b2.

% Actor C might receive: a1, b1, a2, b2
% Or: b1, a1, b2, a2
% Or: a1, a2, b1, b2
% Guaranteed: a1 before a2, b1 before b2
```

### Mailbox Semantics

**Unlimited buffering**:

```erlang
% Mailbox is unbounded queue
Actor ! msg1,
Actor ! msg2,
Actor ! msg3,
% All messages queued, send returns immediately
```

**Selective receive**:

```erlang
% Scan mailbox for matching message
receive
    {priority, Msg} -> handle(Msg)
end.

% If mailbox contains: [normal1, normal2, priority1]
% priority1 is processed first
% normal1, normal2 remain in mailbox
```

## Comparison with Real-World Metaphors

### Postal System (Actors)

```
- People (actors) have private homes (isolated state)
- Send letters (messages) via post office
- Letters may be lost (at-most-once)
- Can't force someone to read mail (asynchronous)
- Letters from same sender arrive in order (FIFO)
```

### Telephone System (Not Actors)

```
- Caller blocks until receiver answers (synchronous)
- Both parties occupy line (coupled)
- Can't talk to disconnected party (no buffering)
```

### Office Memos (Not Actors)

```
- Shared bulletin board (shared state)
- Race conditions (two people update at once)
- Locks needed (reserve board before writing)
```

## AGI and Multi-Agent Systems

### Erlang Actors as Blueprint for AI Agents

**Properties needed for AI agents**:
- ✅ Independence - Each agent has own goals/state
- ✅ Asynchronous communication - Don't block on slow agents
- ✅ Fault tolerance - One agent crash doesn't kill system
- ✅ Scalability - Millions of agents on distributed hardware
- ✅ Location transparency - Agents move between machines

**Erlang provides all of these!**

### Example: Distributed AI System

```erlang
% AI Agent actor
agent_loop(KnowledgeBase) ->
    receive
        {query, From, Question} ->
            Answer = reason(Question, KnowledgeBase),
            From ! {answer, Answer},
            agent_loop(KnowledgeBase);

        {learn, Fact} ->
            NewKB = update_knowledge(Fact, KnowledgeBase),
            agent_loop(NewKB);

        {collaborate, OtherAgent, Topic} ->
            % Agent-to-agent communication
            OtherAgent ! {query, self(), Topic},
            receive
                {answer, Answer} ->
                    NewKB = integrate(Answer, KnowledgeBase),
                    agent_loop(NewKB)
            after 5000 ->
                agent_loop(KnowledgeBase)  % Timeout - continue without
            end
    end.

% Spawn millions of AI agents
Agents = [spawn(fun() -> agent_loop(init_kb()) end) || _ <- lists:seq(1, 1000000)].

% Agents communicate via messages
% Each agent evolves independently
% Fault tolerance via supervision
% Scales to distributed cluster
```

## Limitations and Trade-offs

### No Shared State

**Limitation**: Can't directly share large data

```erlang
% Bad: Copying large data in messages
BigData = load_gigabytes(),
[Pid ! {data, BigData} || Pid <- Workers].  % Copies data to each worker!
```

**Solution**: Use ETS (shared memory optimization)

```erlang
% Good: Share via ETS
Table = ets:new(shared_data, [public, {read_concurrency, true}]),
ets:insert(Table, {key, BigData}),
[Pid ! {table, Table} || Pid <- Workers].  % Only pass table reference
```

### Message Copying

**Limitation**: Messages are copied between processes

```erlang
% Each send copies data
Pid ! LargeList.  % List is copied to Pid's heap
```

**Solution**: Use binaries (reference counted, not copied)

```erlang
% Binaries > 64 bytes are reference counted
Binary = term_to_binary(LargeList),
Pid ! Binary.  % Only reference copied, data shared
```

### Mailbox Growth

**Limitation**: Unbounded mailbox can consume memory

```erlang
% Fast sender, slow receiver
[Pid ! Message || Message <- lists:seq(1, 1000000)].
% Pid's mailbox grows to 1M messages!
```

**Solution**: Flow control (see [Optimize Message Passing](../how-to/optimize-message-passing.md))

## Summary

**Actor Model Advantages**:
- ✅ No shared state → No race conditions
- ✅ Asynchronous → High throughput
- ✅ Isolation → Fault tolerance
- ✅ Location transparency → Distribution
- ✅ Simplicity → Easy to reason about

**Erlang's Additions**:
- ✅ Lightweight processes (millions per machine)
- ✅ Supervision (automatic crash recovery)
- ✅ Hot code loading (upgrade without downtime)
- ✅ Distribution primitives (built-in clustering)

**Best suited for**:
- Concurrent systems (web servers, chat)
- Distributed systems (microservices, clusters)
- Fault-tolerant systems (telecom, databases)
- Real-time systems (trading, gaming)
- Multi-agent AI (research frontier!)

## Further Reading

- [Tutorial: Message Passing Basics](../tutorials/02-message-passing-basics.md) - Practical examples
- [How-To: Optimize Message Passing](../how-to/optimize-message-passing.md) - Performance patterns
- [BEAM VM Architecture](beam-vm-architecture.md) - Implementation details

## References

- Carl Hewitt - "Actor Model of Computation" (1973)
- Joe Armstrong - "Making reliable distributed systems in the presence of software errors"
- Gul Agha - "Actors: A Model of Concurrent Computation in Distributed Systems"
- Fred Hebert - "Learn You Some Erlang" (Concurrency chapter)

---

**Key Quote**: "Erlang's processes are to concurrency what functions are to programming." - Robert Virding (Erlang co-creator)
