# 5-Minute Hands-On: gen_server Quickstart

**Time**: 5 minutes | **Style**: Hands-on | **Level**: Beginner

## What You'll Build

A counter server that:
- Starts up
- Increments a count
- Returns the current count

**Why This Matters**: This pattern scales to WhatsApp (2B users), T-Mobile, Goldman Sachs.

## Setup (30 seconds)

```bash
# 1. Clone the file
cp gen_server.erl ~/counter_server.erl
cd ~

# 2. Start Erlang shell
erl
```

## Part 1: Compile and Start (1 minute)

```erlang
% Compile the code
1> c(counter_server).
{ok,counter_server}

% Start the server
2> counter_server:start_link().
{ok,<0.80.0>}
```

**What just happened?**
- `c(counter_server)` → Compiled the module
- `start_link()` → Started a process
- `{ok,<0.80.0>}` → Process ID (your server is running!)

## Part 2: Use the Server (2 minutes)

```erlang
% Increment the counter
3> counter_server:increment().
1

% Increment again
4> counter_server:increment().
2

% Increment a third time
5> counter_server:increment().
3

% Check the current count (without incrementing)
6> counter_server:get().
3
```

**What just happened?**
- Each `increment()` call sent a message to the server
- Server processed the message
- Server updated its internal count
- Server replied with the new count

## Part 3: Understand the Pattern (2 minutes)

Let's trace what happens when you call `increment()`:

```
YOU                    counter_server
 |                           |
 |--- increment() ---------->|
 |                           |  1. Receive 'increment' message
 |                           |  2. Current state: {count = 2}
 |                           |  3. New count = 2 + 1 = 3
 |                           |  4. Update state: {count = 3}
 |                           |  5. Reply: 3
 |<-------- 3 ---------------|
 |
```

**The Magic**: Server's state persists between calls!

```erlang
% First call: count starts at 0, returns 1
counter_server:increment().  % 1

% Second call: count is now 1, returns 2
counter_server:increment().  % 2

% The server REMEMBERS the count!
```

## Part 4: Experiment (Optional - as much time as you want)

Try these experiments:

### Experiment 1: What happens if you restart?
```erlang
% Get current count
7> counter_server:get().
3

% Kill the server (we'll learn proper shutdown later)
8> exit(whereis(counter_server), kill).
true

% Start a new server
9> counter_server:start_link().
{ok,<0.95.0>}

% Check the count
10> counter_server:get().
0  % ← Back to zero! State was in memory.
```

**Lesson**: State lives in the process. Kill the process, lose the state.

### Experiment 2: Rapid fire
```erlang
% Increment 100 times
11> [counter_server:increment() || _ <- lists:seq(1, 100)].
[1,2,3,4,5,6,7,8,9,10,11,...,100]

% Verify
12> counter_server:get().
100
```

**Lesson**: Servers can handle rapid messages easily.

### Experiment 3: Concurrent calls (advanced)
```erlang
% Spawn 10 processes, each increments 10 times
13> [spawn(fun() -> [counter_server:increment() || _ <- lists:seq(1,10)] end) || _ <- lists:seq(1,10)].

% Check final count
14> counter_server:get().
100  % Always 100, never 99 or 101!
```

**Lesson**: gen_server serializes messages (no race conditions!).

## The Core Concept

You've learned the **fundamental pattern**:

```erlang
handle_call(Request, _From, State) ->
    NewState = update(State, Request),
    Response = calculate_response(NewState),
    {reply, Response, NewState}.
```

This pattern is:
1. **Simple**: 3 parts (reply, response, new state)
2. **Powerful**: Scales to billions of users
3. **Safe**: No race conditions (messages are serialized)

## What's Next?

You now understand 80% of gen_server with 20% of the features.

**Next steps** (pick your path):

- **I want more practice**: See `exercises.md`
- **I want production code**: See Combination #3 (Developer Prototype - 30 min)
- **I want to understand deeply**: See Combination #4 (Architect Review - 2 hours)
- **I want ALL the features**: See Combination #5 (Comprehensive - unlimited time)

## Common Questions

**Q**: What if two people call `increment()` at the exact same time?
**A**: gen_server processes messages one at a time (serialized). No race conditions!

**Q**: What if the server crashes?
**A**: That's what supervisors are for (we'll cover this in later combinations).

**Q**: Can I have multiple servers?
**A**: Yes! Each server is an independent process. You can have millions.

**Q**: How fast is this?
**A**: Millions of messages per second on modern hardware.

**Q**: Is this production-ready?
**A**: This code is a learning example. For production, add error handling, supervision, and monitoring (see Combination #3).

## Checklist

After this tutorial, you should be able to:

- ✅ Compile and start a gen_server
- ✅ Call functions that send messages to the server
- ✅ Understand the request → process → reply pattern
- ✅ Explain why state persists between calls
- ✅ Know that messages are processed serially (no race conditions)

## Time Check

**Expected**: 5 minutes
**Actual**: _____ minutes

If you took longer, that's fine! The 5-minute target is for experienced programmers. Beginners often take 10-15 minutes, which is still excellent progress.

## Troubleshooting

**Error: `{undef,[{counter_server,start_link,[]`**
- Solution: Did you compile? Run `c(counter_server).` first.

**Error: `{noproc,...}`**
- Solution: Server isn't running. Call `counter_server:start_link()` first.

**Error: `{already_started,<0.80.0>}`**
- Solution: Server is already running. This is fine! Just use it.

---

**Completed in**: _____ minutes
**Next**: [Hands-on Exercises](exercises.md) or [Developer Prototype](../03-developer-prototype/)
