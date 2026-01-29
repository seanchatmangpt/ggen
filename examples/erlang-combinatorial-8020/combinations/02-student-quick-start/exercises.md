# Hands-On Exercises: gen_server Practice

**Time**: 15-30 minutes | **Difficulty**: Beginner | **Style**: Learn by doing

## Before You Start

Complete the [5-minute tutorial](tutorial.md) first. These exercises build on that foundation.

## Exercise 1: Add a Decrement Function (Easy - 5 minutes)

### Goal
Add a `decrement()` function that decreases the counter.

### What You'll Learn
- How to add new API functions
- How to modify state in different ways
- Pattern matching in handle_call

### Step 1: Add the export
```erlang
-export([decrement/0]).  %% Add this line
```

### Step 2: Add the API function
```erlang
decrement() ->
    gen_server:call(?MODULE, decrement).
```

### Step 3: Add the handler
```erlang
handle_call(decrement, _From, #state{count = Count} = State) ->
    NewCount = Count - 1,
    {reply, NewCount, State#state{count = NewCount}}.
```

### Test It
```erlang
1> c(counter_server).
2> counter_server:start_link().
3> counter_server:increment().  % 1
4> counter_server:increment().  % 2
5> counter_server:decrement().  % 1
6> counter_server:decrement().  % 0
7> counter_server:decrement().  % -1 (it goes negative!)
```

### Challenge
Can you prevent the counter from going below zero?

<details>
<summary>Hint (click to reveal)</summary>

```erlang
handle_call(decrement, _From, #state{count = Count} = State) when Count > 0 ->
    NewCount = Count - 1,
    {reply, NewCount, State#state{count = NewCount}};

handle_call(decrement, _From, #state{count = Count} = State) ->
    {reply, {error, already_zero}, State}.
```

</details>

---

## Exercise 2: Add a Reset Function (Medium - 10 minutes)

### Goal
Add a `reset()` function that sets the counter back to 0.

### What You'll Learn
- Difference between `call` and `cast`
- When to use synchronous vs asynchronous messages
- handle_cast pattern

### Part A: Using `call` (synchronous)

```erlang
%% Export
-export([reset/0]).

%% API
reset() ->
    gen_server:call(?MODULE, reset).

%% Handler
handle_call(reset, _From, State) ->
    {reply, ok, State#state{count = 0}}.
```

### Test It
```erlang
1> counter_server:increment().  % 1
2> counter_server:increment().  % 2
3> counter_server:reset().      % ok
4> counter_server:get().        % 0
```

### Part B: Using `cast` (asynchronous)

```erlang
%% API
reset() ->
    gen_server:cast(?MODULE, reset).  %% cast, not call!

%% Handler (note: handle_cast, not handle_call!)
handle_cast(reset, State) ->
    {noreply, State#state{count = 0}}.  %% noreply, not reply!
```

### Test It
```erlang
1> counter_server:reset().      % ok (immediate)
2> counter_server:get().        % 0 (reset happened)
```

### Discussion Questions
1. What's the difference between `call` and `cast`?
2. When should you use `call` vs `cast`?
3. Why does `handle_cast` return `{noreply, State}` instead of `{reply, Response, State}`?

<details>
<summary>Answers (click to reveal)</summary>

1. **call** waits for reply (synchronous), **cast** doesn't wait (asynchronous)
2. Use **call** when you need confirmation/response, **cast** for fire-and-forget
3. `cast` is "fire and forget" - no response expected, so nothing to reply with

</details>

---

## Exercise 3: Add an `add(N)` Function (Medium - 10 minutes)

### Goal
Add a function that adds an arbitrary amount to the counter.

### What You'll Learn
- Passing arguments to gen_server calls
- Tuple patterns in handle_call

### Implementation

```erlang
%% Export
-export([add/1]).

%% API
add(Amount) ->
    gen_server:call(?MODULE, {add, Amount}).  %% Note the tuple!

%% Handler
handle_call({add, Amount}, _From, #state{count = Count} = State) ->
    NewCount = Count + Amount,
    {reply, NewCount, State#state{count = NewCount}}.
```

### Test It
```erlang
1> counter_server:add(5).       % 5
2> counter_server:add(10).      % 15
3> counter_server:add(-3).      % 12
4> counter_server:increment().  % 13
```

### Challenge
Add input validation: only accept positive integers.

<details>
<summary>Hint (click to reveal)</summary>

```erlang
handle_call({add, Amount}, _From, State) when is_integer(Amount), Amount > 0 ->
    %% ... valid case ...

handle_call({add, _Amount}, _From, State) ->
    {reply, {error, invalid_amount}, State}.
```

</details>

---

## Exercise 4: Track Min and Max (Hard - 15 minutes)

### Goal
Extend the state to track the minimum and maximum values ever seen.

### What You'll Learn
- Expanding state records
- State initialization
- Updating multiple fields

### Step 1: Update the state record
```erlang
-record(state, {
    count :: integer(),
    min   :: integer(),  %% NEW
    max   :: integer()   %% NEW
}).
```

### Step 2: Update initialization
```erlang
init([]) ->
    {ok, #state{
        count = 0,
        min = 0,
        max = 0
    }}.
```

### Step 3: Update increment handler
```erlang
handle_call(increment, _From, #state{count = Count, min = Min, max = Max} = State) ->
    NewCount = Count + 1,
    NewMax = max(NewCount, Max),
    NewMin = min(NewCount, Min),
    {reply, NewCount, State#state{
        count = NewCount,
        min = NewMin,
        max = NewMax
    }}.
```

### Step 4: Add getters
```erlang
%% Exports
-export([get_stats/0]).

%% API
get_stats() ->
    gen_server:call(?MODULE, get_stats).

%% Handler
handle_call(get_stats, _From, #state{count = C, min = Min, max = Max} = State) ->
    {reply, #{count => C, min => Min, max => Max}, State}.
```

### Test It
```erlang
1> counter_server:increment().       % 1
2> counter_server:increment().       % 2
3> counter_server:decrement().       % 1
4> counter_server:decrement().       % 0
5> counter_server:decrement().       % -1
6> counter_server:add(100).          % 99
7> counter_server:get_stats().
#{count => 99, min => -1, max => 99}
```

---

## Exercise 5: Make It Crash-Resistant (Advanced - 20 minutes)

### Goal
Add a supervisor to automatically restart the server if it crashes.

### What You'll Learn
- Supervision trees
- Fault tolerance
- Let it crash philosophy

### Step 1: Create a supervisor module

```erlang
-module(counter_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
    ChildSpecs = [
        #{
            id => counter_server,
            start => {counter_server, start_link, []},
            restart => permanent,
            shutdown => 5000,
            type => worker,
            modules => [counter_server]
        }
    ],
    {ok, {#{strategy => one_for_one}, ChildSpecs}}.
```

### Step 2: Test it

```erlang
% Start the supervisor (which starts the counter)
1> counter_sup:start_link().
{ok,<0.80.0>}

% Use the counter
2> counter_server:increment().
1
3> counter_server:increment().
2

% Kill the counter (simulate a crash)
4> exit(whereis(counter_server), kill).
true

% Wait a moment, then check if it restarted
5> timer:sleep(100).
ok

6> counter_server:get().
0  % It restarted! (but state was lost)
```

### Discussion
- Why did the state reset to 0?
- How could you preserve state across restarts? (Hint: persistence)
- What does `one_for_one` strategy mean?

---

## Exercise 6: Add Persistence (Expert - 30+ minutes)

### Goal
Save state to disk so it survives restarts.

### What You'll Learn
- File I/O in Erlang
- State persistence patterns
- Trade-offs (speed vs durability)

### Hint
You'll need to:
1. Save state to a file in `handle_call` (after each change)
2. Load state from file in `init` (if file exists)
3. Use `:file.write_file` and `:file.read_file`

<details>
<summary>Partial Solution (click to reveal)</summary>

```erlang
%% In init/1
init([]) ->
    InitialState = case file:read_file("counter_state.dat") of
        {ok, Binary} ->
            binary_to_term(Binary);
        {error, enoent} ->
            #state{count = 0}
    end,
    {ok, InitialState}.

%% Helper function
save_state(State) ->
    file:write_file("counter_state.dat", term_to_binary(State)).

%% In handle_call/3 (for increment)
handle_call(increment, _From, #state{count = Count} = State) ->
    NewCount = Count + 1,
    NewState = State#state{count = NewCount},
    save_state(NewState),  %% Save after each change
    {reply, NewCount, NewState}.
```

</details>

---

## Answers and Solutions

Full solutions for all exercises: [solutions/](solutions/)

## Next Steps

After completing these exercises, you should feel comfortable:
- ✅ Adding new API functions
- ✅ Handling different message types
- ✅ Modifying state
- ✅ Using both `call` and `cast`
- ✅ Expanding state records
- ✅ Basic supervision

**What's Next?**

- **I'm ready for production**: See [Developer Prototype](../03-developer-prototype/) (30 min)
- **I want deep understanding**: See [Architect Review](../04-architect-review/) (2 hours)
- **I want ALL features**: See [Comprehensive](../05-comprehensive/) (unlimited)

## Self-Assessment

Rate yourself (1-5) on:
- [ ] I can create a new gen_server from scratch
- [ ] I understand the difference between call and cast
- [ ] I can add state fields and use them
- [ ] I know when to use guards in handle_call
- [ ] I understand supervision basics

**Score 12+**: Move to Developer Prototype
**Score 8-11**: Practice more with these exercises
**Score <8**: Review the tutorial again

---

**Time invested**: _____ minutes
**Exercises completed**: _____ / 6
**Ready for next level**: Yes / No
