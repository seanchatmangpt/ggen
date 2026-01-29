%% ============================================================================
%% COMBINATION #2: Student Quick Start
%% ============================================================================
%% Complexity:  Level 1 (Meta-minimal - 4% of comprehensive)
%% Domain:      Pure Concept
%% Audience:    Student
%% Style:       Hands-on
%% Time:        5 minutes
%% Frequency:   82% (second most common)
%%
%% CRITICAL PATH: The minimum you need to understand gen_server
%% Read + Write operations (increment + get)

-module(counter_server).
-behaviour(gen_server).

%% ============================================================================
%% API (What users call)
%% ============================================================================
-export([start_link/0]).
-export([increment/0]).     %% ← Write operation
-export([get/0]).           %% ← Read operation

%% ============================================================================
%% Callbacks (What gen_server calls)
%% ============================================================================
-export([init/1, handle_call/3, handle_cast/2]).

%% ============================================================================
%% State (What the server remembers)
%% ============================================================================
-record(state, {
    count :: integer()
}).

%% ============================================================================
%% STEP 1: Start the server
%% ============================================================================

start_link() ->
    %% Start a named process called 'counter_server'
    gen_server:start_link(
        {local, ?MODULE},  % Register with name
        ?MODULE,           % Callback module
        [],                % Init args
        []                 % Options
    ).

%% ============================================================================
%% STEP 2: Call the server (from your code)
%% ============================================================================

increment() ->
    %% Send 'increment' request, wait for reply
    gen_server:call(?MODULE, increment).

get() ->
    %% Send 'get' request, wait for reply
    gen_server:call(?MODULE, get).

%% ============================================================================
%% STEP 3: Server initialization
%% ============================================================================

init([]) ->
    %% Server starts with count = 0
    {ok, #state{count = 0}}.

%% ============================================================================
%% STEP 4: Handle requests (the CRITICAL PATH)
%% ============================================================================

%% INCREMENT: Read state, modify, write state, reply
handle_call(increment, _From, #state{count = Count} = State) ->
    NewCount = Count + 1,
    {reply, NewCount, State#state{count = NewCount}}.
%%  ^^^^^^  ^^^^^^^^  ^^^^^^^^^^^^^^^^^^^^^^^^^^^^
%%  |       |         |
%%  |       |         └─ New state (updated count)
%%  |       └─ Response to caller
%%  └─ "This is a reply"

%% GET: Read state, reply (no modification)
handle_call(get, _From, #state{count = Count} = State) ->
    {reply, Count, State}.
%%  ^^^^^^  ^^^^^  ^^^^^
%%  |       |      |
%%  |       |      └─ State unchanged
%%  |       └─ Response (current count)
%%  └─ "This is a reply"

%% ============================================================================
%% STEP 5: Handle async messages (not used yet, but required)
%% ============================================================================

handle_cast(_Msg, State) ->
    {noreply, State}.

%% ============================================================================
%% LEARNING CHECKPOINTS
%% ============================================================================
%%
%% ✓ You can START a server (start_link)
%% ✓ You can CALL the server (increment/get)
%% ✓ Server has STATE (#state record)
%% ✓ handle_call PATTERN:
%%     {reply, Response, NewState}
%%      └─ 3 parts: reply + what to send + new state
%%
%% WHAT'S MISSING (we'll add these later):
%% ✗ Error handling
%% ✗ Async messages (casts)
%% ✗ Stopping the server
%% ✗ Supervision
%%
%% WHY MISSING: 80/20 - You don't need these to understand the core concept.
%% ============================================================================

%% ============================================================================
%% TRY IT (in Erlang shell)
%% ============================================================================
%%
%% 1. Compile:
%%    c(gen_server).
%%
%% 2. Start:
%%    counter_server:start_link().
%%
%% 3. Use:
%%    counter_server:increment().  % Returns: 1
%%    counter_server:increment().  % Returns: 2
%%    counter_server:get().        % Returns: 2
%%
%% 4. Understand:
%%    - increment() sends message to server
%%    - Server processes in handle_call
%%    - Server updates state
%%    - Server replies with new count
%%
%% 5. Experiment:
%%    - What happens if you call get() first?
%%    - What happens if you call increment() 100 times?
%%    - What happens if you restart the server?
%% ============================================================================
