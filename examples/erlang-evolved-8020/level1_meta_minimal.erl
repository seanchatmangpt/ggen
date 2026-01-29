%% ============================================================================
%% counter_server - ADAPTIVE COMPLEXITY RENDERING
%% ============================================================================
%% Generated at complexity level 1 (0-4)
%% Learning value threshold: 80
%%
%% LEVEL 1: META-MINIMAL (4% of comprehensive)
%% The CRITICAL PATH - read + write operations

-module(counter_server).
-behaviour(gen_server).

%% API (Level 1)
-export([start_link/0]).
-export([increment/0]).
-export([get/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%% State record (fields scale with complexity)
-record(state, {
    count :: integer()
}).

%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:call(?MODULE, increment).

get() ->
    gen_server:call(?MODULE, get).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

init([]) ->
    {ok, #state{
        count = 0
    }}.

%% ============================================================================
%% Level 1: 25% Features
%% ============================================================================

%% META-MINIMAL MODE (4%): Critical path only
handle_call(increment, _From, #state{count = Count} = State) ->
    NewCount = Count + 1,
    {reply, NewCount, State#state{count = NewCount}};

handle_call(get, _From, #state{count = Count} = State) ->
    {reply, Count, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% ============================================================================
%% ZOOM IN: Want more features?
%% Re-generate at level 2 to add:
%%   - reset/0 function (reset to zero)
%%   - add/1 function (add arbitrary amount)
%%   - Metadata tracking (start time)
%% ============================================================================

%% ============================================================================
%% ZOOM OUT: Too complex?
%% Re-generate at level 0 to simplify
%% ============================================================================
