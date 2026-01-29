%% ============================================================================
%% counter_server - ADAPTIVE COMPLEXITY RENDERING
%% ============================================================================
%% Generated at complexity level 0 (0-4)
%% Learning value threshold: 100
%%
%% LEVEL 0: ONELINER (0.8% of comprehensive)
%% The ABSOLUTE ESSENCE - just the core operation

-module(counter_server).
-behaviour(gen_server).

%% API (Level 0)
-export([start_link/0]).
-export([increment/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%% Level 0: Inline state (simplest possible)
-define(INIT_STATE, 0).

%% ============================================================================
%% API
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:call(?MODULE, increment).

%% ============================================================================
%% gen_server callbacks
%% ============================================================================

init([]) ->
    {ok, ?INIT_STATE}.

%% ============================================================================
%% Level 0: 0% Features
%% ============================================================================

%% ONELINER MODE (0.8%): Absolute essence
handle_call(increment, _From, Count) ->
    {reply, Count + 1, Count + 1}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%% ============================================================================
%% ZOOM IN: Want more features?
%% Re-generate at level 1 to add:
%%   - get/0 function (retrieve counter value)
%%   - Proper state record
%% ============================================================================
