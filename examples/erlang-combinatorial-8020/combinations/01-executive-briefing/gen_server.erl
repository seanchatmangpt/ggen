%% ============================================================================
%% COMBINATION #1: Executive Briefing
%% ============================================================================
%% Complexity:  Level 0 (Oneliner - 0.8% of comprehensive)
%% Domain:      Pure Concept
%% Audience:    Executive
%% Style:       Visual
%% Time:        1 minute
%% Frequency:   85% (most common use case)
%%
%% ABSOLUTE ESSENCE: The simplest possible gen_server
%% Everything else is optimization of this core concept.

-module(counter_server).
-behaviour(gen_server).

%% API
-export([start_link/0, increment/0]).

%% Callbacks
-export([init/1, handle_call/3, handle_cast/2]).

%% ============================================================================
%% THE CORE CONCEPT (3 operations)
%% ============================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

increment() ->
    gen_server:call(?MODULE, increment).

%% ============================================================================
%% THE IMPLEMENTATION (1 line that matters)
%% ============================================================================

init([]) ->
    {ok, 0}.

handle_call(increment, _From, Count) ->
    {reply, Count + 1, Count + 1}.  %% ← THIS IS THE ENTIRE PATTERN

handle_cast(_Msg, State) ->
    {noreply, State}.

%% ============================================================================
%% EXECUTIVE INSIGHT
%% ============================================================================
%%
%% WHAT: Server that increments a counter
%% HOW:  Message passing + state management
%% WHY:  Foundation for all distributed systems
%%
%% BUSINESS VALUE:
%% - 99.999% uptime (5 nines)
%% - Linear scalability
%% - Zero downtime upgrades
%% - Automatic fault recovery
%%
%% REAL-WORLD USAGE:
%% - WhatsApp: 2 billion users on Erlang
%% - T-Mobile: Call routing
%% - Goldman Sachs: Trading systems
%% - Cisco: Network switches
%%
%% ROI: 10x reduction in downtime cost
%% ============================================================================
