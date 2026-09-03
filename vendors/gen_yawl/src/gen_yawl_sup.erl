%% -*- erlang -*-
%%
%% gen_yawl - YAWL Workflow Engine OTP Behavior
%%
%% Copyright 2025 gen_yawl contributors
%%
%% Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%
%% @author gen_yawl contributors
%% @version 1.0.0
%% @copyright 2025
%%
%% @doc Top-level supervisor for gen_yawl workflow cases.
%%
%% This supervisor manages workflow case processes using a
%% simple_one_for_one strategy, allowing dynamic case creation
%% and isolated failure handling.
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_yawl_sup).
-behaviour(supervisor).

%%====================================================================
%% Exports
%%====================================================================

%% API Functions
-export([start_link/0,
         start_link/1,
         start_case/1]).

%% supervisor Callbacks
-export([init/1]).

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start the gen_yawl supervisor with default options.
%%
%% @end
-spec start_link() -> {ok, pid()} | {error, term()}.

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @doc Start the gen_yawl supervisor with options.
%%
%% Options:
%% - `{name, Name}' - Registered name for the supervisor
%% @end
-spec start_link(Options :: proplists:proplist()) -> {ok, pid()} | {error, term()}.

start_link(Options) ->
    Name = proplists:get_value(name, Options, ?MODULE),
    supervisor:start_link(Name, ?MODULE, Options).

%% @doc Start a new workflow case under this supervisor.
%%
%% The ChildSpec should be a tuple of {WorkflowMod, InitArgs}.
%% @end
-spec start_case({WorkflowMod :: atom(), InitArgs :: term()}) ->
          {ok, pid()} | {error, term()}.

start_case({WorkflowMod, InitArgs}) ->
    ChildSpec = #{
        id => WorkflowMod,
        start => {gen_pnet, start_link, [WorkflowMod, InitArgs, []]},
        restart => temporary,
        shutdown => 5000,
        type => worker,
        modules => [WorkflowMod, gen_pnet]
    },
    supervisor:start_child(?MODULE, ChildSpec).

%%====================================================================
%% supervisor Callbacks
%%====================================================================

%% @private
init(_Options) ->
    SupFlags = #{
        strategy => simple_one_for_one,
        intensity => 10,
        period => 60
    },

    ChildSpecs = [],

    {ok, {SupFlags, ChildSpecs}}.
