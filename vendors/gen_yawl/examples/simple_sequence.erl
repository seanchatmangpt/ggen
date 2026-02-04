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

%% @doc Example: Simple sequential workflow (WP1: Sequence)
%%
%% This example demonstrates a basic three-step workflow:
%%   step_a -> step_b -> step_c
%%
%% @end

-module(simple_sequence).
-behaviour(gen_yawl).

%% Callbacks
-export([init_workflow/1, execute_task/2, handle_work_item_completed/3]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_yawl.hrl").

%%====================================================================
%% gen_yawl Callbacks
%%====================================================================

%% @doc Initialize the workflow specification.
init_workflow(_Args) ->
    Spec = #yawl_spec{
        id = <<"simple-sequence">>,
        name = <<"Simple Sequence Workflow">>,
        version = {1, 0},
        tasks = #{
            step_a => #task_def{
                id = step_a,
                name = <<"Step A">>,
                kind = atomic,
                split_type = sequence,
                join_type = sequence
            },
            step_b => #task_def{
                id = step_b,
                name = <<"Step B">>,
                kind = atomic,
                split_type = sequence,
                join_type = sequence
            },
            step_c => #task_def{
                id = step_c,
                name = <<"Step C">>,
                kind = atomic,
                split_type = sequence,
                join_type = sequence
            }
        },
        flows = [
            #flow{from = input, to = step_a},
            #flow{from = step_a, to = step_b},
            #flow{from = step_b, to = step_c},
            #flow{from = step_c, to = output}
        ],
        input_condition = input,
        output_condition = output
    },
    {ok, Spec}.

%% @doc Execute a task.
execute_task(#work_item{task_id = step_a, data = Data}, State) ->
    io:format("Executing Step A with data: ~p~n", [Data]),
    Result = #{
        step_a_output => <<"Step A completed">>,
        timestamp => os:system_time(millisecond)
    },
    {ok, Result, State};

execute_task(#work_item{task_id = step_b, data = Data}, State) ->
    io:format("Executing Step B with data: ~p~n", [Data]),
    Result = #{
        step_b_output => <<"Step B completed">>,
        timestamp => os:system_time(millisecond)
    },
    {ok, Result, State};

execute_task(#work_item{task_id = step_c, data = Data}, State) ->
    io:format("Executing Step C with data: ~p~n", [Data]),
    Result = #{
        step_c_output => <<"Step C completed">>,
        timestamp => os:system_time(millisecond),
        final_status => <<"Workflow complete">>
    },
    {ok, Result, State}.

%% @doc Handle task completion.
handle_work_item_completed(_WorkItem, _Result, State) ->
    {ok, [], State}.
