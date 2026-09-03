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

%% @doc Example: Parallel approval workflow (WP2+WP3: Parallel Split + Synchronization)
%%
%% This example demonstrates a workflow where multiple reviews happen
%% in parallel, then synchronize before finalization:
%%
%%                   +-> legal_review -+
%% submit_document ->+-> tech_review  +-> finalize
%%                   +-> finance_review -+
%%
%% @end

-module(parallel_approval).
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

%% @doc Initialize the parallel approval workflow specification.
init_workflow(_Args) ->
    Spec = #yawl_spec{
        id = <<"parallel-approval">>,
        name = <<"Parallel Multi-Reviewer Approval">>,
        version = {1, 0},
        tasks = #{
            submit_document => #task_def{
                id = submit_document,
                name = <<"Submit Document">>,
                kind = atomic,
                split_type = and,  %% AND split for parallel execution
                join_type = sequence
            },
            legal_review => #task_def{
                id = legal_review,
                name = <<"Legal Review">>,
                kind = atomic,
                split_type = sequence,
                join_type = and  %% Part of AND-join group
            },
            tech_review => #task_def{
                id = tech_review,
                name = <<"Technical Review">>,
                kind = atomic,
                split_type = sequence,
                join_type = and
            },
            finance_review => #task_def{
                id = finance_review,
                name = <<"Finance Review">>,
                kind = atomic,
                split_type = sequence,
                join_type = and
            },
            finalize => #task_def{
                id = finalize,
                name = <<"Finalize Approval">>,
                kind = atomic,
                split_type = sequence,
                join_type = and  %% AND-join: waits for all reviews
            }
        },
        flows = [
            %% Input to submit
            #flow{from = input, to = submit_document},

            %% AND-split: submit to all reviewers
            #flow{from = submit_document, to = legal_review, split_type = and},
            #flow{from = submit_document, to = tech_review, split_type = and},
            #flow{from = submit_document, to = finance_review, split_type = and},

            %% All reviews go to finalize (AND-join)
            #flow{from = legal_review, to = finalize, join_type = and},
            #flow{from = tech_review, to = finalize, join_type = and},
            #flow{from = finance_review, to = finalize, join_type = and},

            %% Finalize to output
            #flow{from = finalize, to = output}
        ],
        input_condition = input,
        output_condition = output
    },
    {ok, Spec}.

%% @doc Execute a task.
execute_task(#work_item{task_id = submit_document, data = Data}, State) ->
    io:format("Submitting document for parallel review~n"),
    Result = #{
        document_id => maps:get(document_id, Data, <<"doc-unknown">>),
        submitted_at => os:system_time(millisecond),
        reviewers => [legal, tech, finance]
    },
    {ok, Result, State};

execute_task(#work_item{task_id = legal_review, data = Data}, State) ->
    io:format("Legal Review in progress...~n"),
    Result = #{
        reviewer => legal,
        decision => approved,
        comments => <<"All legal requirements met">>,
        reviewed_at => os:system_time(millisecond)
    },
    {ok, Result, State};

execute_task(#work_item{task_id = tech_review, data = Data}, State) ->
    io:format("Technical Review in progress...~n"),
    Result = #{
        reviewer => tech,
        decision => approved,
        comments => <<"Technical implementation validated">>,
        reviewed_at => os:system_time(millisecond)
    },
    {ok, Result, State};

execute_task(#work_item{task_id = finance_review, data = Data}, State) ->
    io:format("Finance Review in progress...~n"),
    Result = #{
        reviewer => finance,
        decision => approved,
        comments => <<"Budget within limits">>,
        reviewed_at => os:system_time(millisecond)
    },
    {ok, Result, State};

execute_task(#work_item{task_id = finalize, data = Data}, State) ->
    io:format("Finalizing approval (all reviews complete)~n"),
    Result = #{
        final_status => approved,
        finalized_at => os:system_time(millisecond),
        total_reviews => 3
    },
    {ok, Result, State}.

%% @doc Handle task completion - return next tasks to enable.
handle_work_item_completed(#work_item{task_id = submit_document}, _Result, State) ->
    %% After submit, all reviewers are enabled in parallel by AND-split
    io:format("Document submitted, enabling all reviewers~n"),
    {ok, [legal_review, tech_review, finance_review], State};

handle_work_item_completed(#work_item{task_id = TaskId}, _Result, State)
        when TaskId =:= legal_review; TaskId =:= tech_review; TaskId =:= finance_review ->
    %% Check if all reviews are complete before finalizing
    io:format("Review ~p complete, checking if all reviews done~n", [TaskId]),
    {ok, [], State};  %% AND-join handles synchronization

handle_work_item_completed(#work_item{task_id = finalize}, _Result, State) ->
    io:format("Approval workflow complete~n"),
    {ok, [], State}.
