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
%% @doc Token lifecycle management for YAWL workflows.
%%
%% This module handles the creation, transformation, and tracking of
%% tokens throughout the workflow execution lifecycle.
%%
%% == Token States ==
%%
%% Tokens flow through the following states:
%%
%%   None -> Enabled -> Started -> Completed
%%             |           |
%%             v           v
%%         Cancelled   Cancelled
%%
%% @end
%% -------------------------------------------------------------------

-module(token_manager).

%%====================================================================
%% Exports
%%====================================================================

%% Token Creation
-export([new_case_token/2,
         new_work_item_token/3,
         new_control_token/3]).

%% Token Transformation
-export([enable_work_item/1,
         start_work_item/2,
         complete_work_item/2,
         cancel_work_item/1,
         timeout_work_item/1]).

%% Token Merging (for join patterns)
-export([merge_join_tokens/2,
         select_split_branches/2]).

%% Token Query
-export([is_complete/1,
         is_active/1,
         get_token_status/1]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_yawl.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type token() :: #case_token{} | #work_item{} | #control_token{}.
-type join_result() :: {complete, #control_token{}} | {pending, #control_token{}}.

%%====================================================================
%% Token Creation Functions
%%====================================================================

%% @doc Create a new case token.
%%
%% A case token represents a workflow instance.
%%
%% @end
-spec new_case_token(SpecId :: binary(), CaseData :: map()) -> #case_token{}.

new_case_token(SpecId, CaseData) ->
    #case_token{
        case_id = generate_id(),
        spec_id = SpecId,
        started_at = os:system_time(nanosecond),
        case_data = CaseData
    }.

%% @doc Create a new work item token.
%%
%% A work item token represents a task instance within a case.
%%
%% @end
-spec new_work_item_token(CaseId :: binary(),
                          TaskId :: atom(),
                          Data :: map()) -> #work_item{}.

new_work_item_token(CaseId, TaskId, Data) ->
    #work_item{
        id = generate_id(),
        case_id = CaseId,
        task_id = TaskId,
        status = enabled,
        data = Data,
        enabled_at = os:system_time(nanosecond)
    }.

%% @doc Create a new control token for split/join patterns.
%%
%% Control tokens manage the synchronization of parallel branches.
%%
%% @end
-spec new_control_token(Type :: atom(),
                        TaskId :: atom(),
                        BranchCount :: non_neg_integer()) -> #control_token{}.

new_control_token(Type, TaskId, BranchCount) ->
    #control_token{
        type = Type,
        task_id = TaskId,
        branch_count = BranchCount,
        completed = []
    }.

%%====================================================================
%% Token Transformation Functions
%%====================================================================

%% @doc Transition a work item token to enabled state.
%%
%% @end
-spec enable_work_item(Token :: #work_item{}) -> #work_item{}.

enable_work_item(Token) ->
    Token#work_item{
        status = enabled,
        enabled_at = os:system_time(nanosecond)
    }.

%% @doc Transition a work item token to started state.
%%
%% @end
-spec start_work_item(Token :: #work_item{},
                      Resource :: atom()) -> #work_item{}.

start_work_item(Token, Resource) ->
    Token#work_item{
        status = started,
        assigned_to = Resource
    }.

%% @doc Transition a work item token to completed state.
%%
%% @end
-spec complete_work_item(Token :: #work_item{},
                         Result :: map()) -> #work_item{}.

complete_work_item(Token, Result) ->
    Token#work_item{
        status = completed,
        data = maps:merge(Token#work_item.data, Result)
    }.

%% @doc Transition a work item token to cancelled state.
%%
%% @end
-spec cancel_work_item(Token :: #work_item{} | #control_token{}) ->
          #work_item{} | #control_token{}.

cancel_work_item(Token = #work_item{}) ->
    Token#work_item{status = cancelled};

cancel_work_item(Token = #control_token{}) ->
    Token#control_token{completed = all}.

%% @doc Mark a work item as timed out.
%%
%% @end
-spec timeout_work_item(Token :: #work_item{}) -> #work_item{}.

timeout_work_item(Token) ->
    Token#work_item{status = cancelled}.

%%====================================================================
%% Token Merging Functions (Join Patterns)
%%====================================================================

%% @doc Merge tokens for AND/OR join synchronization.
%%
%% Returns `{complete, Token}' when all branches have completed,
%% or `{pending, Token}' when still waiting.
%%
%% @end
-spec merge_join_tokens(Existing :: #control_token{} | undefined,
                        Incoming :: #work_item{}) ->
          join_result().

merge_join_tokens(undefined, Incoming) ->
    %% Create new join token
    #control_token{
        type = 'and_join',
        task_id = Incoming#work_item.task_id,
        branch_count = 1,  %% Will be updated by caller
        completed = [Incoming#work_item.task_id]
    };

merge_join_tokens(JoinToken = #control_token{type = Type,
                                             branch_count = BranchCount,
                                             completed = Completed},
                  Incoming) ->
    TaskId = Incoming#work_item.task_id,
    UpdatedCompleted = [TaskId | Completed],
    UpdatedToken = JoinToken#control_token{completed = UpdatedCompleted},

    case length(UpdatedCompleted) >= BranchCount of
        true when Type =:= 'and_join' ->
            {complete, UpdatedToken};
        true when Type =:= 'xor_join' ->
            %% XOR join completes on first arrival
            {complete, UpdatedToken};
        true when Type =:= 'or_join' ->
            %% OR join needs explicit branch count handling
            {complete, UpdatedToken};
        false ->
            {pending, UpdatedToken}
    end.

%% @doc Select branches for split patterns.
%%
%% @end
-spec select_split_branches(SplitType :: atom(),
                            Options :: [{atom(), function()}]) ->
          [atom()].

select_split_branches('and', Targets) ->
    %% AND split: select all targets
    [TaskId || {TaskId, _Fun} <- Targets];

select_split_branches('xor', Targets) ->
    %% XOR split: select first matching target
    case lists:keyfind(true, 2,
        [{Task, case Fun of undefined -> true; _ -> Fun(#{}) end}
         || {Task, Fun} <- Targets]) of
        {TaskId, _Condition} -> [TaskId];
        false -> []
    end;

select_split_branches('or', Targets) ->
    %% OR split: select all matching targets
    [TaskId || {TaskId, Fun} <- Targets,
               case Fun of undefined -> true; _ -> Fun(#{}) end].

%%====================================================================
%% Token Query Functions
%%====================================================================

%% @doc Check if a token is in a completed state.
%%
%% @end
-spec is_complete(token()) -> boolean().

is_complete(#case_token{}) ->
    false;  %% Case tokens don't have a "complete" state
is_complete(#work_item{status = Status}) ->
    Status =:= completed;
is_complete(#control_token{completed = Completed, branch_count = Count}) ->
    length(Completed) >= Count.

%% @doc Check if a token is in an active state.
%%
%% @end
-spec is_active(token()) -> boolean().

is_active(#case_token{}) ->
    true;  %% Case tokens are always active while case exists
is_active(#work_item{status = Status}) ->
    lists:member(Status, [enabled, started]);
is_active(#control_token{completed = all}) ->
    false;
is_active(#control_token{}) ->
    true.

%% @doc Get the status of a token.
%%
%% @end
-spec get_token_status(token()) -> atom().

get_token_status(#case_token{}) ->
    case_active;
get_token_status(#work_item{status = Status}) ->
    Status;
get_token_status(#control_token{completed = all}) ->
    cancelled;
get_token_status(#control_token{completed = Completed, branch_count = Count})
  when length(Completed) >= Count ->
    complete;
get_token_status(#control_token{type = Type}) ->
    Type.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Generate a unique identifier.
-spec generate_id() -> binary().

generate_id() ->
    Timestamp = os:system_time(nanosecond),
    Random = crypto:strong_rand_bytes(8),
    Hash = crypto:hash(md5, <<Timestamp:64, Random/binary>>),
    binary:encode_hex(Hash).
