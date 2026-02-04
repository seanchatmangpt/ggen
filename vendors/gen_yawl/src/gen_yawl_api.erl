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
%% @doc Public API for gen_yawl workflow engine.
%%
%% This module provides a high-level API for starting workflows,
%% querying state, and completing work items.
%%
%% @end
%% -------------------------------------------------------------------

-module(gen_yawl_api).

%%====================================================================
%% Exports
%%====================================================================

%% Workflow Lifecycle
-export([start_workflow/2,
         start_workflow/3,
         stop_workflow/1,
         workflow_status/1]).

%% Work Item Management
-export([get_work_items/1,
         complete_work_item/3,
         get_work_item/2]).

%% Case Queries
-export([get_case_id/1,
         get_case_state/1]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_yawl.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Start a new workflow instance.
%%
%% @end
-spec start_workflow(Spec :: #yawl_spec{}, CaseData :: map()) ->
          {ok, pid(), binary()} | {error, term()}.

start_workflow(Spec, CaseData) ->
    start_workflow(Spec, CaseData, #{}).

%% @doc Start a new workflow instance with options.
%%
%% @end
-spec start_workflow(Spec :: #yawl_spec{},
                     CaseData :: map(),
                     Options :: map()) ->
          {ok, pid(), binary()} | {error, term()}.

start_workflow(Spec, CaseData, Options) ->
    Args = #{
        spec => Spec,
        case_data => CaseData
    },

    case gen_pnet:start_link(gen_yawl, Args, maps:get(opts, Options, [])) of
        {ok, Pid} ->
            case gen_yawl_api:get_case_id(Pid) of
                {ok, CaseId} ->
                    {ok, Pid, CaseId};
                {error, Reason} ->
                    {error, Reason}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Stop a workflow instance.
%%
%% @end
-spec stop_workflow(WorkflowPid :: pid()) -> ok.

stop_workflow(WorkflowPid) ->
    gen_pnet:stop(WorkflowPid).

%% @doc Get the status of a workflow instance.
%%
%% @end
-spec workflow_status(WorkflowPid :: pid()) ->
          {ok, map()} | {error, term()}.

workflow_status(WorkflowPid) ->
    case gen_yawl_api:get_case_state(WorkflowPid) of
        {ok, State} ->
            {ok, #{
                case_id => maps:get(case_id, State),
                work_items => maps:get(work_items, State),
                completed_tasks => maps:get(completed_tasks, State),
                receipts => maps:get(receipts, State)
            }};
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get all work items for a workflow instance.
%%
%% @end
-spec get_work_items(WorkflowPid :: pid()) ->
          {ok, map()} | {error, term()}.

get_work_items(WorkflowPid) ->
    gen_pnet:call(WorkflowPid, get_work_items).

%% @doc Complete a work item with results.
%%
%% @end
-spec complete_work_item(WorkflowPid :: pid(),
                         WorkItemId :: binary(),
                         Result :: map()) ->
          ok | {error, term()}.

complete_work_item(WorkflowPid, WorkItemId, Result) ->
    gen_pnet:call(WorkflowPid, {complete_work_item, WorkItemId, Result}).

%% @doc Get a specific work item.
%%
%% @end
-spec get_work_item(WorkflowPid :: pid(),
                    WorkItemId :: binary()) ->
          {ok, #work_item{}} | {error, term()}.

get_work_item(WorkflowPid, WorkItemId) ->
    case gen_yawl_api:get_work_items(WorkflowPid) of
        {ok, WorkItems} ->
            case maps:get(WorkItemId, WorkItems, undefined) of
                undefined -> {error, not_found};
                WorkItem -> {ok, WorkItem}
            end;
        {error, Reason} ->
            {error, Reason}
    end.

%% @doc Get the case ID for a workflow instance.
%%
%% @end
-spec get_case_id(WorkflowPid :: pid()) ->
          {ok, binary()} | {error, term()}.

get_case_id(WorkflowPid) ->
    gen_pnet:call(WorkflowPid, get_case_id).

%% @doc Get the full case state for a workflow instance.
%%
%% @end
-spec get_case_state(WorkflowPid :: pid()) ->
          {ok, map()} | {error, term()}.

get_case_state(WorkflowPid) ->
    gen_pnet:call(WorkflowPid, get_state).
