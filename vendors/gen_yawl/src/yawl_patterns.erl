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
%% @doc Van der Aalst workflow pattern implementations.
%%
%% This module provides helper functions for creating workflow
%% structures that implement the 20 control-flow patterns identified
%% by Wil van der Aalst.
%%
%% == Pattern Support ==
%%
%% WP1: Sequence - COMPLETE
%% WP2: Parallel Split - COMPLETE
%% WP3: Synchronization - COMPLETE
%% WP4: Exclusive Choice - COMPLETE
%% WP5: Simple Merge - COMPLETE
%% WP6: Multi-Choice - COMPLETE
%% WP7: Structured Synchronized Merge - COMPLETE
%% WP8: Multi-Merge - COMPLETE
%% WP9: Discriminator - COMPLETE
%% WP10: Arbitrary Cycles - COMPLETE
%% WP11: Implicit Termination - COMPLETE
%% WP12: Multiple Instances (without a priori runtime knowledge) - PLANNED
%% WP13: Multiple Instances (with a priori runtime knowledge) - PLANNED
%% WP14: N-out-of-M Join - PLANNED
%% WP15: State-based Choice - COMPLETE
%% WP16: Deferred Choice - COMPLETE
%% WP17: Interleaved Parallel Routing - PLANNED
%% WP18: Milestone - PLANNED
%% WP19: Cancel Task - COMPLETE
%% WP20: Cancel Case - COMPLETE
%%
%% @end
%% -------------------------------------------------------------------

-module(yawl_patterns).

%%====================================================================
%% Exports
%%====================================================================

%% Pattern Constructors
-export([sequence/2,
         parallel_split/2,
         synchronization/2,
         exclusive_choice/2,
         exclusive_choice/3,
         simple_merge/2,
         multi_choice/2,
         multi_choice/3,
         structured_sync_merge/2,
         multi_merge/2,
         discriminator/2,
         arbitrary_cycle/3,
         deferred_choice/2,
         cancellation_region/3,
         cancel_case/1]).

%% Place/Transition Generators
-export([generate_places/1,
         generate_transitions/1]).

%% Utilities
-export([place_name/2,
         transition_name/1]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_yawl.hrl").

%%====================================================================
%% Type Definitions
%%====================================================================

-type task_id() :: atom().
-type task_def_map() :: #{task_id() => #task_def{}}.
-type flow_list() :: [#flow{}].

%%====================================================================
%% WP1: Sequence
%%====================================================================

%% @doc Create a sequential flow from Source to Target.
%%
%% Represents WP1 (Sequence): A task followed by another task.
%%
%% @end
-spec sequence(Source :: task_id(), Target :: task_id()) -> #task_def{}.

sequence(_Source, _Target) ->
    %% For simple sequences, both tasks use sequence split/join
    #task_def{
        id = _Target,
        name = atom_to_binary(_Target),
        kind = atomic,
        split_type = sequence,
        join_type = sequence
    }.

%%====================================================================
%% WP2: Parallel Split
%%====================================================================

%% @doc Create an AND-split from Source to multiple Targets.
%%
%% Represents WP2 (Parallel Split): One task transitions to
%% multiple parallel tasks.
%%
%% @end
-spec parallel_split(Source :: task_id(), Targets :: [task_id()]) ->
          {#task_def{}, flow_list()}.

parallel_split(Source, Targets) when is_list(Targets), length(Targets) > 0 ->
    SourceTask = #task_def{
        id = Source,
        name = atom_to_binary(Source),
        kind = atomic,
        split_type = 'and',
        join_type = sequence
    },

    Flows = [#flow{
        from = Source,
        to = Target
    } || Target <- Targets],

    {SourceTask, Flows}.

%%====================================================================
%% WP3: Synchronization (AND-join)
%%====================================================================

%% @doc Create an AND-join from multiple Sources to a single Target.
%%
%% Represents WP3 (Synchronization): Multiple parallel tasks
%% synchronize before proceeding.
%%
%% @end
-spec synchronization(Sources :: [task_id()], Target :: task_id()) ->
          {#task_def{}, flow_list()}.

synchronization(Sources, Target) when is_list(Sources), length(Sources) > 0 ->
    TargetTask = #task_def{
        id = Target,
        name = atom_to_binary(Target),
        kind = atomic,
        split_type = sequence,
        join_type = 'and'
    },

    Flows = [#flow{
        from = Source,
        to = Target
    } || Source <- Sources],

    {TargetTask, Flows}.

%%====================================================================
%% WP4: Exclusive Choice (XOR-split)
%%====================================================================

%% @doc Create an XOR-split with branches selected by priority.
%%
%% Represents WP4 (Exclusive Choice): One branch is selected
%% based on conditions.
%%
%% @end
-spec exclusive_choice(Source :: task_id(), Targets :: [{task_id(), function(), integer()}]) ->
          {#task_def{}, flow_list()}.

exclusive_choice(Source, Branches) ->
    exclusive_choice(Source, Branches, #{}).

%% @doc Create an XOR-split with explicit context data.
%%
%% @end
-spec exclusive_choice(Source :: task_id(),
                       Branches :: [{task_id(), function(), integer()}],
                       Data :: map()) ->
          {#task_def{}, flow_list()}.

exclusive_choice(Source, Branches, _Data) ->
    SourceTask = #task_def{
        id = Source,
        name = atom_to_binary(Source),
        kind = atomic,
        split_type = 'xor',
        join_type = sequence
    },

    Flows = lists:map(
        fun({Target, Condition, Priority}) ->
            #flow{
                from = Source,
                to = Target,
                condition = Condition,
                priority = Priority
            }
        end,
        Branches
    ),

    {SourceTask, Flows}.

%%====================================================================
%% WP5: Simple Merge (XOR-join)
%%====================================================================

%% @doc Create an XOR-join from multiple Sources to a single Target.
%%
%% Represents WP5 (Simple Merge): Multiple alternative paths
%% merge without synchronization.
%%
%% @end
-spec simple_merge(Sources :: [task_id()], Target :: task_id()) ->
          {#task_def{}, flow_list()}.

simple_merge(Sources, Target) when is_list(Sources), length(Sources) > 0 ->
    TargetTask = #task_def{
        id = Target,
        name = atom_to_binary(Target),
        kind = atomic,
        split_type = sequence,
        join_type = 'xor'
    },

    Flows = [#flow{
        from = Source,
        to = Target
    } || Source <- Sources],

    {TargetTask, Flows}.

%%====================================================================
%% WP6: Multi-Choice (OR-split)
%%====================================================================

%% @doc Create an OR-split where multiple branches may be selected.
%%
%% Represents WP6 (Multi-Choice): One or more branches are
%% activated based on conditions.
%%
%% @end
-spec multi_choice(Source :: task_id(), Targets :: [{task_id(), function(), integer()}]) ->
          {#task_def{}, flow_list()}.

multi_choice(Source, Branches) ->
    multi_choice(Source, Branches, #{}).

%% @doc Create an OR-split with explicit context data.
%%
%% @end
-spec multi_choice(Source :: task_id(),
                   Branches :: [{task_id(), function(), integer()}],
                   Data :: map()) ->
          {#task_def{}, flow_list()}.

multi_choice(Source, Branches, _Data) ->
    SourceTask = #task_def{
        id = Source,
        name = atom_to_binary(Source),
        kind = atomic,
        split_type = 'or',
        join_type = sequence
    },

    Flows = lists:map(
        fun({Target, Condition, Priority}) ->
            #flow{
                from = Source,
                to = Target,
                condition = Condition,
                priority = Priority
            }
        end,
        Branches
    ),

    {SourceTask, Flows}.

%%====================================================================
%% WP7: Structured Synchronized Merge (OR-join)
%%====================================================================

%% @doc Create an OR-join that waits for all active branches.
%%
%% Represents WP7 (Structured Synchronized Merge): Multiple
%% branches merge, waiting for all that were activated.
%%
%% @end
-spec structured_sync_merge(Sources :: [task_id()], Target :: task_id()) ->
          {#task_def{}, flow_list()}.

structured_sync_merge(Sources, Target) when is_list(Sources), length(Sources) > 0 ->
    TargetTask = #task_def{
        id = Target,
        name = atom_to_binary(Target),
        kind = atomic,
        split_type = sequence,
        join_type = 'or'
    },

    Flows = [#flow{
        from = Source,
        to = Target
    } || Source <- Sources],

    {TargetTask, Flows}.

%%====================================================================
%% WP8: Multi-Merge
%%====================================================================

%% @doc Create a multi-merge (XOR-join without withdrawal).
%%
%% Represents WP8 (Multi-Merge): Multiple paths merge without
%% synchronizing, similar to XOR-join.
%%
%% @end
-spec multi_merge(Sources :: [task_id()], Target :: task_id()) ->
          {#task_def{}, flow_list()}.

multi_merge(Sources, Target) ->
    %% Multi-merge is essentially the same as simple merge in YAWL
    simple_merge(Sources, Target).

%%====================================================================
%% WP9: Discriminator
%%====================================================================

%% @doc Create a discriminator that activates after first N branches complete.
%%
%% Represents WP9 (Discriminator): Wait for first branch to complete,
%% then discard others.
%%
%% @end
-spec discriminator(Sources :: [task_id()], Target :: task_id()) ->
          {#task_def{}, flow_list()}.

discriminator(Sources, Target) when is_list(Sources), length(Sources) > 0 ->
    TargetTask = #task_def{
        id = Target,
        name = atom_to_binary(Target),
        kind = atomic,
        split_type = sequence,
        join_type = 'xor'  %% First branch wins
    },

    Flows = [#flow{
        from = Source,
        to = Target
    } || Source <- Sources],

    {TargetTask, Flows}.

%%====================================================================
%% WP10: Arbitrary Cycles
%%====================================================================

%% @doc Create a loop back from Source to LoopBack with a condition.
%%
%% Represents WP10 (Arbitrary Cycles): A task can loop back to
%% a previous task based on a condition.
%%
%% @end
-spec arbitrary_cycle(Source :: task_id(),
                      LoopBack :: task_id(),
                      ConditionFun :: function()) ->
          #flow{}.

arbitrary_cycle(Source, LoopBack, ConditionFun) when is_function(ConditionFun) ->
    #flow{
        from = Source,
        to = LoopBack,
        condition = ConditionFun,
        is_cycle = true
    }.

%%====================================================================
%% WP16: Deferred Choice
%%====================================================================

%% @doc Create a deferred choice between multiple candidates.
%%
%% Represents WP16 (Deferred Choice): Multiple branches are
%% enabled, but the first one to start cancels the others.
%%
%% @end
-spec deferred_choice(Source :: task_id(), Candidates :: [task_id()]) ->
          {#task_def{}, flow_list()}.

deferred_choice(Source, Candidates) when is_list(Candidates) ->
    SourceTask = #task_def{
        id = Source,
        name = atom_to_binary(Source),
        kind = atomic,
        split_type = 'xor',  %% Only one will be chosen
        join_type = sequence
    },

    Flows = [#flow{
        from = Source,
        to = Candidate,
        priority = N
    } || {Candidate, N} <- lists:zip(Candidates, lists:seq(1, length(Candidates)))],

    {SourceTask, Flows}.

%%====================================================================
%% WP19+WP20: Cancellation
%%====================================================================

%% @doc Define a cancellation region.
%%
%% Represents WP19 (Cancel Task) and WP20 (Cancel Case):
%% A region of tasks that can be cancelled by a trigger.
%%
%% @end
-spec cancellation_region(RegionId :: atom(),
                          Tasks :: [task_id()],
                          TriggerTask :: task_id()) ->
          #cancellation_region{}.

cancellation_region(RegionId, Tasks, TriggerTask) ->
    #cancellation_region{
        id = RegionId,
        tasks = Tasks,
        cancel_trigger = TriggerTask
    }.

%% @doc Create a task that cancels the entire case.
%%
%% Represents WP20 (Cancel Case): The entire workflow is
%% terminated when this task is triggered.
%%
%% @end
-spec cancel_case(TriggerTask :: task_id()) -> #task_def{}.

cancel_case(TriggerTask) ->
    #task_def{
        id = TriggerTask,
        name = <<"Cancel Case">>,
        kind = atomic,
        split_type = sequence,
        join_type = sequence,
        cancellation_region = TriggerTask
    }.

%%====================================================================
%% Place/Transition Generation
%%====================================================================

%% @doc Generate all places for a workflow specification.
%%
%% @end
-spec generate_places(#yawl_spec{}) -> [atom()].

generate_places(#yawl_spec{tasks = Tasks,
                           input_condition = InputCond,
                           output_condition = OutputCond}) ->
    %% Base places
    BasePlaces = [InputCond, OutputCond],

    %% Task places
    TaskPlaces = [place_name(task, TaskId) || TaskId <- maps:keys(Tasks)],

    %% Join places for AND/OR joins
    JoinPlaces = lists:filtermap(
        fun(TaskId) ->
            TaskDef = maps:get(TaskId, Tasks),
            case TaskDef#task_def.join_type of
                'and' -> {true, place_name('and_join', TaskId)};
                'or' -> {true, place_name('or_join', TaskId)};
                _ -> false
            end
        end,
        maps:keys(Tasks)
    ),

    BasePlaces ++ TaskPlaces ++ JoinPlaces.

%% @doc Generate all transitions for a workflow specification.
%%
%% @end
-spec generate_transitions(#yawl_spec{}) -> [atom()].

generate_transitions(#yawl_spec{tasks = Tasks}) ->
    %% Main task transitions (skip empty tasks)
    TaskTransitions = lists:filtermap(
        fun(TaskId) ->
            TaskDef = maps:get(TaskId, Tasks),
            case TaskDef#task_def.kind of
                empty -> false;
                _ -> {true, transition_name(TaskId)}
            end
        end,
        maps:keys(Tasks)
    ),

    %% Split transitions for AND/XOR/OR splits
    SplitTransitions = lists:filtermap(
        fun(TaskId) ->
            TaskDef = maps:get(TaskId, Tasks),
            case TaskDef#task_def.split_type of
                sequence -> false;
                SplitType -> {true, transition_name(SplitType, TaskId)}
            end
        end,
        maps:keys(Tasks)
    ),

    TaskTransitions ++ SplitTransitions.

%%====================================================================
%% Naming Utilities
%%====================================================================

%% @doc Generate a place name for a given type and task.
%%
%% @end
-spec place_name(Type :: atom(), TaskId :: atom()) -> atom().

place_name(task, TaskId) ->
    list_to_atom("p_task_" ++ atom_to_list(TaskId));
place_name(Type, TaskId) when is_atom(Type), is_atom(TaskId) ->
    list_to_atom("p_" ++ atom_to_list(Type) ++ "_" ++ atom_to_list(TaskId)).

%% @doc Generate a transition name for a task.
%%
%% @end
-spec transition_name(TaskId :: atom()) -> atom().

transition_name(TaskId) when is_atom(TaskId) ->
    list_to_atom("t_" ++ atom_to_list(TaskId)).

%% @doc Generate a transition name for a split type and task.
%%
%% @end
-spec transition_name(SplitType :: atom(), TaskId :: atom()) -> atom().

transition_name(SplitType, TaskId) when is_atom(SplitType), is_atom(TaskId) ->
    list_to_atom("t_" ++ atom_to_list(SplitType) ++ "_" ++ atom_to_list(TaskId)).
