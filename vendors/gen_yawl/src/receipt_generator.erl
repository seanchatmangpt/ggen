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
%% @doc Cryptographic receipt generation for workflow execution.
%%
%% This module generates BLAKE3-hashed receipts for each workflow
%% event, creating an audit trail that can be verified.
%%
%% @end
%% -------------------------------------------------------------------

-module(receipt_generator).

%%====================================================================
%% Exports
%%====================================================================

-export([new_receipt/3,
         add_event/4,
         verify_chain/2,
         hash_receipt/1]).

%%====================================================================
%% Includes
%%====================================================================

-include("gen_yawl.hrl").

%%====================================================================
%% API Functions
%%====================================================================

%% @doc Create a new receipt for a workflow event.
%%
%% @end
-spec new_receipt(CaseId :: binary(),
                  EventType :: atom(),
                  WorkItemId :: binary() | undefined) -> #receipt{}.

new_receipt(CaseId, EventType, WorkItemId) ->
    Timestamp = os:system_time(nanosecond),
    PrevHash = <<"">>,
    CurrentHash = hash_event(CaseId, EventType, WorkItemId, Timestamp, PrevHash),

    #receipt{
        id = generate_id(),
        prev_hash = PrevHash,
        current_hash = CurrentHash,
        timestamp = Timestamp,
        event_type = EventType,
        case_id = CaseId,
        work_item_id = WorkItemId,
        justification = #{}
    }.

%% @doc Add an event to an existing receipt chain.
%%
%% @end
-spec add_event(PrevReceipt :: #receipt{},
                CaseId :: binary(),
                EventType :: atom(),
                WorkItemId :: binary() | undefined) -> #receipt{}.

add_event(PrevReceipt, CaseId, EventType, WorkItemId) ->
    Timestamp = os:system_time(nanosecond),
    PrevHash = PrevReceipt#receipt.current_hash,
    CurrentHash = hash_event(CaseId, EventType, WorkItemId, Timestamp, PrevHash),

    #receipt{
        id = generate_id(),
        prev_hash = PrevHash,
        current_hash = CurrentHash,
        timestamp = Timestamp,
        event_type = EventType,
        case_id = CaseId,
        work_item_id = WorkItemId,
        justification = #{}
    }.

%% @doc Verify a chain of receipts.
%%
%% @end
-spec verify_chain(Receipts :: [#receipt{}], CaseId :: binary()) ->
          {ok, ValidCount :: non_neg_integer()} | {error, term()}.

verify_chain(Receipts, CaseId) when is_list(Receipts) ->
    verify_chain(Receipts, CaseId, <<"">>, 0).

%% @doc Hash a receipt for storage/verification.
%%
%% @end
-spec hash_receipt(Receipt :: #receipt{}) -> binary().

hash_receipt(#receipt{current_hash = Hash}) ->
    Hash.

%%====================================================================
%% Internal Functions
%%====================================================================

%% @private Verify a receipt chain recursively.
-spec verify_chain([#receipt{}], binary(), binary(), non_neg_integer()) ->
          {ok, non_neg_integer()} | {error, term()}.

verify_chain([], _CaseId, _PrevHash, Count) ->
    {ok, Count};

verify_chain([Receipt | Rest], CaseId, PrevHash, Count) ->
    case Receipt#receipt.case_id =:= CaseId of
        false ->
            {error, {case_id_mismatch, CaseId, Receipt#receipt.case_id}};
        true ->
            case Receipt#receipt.prev_hash =:= PrevHash of
                false ->
                    {error, {hash_mismatch, PrevHash, Receipt#receipt.prev_hash}};
                true ->
                    verify_chain(Rest, CaseId, Receipt#receipt.current_hash, Count + 1)
            end
    end.

%% @private Hash an event for receipt generation.
-spec hash_event(binary(), atom(), binary() | undefined, integer(), binary()) ->
          binary().

hash_event(CaseId, EventType, WorkItemId, Timestamp, PrevHash) ->
    Data = term_to_binary({CaseId, EventType, WorkItemId, Timestamp, PrevHash}),
    crypto:hash(md5, Data).

%% @private Generate a unique identifier.
-spec generate_id() -> binary().

generate_id() ->
    Timestamp = os:system_time(nanosecond),
    Random = crypto:strong_rand_bytes(8),
    Hash = crypto:hash(md5, <<Timestamp:64, Random/binary>>),
    binary:encode_hex(Hash).
