%%%-------------------------------------------------------------------
%% @doc tai_receipts_init: Initialize ETS tables for receipts
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_receipts_init).

%% API
-export([init/0]).

%%%===================================================================
%% API
%%%===================================================================

-spec init() -> ok.
init() ->
    %% Receipt store
    ets:new(tai_receipts_store, [named_table, set, public, {keypos, 1}]),
    %% Receipt deduplication
    ets:new(tai_receipts_dedup, [named_table, set, public, {keypos, 1}]),
    %% Receipt chain
    ets:new(tai_receipts_chain, [named_table, set, public, {keypos, 1}]),
    ok.
