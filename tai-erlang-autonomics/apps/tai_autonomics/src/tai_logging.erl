%%%-------------------------------------------------------------------
%% @doc tai_logging: Structured JSON logging
%%
%% Logs include tenant_id, sku_id, receipt_hash, timestamp.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_logging).

%% API
-export([log/3, log_receipt/1]).

%%%===================================================================
%% API
%%%===================================================================

-spec log(Level, Message, Metadata) -> ok
  when Level :: info | warn | error | debug,
       Message :: binary(),
       Metadata :: map().
log(Level, Message, Metadata) ->
    LogEntry = #{
        timestamp => erlang:system_time(millisecond),
        level => atom_to_binary(Level, utf8),
        message => Message,
        metadata => Metadata
    },
    LogJson = jsx:encode(LogEntry),
    logger:log(Level, "~s", [LogJson]),
    ok.

-spec log_receipt(Receipt) -> ok
  when Receipt :: map().
log_receipt(Receipt) ->
    TenantId = maps:get(tenant_id, Receipt, <<>>),
    ReceiptId = maps:get(id, Receipt, <<>>),
    Metadata = #{
        tenant_id => TenantId,
        receipt_id => ReceiptId,
        receipt_hash => maps:get(hash, Receipt, <<>>)
    },
    log(info, <<"Receipt emitted">>, Metadata),
    ok.
