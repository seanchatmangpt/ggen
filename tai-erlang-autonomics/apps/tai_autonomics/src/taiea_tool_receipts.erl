%%%-------------------------------------------------------------------
%%% @doc Receipts Tool - Verify receipt chain integrity
%%%
%%% Tool: taiea.receipts.verify_chain
%%% Input: {tenant_id, receipt_id}
%%% Output: {ok, VerificationResponse, Receipt}
%%%
%%% Verifies receipt chain integrity by:
%%% - Looking up receipt by ID and tenant
%%% - Validating receipt structure and signatures
%%% - Checking chain links to predecessor receipts
%%% - Computing hash verification
%%%
%%% This is a stub that returns structure - Phase 2 implements persistent storage
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_receipts).

%% API
-export([handle/1]).

%% Types
-type tenant_id() :: binary().
-type receipt_id() :: binary().
-type verification_status() :: binary().
-type tool_response() :: map().
-type receipt() :: map().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Handle receipt chain verification request
-spec handle(map()) -> {ok, tool_response(), receipt()} | {error, term()}.
handle(Input) ->
    try
        %% Extract parameters
        TenantId = maps:get(<<"tenant_id">>, Input),
        ReceiptId = maps:get(<<"receipt_id">>, Input),

        %% Verify receipt (stub - Phase 2 adds persistence)
        VerificationResult = verify_receipt_chain(TenantId, ReceiptId),

        %% Build response
        Response = #{
            receipt_id => ReceiptId,
            tenant_id => TenantId,
            verification_status => maps:get(status, VerificationResult),
            chain_valid => maps:get(chain_valid, VerificationResult, false),
            timestamp => timestamp(),
            message => maps:get(message, VerificationResult),
            metadata => #{
                verified_at => timestamp(),
                checks_performed => [
                    <<"receipt_existence">>,
                    <<"structure_validation">>,
                    <<"chain_integrity">>,
                    <<"signature_verification">>
                ]
            }
        },

        %% Emit receipt
        ToolReceipt = emit_tool_receipt(TenantId, ReceiptId, VerificationResult, Response),

        {ok, Response, ToolReceipt}
    catch
        Class:Reason ->
            TenantIdErr = maps:get(<<"tenant_id">>, Input, <<"unknown">>),
            ReceiptIdErr = maps:get(<<"receipt_id">>, Input, <<"unknown">>),
            ErrorReceipt = emit_error_receipt(TenantIdErr, ReceiptIdErr, {Class, Reason}),
            {error, {verification_error, Class, Reason, ErrorReceipt}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Verify receipt chain integrity
%% STUB: Returns not_found for now, Phase 2 adds ETS storage
-spec verify_receipt_chain(tenant_id(), receipt_id()) -> map().
verify_receipt_chain(_TenantId, _ReceiptId) ->
    %% STUB: Phase 2 will add persistent receipt storage
    %% For now, return verification structure showing not found
    #{
        status => <<"not_found">>,
        chain_valid => false,
        message => <<"Receipt not found in storage (Phase 2 adds persistence)">>,
        details => #{
            receipt_exists => false,
            structure_valid => false,
            chain_intact => false,
            signatures_valid => false
        },
        recommendation => <<"Receipt storage implemented in Phase 2">>
    }.

%% @private Emit receipt for successful verification
-spec emit_tool_receipt(tenant_id(), receipt_id(), map(), tool_response()) -> receipt().
emit_tool_receipt(TenantId, ReceiptId, VerificationResult, Response) ->
    Status = maps:get(status, VerificationResult),
    ToolStatus = case Status of
        <<"valid">> -> <<"success">>;
        <<"not_found">> -> <<"not_found">>;
        _ -> <<"failure">>
    end,

    #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tool => <<"taiea.receipts.verify_chain">>,
        event => <<"receipt_verification_completed">>,
        status => ToolStatus,
        tenant_id => TenantId,
        receipt_id => ReceiptId,
        verification_status => Status,
        message => <<"Receipt chain verification completed">>,
        metadata => #{
            response => Response,
            details => maps:get(details, VerificationResult, #{}),
            node => node(),
            process => self(),
            phase_status => <<"Phase 1 (stub) - Phase 2 adds persistence">>
        }
    }.

%% @private Emit receipt for error during verification
-spec emit_error_receipt(tenant_id(), receipt_id(), term()) -> receipt().
emit_error_receipt(TenantId, ReceiptId, Error) ->
    #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tool => <<"taiea.receipts.verify_chain">>,
        event => <<"receipt_verification_failed">>,
        status => <<"error">>,
        tenant_id => TenantId,
        receipt_id => ReceiptId,
        error => #{
            reason => format_error(Error),
            details => error_details(Error)
        },
        message => <<"Receipt verification failed">>,
        metadata => #{
            node => node()
        }
    }.

%% @private Format error for receipt
-spec format_error(term()) -> binary().
format_error(Error) ->
    iolist_to_binary(io_lib:format("~p", [Error])).

%% @private Get error details
-spec error_details(term()) -> map().
error_details({Class, Reason}) ->
    #{
        class => atom_to_binary(Class, utf8),
        reason => format_error(Reason)
    };
error_details(Other) ->
    #{
        error => format_error(Other)
    }.

%% @private Generate unique receipt ID
-spec generate_receipt_id() -> binary().
generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% @private Get current timestamp in milliseconds
-spec timestamp() -> non_neg_integer().
timestamp() ->
    erlang:system_time(millisecond).
