%%%-------------------------------------------------------------------
%% @doc Eval Mode Integration Example
%%
%% This module demonstrates how to integrate ac_eval_mode into a
%% pricing engine application. It shows:
%%
%% 1. Initialization with eval mode verification
%% 2. Session management per request
%% 3. Payload decoration for all calculations
%% 4. Receipt generation with eval markers
%% 5. API response decoration
%% 6. Proper error handling
%%
%% This is NOT production code, but a template for integration.
%%-------------------------------------------------------------------
-module(eval_mode_integration_example).

%% API
-export([
    start_pricing_service/0,
    handle_calculate_value_request/4,
    handle_get_receipt_request/2,
    handle_list_receipts_request/3,
    cleanup_session/1
]).

-include("pricing_engine.hrl").

%%%===================================================================
%% Initialization - Verify Eval Mode at Startup
%%%===================================================================

%% @doc Start the pricing service with eval mode enforcement
%% This ensures eval mode is verified before any calculations begin.
-spec start_pricing_service() -> ok | {error, term()}.
start_pricing_service() ->
    %% Verify eval mode before starting
    case ac_eval_mode:ensure_eval() of
        ok ->
            logger:info("Pricing service initialized in eval mode", #{}),
            logger:info("Banner: ~p", [ac_eval_mode:banner()]),

            %% Start pricing engine
            case pricing_engine:start_link(#{}) of
                {ok, _Pid} ->
                    logger:info("Pricing engine started successfully", #{}),
                    ok;
                {error, Reason} ->
                    {error, {pricing_engine_start_failed, Reason}}
            end;
        {error, not_eval_mode} ->
            {error, eval_mode_verification_failed}
    end.

%%%===================================================================
%% Request Handlers
%%%===================================================================

%% @doc Handle a value calculation request
%% Example: POST /api/v1/calculate
-spec handle_calculate_value_request(
    binary(),  % customer_id
    list(),    % metrics
    map(),     % pricing_config
    map()      % request_context (request_id, client_id, etc.)
) -> {ok, map()} | {error, term()}.
handle_calculate_value_request(CustomerId, Metrics, PricingConfig, RequestCtx) ->
    %% Start session for this request
    case ac_eval_mode:start_session(RequestCtx) of
        {ok, SessionId, _SessionSecret} ->
            try
                %% Validate inputs
                case validate_inputs(CustomerId, Metrics, PricingConfig) of
                    {ok, ValidatedMetrics} ->
                        %% Call pricing engine
                        case pricing_engine:calculate_value(
                            CustomerId,
                            ValidatedMetrics,
                            PricingConfig,
                            #{aggregation_method => weighted_sum}
                        ) of
                            {ok, ValueRecord} ->
                                %% Decorate with eval mode
                                case ac_eval_mode:decorate_payload(ValueRecord) of
                                    {ok, DecoratedPayload} ->
                                        %% Build response
                                        {ok, build_response(
                                            DecoratedPayload,
                                            RequestCtx,
                                            success
                                        )};
                                    {error, DecorateReason} ->
                                        {error, {payload_decoration_failed, DecorateReason}}
                                end;
                            {error, CalcReason} ->
                                {error, {calculation_failed, CalcReason}}
                        end;
                    {error, ValidationReason} ->
                        {error, {validation_failed, ValidationReason}}
                end
            after
                %% Always clean up session
                ac_eval_mode:end_session(SessionId)
            end;
        {error, SessionReason} ->
            {error, {session_start_failed, SessionReason}}
    end.

%% @doc Handle a get receipt request
%% Example: GET /api/v1/receipts/{receipt_id}
-spec handle_get_receipt_request(binary(), map()) ->
    {ok, map()} | {error, term()}.
handle_get_receipt_request(CustomerId, RequestCtx) ->
    case ac_eval_mode:start_session(RequestCtx) of
        {ok, SessionId, _SessionSecret} ->
            try
                %% Get receipt from storage
                case fetch_customer_receipt(CustomerId) of
                    {ok, Receipt} ->
                        %% Decorate receipt with eval mode
                        case ac_eval_mode:decorate_receipt(Receipt) of
                            {ok, DecoratedReceipt} ->
                                {ok, build_response(
                                    DecoratedReceipt,
                                    RequestCtx,
                                    success
                                )};
                            {error, DecorateReason} ->
                                {error, {receipt_decoration_failed, DecorateReason}}
                        end;
                    {error, FetchReason} ->
                        {error, {receipt_fetch_failed, FetchReason}}
                end
            after
                ac_eval_mode:end_session(SessionId)
            end;
        {error, SessionReason} ->
            {error, {session_start_failed, SessionReason}}
    end.

%% @doc Handle list receipts request
%% Example: GET /api/v1/receipts?customer_id={cid}&limit={n}
-spec handle_list_receipts_request(binary(), integer(), map()) ->
    {ok, map()} | {error, term()}.
handle_list_receipts_request(CustomerId, Limit, RequestCtx) ->
    case ac_eval_mode:start_session(RequestCtx) of
        {ok, SessionId, _SessionSecret} ->
            try
                %% Get receipts from storage
                case fetch_customer_receipts(CustomerId, Limit) of
                    {ok, Receipts} ->
                        %% Decorate each receipt
                        case decorate_receipts(Receipts) of
                            {ok, DecoratedReceipts} ->
                                {ok, build_response(
                                    #{
                                        <<"receipts">> => DecoratedReceipts,
                                        <<"count">> => length(DecoratedReceipts),
                                        <<"limited">> => length(Receipts) >= Limit
                                    },
                                    RequestCtx,
                                    success
                                )};
                            {error, DecorateReason} ->
                                {error, {receipt_decoration_failed, DecorateReason}}
                        end;
                    {error, FetchReason} ->
                        {error, {receipts_fetch_failed, FetchReason}}
                end
            after
                ac_eval_mode:end_session(SessionId)
            end;
        {error, SessionReason} ->
            {error, {session_start_failed, SessionReason}}
    end.

%% @doc Cleanup session (for explicit cleanup outside of request handlers)
-spec cleanup_session(binary()) -> ok | {error, term()}.
cleanup_session(SessionId) ->
    ac_eval_mode:end_session(SessionId).

%%%===================================================================
%% Internal Helpers
%%%===================================================================

%% Validate inputs before processing
-spec validate_inputs(binary(), list(), map()) ->
    {ok, list()} | {error, term()}.
validate_inputs(CustomerId, Metrics, PricingConfig)
  when is_binary(CustomerId),
       is_list(Metrics),
       is_map(PricingConfig) ->
    %% Validate metrics format
    case validate_metrics_format(Metrics) of
        {ok, ValidMetrics} ->
            %% Validate pricing config
            case validate_pricing_config(PricingConfig) of
                {ok, _} ->
                    {ok, ValidMetrics};
                Error ->
                    Error
            end;
        Error ->
            Error
    end;
validate_inputs(_CustomerId, _Metrics, _PricingConfig) ->
    {error, invalid_input_types}.

%% Validate metrics format
validate_metrics_format(Metrics) ->
    case lists:all(
        fun({Name, Value}) ->
            is_binary(Name) andalso is_number(Value) andalso Value >= 0
        end,
        Metrics
    ) of
        true -> {ok, Metrics};
        false -> {error, invalid_metrics_format}
    end.

%% Validate pricing configuration
validate_pricing_config(Config) ->
    RequiredKeys = [alpha_coefficient, min_price, max_price],
    case maps:with(RequiredKeys, Config) of
        #{} ->
            {ok, Config};
        _ ->
            {error, missing_pricing_config_keys}
    end.

%% Fetch a single receipt from storage (stub)
fetch_customer_receipt(CustomerId) when is_binary(CustomerId) ->
    %% In production, query storage backend
    %% For now, return example receipt
    {ok, #receipt{
        receipt_id = <<"receipt_123">>,
        customer_id = CustomerId,
        calculated_value = 100.0,
        billed_price = 10000.0,
        currency = <<"USD">>,
        period_start = erlang:system_time(millisecond) - 86400000,
        period_end = erlang:system_time(millisecond),
        calculation_timestamp = erlang:system_time(millisecond),
        value_hash = <<"hash_123">>,
        signature = <<"sig_123">>,
        invoice_id = undefined,
        payment_status = pending,
        verified = false,
        verification_timestamp = undefined
    }}.

%% Fetch multiple receipts from storage (stub)
fetch_customer_receipts(CustomerId, Limit) when
    is_binary(CustomerId),
    is_integer(Limit),
    Limit > 0
->
    %% In production, query storage backend with limit
    %% For now, return examples
    Receipts = [
        #receipt{
            receipt_id = <<"receipt_1">>,
            customer_id = CustomerId,
            calculated_value = 100.0,
            billed_price = 10000.0,
            currency = <<"USD">>,
            period_start = erlang:system_time(millisecond) - 86400000,
            period_end = erlang:system_time(millisecond),
            calculation_timestamp = erlang:system_time(millisecond),
            value_hash = <<"hash_1">>,
            signature = <<"sig_1">>,
            invoice_id = undefined,
            payment_status = pending,
            verified = false,
            verification_timestamp = undefined
        }
    ],
    {ok, lists:sublist(Receipts, Limit)}.

%% Decorate a single receipt
decorate_receipts([]) ->
    {ok, []};
decorate_receipts([Receipt | Rest]) ->
    case ac_eval_mode:decorate_receipt(Receipt) of
        {ok, Decorated} ->
            case decorate_receipts(Rest) of
                {ok, RestDecorated} ->
                    {ok, [Decorated | RestDecorated]};
                Error ->
                    Error
            end;
        Error ->
            Error
    end.

%% Build API response with proper metadata
build_response(Data, RequestCtx, Status) ->
    case ac_eval_mode:decorate_meta(
        #{
            <<"status">> => Status,
            <<"data">> => Data,
            <<"version">> => <<"1.0">>
        },
        RequestCtx
    ) of
        {ok, DecoratedMeta} ->
            DecoratedMeta;
        {error, Reason} ->
            logger:error(
                "Failed to decorate response metadata",
                #{error => Reason}
            ),
            #{
                <<"status">> => error,
                <<"error">> => <<"Internal server error">>,
                <<"data">> => null
            }
    end.

%%%===================================================================
%% Example Usage
%%%===================================================================

%% Example 1: Initialize service
% start_pricing_service().
% Output: ok (eval mode verified, pricing engine running)

%% Example 2: Calculate value
% RequestCtx = #{
%     request_id => <<"req_12345">>,
%     client_id => <<"client_abc">>
% },
% CustomerId = <<"customer_001">>,
% Metrics = [{<<"throughput">>, 100.0}, {<<"latency">>, 50.0}],
% PricingConfig = #{
%     alpha_coefficient => 100.0,
%     min_price => 0.0,
%     max_price => 10000.0
% },
% {ok, Response} = handle_calculate_value_request(
%     CustomerId,
%     Metrics,
%     PricingConfig,
%     RequestCtx
% ).
% Output: {ok, #{
%     <<"status">> => success,
%     <<"mode">> => eval,
%     <<"authority">> => advisory,
%     <<"disclaimer">> => <<"...">>,
%     <<"data">> => #{
%         <<"eval_only">> => true,
%         <<"calculated_value">> => 100.0,
%         <<"billed_price">> => 10000.0,
%         <<"session_id">> => <<"uuid_...">>,
%         <<"session_hash">> => <<"...">>,
%         ...
%     }
% }}

%% Example 3: Get receipt
% {ok, ReceiptResponse} = handle_get_receipt_request(
%     <<"customer_001">>,
%     #{request_id => <<"req_receipt_1">>}
% ).
% Output: {ok, #{
%     <<"status">> => success,
%     <<"mode">> => eval,
%     <<"data">> => #{
%         <<"eval_only">> => true,
%         <<"non_contractual">> => true,
%         <<"use_for_billing_prohibited">> => true,
%         <<"receipt_id">> => <<"...">>,
%         <<"session_id">> => <<"...">>,
%         ...
%     }
% }}
