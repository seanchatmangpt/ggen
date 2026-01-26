%%%-------------------------------------------------------------------
%% @doc Stripe Billing Integration
%%
%% Handles:
%% - Customer registration with Stripe
%% - Payment method management
%% - Invoice creation and payment
%% - Webhook handling for payment events
%% - Billing cycle automation (monthly, quarterly, annual)
%%-------------------------------------------------------------------
-module(stripe_billing).

%% API
-export([
    create_customer/3,
    add_payment_method/3,
    create_invoice/3,
    charge_customer/3,
    handle_payment_webhook/2,
    get_payment_status/2,
    refund_charge/3,
    list_invoices_for_customer/3
]).

-include("pricing_engine.hrl").

-type customer_id() :: binary().
-type stripe_customer_id() :: binary().
-type result(T) :: {ok, T} | {error, term()}.

%%%===================================================================
%% API
%%%===================================================================

%% @doc Create Stripe customer from our customer
-spec create_customer(customer_id(), map(), map()) ->
    result(stripe_customer_id()).
create_customer(CustomerId, CustomerData, Options) when
    is_binary(CustomerId),
    is_map(CustomerData),
    is_map(Options)
->
    StripeApiKey = maps:get(stripe_api_key, Options, undefined),

    case StripeApiKey of
        undefined ->
            {error, missing_stripe_api_key};
        Key when is_binary(Key) ->
            RequestBody = build_customer_request(CustomerId, CustomerData),
            case make_stripe_request(<<"POST">>, <<"/customers">>, RequestBody, Key) of
                {ok, Response} ->
                    StripeId = maps:get(<<"id">>, Response, undefined),
                    {ok, StripeId};
                Error ->
                    Error
            end
    end.

%% @doc Add payment method (credit card) to customer
-spec add_payment_method(stripe_customer_id(), map(), map()) ->
    result(binary()).
add_payment_method(StripeCustomerId, PaymentMethodData, Options) when
    is_binary(StripeCustomerId),
    is_map(PaymentMethodData),
    is_map(Options)
->
    StripeApiKey = maps:get(stripe_api_key, Options, undefined),

    case StripeApiKey of
        undefined ->
            {error, missing_stripe_api_key};
        Key when is_binary(Key) ->
            RequestBody = build_payment_method_request(PaymentMethodData),
            Endpoint = <<"/customers/", StripeCustomerId/binary, "/payment_methods">>,
            case make_stripe_request(<<"POST">>, Endpoint, RequestBody, Key) of
                {ok, Response} ->
                    MethodId = maps:get(<<"id">>, Response, undefined),
                    {ok, MethodId};
                Error ->
                    Error
            end
    end.

%% @doc Create invoice for customer billing cycle
-spec create_invoice(#billing_cycle{}, map(), map()) ->
    result(binary()).
create_invoice(
    #billing_cycle{
        cycle_id = CycleId,
        customer_id = CustomerId,
        total_price = Amount
    },
    CustomerData,
    Options
) ->
    StripeApiKey = maps:get(stripe_api_key, Options, undefined),
    StripeCustomerId = maps:get(stripe_customer_id, CustomerData, undefined),

    case {StripeApiKey, StripeCustomerId} of
        {undefined, _} ->
            {error, missing_stripe_api_key};
        {_, undefined} ->
            {error, missing_stripe_customer_id};
        {Key, StripeId} when is_binary(Key), is_binary(StripeId) ->
            RequestBody = #{
                <<"customer">> => StripeId,
                <<"amount_paid">> => trunc(Amount * 100),  %% Amount in cents
                <<"description">> => <<"Autonomic Service Billing">>,
                <<"metadata">> => #{
                    <<"cycle_id">> => CycleId,
                    <<"customer_id">> => CustomerId
                }
            },
            case make_stripe_request(<<"POST">>, <<"/invoices">>, RequestBody, Key) of
                {ok, Response} ->
                    InvoiceId = maps:get(<<"id">>, Response, undefined),
                    {ok, InvoiceId};
                Error ->
                    Error
            end
    end.

%% @doc Charge customer's payment method
-spec charge_customer(stripe_customer_id(), float(), map()) ->
    result(binary()).
charge_customer(StripeCustomerId, Amount, Options) when
    is_binary(StripeCustomerId),
    is_number(Amount),
    Amount > 0,
    is_map(Options)
->
    StripeApiKey = maps:get(stripe_api_key, Options, undefined),

    case StripeApiKey of
        undefined ->
            {error, missing_stripe_api_key};
        Key when is_binary(Key) ->
            RequestBody = #{
                <<"amount">> => trunc(Amount * 100),  %% Convert to cents
                <<"currency">> => <<"usd">>,
                <<"customer">> => StripeCustomerId,
                <<"description">> => <<"Autonomic Value-Based Billing">>,
                <<"off_session">> => true,
                <<"confirm">> => true
            },
            case make_stripe_request(<<"POST">>, <<"/payment_intents">>, RequestBody, Key) of
                {ok, Response} ->
                    ChargeId = maps:get(<<"id">>, Response, undefined),
                    Status = maps:get(<<"status">>, Response, undefined),
                    case Status of
                        <<"succeeded">> -> {ok, ChargeId};
                        <<"processing">> -> {ok, ChargeId};
                        <<"requires_action">> -> {error, requires_customer_action};
                        _ -> {error, {charge_failed, Status}}
                    end;
                Error ->
                    Error
            end
    end.

%% @doc Handle Stripe webhook (payment status updates)
-spec handle_payment_webhook(binary(), map()) ->
    result(ok).
handle_payment_webhook(EventType, EventData) when
    is_binary(EventType),
    is_map(EventData)
->
    case EventType of
        <<"payment_intent.succeeded">> ->
            handle_payment_succeeded(EventData);
        <<"payment_intent.payment_failed">> ->
            handle_payment_failed(EventData);
        <<"customer.deleted">> ->
            handle_customer_deleted(EventData);
        <<"invoice.payment_succeeded">> ->
            handle_invoice_paid(EventData);
        <<"invoice.payment_failed">> ->
            handle_invoice_failed(EventData);
        _Other ->
            {error, {unsupported_event_type, EventType}}
    end.

%% @doc Get payment status for charge
-spec get_payment_status(binary(), map()) -> result(atom()).
get_payment_status(ChargeId, Options) when is_binary(ChargeId), is_map(Options) ->
    StripeApiKey = maps:get(stripe_api_key, Options, undefined),

    case StripeApiKey of
        undefined ->
            {error, missing_stripe_api_key};
        Key when is_binary(Key) ->
            Endpoint = <<"/payment_intents/", ChargeId/binary>>,
            case make_stripe_request(<<"GET">>, Endpoint, #{}, Key) of
                {ok, Response} ->
                    Status = maps:get(<<"status">>, Response, unknown),
                    {ok, stripe_status_to_atom(Status)};
                Error ->
                    Error
            end
    end.

%% @doc Refund a charge
-spec refund_charge(binary(), float(), map()) -> result(binary()).
refund_charge(ChargeId, Amount, Options) when
    is_binary(ChargeId),
    is_number(Amount),
    Amount > 0,
    is_map(Options)
->
    StripeApiKey = maps:get(stripe_api_key, Options, undefined),

    case StripeApiKey of
        undefined ->
            {error, missing_stripe_api_key};
        Key when is_binary(Key) ->
            RequestBody = #{
                <<"payment_intent">> => ChargeId,
                <<"amount">> => trunc(Amount * 100),  %% Convert to cents
                <<"reason">> => <<"requested_by_customer">>
            },
            case make_stripe_request(<<"POST">>, <<"/refunds">>, RequestBody, Key) of
                {ok, Response} ->
                    RefundId = maps:get(<<"id">>, Response, undefined),
                    {ok, RefundId};
                Error ->
                    Error
            end
    end.

%% @doc List invoices for customer in date range
-spec list_invoices_for_customer(stripe_customer_id(), map(), map()) ->
    result([map()]).
list_invoices_for_customer(StripeCustomerId, DateRange, Options) when
    is_binary(StripeCustomerId),
    is_map(DateRange),
    is_map(Options)
->
    StripeApiKey = maps:get(stripe_api_key, Options, undefined),

    case StripeApiKey of
        undefined ->
            {error, missing_stripe_api_key};
        Key when is_binary(Key) ->
            QueryParams = build_query_params(StripeCustomerId, DateRange),
            Endpoint = <<"/invoices?", QueryParams/binary>>,
            case make_stripe_request(<<"GET">>, Endpoint, #{}, Key) of
                {ok, Response} ->
                    Invoices = maps:get(<<"data">>, Response, []),
                    {ok, Invoices};
                Error ->
                    Error
            end
    end.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% Build customer creation request
build_customer_request(CustomerId, CustomerData) ->
    #{
        <<"name">> => maps:get(customer_name, CustomerData, CustomerId),
        <<"email">> => maps:get(customer_email, CustomerData, undefined),
        <<"description">> => <<"Autonomic SKU Customer">>,
        <<"metadata">> => #{
            <<"customer_id">> => CustomerId,
            <<"created_at">> => erlang:system_time(second)
        }
    }.

%% Build payment method request
build_payment_method_request(PaymentMethodData) ->
    #{
        <<"type">> => <<"card">>,
        <<"card">> => #{
            <<"number">> => maps:get(card_number, PaymentMethodData, undefined),
            <<"exp_month">> => maps:get(exp_month, PaymentMethodData, undefined),
            <<"exp_year">> => maps:get(exp_year, PaymentMethodData, undefined),
            <<"cvc">> => maps:get(cvc, PaymentMethodData, undefined)
        }
    }.

%% Make HTTP request to Stripe API
make_stripe_request(Method, Endpoint, Body, ApiKey) ->
    Url = <<"https://api.stripe.com/v1">> <> Endpoint,
    Headers = build_stripe_headers(ApiKey),

    Options = [
        {timeout, 30000},
        {connect_timeout, 10000}
    ],

    case httpc:request(
        binary_to_atom(Method),
        {binary_to_list(Url), Headers, "application/json",
         jiffy:encode(Body)},
        Options,
        []
    ) of
        {ok, {{_HttpVersion, 200, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} ->
            {ok, jiffy:decode(ResponseBody, [return_maps])};
        {ok, {{_HttpVersion, 201, _ReasonPhrase}, _ResponseHeaders, ResponseBody}} ->
            {ok, jiffy:decode(ResponseBody, [return_maps])};
        {ok, {{_HttpVersion, Code, ReasonPhrase}, _ResponseHeaders, ResponseBody}} ->
            logger:warning("Stripe API error: ~p ~p", [Code, ReasonPhrase]),
            {error, {api_error, Code, ResponseBody}};
        {error, Reason} ->
            logger:error("Stripe request failed: ~p", [Reason]),
            {error, {request_failed, Reason}}
    end.

%% Build Stripe API request headers
build_stripe_headers(ApiKey) ->
    AuthHeader = <<"Basic ", (base64:encode(ApiKey))/binary>>,
    [
        {"Authorization", binary_to_list(AuthHeader)},
        {"Content-Type", "application/json"},
        {"User-Agent", "TAI-Pricing-Engine/1.0"}
    ].

%% Handle payment succeeded event
handle_payment_succeeded(EventData) ->
    _CustomerId = maps:get(<<"metadata">>, EventData, #{}) |> maps:get(<<"customer_id">>, <<"unknown">>),
    %% Update billing status in database
    {ok, ok}.

%% Handle payment failed event
handle_payment_failed(EventData) ->
    _CustomerId = maps:get(<<"metadata">>, EventData, #{}) |> maps:get(<<"customer_id">>, <<"unknown">>),
    %% Update billing status, trigger retry logic
    {ok, ok}.

%% Handle customer deleted event
handle_customer_deleted(EventData) ->
    _StripeCustomerId = maps:get(<<"id">>, EventData, undefined),
    %% Mark customer as deleted, prevent future charges
    {ok, ok}.

%% Handle invoice paid event
handle_invoice_paid(EventData) ->
    _InvoiceId = maps:get(<<"id">>, EventData, undefined),
    %% Mark invoice as paid, update records
    {ok, ok}.

%% Handle invoice failed event
handle_invoice_failed(EventData) ->
    _InvoiceId = maps:get(<<"id">>, EventData, undefined),
    %% Mark invoice as failed, trigger retries
    {ok, ok}.

%% Convert Stripe status to atom
stripe_status_to_atom(<<"succeeded">>) -> succeeded;
stripe_status_to_atom(<<"processing">>) -> processing;
stripe_status_to_atom(<<"requires_action">>) -> requires_action;
stripe_status_to_atom(<<"requires_capture">>) -> requires_capture;
stripe_status_to_atom(<<"canceled">>) -> canceled;
stripe_status_to_atom(_Other) -> unknown.

%% Build query parameters for list request
build_query_params(CustomerId, DateRange) ->
    StartTime = maps:get(start_time, DateRange, 0),
    EndTime = maps:get(end_time, DateRange, erlang:system_time(second)),
    Limit = maps:get(limit, DateRange, 100),

    Params = [
        {<<"customer">>, CustomerId},
        {<<"created[gte]">>, integer_to_binary(StartTime)},
        {<<"created[lte]">>, integer_to_binary(EndTime)},
        {<<"limit">>, integer_to_binary(Limit)}
    ],

    encode_query_params(Params).

%% Encode parameters for URL
encode_query_params(Params) ->
    Encoded = [
        <<(encode_param_key(K))/binary, "=", (encode_param_value(V))/binary>>
        || {K, V} <- Params
    ],
    erlang:iolist_to_binary(string:join(Encoded, "&")).

encode_param_key(Key) when is_binary(Key) -> Key;
encode_param_key(Key) when is_atom(Key) -> atom_to_binary(Key).

encode_param_value(Value) when is_binary(Value) -> Value;
encode_param_value(Value) when is_integer(Value) -> integer_to_binary(Value);
encode_param_value(Value) when is_atom(Value) -> atom_to_binary(Value).
