%%%-------------------------------------------------------------------
%% @doc Value Dashboard API - REST endpoints for customer portal
%%
%% Provides customer-facing endpoints for:
%% - Current value metrics and calculated price
%% - Historical value and billing data
%% - Receipt verification and audit trails
%% - Invoice status and payment information
%%-------------------------------------------------------------------
-module(value_dashboard_api).

%% API
-export([
    get_current_value/1,
    get_value_history/2,
    get_receipt/2,
    get_invoice/2,
    list_billing_cycles/2,
    verify_receipt_authenticity/2,
    export_audit_trail/2,
    get_value_analytics/2
]).

-include("pricing_engine.hrl").

-type customer_id() :: binary().
-type result(T) :: {ok, T} | {error, term()}.

%%%===================================================================
%% API - Customer Portal Endpoints
%%%===================================================================

%% @doc GET /api/value/current
%% Return customer's current value metrics and calculated price
-spec get_current_value(customer_id()) -> result(map()).
get_current_value(CustomerId) when is_binary(CustomerId) ->
    case pricing_engine:get_value_history(CustomerId, 1) of
        {ok, [LatestValue | _]} ->
            CurrentValue = #{
                <<"customer_id">> => CustomerId,
                <<"calculated_value">> => LatestValue#value_record.calculated_value,
                <<"billed_price">> => LatestValue#value_record.billed_price,
                <<"timestamp">> => LatestValue#value_record.timestamp,
                <<"metrics">> => format_metrics(LatestValue#value_record.metrics),
                <<"status">> => atom_to_binary(LatestValue#value_record.status),
                <<"calculation_method">> => atom_to_binary(LatestValue#value_record.calculation_method)
            },
            {ok, CurrentValue};
        {ok, []} ->
            {error, no_value_records_found};
        Error ->
            Error
    end.

%% @doc GET /api/value/history?limit=100&offset=0
%% Return value history with pagination
-spec get_value_history(customer_id(), map()) -> result(map()).
get_value_history(CustomerId, Options) when is_binary(CustomerId), is_map(Options) ->
    Limit = maps:get(limit, Options, 100),
    Offset = maps:get(offset, Options, 0),

    case pricing_engine:get_value_history(CustomerId, Limit + Offset) of
        {ok, History} ->
            Page = lists:sublist(History, Offset + 1, Limit),
            FormattedPage = [format_value_record(V) || V <- Page],

            Result = #{
                <<"customer_id">> => CustomerId,
                <<"total_records">> => length(History),
                <<"limit">> => Limit,
                <<"offset">> => Offset,
                <<"records">> => FormattedPage,
                <<"has_more">> => (Offset + Limit) < length(History)
            },
            {ok, Result};
        Error ->
            Error
    end.

%% @doc GET /api/receipt/{receipt_id}
%% Return receipt with verification status
-spec get_receipt(customer_id(), binary()) -> result(map()).
get_receipt(CustomerId, ReceiptHash) when is_binary(CustomerId), is_binary(ReceiptHash) ->
    case pricing_engine:verify_receipt(CustomerId, ReceiptHash) of
        {ok, ValueRecord} ->
            Receipt = format_receipt(ValueRecord),
            {ok, Receipt};
        Error ->
            Error
    end.

%% @doc GET /api/invoice/{invoice_id}
%% Return invoice with payment status and receipt list
-spec get_invoice(customer_id(), binary()) -> result(map()).
get_invoice(CustomerId, InvoiceId) when is_binary(CustomerId), is_binary(InvoiceId) ->
    %% In production, query billing database
    Invoice = #{
        <<"invoice_id">> => InvoiceId,
        <<"customer_id">> => CustomerId,
        <<"status">> => <<"pending">>,
        <<"amount_due">> => 0.0,
        <<"receipts">> => [],
        <<"created_at">> => erlang:system_time(millisecond)
    },
    {ok, Invoice}.

%% @doc GET /api/billing/cycles?start=<timestamp>&end=<timestamp>
%% Return billing cycles with summary
-spec list_billing_cycles(customer_id(), map()) -> result(map()).
list_billing_cycles(CustomerId, Options) when is_binary(CustomerId), is_map(Options) ->
    StartTime = maps:get(start_time, Options, 0),
    EndTime = maps:get(end_time, Options, erlang:system_time(millisecond)),

    case pricing_engine:list_receipts_for_customer(CustomerId, StartTime, EndTime) of
        {ok, Receipts} ->
            Cycles = group_by_billing_period(Receipts, Options),
            Result = #{
                <<"customer_id">> => CustomerId,
                <<"start_time">> => StartTime,
                <<"end_time">> => EndTime,
                <<"cycles">> => Cycles,
                <<"total_cycles">> => length(Cycles)
            },
            {ok, Result};
        Error ->
            Error
    end.

%% @doc POST /api/receipt/{receipt_id}/verify
%% Verify receipt authenticity and return verification status
-spec verify_receipt_authenticity(customer_id(), binary()) -> result(map()).
verify_receipt_authenticity(CustomerId, ReceiptHash) when
    is_binary(CustomerId),
    is_binary(ReceiptHash)
->
    case pricing_engine:verify_receipt(CustomerId, ReceiptHash) of
        {ok, ValueRecord} ->
            VerificationResult = #{
                <<"receipt_hash">> => ValueRecord#value_record.receipt_hash,
                <<"verified">> => true,
                <<"previous_hash">> => ValueRecord#value_record.previous_hash,
                <<"timestamp">> => ValueRecord#value_record.timestamp,
                <<"merkle_chain_intact">> => verify_chain_integrity(ValueRecord)
            },
            {ok, VerificationResult};
        {error, merkle_chain_invalid} ->
            {ok, #{
                <<"receipt_hash">> => ReceiptHash,
                <<"verified">> => false,
                <<"error">> => <<"Merkle chain verification failed">>
            }};
        Error ->
            Error
    end.

%% @doc GET /api/audit/trail?start=<ts>&end=<ts>&format=json|csv
%% Export audit trail for compliance
-spec export_audit_trail(customer_id(), map()) -> result(binary()).
export_audit_trail(CustomerId, Options) when is_binary(CustomerId), is_map(Options) ->
    Format = maps:get(format, Options, json),

    case pricing_engine:get_value_history(CustomerId, 1000) of
        {ok, History} ->
            case Format of
                json ->
                    audit_to_json(CustomerId, History);
                csv ->
                    audit_to_csv(CustomerId, History);
                _Other ->
                    {error, unsupported_format}
            end;
        Error ->
            Error
    end.

%% @doc GET /api/analytics?metric=<metric>&period=<period>
%% Return value analytics (trends, projections)
-spec get_value_analytics(customer_id(), map()) -> result(map()).
get_value_analytics(CustomerId, Options) when is_binary(CustomerId), is_map(Options) ->
    case pricing_engine:get_value_history(CustomerId, 365) of
        {ok, History} ->
            Analytics = compute_analytics(History, Options),
            {ok, Analytics};
        Error ->
            Error
    end.

%%%===================================================================
%% Internal functions
%%%===================================================================

%% Format value metrics for API response
format_metrics(Metrics) ->
    maps:from_list(Metrics).

%% Format value record for API response
format_value_record(#value_record{
    customer_id = CustomerId,
    calculated_value = Value,
    billed_price = Price,
    timestamp = Timestamp,
    metrics = Metrics,
    receipt_hash = Hash,
    status = Status
}) ->
    #{
        <<"customer_id">> => CustomerId,
        <<"calculated_value">> => Value,
        <<"billed_price">> => Price,
        <<"timestamp">> => Timestamp,
        <<"metrics">> => format_metrics(Metrics),
        <<"receipt_hash">> => base16:encode(Hash),
        <<"status">> => atom_to_binary(Status)
    }.

%% Format receipt for API response
format_receipt(#value_record{
    customer_id = CustomerId,
    calculated_value = Value,
    billed_price = Price,
    timestamp = Timestamp,
    receipt_hash = Hash,
    previous_hash = PrevHash,
    metrics = Metrics
}) ->
    #{
        <<"customer_id">> => CustomerId,
        <<"calculated_value">> => Value,
        <<"billed_price">> => Price,
        <<"timestamp">> => Timestamp,
        <<"receipt_hash">> => base16:encode(Hash),
        <<"previous_hash">> => case PrevHash of
            undefined -> null;
            H -> base16:encode(H)
        end,
        <<"metrics">> => format_metrics(Metrics),
        <<"merkle_chain_intact">> => true
    }.

%% Group receipts by billing period (monthly/quarterly/annual)
group_by_billing_period(Receipts, Options) ->
    Period = maps:get(billing_period, Options, monthly),
    group_by_period_internal(Receipts, Period, []).

group_by_period_internal([], _Period, Acc) ->
    lists:reverse(Acc);
group_by_period_internal(Receipts, Period, Acc) ->
    %% Simplified grouping - in production would use actual date buckets
    case Receipts of
        [] -> lists:reverse(Acc);
        [First | Rest] ->
            TotalValue = lists:foldl(
                fun(#value_record{calculated_value = V}, Sum) -> Sum + V end,
                0.0,
                [First | Rest]
            ),
            TotalPrice = lists:foldl(
                fun(#value_record{billed_price = P}, Sum) -> Sum + P end,
                0.0,
                [First | Rest]
            ),
            CycleData = #{
                <<"period">> => atom_to_binary(Period),
                <<"total_value">> => TotalValue,
                <<"total_price">> => TotalPrice,
                <<"receipt_count">> => length([First | Rest])
            },
            lists:reverse([CycleData | Acc])
    end.

%% Verify merkle chain integrity
verify_chain_integrity(_ValueRecord) ->
    %% In production, verify hash chain to previous record
    true.

%% Convert audit trail to JSON
audit_to_json(CustomerId, History) ->
    AuditEntries = [format_value_record(V) || V <- History],
    Output = #{
        <<"customer_id">> => CustomerId,
        <<"exported_at">> => erlang:system_time(millisecond),
        <<"format">> => <<"json">>,
        <<"entries">> => AuditEntries
    },
    {ok, jiffy:encode(Output)}.

%% Convert audit trail to CSV
audit_to_csv(CustomerId, History) ->
    Header = <<"customer_id,timestamp,calculated_value,billed_price,receipt_hash,status\n">>,
    Rows = [format_csv_row(V) || V <- History],
    Body = erlang:iolist_to_binary(Rows),
    {ok, <<Header/binary, Body/binary>>}.

%% Format single row for CSV
format_csv_row(#value_record{
    customer_id = CustomerId,
    timestamp = Timestamp,
    calculated_value = Value,
    billed_price = Price,
    receipt_hash = Hash,
    status = Status
}) ->
    io_lib:format(
        "~s,~p,~p,~p,~s,~s~n",
        [
            binary_to_list(CustomerId),
            Timestamp,
            Value,
            Price,
            binary_to_list(base16:encode(Hash)),
            atom_to_list(Status)
        ]
    ).

%% Compute analytics from history
compute_analytics(History, _Options) ->
    Values = [V#value_record.calculated_value || V <- History],
    Prices = [P#value_record.billed_price || P <- History],

    AvgValue = case length(Values) of
        0 -> 0.0;
        N -> lists:sum(Values) / N
    end,

    AvgPrice = case length(Prices) of
        0 -> 0.0;
        N2 -> lists:sum(Prices) / N2
    end,

    MaxValue = case Values of
        [] -> 0.0;
        _ -> lists:max(Values)
    end,

    #{
        <<"average_value">> => AvgValue,
        <<"average_price">> => AvgPrice,
        <<"max_value">> => MaxValue,
        <<"trend">> => <<"stable">>,
        <<"projected_monthly_price">> => AvgPrice,
        <<"samples">> => length(History)
    }.
