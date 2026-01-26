%%%-------------------------------------------------------------------
%% @doc Pricing Engine Monitoring - Observability and Alerting
%%
%% Provides:
%% - Prometheus metrics (Grafana dashboards)
%% - OpenTelemetry tracing
%% - Structured JSON logging
%% - Anomaly detection and alerting
%% - Real-time health checks
%%%-------------------------------------------------------------------
-module(pricing_monitoring).

%% API
-export([
    record_value_calculation/3,
    record_receipt_generation/2,
    record_billing_event/3,
    record_anomaly/3,
    record_error/3,
    get_health_status/0,
    get_metrics_snapshot/0,
    emit_trace_span/3
]).

-include("pricing_engine.hrl").

%%%===================================================================
%% Metrics
%%%===================================================================

%% @doc Record value calculation completion
-spec record_value_calculation(binary(), float(), non_neg_integer()) -> ok.
record_value_calculation(CustomerId, CalculatedValue, LatencyMs) ->
    %% Prometheus metrics
    prometheus_histogram:observe(
        pricing_calculation_latency_ms,
        LatencyMs
    ),
    prometheus_counter:inc(pricing_calculations_total),

    %% Update gauge for current value
    prometheus_gauge:set(pricing_current_value, CalculatedValue),

    %% OpenTelemetry span
    emit_trace_span(<<"value_calculation">>, #{
        <<"customer_id">> => CustomerId,
        <<"calculated_value">> => CalculatedValue,
        <<"latency_ms">> => LatencyMs
    }),

    %% Structured logging
    logger:info(
        "Value calculated",
        #{
            customer_id => CustomerId,
            calculated_value => CalculatedValue,
            latency_ms => LatencyMs,
            timestamp => erlang:system_time(millisecond)
        }
    ),

    ok.

%% @doc Record receipt generation
-spec record_receipt_generation(binary(), non_neg_integer()) -> ok.
record_receipt_generation(ReceiptHash, GenerationTimeMs) ->
    prometheus_histogram:observe(
        pricing_receipt_generation_ms,
        GenerationTimeMs
    ),
    prometheus_counter:inc(pricing_receipts_generated_total),

    logger:info(
        "Receipt generated",
        #{
            receipt_hash => ReceiptHash,
            generation_time_ms => GenerationTimeMs,
            timestamp => erlang:system_time(millisecond)
        }
    ),

    ok.

%% @doc Record billing event
-spec record_billing_event(atom(), binary(), map()) -> ok.
record_billing_event(EventType, CustomerId, Details) ->
    Status = maps:get(status, Details, unknown),

    case EventType of
        invoice_created ->
            prometheus_counter:inc(pricing_invoices_created_total);
        payment_succeeded ->
            prometheus_counter:inc(pricing_payments_succeeded_total),
            Amount = maps:get(amount, Details, 0.0),
            prometheus_counter:inc(pricing_revenue_usd, Amount);
        payment_failed ->
            prometheus_counter:inc(pricing_payments_failed_total);
        invoice_refunded ->
            prometheus_counter:inc(pricing_refunds_issued_total);
        _Other ->
            ok
    end,

    logger:info(
        io_lib:format("Billing event: ~p", [EventType]),
        #{
            event_type => EventType,
            customer_id => CustomerId,
            status => Status,
            details => Details,
            timestamp => erlang:system_time(millisecond)
        }
    ),

    ok.

%%%===================================================================
%% Anomaly Detection and Alerting
%%%===================================================================

%% @doc Record detected anomaly for alerting
-spec record_anomaly(atom(), binary(), map()) -> ok.
record_anomaly(AnomalyType, CustomerId, Details) ->
    prometheus_counter:inc(pricing_anomalies_detected_total),

    Severity = maps:get(severity, Details, warning),

    case Severity of
        critical ->
            prometheus_counter:inc(pricing_critical_anomalies),
            logger:critical(
                io_lib:format("Critical pricing anomaly: ~p", [AnomalyType]),
                #{
                    anomaly_type => AnomalyType,
                    customer_id => CustomerId,
                    severity => critical,
                    details => Details,
                    timestamp => erlang:system_time(millisecond)
                }
            );
        _ ->
            logger:warning(
                io_lib:format("Pricing anomaly detected: ~p", [AnomalyType]),
                #{
                    anomaly_type => AnomalyType,
                    customer_id => CustomerId,
                    severity => Severity,
                    details => Details,
                    timestamp => erlang:system_time(millisecond)
                }
            )
    end,

    %% Trigger alerting system for critical anomalies
    case Severity of
        critical -> trigger_alert(AnomalyType, CustomerId, Details);
        _ -> ok
    end,

    ok.

%% @doc Record error for analysis
-spec record_error(atom(), binary(), term()) -> ok.
record_error(ErrorType, CustomerId, ErrorReason) ->
    prometheus_counter:inc(pricing_errors_total),

    logger:error(
        io_lib:format("Pricing error: ~p", [ErrorType]),
        #{
            error_type => ErrorType,
            customer_id => CustomerId,
            reason => ErrorReason,
            timestamp => erlang:system_time(millisecond)
        }
    ),

    ok.

%%%===================================================================
%% Health Checks
%%%===================================================================

%% @doc Get overall system health status
-spec get_health_status() -> map().
get_health_status() ->
    TimestampMs = erlang:system_time(millisecond),
    CalculationSuccessRate = calculate_success_rate(),
    PaymentSuccessRate = calculate_payment_success_rate(),

    HealthStatus = case {CalculationSuccessRate >= 0.99, PaymentSuccessRate >= 0.95} of
        {true, true} -> healthy;
        {true, _} -> degraded;
        _ -> critical
    end,

    #{
        status => HealthStatus,
        timestamp => TimestampMs,
        calculation_success_rate => CalculationSuccessRate,
        payment_success_rate => PaymentSuccessRate,
        active_customers => get_active_customer_count(),
        pending_payments => get_pending_payment_count(),
        total_revenue => get_total_revenue()
    }.

%% @doc Get snapshot of current metrics
-spec get_metrics_snapshot() -> map().
get_metrics_snapshot() ->
    #{
        calculations_total => prometheus_counter:value(pricing_calculations_total),
        calculations_failed => prometheus_counter:value(pricing_errors_total),
        receipts_generated => prometheus_counter:value(pricing_receipts_generated_total),
        invoices_created => prometheus_counter:value(pricing_invoices_created_total),
        payments_succeeded => prometheus_counter:value(pricing_payments_succeeded_total),
        payments_failed => prometheus_counter:value(pricing_payments_failed_total),
        refunds_issued => prometheus_counter:value(pricing_refunds_issued_total),
        revenue_usd => prometheus_counter:value(pricing_revenue_usd),
        anomalies_detected => prometheus_counter:value(pricing_anomalies_detected_total),
        critical_anomalies => prometheus_counter:value(pricing_critical_anomalies),
        avg_calculation_latency_ms => prometheus_histogram:value(pricing_calculation_latency_ms),
        avg_receipt_generation_ms => prometheus_histogram:value(pricing_receipt_generation_ms)
    }.

%%%===================================================================
%% OpenTelemetry Integration
%%%===================================================================

%% @doc Emit trace span for distributed tracing
-spec emit_trace_span(binary(), map()) -> ok.
emit_trace_span(SpanName, Attributes) ->
    %% In production, use opentelemetry:start_span/2
    logger:debug(
        "TRACE_SPAN",
        #{
            span_name => SpanName,
            attributes => Attributes,
            timestamp => erlang:system_time(millisecond)
        }
    ),
    ok.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% Calculate value calculation success rate
calculate_success_rate() ->
    Total = prometheus_counter:value(pricing_calculations_total),
    Failed = prometheus_counter:value(pricing_errors_total),

    case Total of
        0 -> 1.0;
        _ -> (Total - Failed) / Total
    end.

%% Calculate payment success rate
calculate_payment_success_rate() ->
    Succeeded = prometheus_counter:value(pricing_payments_succeeded_total),
    Failed = prometheus_counter:value(pricing_payments_failed_total),
    Total = Succeeded + Failed,

    case Total of
        0 -> 1.0;
        _ -> Succeeded / Total
    end.

%% Get count of active customers
get_active_customer_count() ->
    %% Query from database (simplified)
    0.

%% Get count of pending payments
get_pending_payment_count() ->
    %% Query from database (simplified)
    0.

%% Get total revenue
get_total_revenue() ->
    prometheus_counter:value(pricing_revenue_usd).

%% Trigger alert for critical anomalies
trigger_alert(AnomalyType, CustomerId, Details) ->
    AlertMessage = io_lib:format(
        "CRITICAL PRICING ALERT: ~p for customer ~s",
        [AnomalyType, binary_to_list(CustomerId)]
    ),

    %% In production, integrate with alerting system
    %% - Send to Slack/PagerDuty
    %% - Create Jira incident
    %% - Notify ops team

    logger:critical(AlertMessage, #{
        anomaly_type => AnomalyType,
        customer_id => CustomerId,
        details => Details,
        alert_time => erlang:system_time(millisecond)
    }),

    ok.

%%%===================================================================
%% Prometheus Metric Registration
%%%===================================================================

%% Initialize Prometheus metrics (call once on startup)
ensure_metrics_registered() ->
    prometheus_histogram:new([
        {name, pricing_calculation_latency_ms},
        {help, "Latency of value calculations in milliseconds"},
        {buckets, [10, 50, 100, 500, 1000, 5000]}
    ]),

    prometheus_histogram:new([
        {name, pricing_receipt_generation_ms},
        {help, "Latency of receipt generation in milliseconds"},
        {buckets, [1, 5, 10, 50, 100]}
    ]),

    prometheus_counter:new([
        {name, pricing_calculations_total},
        {help, "Total number of value calculations"}
    ]),

    prometheus_counter:new([
        {name, pricing_receipts_generated_total},
        {help, "Total number of receipts generated"}
    ]),

    prometheus_counter:new([
        {name, pricing_invoices_created_total},
        {help, "Total number of invoices created"}
    ]),

    prometheus_counter:new([
        {name, pricing_payments_succeeded_total},
        {help, "Total number of successful payments"}
    ]),

    prometheus_counter:new([
        {name, pricing_payments_failed_total},
        {help, "Total number of failed payments"}
    ]),

    prometheus_counter:new([
        {name, pricing_refunds_issued_total},
        {help, "Total number of refunds issued"}
    ]),

    prometheus_counter:new([
        {name, pricing_revenue_usd},
        {help, "Total revenue collected in USD"}
    ]),

    prometheus_counter:new([
        {name, pricing_anomalies_detected_total},
        {help, "Total anomalies detected"}
    ]),

    prometheus_counter:new([
        {name, pricing_critical_anomalies},
        {help, "Total critical anomalies"}
    ]),

    prometheus_counter:new([
        {name, pricing_errors_total},
        {help, "Total errors encountered"}
    ]),

    prometheus_gauge:new([
        {name, pricing_current_value},
        {help, "Current calculated value"}
    ]),

    ok.
