%%%-------------------------------------------------------------------
%%% @doc Entitlement Tool - Apply entitlement events and state transitions
%%%
%%% Tool: taiea.entitlement.apply_event
%%% Input: {tenant_id, event_type, event_data}
%%% Output: {ok, DecisionResponse, Receipt}
%%%
%%% Applies entitlement events (provision, deprovision, suspend, resume)
%%% Calls governor for deterministic decision
%%% Emits receipt with decision and metadata
%%%
%%% Event types:
%%% - "provision" - Create new entitlement
%%% - "deprovision" - Remove entitlement
%%% - "suspend" - Temporary suspension
%%% - "resume" - Reactivate from suspension
%%% - "modify" - Modify entitlement configuration
%%%
%%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_entitlement).

%% API
-export([handle/1]).

%% Types
-type tenant_id() :: binary().
-type event_type() :: binary().
-type event_data() :: map().
-type decision() :: binary().
-type tool_response() :: map().
-type receipt() :: map().

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Handle entitlement event application
-spec handle(map()) -> {ok, tool_response(), receipt()} | {error, term()}.
handle(Input) ->
    DefaultTenantId = maps:get(<<"tenant_id">>, Input, <<"unknown">>),
    DefaultEventType = maps:get(<<"event_type">>, Input, <<"unknown">>),

    try
        %% Validate parameters
        ActualTenantId = maps:get(<<"tenant_id">>, Input),
        ActualEventType = maps:get(<<"event_type">>, Input),
        EventData = maps:get(<<"event_data">>, Input, #{}),

        %% Validate event type
        case validate_event_type(ActualEventType) of
            ok ->
                %% Make governor decision (stub for now)
                Decision = get_governor_decision(ActualTenantId, ActualEventType, EventData),

                %% Build response
                Response = #{
                    decision => Decision,
                    tenant_id => ActualTenantId,
                    event_type => ActualEventType,
                    timestamp => timestamp(),
                    message => <<"Entitlement event processed">>,
                    metadata => #{
                        applied => true,
                        event_data => EventData
                    }
                },

                %% Emit receipt
                ToolReceipt = emit_tool_receipt(ActualTenantId, ActualEventType, Decision, Response),

                {ok, Response, ToolReceipt};

            {error, ValidationError} ->
                ErrorReceipt = emit_error_receipt(ActualTenantId, ActualEventType, ValidationError),
                {error, {validation_failed, ValidationError, ErrorReceipt}}
        end
    catch
        Class:Reason ->
            CatchErrorReceipt = emit_error_receipt(DefaultTenantId, DefaultEventType, {Class, Reason}),
            {error, {entitlement_error, Class, Reason, CatchErrorReceipt}}
    end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @private Validate event type
-spec validate_event_type(binary()) -> ok | {error, term()}.
validate_event_type(EventType) ->
    ValidTypes = [
        <<"provision">>,
        <<"deprovision">>,
        <<"suspend">>,
        <<"resume">>,
        <<"modify">>
    ],

    case lists:member(EventType, ValidTypes) of
        true -> ok;
        false -> {error, {invalid_event_type, EventType}}
    end.

%% @private Get decision from entitlement governor
%% This is a stub that returns "allowed" for now
%% Agent 7 will integrate with actual governor
-spec get_governor_decision(tenant_id(), event_type(), event_data()) -> decision().
get_governor_decision(_TenantId, _EventType, _EventData) ->
    %% STUB: Agent 7 will replace this with actual governor call
    %% For now, always allow decisions
    <<"allowed">>.

%% @private Emit receipt for successful entitlement event
-spec emit_tool_receipt(tenant_id(), event_type(), decision(), tool_response()) -> receipt().
emit_tool_receipt(TenantId, EventType, Decision, Response) ->
    #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tool => <<"taiea.entitlement.apply_event">>,
        event => <<"entitlement_event_applied">>,
        status => case Decision of
            <<"allowed">> -> <<"success">>;
            <<"denied">> -> <<"denied">>
        end,
        tenant_id => TenantId,
        event_type => EventType,
        decision => Decision,
        message => format_decision_message(Decision, EventType),
        metadata => #{
            response => Response,
            node => node(),
            process => self()
        }
    }.

%% @private Emit receipt for error during entitlement event
-spec emit_error_receipt(tenant_id(), event_type(), term()) -> receipt().
emit_error_receipt(TenantId, EventType, Error) ->
    #{
        id => generate_receipt_id(),
        timestamp => timestamp(),
        tool => <<"taiea.entitlement.apply_event">>,
        event => <<"entitlement_event_failed">>,
        status => <<"error">>,
        tenant_id => TenantId,
        event_type => EventType,
        error => #{
            reason => format_error(Error),
            details => error_details(Error)
        },
        message => <<"Entitlement event failed">>,
        metadata => #{
            node => node()
        }
    }.

%% @private Format decision message
-spec format_decision_message(decision(), event_type()) -> binary().
format_decision_message(<<"allowed">>, EventType) ->
    <<
        "Entitlement event '",
        EventType/binary,
        "' was approved by governor"
    >>;
format_decision_message(<<"denied">>, EventType) ->
    <<
        "Entitlement event '",
        EventType/binary,
        "' was denied by governor"
    >>.

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
error_details({invalid_event_type, EventType}) ->
    #{
        type => <<"validation_error">>,
        field => <<"event_type">>,
        value => EventType,
        message => <<"Invalid event type">>
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
