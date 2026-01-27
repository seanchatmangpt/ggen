%%%-------------------------------------------------------------------
%% @doc TAIEA Support Model Tool - Support model description and configuration
%%
%% Implements the taiea.support.model MCP tool.
%% - Accepts: empty input map
%% - Returns: {ok, Response, Receipt} with support model information
%% - Emits: Receipt documenting support model query
%%
%% Chicago School TDD: State-based, real collaborators, AAA pattern
%% @end
%%%-------------------------------------------------------------------
-module(taiea_tool_support).

%% API
-export([handle/1]).

%% Types
-type tool_input() :: map().
-type tool_response() :: map().
-type receipt() :: map().

%%%===================================================================
%% API
%%%===================================================================

%% @doc Handle taiea.support.model tool invocation
%% Arrange: Input map (may be empty)
%% Act: Load support model configuration
%% Assert: Return response with support model info and receipt
-spec handle(tool_input()) -> {ok, tool_response(), receipt()}.
handle(_Input) ->
    try
        %% Load support model configuration
        SupportModel = load_support_model(),

        %% Build response
        Response = #{
            support_model => maps:get(model_type, SupportModel),
            tiers => maps:get(tiers, SupportModel),
            capabilities => maps:get(capabilities, SupportModel),
            slo => maps:get(slo, SupportModel),
            timestamp => erlang:system_time(millisecond),
            message => <<"Support model information retrieved successfully">>
        },

        %% Generate receipt
        Receipt = emit_receipt(Response),

        {ok, Response, Receipt}
    catch
        Class:Reason ->
            %% Safe error handling - never crash tool handler
            ErrorReceipt = emit_error_receipt(Class, Reason),
            ErrorResponse = #{
                support_model => <<"unknown">>,
                message => erlang:atom_to_binary(Class, utf8),
                error_details => format_error(Class, Reason)
            },
            {ok, ErrorResponse, ErrorReceipt}
    end.

%%%===================================================================
%% Internal Functions
%%%===================================================================

%% @private Load support model configuration
-spec load_support_model() -> map().
load_support_model() ->
    #{
        model_type => <<"tiered_enterprise">>,
        version => <<"1.0.0">>,
        description => <<"Enterprise support with tiered SLA commitments">>,
        tiers => [
            #{
                name => <<"basic">>,
                response_time_minutes => 480,
                availability_percent => 99.0,
                features => [
                    <<"email_support">>,
                    <<"community_forum">>,
                    <<"documentation">>
                ]
            },
            #{
                name => <<"professional">>,
                response_time_minutes => 120,
                availability_percent => 99.5,
                features => [
                    <<"email_support">>,
                    <<"phone_support">>,
                    <<"dedicated_account_manager">>,
                    <<"priority_queue">>
                ]
            },
            #{
                name => <<"enterprise">>,
                response_time_minutes => 15,
                availability_percent => 99.99,
                features => [
                    <<"24_7_phone_support">>,
                    <<"dedicated_engineering">>,
                    <<"custom_sla">>,
                    <<"quarterly_reviews">>,
                    <<"training_programs">>
                ]
            }
        ],
        capabilities => #{
            incident_tracking => true,
            knowledge_base => true,
            api_access => true,
            custom_integrations => true,
            sso_support => true,
            audit_logging => true
        },
        slo => #{
            target_availability => 99.99,
            max_recovery_time_hours => 1,
            incident_severity_levels => [<<"critical">>, <<"high">>, <<"medium">>, <<"low">>]
        }
    }.

%% @private Emit receipt for successful support model query
-spec emit_receipt(map()) -> receipt().
emit_receipt(_Response) ->
    #{
        id => generate_receipt_id(),
        timestamp => erlang:system_time(millisecond),
        event => <<"support_model_query">>,
        status => <<"success">>,
        tool => <<"taiea.support.model">>,
        message => <<"Support model information retrieved">>,
        metadata => #{
            version => <<"1.0.0">>,
            handler => <<"taiea_tool_support">>,
            timestamp_sec => erlang:system_time(second)
        }
    }.

%% @private Emit receipt for support model error
-spec emit_error_receipt(atom(), term()) -> receipt().
emit_error_receipt(Class, Reason) ->
    #{
        id => generate_receipt_id(),
        timestamp => erlang:system_time(millisecond),
        event => <<"support_model_error">>,
        status => <<"error">>,
        tool => <<"taiea.support.model">>,
        message => format_error(Class, Reason),
        metadata => #{
            error_class => Class,
            version => <<"1.0.0">>
        }
    }.

%% @private Generate unique receipt ID
-spec generate_receipt_id() -> binary().
generate_receipt_id() ->
    base64:encode(crypto:strong_rand_bytes(16)).

%% @private Format error message
-spec format_error(atom(), term()) -> binary().
format_error(Class, Reason) ->
    Msg = io_lib:format("~w:~w", [Class, Reason]),
    erlang:list_to_binary(Msg).

%%%===================================================================
%% End of module
%%%===================================================================
