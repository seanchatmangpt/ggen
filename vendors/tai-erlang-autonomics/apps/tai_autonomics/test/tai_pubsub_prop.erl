%%%-------------------------------------------------------------------
%% @doc tai_pubsub_prop: Proper property tests for Pub/Sub ingress
%%
%% Fuzz envelope mutations; system must not crash.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_pubsub_prop).

-include_lib("proper/include/proper.hrl").

%% API
-export([prop_valid_envelope/0, prop_invalid_envelope/0]).

%%%===================================================================
%% Properties
%%%===================================================================

prop_valid_envelope() ->
    ?FORALL(Envelope, valid_envelope(),
        begin
            case tai_pubsub_ingress:handle_push(Envelope) of
                {ok, _Receipt} -> true;
                {error, _Reason} -> true;
                _ -> false
            end
        end).

prop_invalid_envelope() ->
    ?FORALL(Envelope, invalid_envelope(),
        begin
            case tai_pubsub_ingress:handle_push(Envelope) of
                {error, _Reason} -> true;
                {ok, Receipt} ->
                    %% Should still return refusal receipt
                    <<"refusal">> =:= maps:get(<<"type">>, Receipt, <<>>);
                _ -> false
            end
        end).

%%%===================================================================
%% Generators
%%%===================================================================

valid_envelope() ->
    ?LET({MessageId, Data},
        {binary(), base64_data()},
        #{
            <<"message">> => #{
                <<"messageId">> => MessageId,
                <<"data">> => Data
            }
        }
    ).

invalid_envelope() ->
    oneof([
        #{},
        #{<<"message">> => #{}},
        #{<<"message">> => #{<<"data">> => <<"not base64">>}},
        <<"not a map">>
    ]).

base64_data() ->
    ?LET(Json, json_data(),
        base64:encode(jsx:encode(Json))
    ).

json_data() ->
    ?LET({TenantId, Metric, Value},
        {binary(), binary(), float()},
        #{
            <<"tenant_id">> => TenantId,
            <<"metric">> => Metric,
            <<"value">> => Value,
            <<"timestamp_ms">> => integer()
        }
    ).
