%%%-------------------------------------------------------------------
%% @doc gcp_config: GCP configuration management
%%
%% Reads GCP configuration from environment variables and application config.
%% Provides unified configuration interface for all GCP services.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_config).

%% API
-export([get_project_id/0, get_region/0, get_zone/0]).
-export([is_firestore_enabled/0, is_pubsub_enabled/0, is_tracing_enabled/0]).
-export([get_firestore_database_id/0, get_pubsub_subscription/0]).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Get GCP project ID
-spec get_project_id() -> string().
get_project_id() ->
    case os:getenv("GCP_PROJECT_ID") of
        false ->
            case application:get_env(tai_autonomics, gcp_project_id) of
                undefined -> "local-dev";
                ProjectId when is_list(ProjectId) -> ProjectId;
                ProjectId when is_binary(ProjectId) -> binary_to_list(ProjectId)
            end;
        ProjectId -> ProjectId
    end.

%% @doc Get GCP region
-spec get_region() -> string().
get_region() ->
    case os:getenv("GCP_REGION") of
        false ->
            case application:get_env(tai_autonomics, gcp_region) of
                undefined -> "us-central1";
                Region when is_list(Region) -> Region;
                Region when is_binary(Region) -> binary_to_list(Region)
            end;
        Region -> Region
    end.

%% @doc Get GCP zone
-spec get_zone() -> string() | undefined.
get_zone() ->
    case os:getenv("GCP_ZONE") of
        false ->
            case application:get_env(tai_autonomics, gcp_zone) of
                undefined -> undefined;
                Zone when is_list(Zone) -> Zone;
                Zone when is_binary(Zone) -> binary_to_list(Zone)
            end;
        Zone -> Zone
    end.

%% @doc Check if Firestore is enabled
-spec is_firestore_enabled() -> boolean().
is_firestore_enabled() ->
    case os:getenv("FIRESTORE_ENABLED") of
        false ->
            application:get_env(tai_autonomics, firestore_enabled, true);
        "false" -> false;
        "0" -> false;
        _ -> true
    end.

%% @doc Check if Pub/Sub is enabled
-spec is_pubsub_enabled() -> boolean().
is_pubsub_enabled() ->
    case os:getenv("PUBSUB_ENABLED") of
        false ->
            application:get_env(tai_autonomics, pubsub_enabled, true);
        "false" -> false;
        "0" -> false;
        _ -> true
    end.

%% @doc Check if tracing is enabled
-spec is_tracing_enabled() -> boolean().
is_tracing_enabled() ->
    case os:getenv("TRACING_ENABLED") of
        false ->
            application:get_env(tai_autonomics, tracing_enabled, true);
        "false" -> false;
        "0" -> false;
        _ -> true
    end.

%% @doc Get Firestore database ID
-spec get_firestore_database_id() -> string().
get_firestore_database_id() ->
    case os:getenv("FIRESTORE_DATABASE_ID") of
        false ->
            case application:get_env(tai_autonomics, firestore_database_id) of
                undefined -> "(default)";
                DbId when is_list(DbId) -> DbId;
                DbId when is_binary(DbId) -> binary_to_list(DbId)
            end;
        DbId -> DbId
    end.

%% @doc Get Pub/Sub subscription name
-spec get_pubsub_subscription() -> string().
get_pubsub_subscription() ->
    case os:getenv("PUBSUB_SUBSCRIPTION") of
        false ->
            case application:get_env(tai_autonomics, pubsub_subscription) of
                undefined -> "erlang-autonomics-signals";
                Sub when is_list(Sub) -> Sub;
                Sub when is_binary(Sub) -> binary_to_list(Sub)
            end;
        Sub -> Sub
    end.
