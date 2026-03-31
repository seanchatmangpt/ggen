%%%-------------------------------------------------------------------
%% @doc gcp_metadata: GCP Metadata Server client for authentication
%%
%% Provides access to GCP metadata server for:
%% - Access tokens (for authenticating to GCP services)
%% - Project metadata (project ID, zone, region)
%% - Service account information
%%
%% Supports both production (metadata server) and local development
%% (environment variables, emulators).
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_metadata).
-behaviour(gen_server).

%% API
-export([start_link/0, get_access_token/0, get_access_token/1, get_project_id/0, get_zone/0, get_region/0]).
-export([is_gcp_environment/0, is_cloud_run/0, is_gce/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(METADATA_SERVER_URL, "http://metadata.google.internal").
-define(METADATA_FLAVOR_HEADER, "Metadata-Flavor: Google").
-define(TOKEN_CACHE_TTL, 3000).  % 50 minutes (tokens expire in 1 hour)

-record(state, {
    project_id :: string() | undefined,
    zone :: string() | undefined,
    region :: string() | undefined,
    access_token :: string() | undefined,
    token_expires_at :: integer() | undefined,
    is_gcp :: boolean(),
    is_cloud_run :: boolean(),
    is_gce :: boolean()
}).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the GCP metadata client
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Get access token for GCP services
-spec get_access_token() -> {ok, string()} | {error, term()}.
get_access_token() ->
    get_access_token(default).

%% @doc Get access token for specific scope
-spec get_access_token(Scope) -> {ok, string()} | {error, term()}
    when Scope :: default | atom().
get_access_token(Scope) ->
    gen_server:call(?SERVER, {get_access_token, Scope}).

%% @doc Get GCP project ID
-spec get_project_id() -> {ok, string()} | {error, term()}.
get_project_id() ->
    gen_server:call(?SERVER, get_project_id).

%% @doc Get GCP zone
-spec get_zone() -> {ok, string()} | {error, term()}.
get_zone() ->
    gen_server:call(?SERVER, get_zone).

%% @doc Get GCP region
-spec get_region() -> {ok, string()} | {error, term()}.
get_region() ->
    gen_server:call(?SERVER, get_region).

%% @doc Check if running in GCP environment
-spec is_gcp_environment() -> boolean().
is_gcp_environment() ->
    gen_server:call(?SERVER, is_gcp_environment).

%% @doc Check if running on Cloud Run
-spec is_cloud_run() -> boolean().
is_cloud_run() ->
    gen_server:call(?SERVER, is_cloud_run).

%% @doc Check if running on GCE
-spec is_gce() -> boolean().
is_gce() ->
    gen_server:call(?SERVER, is_gce).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Detect GCP environment
    IsCloudRun = detect_cloud_run(),
    IsGCE = detect_gce(),
    IsGCP = IsCloudRun orelse IsGCE,

    %% Get project metadata
    ProjectId = get_project_id_from_env(),
    Zone = get_zone_from_env(),
    Region = get_region_from_env(),

    State = #state{
        project_id = ProjectId,
        zone = Zone,
        region = Region,
        is_gcp = IsGCP,
        is_cloud_run = IsCloudRun,
        is_gce = IsGCE
    },

    {ok, State}.

handle_call({get_access_token, Scope}, _From, State) ->
    %% Check for failure injection
    case gcp_failure_wrapper:check_metadata_failure(token_refresh) of
        {should_fail, Type, Reason} ->
            logger:warning("Metadata token refresh failure injected: ~p - ~p", [Type, Reason]),
            {reply, {error, Reason}, State};
        should_succeed ->
            handle_get_access_token(Scope, State)
    end;

handle_call(get_project_id, _From, State) ->
    case State#state.project_id of
        undefined ->
            case fetch_project_id() of
                {ok, ProjectId} ->
                    NewState = State#state{project_id = ProjectId},
                    {reply, {ok, ProjectId}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        ProjectId ->
            {reply, {ok, ProjectId}, State}
    end;

handle_call(get_zone, _From, State) ->
    case State#state.zone of
        undefined ->
            case fetch_zone() of
                {ok, Zone} ->
                    NewState = State#state{zone = Zone},
                    {reply, {ok, Zone}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        Zone ->
            {reply, {ok, Zone}, State}
    end;

handle_call(get_region, _From, State) ->
    case State#state.region of
        undefined ->
            case fetch_region(State) of
                {ok, Region} ->
                    NewState = State#state{region = Region},
                    {reply, {ok, Region}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end;
        Region ->
            {reply, {ok, Region}, State}
    end;

handle_call(is_gcp_environment, _From, State) ->
    {reply, State#state.is_gcp, State};

handle_call(is_cloud_run, _From, State) ->
    {reply, State#state.is_cloud_run, State};

handle_call(is_gce, _From, State) ->
    {reply, State#state.is_gce, State};

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%% Internal helper functions
%%%===================================================================

-spec handle_get_access_token(Scope, State) -> {reply, {ok, string()} | {error, term()}, State}
    when Scope :: atom(),
         State :: #state{}.
handle_get_access_token(Scope, State) ->
    Now = erlang:system_time(second),
    case State#state.access_token =/= undefined
         andalso State#state.token_expires_at =/= undefined
         andalso Now < (State#state.token_expires_at - 60) of
        true ->
            {reply, {ok, State#state.access_token}, State};
        false ->
            case fetch_access_token(Scope, State) of
                {ok, Token, ExpiresAt} ->
                    NewState = State#state{
                        access_token = Token,
                        token_expires_at = ExpiresAt
                    },
                    {reply, {ok, Token}, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end.

%%%===================================================================
%% Internal functions
%%%===================================================================

-spec detect_cloud_run() -> boolean().
detect_cloud_run() ->
    case os:getenv("K_SERVICE") of
        false -> false;
        _ -> true
    end.

-spec detect_gce() -> boolean().
detect_gce() ->
    case os:getenv("GOOGLE_CLOUD_PROJECT") of
        false ->
            %% Try metadata server
            case httpc:request(get, {?METADATA_SERVER_URL ++ "/computeMetadata/v1/instance/id", [{"Metadata-Flavor", "Google"}]}, [], []) of
                {ok, {{_, 200, _}, _, _}} -> true;
                _ -> false
            end;
        _ -> true
    end.

-spec get_project_id_from_env() -> string() | undefined.
get_project_id_from_env() ->
    case os:getenv("GCP_PROJECT_ID") of
        false ->
            case os:getenv("GOOGLE_CLOUD_PROJECT") of
                false -> undefined;
                ProjectId -> ProjectId
            end;
        ProjectId -> ProjectId
    end.

-spec get_zone_from_env() -> string() | undefined.
get_zone_from_env() ->
    case os:getenv("GCP_ZONE") of
        false -> undefined;
        Zone -> Zone
    end.

-spec get_region_from_env() -> string() | undefined.
get_region_from_env() ->
    case os:getenv("GCP_REGION") of
        false -> undefined;
        Region -> Region
    end.

-spec fetch_project_id() -> {ok, string()} | {error, term()}.
fetch_project_id() ->
    case os:getenv("GCP_PROJECT_ID") of
        false ->
            case os:getenv("GOOGLE_CLOUD_PROJECT") of
                false ->
                    case metadata_request("/computeMetadata/v1/project/project-id") of
                        {ok, ProjectId} -> {ok, ProjectId};
                        {error, Reason} -> {error, Reason}
                    end;
                ProjectId -> {ok, ProjectId}
            end;
        ProjectId -> {ok, ProjectId}
    end.

-spec fetch_zone() -> {ok, string()} | {error, term()}.
fetch_zone() ->
    case os:getenv("GCP_ZONE") of
        false ->
            case metadata_request("/computeMetadata/v1/instance/zone") of
                {ok, ZoneFull} ->
                    %% Zone format: "projects/123456789/zones/us-central1-a"
                    Parts = string:split(ZoneFull, "/", all),
                    Zone = lists:last(Parts),
                    {ok, Zone};
                {error, Reason} -> {error, Reason}
            end;
        Zone -> {ok, Zone}
    end.

-spec fetch_region(State) -> {ok, string()} | {error, term()}
    when State :: #state{}.
fetch_region(State) ->
    case State#state.zone of
        undefined ->
            case fetch_zone() of
                {ok, Zone} ->
                    %% Extract region from zone (e.g., "us-central1-a" -> "us-central1")
                    Region = extract_region_from_zone(Zone),
                    {ok, Region};
                {error, Reason} -> {error, Reason}
            end;
        Zone ->
            Region = extract_region_from_zone(Zone),
            {ok, Region}
    end.

-spec extract_region_from_zone(Zone) -> string()
    when Zone :: string().
extract_region_from_zone(Zone) ->
    %% Zone format: "us-central1-a" -> region: "us-central1"
    case string:split(Zone, "-", all) of
        [Part1, Part2 | _] ->
            Part1 ++ "-" ++ Part2;
        _ ->
            %% Fallback: try to get from Cloud Run environment
            case os:getenv("GCP_REGION") of
                false -> "us-central1";  % Default
                Region -> Region
            end
    end.

-spec fetch_access_token(Scope, State) -> {ok, string(), integer()} | {error, term()}
    when Scope :: atom(),
         State :: #state{}.
fetch_access_token(Scope, State) ->
    case State#state.is_gcp of
        true ->
            %% Use metadata server
            ScopeStr = scope_to_string(Scope),
            Path = "/computeMetadata/v1/instance/service-accounts/default/token?scopes=" ++ ScopeStr,
            case metadata_request(Path) of
                {ok, JsonBin} ->
                    case jsx:decode(JsonBin, [return_maps]) of
                        #{<<"access_token">> := Token, <<"expires_in">> := ExpiresIn} ->
                            ExpiresAt = erlang:system_time(second) + ExpiresIn,
                            {ok, binary_to_list(Token), ExpiresAt};
                        _ ->
                            {error, invalid_token_response}
                    end;
                {error, Reason} ->
                    {error, Reason}
            end;
        false ->
            %% Local development: use application default credentials or emulator
            case os:getenv("GOOGLE_APPLICATION_CREDENTIALS") of
                false ->
                    %% Emulator mode: return placeholder token
                    {ok, "emulator-token", erlang:system_time(second) + 3600};
                _CredsFile ->
                    %% TODO: Load service account JSON and generate token
                    %% For now, return emulator token
                    {ok, "emulator-token", erlang:system_time(second) + 3600}
            end
    end.

-spec scope_to_string(Scope) -> string()
    when Scope :: atom().
scope_to_string(default) ->
    "https://www.googleapis.com/auth/cloud-platform";
scope_to_string(firestore) ->
    "https://www.googleapis.com/auth/datastore";
scope_to_string(pubsub) ->
    "https://www.googleapis.com/auth/pubsub";
scope_to_string(Scope) when is_atom(Scope) ->
    %% Default to cloud-platform scope
    "https://www.googleapis.com/auth/cloud-platform".

-spec metadata_request(Path) -> {ok, binary()} | {error, term()}
    when Path :: string().
metadata_request(Path) ->
    URL = ?METADATA_SERVER_URL ++ Path,
    Headers = [{"Metadata-Flavor", "Google"}],
    case httpc:request(get, {URL, Headers}, [{timeout, 5000}, {connect_timeout, 3000}], []) of
        {ok, {{_, 200, _}, _, Body}} ->
            {ok, list_to_binary(Body)};
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, {connect_timeout, _}} ->
            {error, connection_timeout};
        {error, {timeout, _}} ->
            {error, request_timeout};
        {error, Reason} ->
            {error, Reason}
    end.
