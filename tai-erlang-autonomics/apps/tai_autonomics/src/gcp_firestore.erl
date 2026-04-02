%%%-------------------------------------------------------------------
%% @doc gcp_firestore: Firestore REST API client
%%
%% Provides Firestore document operations:
%% - Create/update documents
%% - Read documents
%% - Query documents
%% - Batch writes
%%
%% Supports both production Firestore and Firestore emulator for local development.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_firestore).
-behaviour(gen_server).

%% API
-export([start_link/0, create_document/3, update_document/3, get_document/2, delete_document/2]).
-export([batch_write/2, query/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(FIRESTORE_API_VERSION, "v1").
-define(FIRESTORE_TIMEOUT, 30000).  % 30 seconds

-record(state, {
    project_id :: string(),
    database_id :: string(),
    base_url :: string(),
    access_token :: string() | undefined,
    emulator_host :: string() | undefined
}).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the Firestore client
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Create a document in Firestore
-spec create_document(Collection, DocumentId, Data) -> {ok, Document} | {error, Reason}
    when Collection :: string(),
         DocumentId :: string(),
         Data :: map(),
         Document :: map(),
         Reason :: term().
create_document(Collection, DocumentId, Data) ->
    gen_server:call(?SERVER, {create_document, Collection, DocumentId, Data}, ?FIRESTORE_TIMEOUT).

%% @doc Update a document in Firestore
-spec update_document(Collection, DocumentId, Data) -> {ok, Document} | {error, Reason}
    when Collection :: string(),
         DocumentId :: string(),
         Data :: map(),
         Document :: map(),
         Reason :: term().
update_document(Collection, DocumentId, Data) ->
    gen_server:call(?SERVER, {update_document, Collection, DocumentId, Data}, ?FIRESTORE_TIMEOUT).

%% @doc Get a document from Firestore
-spec get_document(Collection, DocumentId) -> {ok, Document} | {error, Reason}
    when Collection :: string(),
         DocumentId :: string(),
         Document :: map(),
         Reason :: term().
get_document(Collection, DocumentId) ->
    gen_server:call(?SERVER, {get_document, Collection, DocumentId}, ?FIRESTORE_TIMEOUT).

%% @doc Delete a document from Firestore
-spec delete_document(Collection, DocumentId) -> ok | {error, Reason}
    when Collection :: string(),
         DocumentId :: string(),
         Reason :: term().
delete_document(Collection, DocumentId) ->
    gen_server:call(?SERVER, {delete_document, Collection, DocumentId}, ?FIRESTORE_TIMEOUT).

%% @doc Batch write documents
-spec batch_write(Collection, Documents) -> {ok, Results} | {error, Reason}
    when Collection :: string(),
         Documents :: [{DocumentId, Data}],
         DocumentId :: string(),
         Data :: map(),
         Results :: [map()],
         Reason :: term().
batch_write(Collection, Documents) ->
    gen_server:call(?SERVER, {batch_write, Collection, Documents}, ?FIRESTORE_TIMEOUT).

%% @doc Query documents
-spec query(Collection, Query) -> {ok, Documents} | {error, Reason}
    when Collection :: string(),
         Query :: map(),
         Documents :: [map()],
         Reason :: term().
query(Collection, Query) ->
    gen_server:call(?SERVER, {query, Collection, Query}, ?FIRESTORE_TIMEOUT).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Get project ID from config
    ProjectId = gcp_config:get_project_id(),
    
    %% Get database ID (default: "(default)")
    DatabaseId = gcp_config:get_firestore_database_id(),
    
    %% Check for emulator
    EmulatorHost = case os:getenv("FIRESTORE_EMULATOR_HOST") of
        false -> undefined;
        Host -> Host
    end,
    
    %% Build base URL
    BaseUrl = case EmulatorHost of
        undefined ->
            "https://firestore.googleapis.com/" ++ ?FIRESTORE_API_VERSION ++
            "/projects/" ++ ProjectId ++ "/databases/" ++ DatabaseId;
        EmulatorHostValue ->
            "http://" ++ EmulatorHostValue ++ "/v1/projects/" ++ ProjectId ++ "/databases/" ++ DatabaseId
    end,
    
    State = #state{
        project_id = ProjectId,
        database_id = DatabaseId,
        base_url = BaseUrl,
        emulator_host = EmulatorHost
    },
    
    {ok, State}.

handle_call({create_document, Collection, DocumentId, Data}, _From, State) ->
    %% Check for failure injection
    case gcp_failure_wrapper:check_firestore_failure(write) of
        {should_fail, Type, Reason} ->
            logger:warning("Firestore write failure injected: ~p - ~p", [Type, Reason]),
            {reply, {error, Reason}, State};
        should_succeed ->
            case ensure_access_token(State) of
                {ok, NewState} ->
                    Result = firestore_create(NewState, Collection, DocumentId, Data),
                    {reply, Result, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({update_document, Collection, DocumentId, Data}, _From, State) ->
    case ensure_access_token(State) of
        {ok, NewState} ->
            Result = firestore_update(NewState, Collection, DocumentId, Data),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({get_document, Collection, DocumentId}, _From, State) ->
    case ensure_access_token(State) of
        {ok, NewState} ->
            Result = firestore_get(NewState, Collection, DocumentId),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({delete_document, Collection, DocumentId}, _From, State) ->
    case ensure_access_token(State) of
        {ok, NewState} ->
            Result = firestore_delete(NewState, Collection, DocumentId),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({batch_write, Collection, Documents}, _From, State) ->
    case ensure_access_token(State) of
        {ok, NewState} ->
            Result = firestore_batch_write(NewState, Collection, Documents),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({query, Collection, Query}, _From, State) ->
    case ensure_access_token(State) of
        {ok, NewState} ->
            Result = firestore_query(NewState, Collection, Query),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

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
%% Internal functions
%%%===================================================================

-spec ensure_access_token(State) -> {ok, #state{}} | {error, term()}
    when State :: #state{}.
ensure_access_token(State) ->
    case State#state.emulator_host of
        undefined ->
            %% Production: get access token
            case whereis(gcp_metadata) of
                undefined ->
                    {error, gcp_metadata_not_started};
                _Pid ->
                    case gcp_metadata:get_access_token(firestore) of
                        {ok, Token} ->
                            {ok, State#state{access_token = Token}};
                        {error, Reason} ->
                            {error, Reason}
                    end
            end;
        _ ->
            %% Emulator: no token needed
            {ok, State}
    end.

-spec firestore_create(State, Collection, DocumentId, Data) -> {ok, map()} | {error, term()}
    when State :: #state{},
         Collection :: string(),
         DocumentId :: string(),
         Data :: map().
firestore_create(State, Collection, DocumentId, Data) ->
    Path = State#state.base_url ++ "/documents/" ++ Collection ++ "/" ++ DocumentId,
    
    %% Convert Erlang map to Firestore document format
    FirestoreDoc = map_to_firestore_document(Data),
    RequestBody = jsx:encode(FirestoreDoc),
    
    Headers = build_headers(State),
    
    case httpc:request(put, {Path, Headers, "application/json", RequestBody}, [{timeout, ?FIRESTORE_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Doc when is_map(Doc) -> {ok, Doc};
                _ -> {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec firestore_update(State, Collection, DocumentId, Data) -> {ok, map()} | {error, term()}
    when State :: #state{},
         Collection :: string(),
         DocumentId :: string(),
         Data :: map().
firestore_update(State, Collection, DocumentId, Data) ->
    %% Update uses PATCH with updateMask
    Path = State#state.base_url ++ "/documents/" ++ Collection ++ "/" ++ DocumentId,
    
    FirestoreDoc = map_to_firestore_document(Data),
    RequestBody = jsx:encode(FirestoreDoc),
    
    Headers = build_headers(State),
    
    case httpc:request(patch, {Path ++ "?updateMask.fieldPaths=*", Headers, "application/json", RequestBody}, [{timeout, ?FIRESTORE_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Doc when is_map(Doc) -> {ok, Doc};
                _ -> {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec firestore_get(State, Collection, DocumentId) -> {ok, map()} | {error, term()}
    when State :: #state{},
         Collection :: string(),
         DocumentId :: string().
firestore_get(State, Collection, DocumentId) ->
    Path = State#state.base_url ++ "/documents/" ++ Collection ++ "/" ++ DocumentId,
    Headers = build_headers(State),
    
    case httpc:request(get, {Path, Headers}, [{timeout, ?FIRESTORE_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Doc when is_map(Doc) -> {ok, Doc};
                _ -> {error, invalid_response}
            end;
        {ok, {{_, 404, _}, _, _}} ->
            {error, not_found};
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec firestore_delete(State, Collection, DocumentId) -> ok | {error, term()}
    when State :: #state{},
         Collection :: string(),
         DocumentId :: string().
firestore_delete(State, Collection, DocumentId) ->
    Path = State#state.base_url ++ "/documents/" ++ Collection ++ "/" ++ DocumentId,
    Headers = build_headers(State),
    
    case httpc:request(delete, {Path, Headers}, [{timeout, ?FIRESTORE_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {{_, Status, _}, _, _}} -> {error, {http_error, Status}};
        {error, Reason} -> {error, Reason}
    end.

-spec firestore_batch_write(State, Collection, Documents) -> {ok, [map()]} | {error, term()}
    when State :: #state{},
         Collection :: string(),
         Documents :: [{string(), map()}].
firestore_batch_write(State, Collection, Documents) ->
    %% Firestore batch write uses commit API
    Path = State#state.base_url ++ ":commit",
    
    Writes = lists:map(fun({DocumentId, Data}) ->
        #{
            <<"update">> => #{
                <<"name">> => list_to_binary(State#state.base_url ++ "/documents/" ++ Collection ++ "/" ++ DocumentId),
                <<"fields">> => map_to_firestore_fields(Data)
            }
        }
    end, Documents),
    
    RequestBody = jsx:encode(#{
        <<"writes">> => Writes
    }),
    
    Headers = build_headers(State),
    
    case httpc:request(post, {Path, Headers, "application/json", RequestBody}, [{timeout, ?FIRESTORE_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"writeResults">> := Results} -> {ok, Results};
                _ -> {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec firestore_query(State, Collection, Query) -> {ok, [map()]} | {error, term()}
    when State :: #state{},
         Collection :: string(),
         Query :: map().
firestore_query(State, Collection, Query) ->
    %% Firestore query uses runQuery API
    Path = State#state.base_url ++ ":runQuery",
    
    %% Build structured query
    StructuredQuery = #{
        <<"from">> => [#{<<"collectionId">> => list_to_binary(Collection)}]
    },
    
    RequestBody = jsx:encode(#{
        <<"structuredQuery">> => StructuredQuery
    }),
    
    Headers = build_headers(State),
    
    case httpc:request(post, {Path, Headers, "application/json", RequestBody}, [{timeout, ?FIRESTORE_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                Results when is_list(Results) ->
                    Documents = lists:filtermap(fun(Result) ->
                        case Result of
                            #{<<"document">> := Doc} -> {true, Doc};
                            _ -> false
                        end
                    end, Results),
                    {ok, Documents};
                _ -> {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec build_headers(State) -> [{string(), string()}]
    when State :: #state{}.
build_headers(State) ->
    BaseHeaders = [
        {"Content-Type", "application/json"},
        {"Accept", "application/json"}
    ],
    case State#state.emulator_host of
        undefined ->
            case State#state.access_token of
                undefined -> BaseHeaders;
                Token -> [{"Authorization", "Bearer " ++ Token} | BaseHeaders]
            end;
        _ ->
            BaseHeaders
    end.

-spec map_to_firestore_document(Data) -> map()
    when Data :: map().
map_to_firestore_document(Data) ->
    #{
        <<"fields">> => map_to_firestore_fields(Data)
    }.

-spec map_to_firestore_fields(Data) -> map()
    when Data :: map().
map_to_firestore_fields(Data) ->
    maps:fold(fun(Key, Value, Acc) ->
        KeyBin = case is_binary(Key) of
            true -> Key;
            false -> list_to_binary(io_lib:format("~p", [Key]))
        end,
        Acc#{KeyBin => erlang_value_to_firestore_value(Value)}
    end, #{}, Data).

-spec erlang_value_to_firestore_value(Value) -> map()
    when Value :: term().
erlang_value_to_firestore_value(Value) when is_binary(Value) ->
    #{<<"stringValue">> => Value};
erlang_value_to_firestore_value(Value) when is_integer(Value) ->
    #{<<"integerValue">> => integer_to_binary(Value)};
erlang_value_to_firestore_value(Value) when is_float(Value) ->
    #{<<"doubleValue">> => float_to_binary(Value)};
erlang_value_to_firestore_value(Value) when is_boolean(Value) ->
    #{<<"booleanValue">> => Value};
erlang_value_to_firestore_value(Value) when is_map(Value) ->
    #{<<"mapValue">> => #{
        <<"fields">> => map_to_firestore_fields(Value)
    }};
erlang_value_to_firestore_value(Value) when is_list(Value) ->
    ArrayValues = lists:map(fun erlang_value_to_firestore_value/1, Value),
    #{<<"arrayValue">> => #{<<"values">> => ArrayValues}};
erlang_value_to_firestore_value(null) ->
    #{<<"nullValue">> => null};
erlang_value_to_firestore_value(Value) ->
    %% Default: convert to string
    #{<<"stringValue">> => list_to_binary(io_lib:format("~p", [Value]))}.
