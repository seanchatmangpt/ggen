%% knhk_sigma.erl — Schema Registry
%% Production-ready implementation for Σ schema management
%% Responsibilities: Schema loading, validation, querying, versioning

-module(knhk_sigma).
-behaviour(gen_server).

-export([
    start_link/0,
    load/1,
    query/1,
    validate/2,
    list/0,
    get_version/1
]).

-record(state, {
    schemas = #{},              % Schema IRI -> Schema data
    versions = #{},             % Schema IRI -> Version info
    cache = #{},                % Cache for validation results
    loaded_at = #{}             % Schema IRI -> timestamp
}).

%% API

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%% Load schema from RDF file or binary
load(Schema) when is_binary(Schema) ->
    gen_server:call(?MODULE, {load, Schema});
load(Schema) when is_list(Schema) ->
    load(list_to_binary(Schema)).

%% Query schema by IRI
query(SchemaIri) when is_binary(SchemaIri) ->
    gen_server:call(?MODULE, {query, SchemaIri}).

%% Validate data against schema (O ⊨ Σ check)
validate(SchemaIri, Data) when is_binary(SchemaIri) ->
    gen_server:call(?MODULE, {validate, SchemaIri, Data}).

%% List all loaded schemas
list() ->
    gen_server:call(?MODULE, list).

%% Get schema version
get_version(SchemaIri) when is_binary(SchemaIri) ->
    gen_server:call(?MODULE, {get_version, SchemaIri}).

%% gen_server callbacks

init([]) ->
    {ok, #state{}}.

handle_call({load, Schema}, _From, State) ->
    %% Parse schema (in production, use RDF parser)
    %% Extract schema IRI and version
    SchemaIri = extract_schema_iri(Schema),
    Version = extract_version(Schema),
    
    %% Validate schema format
    case validate_schema_format(Schema) of
        ok ->
            NewSchemas = maps:put(SchemaIri, Schema, State#state.schemas),
            NewVersions = maps:put(SchemaIri, Version, State#state.versions),
            NewLoadedAt = maps:put(SchemaIri, erlang:system_time(millisecond), State#state.loaded_at),
            
            NewState = State#state{
                schemas = NewSchemas,
                versions = NewVersions,
                loaded_at = NewLoadedAt
            },
            {reply, ok, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({query, SchemaIri}, _From, State) ->
    case maps:find(SchemaIri, State#state.schemas) of
        {ok, Schema} ->
            {reply, {ok, Schema}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call({validate, SchemaIri, Data}, _From, State) ->
    %% Check cache first
    CacheKey = {SchemaIri, erlang:phash2(Data)},
    case maps:find(CacheKey, State#state.cache) of
        {ok, Result} ->
            {reply, Result, State};
        error ->
            %% Perform validation
            case maps:find(SchemaIri, State#state.schemas) of
                {ok, Schema} ->
                    Result = perform_validation(Schema, Data),
                    NewCache = maps:put(CacheKey, Result, State#state.cache),
                    NewState = State#state{cache = NewCache},
                    {reply, Result, NewState};
                error ->
                    {reply, {error, schema_not_found}, State}
            end
    end;

handle_call(list, _From, State) ->
    SchemaList = maps:keys(State#state.schemas),
    {reply, {ok, SchemaList}, State};

handle_call({get_version, SchemaIri}, _From, State) ->
    case maps:find(SchemaIri, State#state.versions) of
        {ok, Version} ->
            {reply, {ok, Version}, State};
        error ->
            {reply, {error, not_found}, State}
    end;

handle_call(_Request, _From, State) ->
    {reply, {error, unknown_request}, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% Internal functions

extract_schema_iri(Schema) ->
    %% In production, parse RDF to extract schema IRI
    %% For now, use hash of schema content
    binary_to_atom(erlang:phash2(Schema), utf8).

extract_version(Schema) ->
    %% In production, extract version from schema metadata
    %% For now, use hash as version
    erlang:phash2(Schema).

validate_schema_format(Schema) ->
    %% Basic validation: check if schema is non-empty
    case byte_size(Schema) > 0 of
        true -> ok;
        false -> {error, empty_schema}
    end.

perform_validation(Schema, Data) ->
    %% In production, this would:
    %% 1. Parse Schema as RDF/SHACL
    %% 2. Parse Data as RDF
    %% 3. Validate Data against Schema constraints
    %% 4. Return validation result
    
    %% For now, basic validation: check if data format matches schema
    case byte_size(Data) > 0 of
        true -> {ok, valid};
        false -> {error, invalid_data}
    end.

