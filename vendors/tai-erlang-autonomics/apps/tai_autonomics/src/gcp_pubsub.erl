%%%-------------------------------------------------------------------
%% @doc gcp_pubsub: Google Cloud Pub/Sub REST API client
%%
%% Provides Pub/Sub operations:
%% - Publish messages
%% - Batch publish
%% - Pull messages (for subscriptions)
%%
%% Supports both production Pub/Sub and Pub/Sub emulator for local development.
%%
%% @end
%%%-------------------------------------------------------------------
-module(gcp_pubsub).
-behaviour(gen_server).

%% API
-export([start_link/0, publish/2, publish/3, publish_batch/2, publish_batch/3]).
-export([pull/2, acknowledge/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).
-define(PUBSUB_API_VERSION, "v1").
-define(PUBSUB_TIMEOUT, 30000).  % 30 seconds

-record(state, {
    project_id :: string(),
    base_url :: string(),
    access_token :: string() | undefined,
    emulator_host :: string() | undefined
}).

%%%===================================================================
%% API
%%%===================================================================

%% @doc Start the Pub/Sub client
-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% @doc Publish a message to a topic
-spec publish(Topic, Message) -> {ok, MessageId} | {error, Reason}
    when Topic :: string(),
         Message :: binary() | map(),
         MessageId :: string(),
         Reason :: term().
publish(Topic, Message) ->
    publish(Topic, Message, []).

%% @doc Publish a message with attributes
-spec publish(Topic, Message, Attributes) -> {ok, MessageId} | {error, Reason}
    when Topic :: string(),
         Message :: binary() | map(),
         Attributes :: [{string(), string()}],
         MessageId :: string(),
         Reason :: term().
publish(Topic, Message, Attributes) ->
    gen_server:call(?SERVER, {publish, Topic, Message, Attributes}, ?PUBSUB_TIMEOUT).

%% @doc Batch publish messages
-spec publish_batch(Topic, Messages) -> {ok, MessageIds} | {error, Reason}
    when Topic :: string(),
         Messages :: [binary() | map()],
         MessageIds :: [string()],
         Reason :: term().
publish_batch(Topic, Messages) ->
    publish_batch(Topic, Messages, []).

%% @doc Batch publish messages with attributes
-spec publish_batch(Topic, Messages, Attributes) -> {ok, MessageIds} | {error, Reason}
    when Topic :: string(),
         Messages :: [binary() | map()],
         Attributes :: [{string(), string()}],
         MessageIds :: [string()],
         Reason :: term().
publish_batch(Topic, Messages, Attributes) ->
    gen_server:call(?SERVER, {publish_batch, Topic, Messages, Attributes}, ?PUBSUB_TIMEOUT).

%% @doc Pull messages from a subscription
-spec pull(Subscription, MaxMessages) -> {ok, Messages} | {error, Reason}
    when Subscription :: string(),
         MaxMessages :: pos_integer(),
         Messages :: [map()],
         Reason :: term().
pull(Subscription, MaxMessages) ->
    gen_server:call(?SERVER, {pull, Subscription, MaxMessages}, ?PUBSUB_TIMEOUT).

%% @doc Acknowledge messages
-spec acknowledge(Subscription, AckIds) -> ok | {error, Reason}
    when Subscription :: string(),
         AckIds :: [string()],
         Reason :: term().
acknowledge(Subscription, AckIds) ->
    gen_server:call(?SERVER, {acknowledge, Subscription, AckIds}, ?PUBSUB_TIMEOUT).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

init([]) ->
    %% Get project ID from config
    ProjectId = gcp_config:get_project_id(),
    
    %% Check for emulator
    EmulatorHost = case os:getenv("PUBSUB_EMULATOR_HOST") of
        false -> undefined;
        Host -> Host
    end,
    
    %% Build base URL
    BaseUrl = case EmulatorHost of
        undefined ->
            "https://pubsub.googleapis.com/" ++ ?PUBSUB_API_VERSION;
        EmulatorHostValue ->
            "http://" ++ EmulatorHostValue ++ "/" ++ ?PUBSUB_API_VERSION
    end,
    
    State = #state{
        project_id = ProjectId,
        base_url = BaseUrl,
        emulator_host = EmulatorHost
    },
    
    {ok, State}.

handle_call({publish, Topic, Message, Attributes}, _From, State) ->
    %% Check for failure injection
    case gcp_failure_wrapper:check_pubsub_failure(publish) of
        {should_fail, Type, Reason} ->
            logger:warning("Pub/Sub publish failure injected: ~p - ~p", [Type, Reason]),
            {reply, {error, Reason}, State};
        should_succeed ->
            case ensure_access_token(State) of
                {ok, NewState} ->
                    Result = pubsub_publish(NewState, Topic, Message, Attributes),
                    {reply, Result, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({publish_batch, Topic, Messages, Attributes}, _From, State) ->
    %% Check for failure injection
    case gcp_failure_wrapper:check_pubsub_failure(publish) of
        {should_fail, Type, Reason} ->
            logger:warning("Pub/Sub batch publish failure injected: ~p - ~p", [Type, Reason]),
            {reply, {error, Reason}, State};
        should_succeed ->
            case ensure_access_token(State) of
                {ok, NewState} ->
                    Result = pubsub_publish_batch(NewState, Topic, Messages, Attributes),
                    {reply, Result, NewState};
                {error, Reason} ->
                    {reply, {error, Reason}, State}
            end
    end;

handle_call({pull, Subscription, MaxMessages}, _From, State) ->
    case ensure_access_token(State) of
        {ok, NewState} ->
            Result = pubsub_pull(NewState, Subscription, MaxMessages),
            {reply, Result, NewState};
        {error, Reason} ->
            {reply, {error, Reason}, State}
    end;

handle_call({acknowledge, Subscription, AckIds}, _From, State) ->
    case ensure_access_token(State) of
        {ok, NewState} ->
            Result = pubsub_acknowledge(NewState, Subscription, AckIds),
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
                    case gcp_metadata:get_access_token(pubsub) of
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

-spec pubsub_publish(State, Topic, Message, Attributes) -> {ok, string()} | {error, term()}
    when State :: #state{},
         Topic :: string(),
         Message :: binary() | map(),
         Attributes :: [{string(), string()}].
pubsub_publish(State, Topic, Message, Attributes) ->
    %% Normalize topic name (handle both short and full names)
    TopicPath = normalize_topic_name(State, Topic),
    
    %% Encode message
    MessageData = case Message of
        M when is_map(M) -> jsx:encode(M);
        M when is_binary(M) -> M;
        M -> list_to_binary(io_lib:format("~p", [M]))
    end,
    
    %% Base64 encode message data (Pub/Sub API expects base64 string)
    MessageDataBase64 = case erlang:function_exported(base64, encode_to_string, 1) of
        true ->
            base64:encode_to_string(MessageData);
        false ->
            %% Fallback: encode to binary then convert to string
            binary_to_list(base64:encode(MessageData))
    end,
    
    %% Build Pub/Sub message
    PubSubMessage = #{
        <<"data">> => list_to_binary(MessageDataBase64),
        <<"attributes">> => maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- Attributes])
    },
    
    RequestBody = jsx:encode(#{
        <<"messages">> => [PubSubMessage]
    }),
    
    Path = State#state.base_url ++ "/" ++ TopicPath ++ ":publish",
    Headers = build_headers(State),
    
    case httpc:request(post, {Path, Headers, "application/json", RequestBody}, [{timeout, ?PUBSUB_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"messageIds">> := [MessageId | _]} ->
                    {ok, binary_to_list(MessageId)};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec pubsub_publish_batch(State, Topic, Messages, Attributes) -> {ok, [string()]} | {error, term()}
    when State :: #state{},
         Topic :: string(),
         Messages :: [binary() | map()],
         Attributes :: [{string(), string()}].
pubsub_publish_batch(State, Topic, Messages, Attributes) ->
    %% Normalize topic name
    TopicPath = normalize_topic_name(State, Topic),
    
    %% Convert messages to Pub/Sub format
    PubSubMessages = lists:map(fun(Message) ->
        MessageData = case Message of
            M when is_map(M) -> jsx:encode(M);
            M when is_binary(M) -> M;
            M -> list_to_binary(io_lib:format("~p", [M]))
        end,
        %% Base64 encode message data (Pub/Sub API expects base64 string)
        MessageDataBase64 = case erlang:function_exported(base64, encode_to_string, 1) of
            true ->
                base64:encode_to_string(MessageData);
            false ->
                %% Fallback: encode to binary then convert to string
                binary_to_list(base64:encode(MessageData))
        end,
        #{
            <<"data">> => list_to_binary(MessageDataBase64),
            <<"attributes">> => maps:from_list([{list_to_binary(K), list_to_binary(V)} || {K, V} <- Attributes])
        }
    end, Messages),
    
    RequestBody = jsx:encode(#{
        <<"messages">> => PubSubMessages
    }),
    
    Path = State#state.base_url ++ "/" ++ TopicPath ++ ":publish",
    Headers = build_headers(State),
    
    case httpc:request(post, {Path, Headers, "application/json", RequestBody}, [{timeout, ?PUBSUB_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"messageIds">> := MessageIds} ->
                    {ok, [binary_to_list(Id) || Id <- MessageIds]};
                _ ->
                    {error, invalid_response}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec pubsub_pull(State, Subscription, MaxMessages) -> {ok, [map()]} | {error, term()}
    when State :: #state{},
         Subscription :: string(),
         MaxMessages :: pos_integer().
pubsub_pull(State, Subscription, MaxMessages) ->
    %% Normalize subscription name
    SubscriptionPath = normalize_subscription_name(State, Subscription),
    
    RequestBody = jsx:encode(#{
        <<"maxMessages">> => MaxMessages
    }),
    
    Path = State#state.base_url ++ "/" ++ SubscriptionPath ++ ":pull",
    Headers = build_headers(State),
    
    case httpc:request(post, {Path, Headers, "application/json", RequestBody}, [{timeout, ?PUBSUB_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, ResponseBody}} ->
            case jsx:decode(list_to_binary(ResponseBody), [return_maps]) of
                #{<<"receivedMessages">> := ReceivedMessages} ->
                    Messages = lists:map(fun(#{<<"message">> := Msg, <<"ackId">> := AckId}) ->
                        Msg#{<<"ackId">> => AckId}
                    end, ReceivedMessages),
                    {ok, Messages};
                _ ->
                    {ok, []}
            end;
        {ok, {{_, Status, _}, _, _}} ->
            {error, {http_error, Status}};
        {error, Reason} ->
            {error, Reason}
    end.

-spec pubsub_acknowledge(State, Subscription, AckIds) -> ok | {error, term()}
    when State :: #state{},
         Subscription :: string(),
         AckIds :: [string()].
pubsub_acknowledge(State, Subscription, AckIds) ->
    %% Normalize subscription name
    SubscriptionPath = normalize_subscription_name(State, Subscription),
    
    RequestBody = jsx:encode(#{
        <<"ackIds">> => [list_to_binary(Id) || Id <- AckIds]
    }),
    
    Path = State#state.base_url ++ "/" ++ SubscriptionPath ++ ":acknowledge",
    Headers = build_headers(State),
    
    case httpc:request(post, {Path, Headers, "application/json", RequestBody}, [{timeout, ?PUBSUB_TIMEOUT}], []) of
        {ok, {{_, 200, _}, _, _}} -> ok;
        {ok, {{_, Status, _}, _, _}} -> {error, {http_error, Status}};
        {error, Reason} -> {error, Reason}
    end.

-spec normalize_topic_name(State, Topic) -> string()
    when State :: #state{},
         Topic :: string().
normalize_topic_name(State, Topic) ->
    case string:prefix(Topic, "projects/") of
        nomatch ->
            "projects/" ++ State#state.project_id ++ "/topics/" ++ Topic;
        _ ->
            Topic
    end.

-spec normalize_subscription_name(State, Subscription) -> string()
    when State :: #state{},
         Subscription :: string().
normalize_subscription_name(State, Subscription) ->
    case string:prefix(Subscription, "projects/") of
        nomatch ->
            "projects/" ++ State#state.project_id ++ "/subscriptions/" ++ Subscription;
        _ ->
            Subscription
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
