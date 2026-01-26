%%%-------------------------------------------------------------------
%% @doc tai_http: HTTP server with Cowboy for Cloud Run
%%
%% Routes:
%%   GET  /health -> health check endpoint
%%   POST /pubsub -> Pub/Sub push handler
%%   POST /marketplace -> Marketplace entitlement handler
%%
%% All endpoints emit receipts. Never return 5xx on client errors.
%%
%% @end
%%%-------------------------------------------------------------------
-module(tai_http).
-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%% API
%%%===================================================================

-spec start_link() -> {ok, pid()} | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%%%===================================================================
%% gen_server callbacks
%%%===================================================================

init([]) ->
    Port = case os:getenv("PORT") of
        false -> 8080;
        PortStr -> list_to_integer(PortStr)
    end,

    Routes = [
        {"/health", tai_http_handler, handle_health},
        {"/pubsub", tai_http_handler, handle_pubsub},
        {"/marketplace", tai_http_handler, handle_marketplace}
    ],

    Dispatch = cowboy_router:compile([
        {'_', Routes}
    ]),

    TransOpts = #{
        socket_opts => [{port, Port}]
    },

    ProtoOpts = #{
        env => #{dispatch => Dispatch}
    },

    case cowboy:start_clear(?SERVER, TransOpts, ProtoOpts) of
        {ok, _Pid} ->
            {ok, #{port => Port}};
        Error ->
            {stop, Error}
    end.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    cowboy:stop_listener(?SERVER),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

