%%%-------------------------------------------------------------------
%% @doc receipt_ledger_sup: Supervisor for receipt persistence layer
%%
%% Manages receipt storage and publishing:
%% - receipt_store: ETS-backed persistent receipt storage
%% - receipt_publisher: Publishes receipts to Google Pub/Sub
%%
%% Restart strategy: one_for_one (independent failure domains)
%% @end
%%%-------------------------------------------------------------------
-module(receipt_ledger_sup).
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

%% @doc Start the receipt ledger supervisor
-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, receipt_ledger_sup}, ?MODULE, []).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Init callback
-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },

    ChildSpecs = [
        %% Receipt storage (ETS-backed, persistent)
        #{
            id => receipt_store,
            start => {receipt_store, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => worker,
            modules => [receipt_store]
        },

        %% Receipt publisher (to Google Pub/Sub)
        #{
            id => receipt_publisher,
            start => {receipt_publisher, start_link, []},
            restart => permanent,
            shutdown => ?SHUTDOWN_TIMEOUT,
            type => worker,
            modules => [receipt_publisher]
        }
    ],

    {ok, {SupFlags, ChildSpecs}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
