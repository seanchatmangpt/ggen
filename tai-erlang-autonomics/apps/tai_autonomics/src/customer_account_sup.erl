%%%-------------------------------------------------------------------
%% @doc customer_account_sup: Supervisor for customer account governor FSM instances
%%
%% Manages customer account state:
%% - Account creation and activation
%% - Payment method management
%% - Credit limit enforcement
%% - Account status transitions
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% @end
%%%-------------------------------------------------------------------
-module(customer_account_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_customer_account_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, customer_account_sup}, ?MODULE, []).

-spec start_customer_account_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_customer_account_governor(TenantId) ->
    ChildSpec = #{
        id => {customer_account_governor, TenantId},
        start => {customer_account_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [customer_account_governor]
    },
    supervisor:start_child(customer_account_sup, ChildSpec).

%%%===================================================================
%% Supervisor callbacks
%%%===================================================================

-spec init(Args) -> {ok, {supervisor:sup_flags(), [supervisor:child_spec()]}} |
                    ignore
  when Args :: [].
init([]) ->
    SupFlags = #{
        strategy => one_for_one,
        intensity => 5,
        period => 60
    },
    {ok, {SupFlags, []}}.

%%%===================================================================
%% Internal functions
%%%===================================================================
