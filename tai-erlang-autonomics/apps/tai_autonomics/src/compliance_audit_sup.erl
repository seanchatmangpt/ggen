%%%-------------------------------------------------------------------
%% @doc compliance_audit_sup: Supervisor for compliance audit governor FSM instances
%%
%% Manages compliance and audit operations:
%% - Policy compliance checking
%% - Audit log generation
%% - Regulatory requirement verification
%% - Compliance report generation
%%
%% Restart strategy: one_for_one (FSM instances are independent)
%% @end
%%%-------------------------------------------------------------------
-module(compliance_audit_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_compliance_audit_governor/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SHUTDOWN_TIMEOUT, 5000).

%%%===================================================================
%% API functions
%%%===================================================================

-spec start_link() -> supervisor:startlink_ret().
start_link() ->
    supervisor:start_link({local, compliance_audit_sup}, ?MODULE, []).

-spec start_compliance_audit_governor(TenantId) -> supervisor:startchild_ret()
  when TenantId :: binary() | string().
start_compliance_audit_governor(TenantId) ->
    ChildSpec = #{
        id => {compliance_audit_governor, TenantId},
        start => {compliance_audit_governor, start_link, [TenantId]},
        restart => temporary,
        shutdown => ?SHUTDOWN_TIMEOUT,
        type => worker,
        modules => [compliance_audit_governor]
    },
    supervisor:start_child(compliance_audit_sup, ChildSpec).

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
