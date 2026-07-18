%% -------------------------------------------------------------------
%% Release Handler for Zero-Downtime Hot Code Reloading
%% Handles upgrade/downgrade of Erlang Autonomics governors
%% -------------------------------------------------------------------

-module(release_handler).
-behaviour(gen_server).

-export([start_link/0, upgrade/0, downgrade/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-record(state, {
  release_dir,
  current_version,
  upgrade_in_progress = false
}).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

upgrade() ->
  gen_server:call(?MODULE, upgrade, 60000).

downgrade() ->
  gen_server:call(?MODULE, downgrade, 60000).

init([]) ->
  {ok, CurrentVersion} = application:get_key(erlang_autonomics, vsn),
  ReleaseDir = code:priv_dir(erlang_autonomics),
  {ok, #state{
    release_dir = ReleaseDir,
    current_version = CurrentVersion
  }}.

handle_call(upgrade, _From, State) ->
  case State#state.upgrade_in_progress of
    true ->
      {reply, {error, upgrade_in_progress}, State};
    false ->
      case do_upgrade(State) of
        {ok, NewVersion, State1} ->
          {reply, {ok, NewVersion}, State1};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end
  end;

handle_call(downgrade, _From, State) ->
  case State#state.upgrade_in_progress of
    true ->
      {reply, {error, downgrade_in_progress}, State};
    false ->
      case do_downgrade(State) of
        {ok, PreviousVersion, State1} ->
          {reply, {ok, PreviousVersion}, State1};
        {error, Reason} ->
          {reply, {error, Reason}, State}
      end
  end.

do_upgrade(State) ->
  CurrentVersion = State#state.current_version,
  ReleaseDir = State#state.release_dir,

  %% 1. Check for new version
  NewVersionFile = filename:join([ReleaseDir, "releases", "NEXT_VERSION"]),
  case file:read_file(NewVersionFile) of
    {ok, NewVersionBin} ->
      NewVersion = string:trim(binary_to_list(NewVersionBin)),

      %% 2. Stop accepting new requests (drains in-flight)
      ok = drain_requests(60000),

      %% 3. Code hot-reload
      case reload_modules(NewVersion) of
        ok ->
          %% 4. State migration (if needed)
          case migrate_state(CurrentVersion, NewVersion) of
            ok ->
              %% 5. Resume serving
              ok = resume_requests(),
              {ok, NewVersion, State#state{current_version = NewVersion}};
            Error ->
              %% Rollback
              ok = resume_requests(),
              {error, Error}
          end;
        Error ->
          ok = resume_requests(),
          {error, Error}
      end;

    {error, _} ->
      {error, no_upgrade_available}
  end.

do_downgrade(State) ->
  CurrentVersion = State#state.current_version,
  ReleaseDir = State#state.release_dir,

  %% 1. Drain in-flight requests
  ok = drain_requests(60000),

  %% 2. Revert code
  PreviousVersionFile = filename:join([ReleaseDir, "releases", "PREVIOUS_VERSION"]),
  case file:read_file(PreviousVersionFile) of
    {ok, PreviousVersionBin} ->
      PreviousVersion = string:trim(binary_to_list(PreviousVersionBin)),

      case reload_modules(PreviousVersion) of
        ok ->
          case migrate_state(CurrentVersion, PreviousVersion) of
            ok ->
              ok = resume_requests(),
              {ok, PreviousVersion, State#state{current_version = PreviousVersion}};
            Error ->
              ok = resume_requests(),
              {error, Error}
          end;
        Error ->
          ok = resume_requests(),
          {error, Error}
      end;

    {error, _} ->
      {error, no_downgrade_available}
  end.

reload_modules(Version) ->
  %% Load new .beam files from release directory
  EbinDir = filename:join([code:priv_dir(erlang_autonomics), "releases", Version, "lib", "*", "ebin"]),
  case filelib:wildcard(EbinDir) of
    [] ->
      {error, no_ebin_found};
    EbinDirs ->
      %% Add to code path and purge old modules
      lists:foreach(fun(Dir) -> code:add_path(Dir) end, EbinDirs),

      %% List all modules to reload
      Modules = [
        governor_statem,
        receipt_ledger,
        entitlement_statem,
        metrics_collector,
        tenant_registry,
        signal_normalizer
      ],

      %% Soft purge (allow in-flight calls to complete)
      case soft_purge_modules(Modules) of
        ok -> ok;
        Error -> Error
      end
  end.

soft_purge_modules([]) ->
  ok;
soft_purge_modules([Module | Rest]) ->
  case soft_purge_module(Module) of
    ok -> soft_purge_modules(Rest);
    Error -> Error
  end.

soft_purge_module(Module) ->
  case code:soft_purge(Module) of
    true ->
      case code:load_file(Module) of
        {module, Module} -> ok;
        Error -> Error
      end;
    false ->
      %% Module has in-flight code; wait and retry
      timer:sleep(100),
      case code:soft_purge(Module) of
        true ->
          case code:load_file(Module) of
            {module, Module} -> ok;
            Error -> Error
          end;
        false ->
          {error, {still_in_use, Module}}
      end
  end.

migrate_state(CurrentVersion, NewVersion) ->
  %% Call version-specific migration handler
  MigrationModule = erlang_autonomics_migration_v1,

  case erlang:function_exported(MigrationModule, migrate, 2) of
    true ->
      MigrationModule:migrate(CurrentVersion, NewVersion);
    false ->
      %% No migration needed
      ok
  end.

drain_requests(TimeoutMs) ->
  %% Set global flag to stop accepting new requests
  erlang:put(draining, true),

  %% Wait for in-flight requests to complete
  WaitInterval = 100,
  MaxWaits = TimeoutMs div WaitInterval,
  wait_for_drain(MaxWaits).

wait_for_drain(0) ->
  {error, drain_timeout};
wait_for_drain(N) ->
  case erlang:get(in_flight_requests) of
    0 ->
      ok;
    undefined ->
      ok;
    _ ->
      timer:sleep(100),
      wait_for_drain(N - 1)
  end.

resume_requests() ->
  erlang:erase(draining),
  ok.

handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.
