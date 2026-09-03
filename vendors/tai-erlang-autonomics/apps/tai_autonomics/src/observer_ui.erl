%%%-------------------------------------------------------------------
%% @doc Observer UI - Built-in Erlang observer with remote access
%%      Provides real-time process tree, message queue monitoring
%%      Can be accessed remotely via observer:start_cli()
%% @end
%%%-------------------------------------------------------------------

-module(observer_ui).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([start_observer/0, stop_observer/0]).
-export([get_observer_status/0, export_observer_dump/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(OBSERVER_DUMP_INTERVAL, 300000).  %% 5 minutes

-record(state, {
    observer_running = false :: boolean(),
    observer_pid :: pid() | undefined,
    dump_timer :: reference() | undefined,
    last_dump_file :: string() | undefined
}).

%%%-------------------------------------------------------------------
%% API Functions
%%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop, 30000).

-spec start_observer() -> ok | {error, term()}.
start_observer() ->
    gen_server:call(?MODULE, start_observer, 30000).

-spec stop_observer() -> ok | {error, term()}.
stop_observer() ->
    gen_server:call(?MODULE, stop_observer, 30000).

-spec get_observer_status() -> {ok, map()}.
get_observer_status() ->
    gen_server:call(?MODULE, get_status, 30000).

-spec export_observer_dump(string()) -> ok | {error, term()}.
export_observer_dump(FilePath) ->
    gen_server:call(?MODULE, {export_dump, FilePath}, 30000).

%%%-------------------------------------------------------------------
%% gen_server Callbacks
%%%-------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),

    %% Schedule periodic dump exports
    DumpDir = application:get_env(observability, observer_dump_dir, "./observer_dumps"),
    filelib:ensure_dir(DumpDir ++ "/"),

    DumpTimer = erlang:send_after(?OBSERVER_DUMP_INTERVAL, self(), export_dump),

    {ok, #state{
        dump_timer = DumpTimer
    }}.

handle_call(start_observer, _From, State) ->
    Result = try
        observer:start(),
        {ok, observer_started}
    catch
        _:ErrorValue -> {error, ErrorValue}
    end,

    case Result of
        {ok, _} ->
            {reply, ok, State#state{observer_running = true}};
        {error, ErrorReason} ->
            {reply, {error, ErrorReason}, State}
    end;

handle_call(stop_observer, _From, State) ->
    Result = try
        observer:stop(),
        ok
    catch
        _:ErrorValue -> {error, ErrorValue}
    end,

    case Result of
        ok ->
            {reply, ok, State#state{observer_running = false}};
        {error, ErrorReason} ->
            {reply, {error, ErrorReason}, State}
    end;

handle_call(get_status, _From, State) ->
    Status = #{
        observer_running => State#state.observer_running,
        last_dump_file => State#state.last_dump_file,
        system_info => collect_system_info(),
        process_tree => collect_process_tree(),
        supervisor_status => collect_supervisor_status()
    },
    {reply, {ok, Status}, State};

handle_call({export_dump, FilePath}, _From, State) ->
    DumpData = collect_observer_dump(),
    Result = file:write_file(FilePath, term_to_binary(DumpData)),

    case Result of
        ok ->
            {reply, ok, State#state{last_dump_file = FilePath}};
        Error ->
            {reply, Error, State}
    end;

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(export_dump, State) ->
    Timestamp = erlang:system_time(millisecond),
    DumpDir = application:get_env(observability, observer_dump_dir, "./observer_dumps"),
    DumpFile = filename:join([DumpDir, "dump_" ++ integer_to_list(Timestamp) ++ ".bin"]),

    DumpData = collect_observer_dump(),
    file:write_file(DumpFile, term_to_binary(DumpData)),

    %% Schedule next dump
    DumpTimer = erlang:send_after(?OBSERVER_DUMP_INTERVAL, self(), export_dump),

    {noreply, State#state{
        dump_timer = DumpTimer,
        last_dump_file = DumpFile
    }};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.dump_timer of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    case State#state.observer_running of
        true ->
            try observer:stop() catch _:_ -> ok end;
        false -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

-spec collect_observer_dump() -> map().
collect_observer_dump() ->
    #{
        timestamp => erlang:system_time(millisecond),
        node => node(),
        system_info => collect_system_info(),
        process_tree => collect_process_tree(),
        supervisor_status => collect_supervisor_status(),
        memory_info => erlang:memory(),
        statistics => erlang:statistics(runtime)
    }.

-spec collect_system_info() -> map().
collect_system_info() ->
    #{
        otp_release => erlang:system_info(otp_release),
        system_version => erlang:system_info(system_version),
        process_count => erlang:system_info(process_count),
        process_limit => erlang:system_info(process_limit),
        schedulers => erlang:system_info(schedulers),
        schedulers_online => erlang:system_info(schedulers_online),
        threads => erlang:system_info(threads),
        thread_pool_size => erlang:system_info(thread_pool_size),
        logical_processors => erlang:system_info(logical_processors),
        logical_processors_available => erlang:system_info(logical_processors_available)
    }.

-spec collect_process_tree() -> list().
collect_process_tree() ->
    AllProcesses = erlang:processes(),
    lists:map(fun(Pid) ->
        collect_process_info(Pid)
    end, AllProcesses).

-spec collect_process_info(pid()) -> map().
collect_process_info(Pid) ->
    case catch erlang:process_info(Pid, [
        registered_name,
        status,
        message_queue_len,
        memory,
        reductions,
        parent,
        initial_call,
        current_stacktrace
    ]) of
        PI when is_list(PI) ->
            #{
                pid => Pid,
                registered_name => proplists:get_value(registered_name, PI, ''),
                status => proplists:get_value(status, PI, unknown),
                message_queue_len => proplists:get_value(message_queue_len, PI, 0),
                memory => proplists:get_value(memory, PI, 0),
                reductions => proplists:get_value(reductions, PI, 0),
                parent => proplists:get_value(parent, PI, undefined),
                initial_call => proplists:get_value(initial_call, PI, undefined)
            };
        _ ->
            #{pid => Pid, status => dead}
    end.

-spec collect_supervisor_status() -> list().
collect_supervisor_status() ->
    %% Find all supervisors
    AllProcesses = erlang:processes(),
    Supervisors = lists:filter(fun(Pid) ->
        case catch erlang:process_info(Pid, initial_call) of
            {initial_call, {supervisor, _, _}} -> true;
            _ -> false
        end
    end, AllProcesses),

    lists:map(fun(SupervisorPid) ->
        collect_supervisor_info(SupervisorPid)
    end, Supervisors).

-spec collect_supervisor_info(pid()) -> map().
collect_supervisor_info(SupervisorPid) ->
    case catch sys:get_status(SupervisorPid) of
        {status, Pid, _, [_, _, _, _, State]} ->
            #{
                pid => Pid,
                state => State,
                children => get_supervisor_children(SupervisorPid)
            };
        _ ->
            #{pid => SupervisorPid, state => unknown, children => []}
    end.

-spec get_supervisor_children(pid()) -> list().
get_supervisor_children(SupervisorPid) ->
    case catch supervisor:which_children(SupervisorPid) of
        Children when is_list(Children) ->
            lists:map(fun({Id, ChildPid, Type, _Modules}) ->
                #{
                    id => Id,
                    pid => ChildPid,
                    type => Type,
                    alive => erlang:is_process_alive(ChildPid)
                }
            end, Children);
        _ -> []
    end.
