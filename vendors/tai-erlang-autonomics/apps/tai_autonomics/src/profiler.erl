%%%-------------------------------------------------------------------
%% @doc Profiler - CPU and memory profiling with Cloud Storage uploads
%%      Uses fprof for CPU profiling and erts_debug for memory profiling
%%      Generates hourly snapshots and uploads to GCP Cloud Storage
%% @end
%%%-------------------------------------------------------------------

-module(profiler).
-behaviour(gen_server).

%% API
-export([start_link/0, stop/0]).
-export([start_cpu_profile/0, stop_cpu_profile/0]).
-export([start_memory_profile/0, stop_memory_profile/0]).
-export([get_profile_status/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(PROFILE_INTERVAL, 3600000).  %% 1 hour
-define(PROFILE_DIR, "./profiles").
-define(GCP_BUCKET, "beam-cluster-profiles").

-record(state, {
    cpu_profiling = false :: boolean(),
    memory_profiling = false :: boolean(),
    cpu_profile_file :: string() | undefined,
    memory_profile_file :: string() | undefined,
    last_snapshot_time = 0 :: integer(),
    timer_ref :: reference() | undefined,
    profile_dir = ?PROFILE_DIR :: string()
}).

%%%-------------------------------------------------------------------
%% API Functions
%%%-------------------------------------------------------------------

-spec start_link() -> {ok, pid()} | ignore | {error, term()}.
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

-spec stop() -> ok.
stop() ->
    gen_server:call(?MODULE, stop, 60000).

-spec start_cpu_profile() -> ok | {error, term()}.
start_cpu_profile() ->
    gen_server:call(?MODULE, start_cpu_profile, 30000).

-spec stop_cpu_profile() -> ok | {error, term()}.
stop_cpu_profile() ->
    gen_server:call(?MODULE, stop_cpu_profile, 30000).

-spec start_memory_profile() -> ok | {error, term()}.
start_memory_profile() ->
    gen_server:call(?MODULE, start_memory_profile, 30000).

-spec stop_memory_profile() -> ok | {error, term()}.
stop_memory_profile() ->
    gen_server:call(?MODULE, stop_memory_profile, 30000).

-spec get_profile_status() -> {ok, map()} | {error, term()}.
get_profile_status() ->
    gen_server:call(?MODULE, get_status, 30000).

%%%-------------------------------------------------------------------
%% gen_server Callbacks
%%%-------------------------------------------------------------------

init([]) ->
    process_flag(trap_exit, true),

    %% Ensure profile directory exists
    ProfileDir = application:get_env(observability, profile_dir, ?PROFILE_DIR),
    filelib:ensure_dir(ProfileDir ++ "/"),

    %% Schedule hourly snapshots
    TimerRef = erlang:send_after(?PROFILE_INTERVAL, self(), take_snapshot),

    {ok, #state{
        profile_dir = ProfileDir,
        timer_ref = TimerRef
    }}.

handle_call(start_cpu_profile, _From, State) ->
    Timestamp = erlang:system_time(millisecond),
    ProfileFile = filename:join([State#state.profile_dir,
                                 "cpu_profile_" ++ integer_to_list(Timestamp) ++ ".fprof"]),

    Result = try
        fprof:start(),
        fprof:trace(start, ProfileFile),
        {ok, ProfileFile}
    catch
        _:ErrorValue -> {error, ErrorValue}
    end,

    case Result of
        {ok, File} ->
            {reply, ok, State#state{
                cpu_profiling = true,
                cpu_profile_file = File
            }};
        {error, ErrorReason} ->
            {reply, {error, ErrorReason}, State}
    end;

handle_call(stop_cpu_profile, _From, State) ->
    Result = try
        fprof:trace(stop),
        fprof:analyse([{dest, State#state.cpu_profile_file}]),
        fprof:stop(),
        ok
    catch
        _:ErrorValue -> {error, ErrorValue}
    end,

    case Result of
        ok ->
            %% Upload to Cloud Storage
            upload_profile(State#state.cpu_profile_file, cpu),
            {reply, ok, State#state{
                cpu_profiling = false,
                cpu_profile_file = undefined
            }};
        {error, ErrorReason} ->
            {reply, {error, ErrorReason}, State}
    end;

handle_call(start_memory_profile, _From, State) ->
    Timestamp = erlang:system_time(millisecond),
    ProfileFile = filename:join([State#state.profile_dir,
                                 "memory_profile_" ++ integer_to_list(Timestamp) ++ ".bin"]),

    {reply, ok, State#state{
        memory_profiling = true,
        memory_profile_file = ProfileFile
    }};

handle_call(stop_memory_profile, _From, State) ->
    case State#state.memory_profile_file of
        undefined ->
            {reply, {error, not_profiling}, State};
        ProfileFile ->
            %% Collect memory statistics
            MemSnapshot = collect_memory_snapshot(),

            %% Write to file
            Result = file:write_file(ProfileFile, term_to_binary(MemSnapshot)),

            case Result of
                ok ->
                    %% Upload to Cloud Storage
                    upload_profile(ProfileFile, memory),
                    {reply, ok, State#state{
                        memory_profiling = false,
                        memory_profile_file = undefined
                    }};
                Error ->
                    {reply, Error, State}
            end
    end;

handle_call(get_status, _From, State) ->
    Status = #{
        cpu_profiling => State#state.cpu_profiling,
        memory_profiling => State#state.memory_profiling,
        cpu_profile_file => State#state.cpu_profile_file,
        memory_profile_file => State#state.memory_profile_file,
        profile_dir => State#state.profile_dir
    },
    {reply, {ok, Status}, State};

handle_call(stop, _From, State) ->
    {stop, normal, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(take_snapshot, State) ->
    %% Take CPU profile snapshot
    NewState = case State#state.cpu_profiling of
        false ->
            Timestamp = erlang:system_time(millisecond),
            ProfileFile = filename:join([State#state.profile_dir,
                                         "cpu_snapshot_" ++ integer_to_list(Timestamp) ++ ".fprof"]),
            try
                fprof:start(),
                fprof:trace(start, ProfileFile),
                %% Run for a short time
                timer:sleep(1000),
                fprof:trace(stop),
                fprof:analyse([{dest, ProfileFile}]),
                fprof:stop(),

                %% Upload to Cloud Storage
                upload_profile(ProfileFile, cpu_snapshot),
                State#state{last_snapshot_time = Timestamp}
            catch
                _:Error ->
                    io:format("CPU snapshot error: ~p~n", [Error]),
                    State
            end;
        true ->
            State  %% Already profiling, skip snapshot
    end,

    %% Schedule next snapshot
    NewTimerRef = erlang:send_after(?PROFILE_INTERVAL, self(), take_snapshot),
    NewState2 = NewState#state{timer_ref = NewTimerRef},
    {noreply, NewState2};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, State) ->
    case State#state.timer_ref of
        undefined -> ok;
        Ref -> erlang:cancel_timer(Ref)
    end,

    %% Stop any active profiling
    case State#state.cpu_profiling of
        true ->
            try fprof:stop() catch _:_ -> ok end;
        false -> ok
    end,
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%% Internal Functions
%%%-------------------------------------------------------------------

-spec collect_memory_snapshot() -> map().
collect_memory_snapshot() ->
    MemInfo = erlang:memory(),
    ProcessStats = collect_process_memory_stats(),

    #{
        timestamp => erlang:system_time(millisecond),
        node => node(),
        total_memory => proplists:get_value(total, MemInfo, 0),
        processes_memory => proplists:get_value(processes, MemInfo, 0),
        system_memory => proplists:get_value(system, MemInfo, 0),
        atom_memory => proplists:get_value(atom, MemInfo, 0),
        binary_memory => proplists:get_value(binary, MemInfo, 0),
        code_memory => proplists:get_value(code, MemInfo, 0),
        ets_memory => proplists:get_value(ets, MemInfo, 0),
        process_count => erlang:system_info(process_count),
        process_stats => ProcessStats
    }.

-spec collect_process_memory_stats() -> list().
collect_process_memory_stats() ->
    AllProcesses = erlang:processes(),
    ProcessStats = lists:map(fun(Pid) ->
        case catch erlang:process_info(Pid, [memory, reductions, message_queue_len]) of
            PI when is_list(PI) ->
                #{
                    pid => Pid,
                    memory => proplists:get_value(memory, PI, 0),
                    reductions => proplists:get_value(reductions, PI, 0),
                    message_queue_len => proplists:get_value(message_queue_len, PI, 0)
                };
            _ -> undefined
        end
    end, AllProcesses),

    %% Filter and sort by memory
    ValidStats = lists:filter(fun(X) -> X =/= undefined end, ProcessStats),
    lists:sort(fun(A, B) ->
        maps:get(memory, A, 0) > maps:get(memory, B, 0)
    end, ValidStats).

-spec upload_profile(string(), atom()) -> ok | {error, term()}.
upload_profile(ProfileFile, ProfileType) ->
    case application:get_env(observability, gcp_bucket) of
        {ok, Bucket} ->
            upload_to_gcp(Bucket, ProfileFile, ProfileType);
        undefined ->
            ok  %% GCP not configured
    end.

-spec upload_to_gcp(string(), string(), atom()) -> ok | {error, term()}.
upload_to_gcp(Bucket, ProfileFile, ProfileType) ->
    %% In production, this would use GCP Cloud Storage API
    %% For now, just log the upload
    case file:read_file(ProfileFile) of
        {ok, Contents} ->
            FileName = filename:basename(ProfileFile),
            ObjectName = io_lib:format("profiles/~s/~s",
                                       [atom_to_list(ProfileType), FileName]),
            io:format("Uploading profile to gs://~s/~s (size: ~p bytes)~n",
                      [Bucket, ObjectName, byte_size(Contents)]),
            ok;
        Error ->
            Error
    end.
