%% Stub modules for Erlang supervision tree
%% These will be fully implemented in subsequent phases

-module(knhks_sigma).
-behaviour(gen_server).
-export([start_link/0, load/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
load(Sigma) -> gen_server:call(?MODULE, {load, Sigma}).

-module(knhks_q).
-behaviour(gen_server).
-export([start_link/0, load/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
load(Q) -> gen_server:call(?MODULE, {load, Q}).

-module(knhks_ingest).
-behaviour(gen_server).
-export([start_link/0, submit/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
submit(Δ) -> gen_server:call(?MODULE, {submit, Δ}).

-module(knhks_unrdf).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.

-module(knhks_shapes).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.

-module(knhks_lockchain).
-behaviour(gen_server).
-export([start_link/0, read/1, merge/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
read(Id) -> gen_server:call(?MODULE, {read, Id}).
merge(Receipts) -> gen_server:call(?MODULE, {merge, Receipts}).

-module(knhks_bus).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.

-module(knhks_repl).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.

-module(knhks_otel).
-behaviour(gen_server).
-export([start_link/0, metrics/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
metrics() -> gen_server:call(?MODULE, metrics).

-module(knhks_darkmatter).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.

-module(knhks_connect).
-behaviour(gen_server).
-export([start_link/0, register/5]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
register(Name, SigmaIri, Src, Map, Guard) -> gen_server:call(?MODULE, {register, Name, SigmaIri, Src, Map, Guard}).

-module(knhks_cover).
-behaviour(gen_server).
-export([start_link/0, define/2]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
define(SelectSpec, ShardSpec) -> gen_server:call(?MODULE, {define, SelectSpec, ShardSpec}).

-module(knhks_hooks).
-behaviour(gen_server).
-export([start_link/0, install/7]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
install(Name, Op, P, Off, Len, Args, EpochTag) -> gen_server:call(?MODULE, {install, Name, Op, P, Off, Len, Args, EpochTag}).

-module(knhks_epoch).
-behaviour(gen_server).
-export([start_link/0, schedule/3, run/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
schedule(Tau, Plan, CoverId) -> gen_server:call(?MODULE, {schedule, Tau, Plan, CoverId}).
run(EpochId) -> gen_server:call(?MODULE, {run, EpochId}).

-module(knhks_route).
-behaviour(gen_server).
-export([start_link/0, install/4]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
install(Name, Kind, Target, Codec) -> gen_server:call(?MODULE, {install, Name, Kind, Target, Codec}).

