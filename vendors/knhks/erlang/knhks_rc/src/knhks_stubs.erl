%% Stub modules for Erlang supervision tree
%% These will be fully implemented in subsequent phases

-module(knhks_sigma).
-behaviour(gen_server).
-export([start_link/0, load/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call({load, Sigma}, _From, State) -> {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.
load(Sigma) -> gen_server:call(?MODULE, {load, Sigma}).

-module(knhks_q).
-behaviour(gen_server).
-export([start_link/0, load/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call({load, Q}, _From, State) -> {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.
load(Q) -> gen_server:call(?MODULE, {load, Q}).

-module(knhks_ingest).
-behaviour(gen_server).
-export([start_link/0, submit/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call({submit, Δ}, _From, State) -> {reply, ok, State};
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.
submit(Δ) -> gen_server:call(?MODULE, {submit, Δ}).

-module(knhks_unrdf).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.

-module(knhks_shapes).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.

-module(knhks_lockchain).
-behaviour(gen_server).
-export([start_link/0, read/1, merge/1, write/1]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call({read, Id}, _From, State) -> {reply, {ok, #{}}, State};
handle_call({merge, Receipts}, _From, State) -> {reply, {ok, #{}}, State};
handle_call({write, Receipt}, _From, State) -> {reply, {ok, erlang:phash2(Receipt)}, State};
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.
read(Id) -> gen_server:call(?MODULE, {read, Id}).
merge(Receipts) -> gen_server:call(?MODULE, {merge, Receipts}).
write(Receipt) -> gen_server:call(?MODULE, {write, Receipt}).

-module(knhks_bus).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.

-module(knhks_repl).
-behaviour(gen_server).
-export([start_link/0]).
start_link() -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).
init([]) -> {ok, #{}}.
handle_call(_Request, _From, State) -> {reply, {error, unknown}, State}.

