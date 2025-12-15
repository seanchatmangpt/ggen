%% knhk_rc.erl — Reflexive Control (Erlang, higher level)
%% Speaks Σ, Λ, Π, Γ, τ, Q, O, A, Δ. μ executes in C.
-module(knhk_rc).
-export([
  boot/1,                          %% Σ,Q → ok
  connect/1,                       %% ConnectorSpec → ConnId
  cover/1,                         %% CoverSpec → CoverId          (Γ over O)
  admit/1,                         %% Δ → ok                       (O ⊔ Δ)
  reflex/1,                        %% ReflexSpec → HookId          (μ-hot op)
  epoch/1,                         %% #{τ := <=8, Λ := plan} → EpochId
  run/1,                           %% EpochId → {A, Receipt}       (A = μ(O))
  route/1,                         %% ActionSpec → RouteId         (A ports)
  receipt/1,                       %% Id → Receipt
  merge/1,                         %% [Receipt] → Receipt          (Π ⊕)
  metrics/0,                       %% OTEL-friendly map
  coverage/0                       %% Dark Matter 80/20 coverage
]).

%% -------- Type sketches (KGC-aligned)
-type sigma()   :: binary().
-type q_inv()   :: binary().
-type id()      :: binary().
-type receipt() :: #{hash := binary(), span := binary(), ticks := integer()}.

%% -------- API

%% 1) Initialize Σ, Q
boot(#{sigma := Sigma, q := Q}) ->
  %% O ⊨ Σ, preserve(Q)
  knhk_sigma:load(Sigma),
  knhk_q:load(Q),
  ok.

%% 2) Connect typed sources (Dark Matter 80/20)
%%    Each ConnectorSpec declares Σ mapping and Δ framing.
connect(#{name := Name, schema := SigmaIri, source := Src, map := Map, guard := Guard}) ->
  %% Γ registers typed inlet; Δ validated; SoA prepared
  knhk_connect:register(Name, SigmaIri, Src, Map, Guard).

%% 3) Define cover over O (select S ⊂ O, shard runs len ≤ 8)
cover(#{select := SelectSpec, shard := ShardSpec}) ->
  %% glue(Cover(O)) = Γ(O)
  knhk_cover:define(SelectSpec, ShardSpec).

%% 4) Admit Δ into O (typed, guarded)
admit(Δ) ->
  %% μ(O ⊔ Δ) = μ(O) ⊔ μ(Δ)
  knhk_ingest:submit(Δ).

%% 5) Declare a reflex (hot op) as knowledge, not code
%%    ReflexSpec chooses one in H_hot: ASK_SP | COUNT_SP_≥ | ASK_SPO | UNIQUE_SP | COUNT_OP_* | COMPARE_O_* | CONSTRUCT8
reflex(#{name := Name, op := Op, run := #{pred := P, off := Off, len := Len},
         args := Args, epoch := EpochTag}) when Len =< 8 ->
  %% Compiles to Hook IR; violation hits H
  knhk_hooks:install(Name, Op, P, Off, Len, Args, EpochTag).

%% 6) Plan a deterministic epoch (Λ ≺-total, τ ≤ 8)
epoch(#{tau := Tau, lambda := Plan, cover := CoverId}) when Tau =< 8 ->
  knhk_epoch:schedule(Tau, Plan, CoverId).

%% 7) Execute μ over O for the epoch; return A and receipt
run(EpochId) ->
  %% A = μ(O); hash(A) = hash(μ(O))
  knhk_epoch:run(EpochId).

%% 8) Route actions A to outputs (A ports)
route(#{name := Name, kind := Kind, target := Target, encode := Codec}) ->
  knhk_route:install(Name, Kind, Target, Codec).

%% 9) Fetch and merge receipts (Π ⊕)
receipt(Id) ->
  knhk_lockchain:read(Id).

merge(Receipts) ->
  knhk_lockchain:merge(Receipts).

metrics() ->
  knhk_otel:metrics().

coverage() ->
  knhk_darkmatter:coverage().

