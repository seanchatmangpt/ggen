%%====================================================================
%% gen_yawl - YAWL Workflow Engine OTP Behavior
%%====================================================================
%% Record definitions for gen_yawl workflow engine
%% @doc Workflow specification and runtime state records

%%====================================================================
%% Workflow Specification Records
%%====================================================================

-record(yawl_spec, {
    id                  :: binary(),
    name                :: binary(),
    version             :: {non_neg_integer(), non_neg_integer()},
    tasks               :: map(),     % #{atom() => #task_def{}}
    flows               :: list(),    % [#flow{}]
    input_condition     :: atom(),
    output_condition    :: atom(),
    cancellation_regions = [] :: list(),
    metadata            = #{} :: map()
}).

-record(task_def, {
    id                  :: atom(),
    name                :: binary(),
    kind                :: atom(),    % atomic | composite | multiple_instance | empty
    split_type          :: atom(),    % sequence | and | xor | or
    join_type           :: atom(),    % sequence | and | xor | or
    decomposes_to       :: atom() | undefined,
    timeout             :: pos_integer() | undefined,
    required_roles      = [] :: list(),
    resource_pattern    :: binary() | undefined,
    cancellation_region :: atom() | undefined,
    cancellation_set    = [] :: list(),
    pre_condition       :: undefined | function(),
    post_condition      :: undefined | function()
}).

-record(flow, {
    from                :: atom(),
    to                  :: atom(),
    condition           :: undefined | function(),
    priority            = 0 :: non_neg_integer(),
    is_default          = false :: boolean(),
    is_cycle            = false :: boolean()
}).

-record(cancellation_region, {
    id                  :: atom(),
    tasks               = [] :: list(),
    cancel_trigger      :: atom()
}).

%%====================================================================
%% Runtime State Records
%%====================================================================

-record(yawl_state, {
    spec                :: #yawl_spec{},
    case_id             :: binary(),
    status              :: atom(),    % active | completed | cancelled | failed
    work_items          = #{} :: map(),
    active_tokens       = #{} :: map(),
    completed_tasks     :: sets:set(atom()),  % Use sets module
    receipts            = [] :: list(),
    started_at          :: integer(),
    updated_at          :: integer(),
    metadata            = #{} :: map()
}).

-record(work_item, {
    id                  :: binary(),
    case_id             :: binary(),
    task_id             :: atom(),
    status              :: atom(),    % enabled | fired | started | completed | cancelled
    data                = #{} :: map(),
    assigned_to         :: undefined | atom(),
    enabled_at          :: integer(),
    started_at          :: undefined | integer(),
    completed_at        :: undefined | integer(),
    receipts            = [] :: list()
}).

-record(receipt, {
    id                  :: binary(),
    prev_hash           :: binary(),
    current_hash        :: binary(),
    timestamp           :: integer(),
    event_type          :: atom(),
    case_id             :: binary(),
    work_item_id        :: undefined | binary(),
    justification       = #{} :: map()
}).

%%====================================================================
%% Token Records (for gen_pnet integration)
%%====================================================================

-record(case_token, {
    case_id             :: binary(),
    spec_id             :: binary(),
    started_at          :: integer(),
    parent_case         :: undefined | binary(),
    case_data           = #{} :: map()
}).

-record(work_item_token, {
    work_item_id        :: binary(),
    case_id             :: binary(),
    task_id             :: atom(),
    status              :: atom(),    % enabled | started | completed | cancelled
    data                = #{} :: map(),
    assigned_to         :: undefined | atom(),
    enabled_at          :: integer()
}).

-record(control_token, {
    type                :: atom(),    % and_join | xor_join | or_join | discriminator
    task_id             :: atom(),
    branch_count        :: non_neg_integer(),
    completed           = [] :: list()
}).

-record(receipt_token, {
    receipt_id          :: binary(),
    prev_hash           :: binary(),
    current_hash        :: binary(),
    timestamp           :: integer(),
    event_type          :: atom()
}).

%%====================================================================
%% Utility Macros
%%====================================================================

-define(IS_ENABLED(Task, Mode, State),
    (is_tuple(State) andalso
     element(1, State) =:= yawl_state andalso
     maps:is_key(Task, State#yawl_state.spec#yawl_spec.tasks))).

-define(GET_TASK(Task, Spec),
    maps:get(Task, Spec#yawl_spec.tasks, #task_def{id = Task})).

-define(GENERATE_ID(),
    binary:encode_hex(crypto:hash(md5, term_to_binary({self(), os:system_time(nanosecond)})))).

-define(NOW(), os:system_time(nanosecond)).

%% @doc Generate a place name for a task.
-define(YAWL_PLACE(Task), list_to_atom("p_" ++ atom_to_list(Task))).

%% @doc Generate a place name with a type prefix.
-define(YAWL_PLACE(Type, Task), list_to_atom("p_" ++ atom_to_list(Type) ++ "_" ++ atom_to_list(Task))).

%% @doc Generate a transition name for a task.
-define(YAWL_TRANSITION(Task), list_to_atom("t_" ++ atom_to_list(Task))).

%% @doc Generate a transition name with a type prefix.
-define(YAWL_TRANSITION(Type, Task), list_to_atom("t_" ++ atom_to_list(Type) ++ "_" ++ atom_to_list(Task))).

%%====================================================================
%% Atom Definitions for Split/Join Types
%%====================================================================

-define(SPLIT_SEQUENCE, sequence).
-define(SPLIT_AND, 'and').
-define(SPLIT_XOR, xor).
-define(SPLIT_OR, or).

-define(JOIN_SEQUENCE, sequence).
-define(JOIN_AND, 'and').
-define(JOIN_XOR, xor).
-define(JOIN_OR, or).
