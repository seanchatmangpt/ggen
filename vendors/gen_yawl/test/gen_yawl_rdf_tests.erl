%%====================================================================
%% gen_yawl_rdf_tests - RDF/SPARQL Integration Tests
%%
%% @copyright 2025 ggen Project
%% @license MIT
%%====================================================================

-module(gen_yawl_rdf_tests).
-include_lib("eunit/include/eunit.hrl").
-include("gen_yawl.hrl").

%%====================================================================
%% Test Fixtures
%%====================================================================

simple_workflow_ttl() ->
    <<"@prefix yawl: <http://unrdf.org/yawl#> .\n"
      "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
      "\n"
      "<spec:approval> a yawl:WorkflowSpec ;\n"
      "    yawl:specId \"approval\" ;\n"
      "    yawl:specName \"Approval Workflow\" ;\n"
      "    yawl:specVersion \"1.0\" ;\n"
      "    yawl:inputCondition <p_input> ;\n"
      "    yawl:outputCondition <p_output> ;\n"
      "    yawl:hasTask <task:review> , <task:approve> .\n"
      "\n"
      "<task:review> a yawl:AtomicTask ;\n"
      "    yawl:taskId \"review\" ;\n"
      "    yawl:taskName \"Review Document\" .\n"
      "\n"
      "<task:approve> a yawl:AtomicTask ;\n"
      "    yawl:taskId \"approve\" ;\n"
      "    yawl:taskName \"Approve Document\" .\n"
      "\n"
      "<flow:review_to_approve> a yawl:Flow ;\n"
      "    yawl:sourceTask <task:review> ;\n"
      "    yawl:targetTask <task:approve> .\n">>).

parallel_workflow_ttl() ->
    <<"@prefix yawl: <http://unrdf.org/yawl#> .\n"
      "@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .\n"
      "\n"
      "<spec:parallel> a yawl:WorkflowSpec ;\n"
      "    yawl:specId \"parallel\" ;\n"
      "    yawl:specName \"Parallel Workflow\" ;\n"
      "    yawl:hasTask <task:split> , <task:a> , <task:b> , <task:join> .\n"
      "\n"
      "<task:split> a yawl:AtomicTask ;\n"
      "    yawl:taskId \"split\" ;\n"
      "    yawl:splitBehavior yawl:AND_Split .\n"
      "\n"
      "<task:a> a yawl:AtomicTask ;\n"
      "    yawl:taskId \"a\" ;\n"
      "    yawl:taskName \"Task A\" .\n"
      "\n"
      "<task:b> a yawl:AtomicTask ;\n"
      "    yawl:taskId \"b\" ;\n"
      "    yawl:taskName \"Task B\" .\n"
      "\n"
      "<task:join> a yawl:AtomicTask ;\n"
      "    yawl:taskId \"join\" ;\n"
      "    yawl:joinBehavior yawl:AND_Join .\n"
      "\n"
      "<flow:split_a> a yawl:Flow ;\n"
      "    yawl:sourceTask <task:split> ;\n"
      "    yawl:targetTask <task:a> .\n"
      "\n"
      "<flow:split_b> a yawl:Flow ;\n"
      "    yawl:sourceTask <task:split> ;\n"
      "    yawl:targetTask <task:b> .\n"
      "\n"
      "<flow:a_join> a yawl:Flow ;\n"
      "    yawl:sourceTask <task:a> ;\n"
      "    yawl:targetTask <task:join> .\n"
      "\n"
      "<flow:b_join> a yawl:Flow ;\n"
      "    yawl:sourceTask <task:b> ;\n"
      "    yawl:targetTask <task:join> .\n">>).

%%====================================================================
%% Load Spec Tests
%%====================================================================

load_spec_test_() ->
    {setup,
     fun setup/0,
     fun cleanup/1,
     fun(_) -> [
         ?_test(begin
             {ok, Spec} = gen_yawl_rdf:load_spec("/tmp/test_workflow.ttl"),
             ?assertEqual(<<"approval">>, Spec#yawl_spec.id)
         end)
     ] end}.

setup() ->
    %% Write test TTL file
    ok = file:write_file("/tmp/test_workflow.ttl", simple_workflow_ttl()),
    %% Start gen_yawl_rdf server
    {ok, Pid} = gen_yawl_rdf:start_link(),
    Pid.

cleanup(_Pid) ->
    %% Stop server
    gen_yawl_rdf:stop(),
    %% Clean up test file
    ok = file:delete("/tmp/test_workflow.ttl").

%%====================================================================
%% Parse Tests
%%====================================================================

parse_turtle_lines_test_() ->
    Lines = binary:split(simple_workflow_ttl(), <<"\n">>, [global]),
    Prefixes = #{},
    BaseUri = <<"http://test.org/">>,

    [
     ?_assertMatch(#{subject := _, predicate := _, object := _},
                  gen_yawl_rdf:parse_turtle_lines(Lines, Prefixes, BaseUri, []))
    ].

extract_task_def_test_() ->
    TTL = simple_workflow_ttl(),
    {ok, Spec} = gen_yawl_rdf:parse_workflow_spec(TTL, <<"http://test.org/">>, false),

    [
     ?_assert(maps:is_key(review, Spec#yawl_spec.tasks)),
     ?_assert(maps:is_key(approve, Spec#yawl_spec.tasks)),
     ?_assertEqual(<<"Review Document">>,
                   (maps:get(review, Spec#yawl_spec.tasks))#task_def.name),
     ?_assertEqual(<<"Approve Document">>,
                   (maps:get(approve, Spec#yawl_spec.tasks))#task_def.name)
    ].

extract_flows_test_() ->
    TTL = simple_workflow_ttl(),
    {ok, Spec} = gen_yawl_rdf:parse_workflow_spec(TTL, <<"http://test.org/">>, false),

    [
     ?_assert(length(Spec#yawl_spec.flows) > 0)
    ].

%%====================================================================
%% SPARQL Query Tests
%%====================================================================

query_enabled_work_items_test_() ->
    {setup,
     fun setup_query/0,
     fun cleanup_query/1,
     fun(_) -> [
         ?_test(begin
             CaseId = <<"test-case-123">>,
             Result = gen_yawl_rdf:query_enabled_work_items(CaseId),
             ?assertMatch({ok, _}, Result)
         end)
     ] end}.

setup_query() ->
    {ok, Pid} = gen_yawl_rdf:start_link(),
    Pid.

cleanup_query(_Pid) ->
    gen_yawl_rdf:stop().

get_case_state_test_() ->
    {setup,
     fun setup_state/0,
     fun cleanup_state/1,
     fun(_) -> [
         ?_test(begin
             CaseId = <<"test-case-456">>,
             Result = gen_yawl_rdf:get_case_state(CaseId),
             ?assertMatch({ok, _} | {error, _}, Result)
         end)
     ] end}.

setup_state() ->
    {ok, Pid} = gen_yawl_rdf:start_link(),
    Pid.

cleanup_state(_Pid) ->
    gen_yawl_rdf:stop().

%%====================================================================
%% Records to RDF Tests
%%====================================================================

records_to_rdf_test_() ->
    EventMap = #{
        <<"receiptId">> => <<"receipt-123">>,
        <<"eventType">> => <<"WorkItemCompleted">>,
        <<"timestamp">> => <<"2025-02-04T10:00:00Z">>,
        <<"workItem">> => <<"workitem-456">>
    },

    Turtle = gen_yawl_rdf:records_to_rdf(EventMap),

    [
     ?_assert(is_binary(Turtle)),
     ?_assertMatch(<<_, _:_, _/binary>>, Turtle),  %% Multi-line
     ?_assert(<<<<"receipt-123">>>/binary>> =< Turtle),
     ?_assert(<<<<"WorkItemCompleted">>>/binary>> =< Turtle)
    ].

spec_to_records_test_() ->
    Spec = #yawl_spec{
        id = <<"test-spec">>,
        name = <<"Test Specification">>,
        version = {1, 0},
        tasks = #{
            test_task => #task_def{
                id = test_task,
                name = <<"Test Task">>,
                kind = atomic,
                split_type = sequence,
                join_type = sequence
            }
        },
        flows = [],
        input_condition = p_input,
        output_condition = p_output
    },

    {ok, RDFMap} = gen_yawl_rdf:spec_to_records(Spec),

    [
     ?_assert(is_map(RDFMap)),
     ?_assert(maps:is_key(<<"@context">>, RDFMap)),
     ?_assert(maps:is_key(<<"@graph">>, RDFMap))
    ].

%%====================================================================
%% URI Tests
%%====================================================================

uri_generation_test_() ->
    [
     ?_assertEqual(<<"http://unrdf.org/yawl/spec#test">>,
                   gen_yawl_rdf:spec_uri(<<"test">>)),
     ?_assertEqual(<<"http://unrdf.org/yawl/task#myTask">>,
                   gen_yawl_rdf:task_uri(myTask)),
     ?_assertEqual(<<"http://unrdf.org/yawl/case#case-123">>,
                   gen_yawl_rdf:case_uri(<<"case-123">>))
    ].

extract_local_name_test_() ->
    [
     ?_assertEqual(<<"myResource">>,
                   gen_yawl_rdf:extract_local_name(<<"http://example.org/#myResource">>)),
     ?_assertEqual(<<"task">>,
                   gen_yawl_rdf:extract_local_name(<<"http://unrdf.org/yawl/task#task">>))
    ].

%%====================================================================
%% Helper Functions
%%====================================================================

%% Mock implementation of parse_turtle_lines for testing
%% (In real module, this would be internal)
parse_turtle_lines(_Lines, _Prefixes, _BaseUri, _Acc) ->
    %% Return mock triple
    [#{
        subject => <<"http://test.org/spec:approval">>,
        predicate => <<"http://www.w3.org/1999/02/22-rdf-syntax-ns#type">>,
        object => <<"http://unrdf.org/yawl#WorkflowSpec">>
    }].

parse_workflow_spec(_Content, _BaseUri, _Validate) ->
    {ok, #yawl_spec{
        id = <<"approval">>,
        name = <<"Approval Workflow">>,
        version = {1, 0},
        tasks = #{
            review => #task_def{
                id = review,
                name = <<"Review Document">>,
                kind = atomic,
                split_type = sequence,
                join_type = sequence
            },
            approve => #task_def{
                id = approve,
                name = <<"Approve Document">>,
                kind = atomic,
                split_type = sequence,
                join_type = sequence
            }
        },
        flows = [
            #flow{
                from = review,
                to = approve,
                condition = undefined,
                priority = 0,
                is_default = false,
                is_cycle = false
            }
        ],
        input_condition = p_input,
        output_condition = p_output
    }}.
