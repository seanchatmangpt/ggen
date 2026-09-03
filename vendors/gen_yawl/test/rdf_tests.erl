%%====================================================================
%% rdf_tests - RDF/SPARQL Integration Tests
%%====================================================================
%% @doc Tests for RDF workflow specification loading and SPARQL queries.
%% Validates that workflow specs can be loaded from RDF and queried.

-module(rdf_tests).
-include_lib("eunit/include/eunit.hrl").
-include("include/gen_yawl.hrl").

%%====================================================================
%% Test Setup
%%====================================================================

setup_rdf_store() ->
    %% Initialize in-memory RDF store for testing
    {ok, _} = gen_yawl_rdf:start_link(),
    ok.

cleanup_rdf_store(_State) ->
    gen_yawl_rdf:stop(),
    ok.

%%====================================================================
%% RDF Loading Tests
%%====================================================================

rdf_load_test_() ->
    {setup,
     fun setup_rdf_store/0,
     fun cleanup_rdf_store/1,
     fun rdf_load_tests/1}.

rdf_load_tests(_State) ->
    [
        {"Load workflow spec from RDF TTL",
         fun() ->
            TTL = "
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:SimpleApproval a yawl:WorkflowSpec ;
                    yawl:specId \"simple-approval\" ;
                    yawl:specName \"Simple Approval Workflow\" ;
                    yawl:version \"1.0\" ;
                    yawl:hasTask ex:draft, ex:review, ex:approve .

                ex:draft a yawl:AtomicTask ;
                    yawl:taskId \"draft\" ;
                    yawl:taskName \"Draft Document\" .

                ex:review a yawl:AtomicTask ;
                    yawl:taskId \"review\" ;
                    yawl:taskName \"Review Document\" .

                ex:approve a yawl:AtomicTask ;
                    yawl:taskId \"approve\" ;
                    yawl:taskName \"Approve Document\" .
            ",

            {ok, Spec} = gen_yawl_rdf:load_workflow_from_ttl(TTL),
            ?assert(is_record(Spec, yawl_spec)),
            ?assertEqual(<<"simple-approval">>, Spec#yawl_spec.id),
            ?assertEqual(3, maps:size(Spec#yawl_spec.tasks))
         end},
        {"Parse task definitions from RDF",
         fun() ->
            TTL = io_lib:format("
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:Spec a yawl:WorkflowSpec ;
                    yawl:hasTask ex:task1 .

                ex:task1 a yawl:AtomicTask ;
                    yawl:taskId \"task1\" ;
                    yawl:taskName \"Task One\" ;
                    yawl:splitBehavior yawl:XOR_Split ;
                    yawl:joinBehavior yawl:AND_Join .
            ", []),

            {ok, Spec} = gen_yawl_rdf:load_workflow_from_ttl(list_to_binary(TTL)),
            Task = maps:get(task1, Spec#yawl_spec.tasks),

            ?assertEqual(task1, Task#task_def.id),
            ?assertEqual(<<"Task One">>, Task#task_def.name),
            ?assertEqual(xor, Task#task_def.split_type),
            ?assertEqual(and, Task#task_def.join_type)
         end},
        {"Parse flow definitions from RDF",
         fun() ->
            TTL = "
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:Spec a yawl:WorkflowSpec ;
                    yawl:hasTask ex:a, ex:b .

                ex:a a yawl:AtomicTask ;
                    yawl:taskId \"a\" .

                ex:b a yawl:AtomicTask ;
                    yawl:taskId \"b\" .

                ex:a yawl:flowsTo ex:b .
            ",

            {ok, Spec} = gen_yawl_rdf:load_workflow_from_ttl(TTL),
            ?assert(lists:any(fun(F) -> F#flow.from =:= a andalso F#flow.to =:= b end,
                               Spec#yawl_spec.flows))
         end},
        {"Error on invalid RDF",
         fun() ->
            InvalidTTL = <<"this is not valid turtle @#$%">>,
            Result = gen_yawl_rdf:load_workflow_from_ttl(InvalidTTL),
            ?assertMatch({error, _}, Result)
         end}
    ].

%%====================================================================
%% SPARQL Query Tests
%%====================================================================

sparql_query_test_() ->
    {setup,
     fun setup_rdf_store/0,
     fun cleanup_rdf_store/1,
     fun sparql_tests/1}.

sparql_tests(_State) ->
    [
        {"Query all tasks in workflow",
         fun() ->
            %% First load a workflow
            TTL = "
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:Workflow a yawl:WorkflowSpec ;
                    yawl:hasTask ex:t1, ex:t2, ex:t3 .

                ex:t1 a yawl:AtomicTask ; yawl:taskId \"t1\" .
                ex:t2 a yawl:AtomicTask ; yawl:taskId \"t2\" .
                ex:t3 a yawl:AtomicTask ; yawl:taskId \"t3\" .
            ",
            {ok, _Spec} = gen_yawl_rdf:load_workflow_from_ttl(TTL),

            %% Query tasks
            SPARQL = "
                PREFIX yawl: <http://unrdf.org/yawl#>
                SELECT ?taskId ?taskName
                WHERE {
                    ?spec a yawl:WorkflowSpec .
                    ?spec yawl:hasTask ?task .
                    ?task yawl:taskId ?taskId .
                }
            ",

            {ok, Results} = gen_yawl_rdf:sparql_query(SPARQL),
            ?assert(length(Results) >= 3)
         end},
        {"Query tasks by split type",
         fun() ->
            TTL = "
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:Spec a yawl:WorkflowSpec ;
                    yawl:hasTask ex:andTask .

                ex:andTask a yawl:AtomicTask ;
                    yawl:taskId \"andTask\" ;
                    yawl:splitBehavior yawl:AND_Split .
            ",
            {ok, _Spec} = gen_yawl_rdf:load_workflow_from_ttl(TTL),

            SPARQL = "
                PREFIX yawl: <http://unrdf.org/yawl#>
                SELECT ?taskId
                WHERE {
                    ?task a yawl:AtomicTask ;
                          yawl:splitBehavior yawl:AND_Split ;
                          yawl:taskId ?taskId .
                }
            ",

            {ok, Results} = gen_yawl_rdf:sparql_query(SPARQL),
            ?assert(length(Results) >= 1)
         end},
        {"Query enabled work items for case",
         fun() ->
            %% Create a running case
            {ok, Pid} = simple_sequence:start_link(),
            CaseId = gen_yawl:case_id(Pid),

            %% Query enabled work items
            {ok, WorkItems} = gen_yawl_rdf:query_enabled_work_items(CaseId),

            ?assert(is_list(WorkItems)),
            ?assert(length(WorkItems) >= 1),

            gen_yawl:stop(Pid)
         end}
    ].

%%====================================================================
%% RDF Serialization Tests
%%====================================================================

rdf_serialize_test_() ->
    {setup,
     fun setup_rdf_store/0,
     fun cleanup_rdf_store/1,
     fun serialize_tests/1}.

serialize_tests(_State) ->
    [
        {"Serialize workflow spec to RDF",
         fun() ->
            Spec = #yawl_spec{
                id = <<"test-spec">>,
                name = <<"Test Workflow">>,
                version = {1, 0},
                tasks = #{
                    task1 => #task_def{
                        id = task1,
                        name = <<"Task 1">>,
                        kind = atomic,
                        split_type = xor,
                        join_type = sequence
                    }
                },
                flows = [
                    #flow{from = input, to = task1}
                ],
                input_condition = input,
                output_condition = output
            },

            TTL = gen_yawl_rdf:spec_to_ttl(Spec),
            ?assert(is_binary(TTL)),
            ?assert(byte_size(TTL) > 0),

            %% Should be able to parse it back
            ?assert(string:find(TTL, "test-spec") =/= nomatch)
         end},
        {"Serialize case state to RDF",
         fun() ->
            {ok, Pid} = simple_sequence:start_link(),
            CaseId = gen_yawl:case_id(Pid),

            TTL = gen_yawl_rdf:case_state_to_ttl(Pid, CaseId),
            ?assert(is_binary(TTL)),

            gen_yawl:stop(Pid)
         end},
        {"Round-trip spec through RDF",
         fun() ->
            OriginalSpec = #yawl_spec{
                id = <<"roundtrip-test">>,
                name = <<"Round Trip Test">>,
                version = {1, 0},
                tasks = #{
                    t1 => #task_def{id = t1, name = <<"T1">>, kind = atomic},
                    t2 => #task_def{id = t2, name = <<"T2">>, kind = atomic}
                },
                flows = [
                    #flow{from = input, to = t1},
                    #flow{from = t1, to = t2},
                    #flow{from = t2, to = output}
                ],
                input_condition = input,
                output_condition = output
            },

            %% Serialize to TTL
            TTL = gen_yawl_rdf:spec_to_ttl(OriginalSpec),

            %% Parse back
            {ok, ParsedSpec} = gen_yawl_rdf:load_workflow_from_ttl(TTL),

            %% Verify key attributes match
            ?assertEqual(OriginalSpec#yawl_spec.id, ParsedSpec#yawl_spec.id),
            ?assertEqual(OriginalSpec#yawl_spec.name, ParsedSpec#yawl_spec.name),
            ?assertEqual(maps:size(OriginalSpec#yawl_spec.tasks),
                        maps:size(ParsedSpec#yawl_spec.tasks))
         end}
    ].

%%====================================================================
%% RDF Validation Tests
%%====================================================================

rdf_validation_test_() ->
    {setup,
     fun setup_rdf_store/0,
     fun cleanup_rdf_store/1,
     fun validation_tests/1}.

validation_tests(_State) ->
    [
        {"Validate required RDF properties present",
         fun() ->
            ValidTTL = "
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:Spec a yawl:WorkflowSpec ;
                    yawl:specId \"valid-spec\" ;
                    yawl:specName \"Valid Spec\" ;
                    yawl:version \"1.0\" ;
                    yawl:hasTask ex:t1 .

                ex:t1 a yawl:AtomicTask ; yawl:taskId \"t1\" .
            ",

            Result = gen_yawl_rdf:validate_workflow_ttl(ValidTTL),
            ?assertEqual({ok, valid}, Result)
         end},
        {"Detect missing spec ID",
         fun() ->
            InvalidTTL = "
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:Spec a yawl:WorkflowSpec ;
                    yawl:specName \"No ID\" .
            ",

            Result = gen_yawl_rdf:validate_workflow_ttl(InvalidTTL),
            ?assertMatch({error, {missing_property, _, specId}}, Result)
         end},
        {"Validate task references exist",
         fun() ->
            InvalidTTL = "
                @prefix yawl: <http://unrdf.org/yawl#> .
                @prefix ex: <http://example.org/> .

                ex:Spec a yawl:WorkflowSpec ;
                    yawl:specId \"bad-refs\" ;
                    yawl:hasTask ex:t1 .

                ex:t1 a yawl:AtomicTask ;
                    yawl:taskId \"t1\" ;
                    yawl:flowsTo ex:nonexistent .
            ",

            Result = gen_yawl_rdf:validate_workflow_ttl(InvalidTTL),
            ?assertMatch({error, {invalid_reference, _}}, Result)
         end}
    ].

%%====================================================================
%% RDF Case State Tests
%%====================================================================

case_state_rdf_test_() ->
    {foreach,
     fun() ->
        gen_yawl_rdf:start_link(),
        {ok, Pid} = simple_sequence:start_link(),
        {Pid, gen_yawl:case_id(Pid)}
     end,
     fun({Pid, _CaseId}) ->
        gen_yawl:stop(Pid),
        gen_yawl_rdf:stop()
     end,
     [
        fun({_Pid, CaseId}) ->
            {"Query case state via SPARQL",
             fun() ->
                SPARQL = io_lib:format("
                    PREFIX yawl: <http://unrdf.org/yawl#>
                    SELECT ?status
                    WHERE {
                        <case:~s> a yawl:WorkflowCase ;
                                  yawl:caseStatus ?status .
                    }
                ", [CaseId]),

                {ok, [[Status]]} = gen_yawl_rdf:sparql_query(list_to_binary(SPARQL)),
                ?assertEqual(<<"active">>, list_to_binary(Status))
             end}
         end,
        fun({Pid, _CaseId}) ->
            {"Query work items for case",
             fun() ->
                {ok, WorkItems} = gen_yawl_rdf:query_case_work_items(Pid),
                ?assert(is_list(WorkItems)),
                ?assert(length(WorkItems) > 0)
             end}
         end
     ]}.
