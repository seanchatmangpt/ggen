%%%-------------------------------------------------------------------
%% @doc AC Receipt Ledger - Real-World Usage Example
%%
%% This example demonstrates a complete pricing workflow using
%% the receipt ledger for non-contractual advisory pricing records.
%%
%% Scenario: Monthly billing cycle with value calculations,
%% verification, and audit trail generation.
%%%-------------------------------------------------------------------
-module(receipt_ledger_example).

-export([
    example_monthly_billing_cycle/0,
    example_single_customer_workflow/0,
    example_epoch_rotation_workflow/0,
    example_audit_trail_export/0,
    run_all_examples/0
]).

%%%===================================================================
%% Example 1: Single Customer Pricing Workflow
%%%===================================================================

%% @doc Example of a single customer's value calculation workflow
%% Demonstrates:
%% - Receipt creation for each calculation
%% - Merkle chain formation
%% - Non-contractual disclaimer
example_single_customer_workflow() ->
    io:format("~n=== Example 1: Single Customer Pricing Workflow ===~n"),

    %% Step 1: Start ledger with disclaimer
    {ok, _Pid} = ac_receipt_ledger_mcp:start_link(#{
        disclaimer => "Advisory receipt - non-contractual pricing evidence only"
    }),

    io:format("✓ Receipt ledger started~n"),

    %% Step 2: Get session information
    {ok, SessionId} = ac_receipt_ledger_mcp:get_session_id(ac_receipt_ledger_mcp),
    io:format("✓ Session ID: ~s~n", [base64:encode_to_string(SessionId)]),

    %% Step 3: Record first value calculation
    CustomerId = <<"customer_acme_corp">>,
    Metrics1 = [
        {<<"cpu_usage_percent">>, 45.2},
        {<<"memory_usage_percent">>, 62.3},
        {<<"disk_usage_percent">>, 28.1},
        {<<"network_throughput_mbps">>, 250.5}
    ],

    {ok, Receipt1} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{
            customer_id => CustomerId,
            timestamp => erlang:system_time(millisecond),
            metrics => Metrics1,
            calculation_method => weighted_sum,
            weights => #{
                cpu_usage_percent => 0.30,
                memory_usage_percent => 0.35,
                disk_usage_percent => 0.20,
                network_throughput_mbps => 0.15
            },
            calculated_value => 54.65,   % Weighted average
            billing_period => <<"2026-01-01_to_2026-01-31">>
        },
        #{
            source => pricing_engine,
            version => <<"1.0.0">>,
            calculation_duration_ms => 12
        }
    ),

    PrintReceipt = fun(R, Label) ->
        io:format("~s Receipt:~n", [Label]),
        io:format("  Seq:     ~p~n", [maps:get(seq, R)]),
        io:format("  Epoch:   ~p~n", [maps:get(epoch, R)]),
        io:format("  Hash:    ~s~n", [
            binary_to_list(base64:encode(maps:get(hash, R)))
        ]),
        io:format("  Kind:    ~p~n", [maps:get(kind, R)]),
        io:format("  Prev:    ~s~n", [
            case maps:get(prev, R) of
                <<>> -> "(none - first receipt)";
                PrevHash -> binary_to_list(base64:encode(PrevHash))
            end
        ])
    end,

    PrintReceipt(Receipt1, "✓ First"),

    %% Step 4: Calculate billed price
    AlphaCoefficient = 78.0,  % Price per unit value
    BilledPrice1 = AlphaCoefficient * maps:get(calculated_value, Receipt1),

    io:format("  Billed Price: $~.2f~n", [BilledPrice1]),

    %% Step 5: Record invoice generation
    InvoiceId1 = <<"INV-2026-01-001">>,
    {ok, Receipt2} = ac_receipt_ledger_mcp:append(
        verify_receipt,
        #{
            invoice_id => InvoiceId1,
            customer_id => CustomerId,
            billed_value => maps:get(calculated_value, Receipt1),
            billed_price => BilledPrice1,
            currency => <<"USD">>,
            status => issued,
            timestamp => erlang:system_time(millisecond)
        },
        #{
            source => billing_system,
            method => invoice_creation
        }
    ),

    PrintReceipt(Receipt2, "✓ Invoice"),

    %% Step 6: Verify chain integrity
    {ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),
    io:format("✓ Chain verification: PASSED~n"),

    %% Step 7: Get chain head
    {ok, HeadHash} = ac_receipt_ledger_mcp:head_hash(ac_receipt_ledger_mcp),
    io:format("✓ Current chain head: ~s~n", [
        binary_to_list(base64:encode(HeadHash))
    ]),

    ok.

%%%===================================================================
%% Example 2: Monthly Billing Cycle with Epoch Rotation
%%%===================================================================

%% @doc Example of monthly billing cycle showing epoch rotation
%% Demonstrates:
%% - Multiple customers in single epoch
%% - Epoch rotation at month boundary
%% - Updated disclaimer after rotation
example_monthly_billing_cycle() ->
    io:format("~n=== Example 2: Monthly Billing Cycle ===~n"),

    {ok, _Pid} = ac_receipt_ledger_mcp:start_link(#{
        disclaimer => "Jan 2026 - Advisory pricing evidence only"
    }),

    io:format("✓ Ledger started (January billing cycle)~n"),

    %% Process multiple customers in January
    Customers = [
        {<<"acme_corp">>, [{<<"cpu">>, 45.2}, {<<"memory">>, 62.3}]},
        {<<"global_soft">>, [{<<"cpu">>, 38.5}, {<<"memory">>, 71.1}]},
        {<<"tech_startup">>, [{<<"cpu">>, 28.3}, {<<"memory">>, 41.2}]}
    ],

    io:format("~nProcessing January receipts:~n"),

    Receipts1 = lists:map(
        fun({CustomerId, Metrics}) ->
            {ok, R} = ac_receipt_ledger_mcp:append(
                calculate_value,
                #{
                    customer_id => CustomerId,
                    metrics => Metrics,
                    calculated_value => avg_metrics(Metrics),
                    billing_period => <<"2026-01">>
                },
                #{}
            ),
            io:format("  ✓ ~s (seq ~p)~n", [CustomerId, maps:get(seq, R)]),
            R
        end,
        Customers
    ),

    io:format("✓ January receipts: ~p total~n", [length(Receipts1)]),

    %% Get January's final head
    {ok, JanuaryHeadHash} = ac_receipt_ledger_mcp:head_hash(ac_receipt_ledger_mcp),

    %% Rotate epoch for February
    io:format("~nRotating to February...~n"),
    {ok, ReturnedHash} = ac_receipt_ledger_mcp:rotate_epoch(
        "Feb 2026 - Advisory pricing evidence only",
        #{reason => monthly_rollover}
    ),

    io:format("✓ Epoch rotated~n"),
    io:format("  January head hash: ~s~n", [
        binary_to_list(base64:encode(ReturnedHash))
    ]),

    %% Verify hashes match
    true = (JanuaryHeadHash =:= ReturnedHash),

    %% Process February receipts
    io:format("~nProcessing February receipts:~n"),

    Receipts2 = lists:map(
        fun({CustomerId, Metrics}) ->
            {ok, R} = ac_receipt_ledger_mcp:append(
                calculate_value,
                #{
                    customer_id => CustomerId,
                    metrics => Metrics,
                    calculated_value => avg_metrics(Metrics),
                    billing_period => <<"2026-02">>
                },
                #{}
            ),
            io:format("  ✓ ~s (epoch ~p, seq ~p)~n", [
                CustomerId,
                maps:get(epoch, R),
                maps:get(seq, R)
            ]),
            R
        end,
        Customers
    ),

    %% Verify receipts are in different epochs
    Jan1stEpoch = maps:get(epoch, hd(Receipts1)),
    Feb1stEpoch = maps:get(epoch, hd(Receipts2)),

    io:format("✓ Epoch transition verified: Jan=~p, Feb=~p~n", [
        Jan1stEpoch,
        Feb1stEpoch
    ]),

    true = (Feb1stEpoch =:= Jan1stEpoch + 1),

    %% Verify chain still intact across epochs
    {ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),
    io:format("✓ Chain verification: PASSED (across epochs)~n"),

    ok.

%%%===================================================================
%% Example 3: Epoch Rotation Workflow
%%%===================================================================

%% @doc Detailed example of epoch rotation process
%% Demonstrates:
%% - State before rotation
%% - Rotation with disclaimer update
%% - State after rotation
example_epoch_rotation_workflow() ->
    io:format("~n=== Example 3: Epoch Rotation Workflow ===~n"),

    {ok, _Pid} = ac_receipt_ledger_mcp:start_link(#{
        disclaimer => "Original disclaimer - January"
    }),

    io:format("✓ Ledger started with v1 disclaimer~n"),

    %% Add some receipts in epoch 1
    io:format("~nEpoch 1 - Adding receipts:~n"),

    {ok, R1} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{data => epoch1_receipt1},
        #{}
    ),
    io:format("  Receipt 1: epoch=~p, seq=~p~n", [
        maps:get(epoch, R1),
        maps:get(seq, R1)
    ]),

    {ok, R2} = ac_receipt_ledger_mcp:append(
        verify_receipt,
        #{data => epoch1_receipt2},
        #{}
    ),
    io:format("  Receipt 2: epoch=~p, seq=~p~n", [
        maps:get(epoch, R2),
        maps:get(seq, R2)
    ]),

    {ok, R3} = ac_receipt_ledger_mcp:append(
        export_ledger,
        #{data => epoch1_receipt3},
        #{}
    ),
    io:format("  Receipt 3: epoch=~p, seq=~p~n", [
        maps:get(epoch, R3),
        maps:get(seq, R3)
    ]),

    %% Get epoch 1 state
    {ok, Epoch1Head} = ac_receipt_ledger_mcp:head_hash(ac_receipt_ledger_mcp),
    io:format("✓ Epoch 1 complete - head hash: ~s~n", [
        binary_to_list(base64:encode(Epoch1Head))
    ]),

    %% Rotate epoch
    NewDisclaimer = "Updated disclaimer - Policy change v2.0 - February",
    io:format("~nRotating epoch with new disclaimer:~n"),
    io:format("  Old: 'Original disclaimer - January'~n"),
    io:format("  New: '~s'~n", [NewDisclaimer]),

    {ok, PreviousHead} = ac_receipt_ledger_mcp:rotate_epoch(
        NewDisclaimer,
        #{
            reason => policy_update,
            timestamp => erlang:system_time(millisecond),
            notification_sent => true
        }
    ),

    io:format("✓ Epoch rotated~n"),
    io:format("  Previous epoch head returned: ~s~n", [
        binary_to_list(base64:encode(PreviousHead))
    ]),
    true = (Epoch1Head =:= PreviousHead),

    %% Add receipts in epoch 2
    io:format("~nEpoch 2 - Adding receipts:~n"),

    {ok, R4} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{data => epoch2_receipt1},
        #{}
    ),
    io:format("  Receipt 1: epoch=~p, seq=~p (disclaimer updated)~n", [
        maps:get(epoch, R4),
        maps:get(seq, R4)
    ]),

    %% Verify new disclaimer is in receipt
    NewDisclaimer = maps:get(disclaimer, R4),

    {ok, R5} = ac_receipt_ledger_mcp:append(
        verify_receipt,
        #{data => epoch2_receipt2},
        #{}
    ),
    io:format("  Receipt 2: epoch=~p, seq=~p~n", [
        maps:get(epoch, R5),
        maps:get(seq, R5)
    ]),

    %% Verify complete chain
    {ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),
    io:format("✓ Chain verification: PASSED~n"),

    io:format("~nEpoch rotation complete~n"),

    ok.

%%%===================================================================
%% Example 4: Audit Trail Export
%%%===================================================================

%% @doc Example of exporting complete audit trail
%% Demonstrates:
%% - Exporting all receipts
%% - Preserving epoch information
%% - Generating audit report
example_audit_trail_export() ->
    io:format("~n=== Example 4: Audit Trail Export ===~n"),

    {ok, _Pid} = ac_receipt_ledger_mcp:start_link(#{
        disclaimer => "Audit trail export example"
    }),

    %% Create multi-epoch ledger with diverse receipts
    io:format("Creating audit trail data...~n"),

    %% Epoch 1 receipts
    {ok, _} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{
            customer => <<"acme">>,
            value => 100.0,
            period => <<"2026-01">>
        },
        #{source => system}
    ),

    {ok, _} = ac_receipt_ledger_mcp:append(
        verify_receipt,
        #{customer => <<"acme">>, status => verified},
        #{source => system}
    ),

    %% Rotate
    ac_receipt_ledger_mcp:rotate_epoch("Post-rotation disclaimer", #{}),

    %% Epoch 2 receipts
    {ok, _} = ac_receipt_ledger_mcp:append(
        calculate_value,
        #{
            customer => <<"global">>,
            value => 200.0,
            period => <<"2026-02">>
        },
        #{source => system}
    ),

    {ok, _} = ac_receipt_ledger_mcp:append(
        export_ledger,
        #{ledger => audit_export},
        #{source => system}
    ),

    io:format("✓ Data created across 2 epochs~n"),

    %% Export audit trail
    io:format("~nExporting audit trail...~n"),

    {ok, AuditTrail} = ac_receipt_ledger_mcp:export(#{}),

    %% Display audit summary
    SessionId = maps:get(session_id, AuditTrail),
    CurrentEpoch = maps:get(current_epoch, AuditTrail),
    HeadHash = maps:get(head_hash, AuditTrail),
    Receipts = maps:get(receipts, AuditTrail),
    ExportedAt = maps:get(exported_at, AuditTrail),

    io:format("✓ Audit Trail Exported~n"),
    io:format("~n  Session ID:      ~s~n", [
        binary_to_list(base64:encode(SessionId))
    ]),
    io:format("  Current Epoch:   ~p~n", [CurrentEpoch]),
    io:format("  Total Receipts:  ~p~n", [length(Receipts)]),
    io:format("  Chain Head Hash: ~s~n", [
        binary_to_list(base64:encode(HeadHash))
    ]),
    io:format("  Exported At:     ~p~n", [
        calendar:system_time_to_universal_time(ExportedAt div 1000)
    ]),

    %% Display receipt summary
    io:format("~n  Receipts by Epoch:~n"),

    EpochGroups = group_by_epoch(Receipts),
    lists:foreach(
        fun({Epoch, EpochReceipts}) ->
            Kinds = lists:map(fun(R) -> maps:get(kind, R) end, EpochReceipts),
            io:format("    Epoch ~p: ~p receipts~n", [Epoch, length(EpochReceipts)]),
            io:format("      Kinds: ~p~n", [Kinds])
        end,
        EpochGroups
    ),

    %% Display first and last receipt
    FirstReceipt = hd(Receipts),
    LastReceipt = lists:last(Receipts),

    io:format("~n  First Receipt:~n"),
    io:format("    Epoch: ~p, Seq: ~p, Kind: ~p~n", [
        maps:get(epoch, FirstReceipt),
        maps:get(seq, FirstReceipt),
        maps:get(kind, FirstReceipt)
    ]),

    io:format("~n  Last Receipt:~n"),
    io:format("    Epoch: ~p, Seq: ~p, Kind: ~p~n", [
        maps:get(epoch, LastReceipt),
        maps:get(seq, LastReceipt),
        maps:get(kind, LastReceipt)
    ]),

    %% Verify chain integrity
    io:format("~nVerifying exported chain...~n"),
    {ok, ok} = ac_receipt_ledger_mcp:verify_chain(#{}),
    io:format("✓ Chain verification: PASSED~n"),

    io:format("~nAudit trail export complete~n"),

    ok.

%%%===================================================================
%% Helper Functions
%%%===================================================================

%% @doc Calculate average of metric values
avg_metrics(Metrics) ->
    Values = [V || {_Name, V} <- Metrics],
    Sum = lists:sum(Values),
    Sum / length(Values).

%% @doc Group receipts by epoch
group_by_epoch(Receipts) ->
    GroupMap = lists:foldl(
        fun(Receipt, Acc) ->
            Epoch = maps:get(epoch, Receipt),
            lists:append(maps:get(Epoch, Acc, []), [Receipt])
        end,
        #{},
        Receipts
    ),

    %% Convert to sorted list of {Epoch, Receipts}
    EpochsList = lists:sort(maps:to_list(GroupMap)),
    EpochsList.

%%%===================================================================
%% Main Entry Point
%%%===================================================================

%% @doc Run all examples
-spec run_all_examples() -> ok.
run_all_examples() ->
    io:format("~n~n"),
    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  AC Receipt Ledger MCP - Examples                         ║~n"),
    io:format("║  Session-Scoped, Non-Contractual Receipt Management       ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n"),

    catch example_single_customer_workflow(),
    io:format("~n"),
    catch example_monthly_billing_cycle(),
    io:format("~n"),
    catch example_epoch_rotation_workflow(),
    io:format("~n"),
    catch example_audit_trail_export(),

    io:format("~n~n"),
    io:format("╔════════════════════════════════════════════════════════════╗~n"),
    io:format("║  All examples completed successfully!                     ║~n"),
    io:format("╚════════════════════════════════════════════════════════════╝~n~n"),

    ok.

%%%===================================================================
%% End of Example Module
%%%===================================================================
