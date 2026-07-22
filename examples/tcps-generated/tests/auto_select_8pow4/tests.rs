chicago_tdd_tools::test!(contracts_name_the_real_selection_and_authorization_boundaries, {
    let registry = TestContractRegistry::new(CONTRACTS);
    assert_eq!(registry.len(), 3);
    assert!(registry
        .uncovered_modules(&[
            "tcps.auto-select",
            "tcps.8pow4.matrix",
            "tcps.blue-river-dam",
            "tcps.8pow4.receipt",
        ])
        .is_empty());
});

chicago_tdd_tools::test!(gray_codec_matches_independent_known_vectors, {
    const FIRST_SIXTEEN: [u16; 16] = [0, 1, 3, 2, 6, 7, 5, 4, 12, 13, 15, 14, 10, 11, 9, 8];
    assert_eq!((0_u16..16).map(gray).collect::<Vec<_>>(), FIRST_SIXTEEN);
    for value in 0_u16..u16::try_from(CELL_COUNT).expect("cell count fits") {
        assert_eq!(ungray(gray(value)), value);
    }
});

chicago_tdd_tools::test!(gf8_uses_the_pinned_x3_plus_x_plus_1_field, {
    const TABLE: [[u8; WIDTH]; WIDTH] = [
        [0, 0, 0, 0, 0, 0, 0, 0],
        [0, 1, 2, 3, 4, 5, 6, 7],
        [0, 2, 4, 6, 3, 1, 7, 5],
        [0, 3, 6, 5, 7, 4, 1, 2],
        [0, 4, 3, 7, 6, 2, 5, 1],
        [0, 5, 1, 4, 2, 7, 3, 6],
        [0, 6, 7, 1, 5, 3, 2, 4],
        [0, 7, 5, 2, 1, 6, 4, 3],
    ];
    for left in 0..WIDTH as u8 {
        for right in 0..WIDTH as u8 {
            assert_eq!(
                gf8_mul(left, right),
                TABLE[usize::from(left)][usize::from(right)]
            );
        }
    }
});

chicago_tdd_tools::test!(coverage_ladder_has_real_strength_one_through_four, {
    let rails = coverage_rails();
    assert_eq!(rails.each_ref().map(Vec::len), [8, 64, 512, 4_096]);

    for axis in 0..DIMENSIONS {
        let states: BTreeSet<_> = rails[0]
            .iter()
            .map(|coordinates| match axis {
                0 => coordinates.authority,
                1 => coordinates.readiness,
                2 => coordinates.time_budget,
                _ => coordinates.mode,
            })
            .collect();
        assert_eq!(states.len(), WIDTH);
    }

    for left in 0..DIMENSIONS {
        for right in (left + 1)..DIMENSIONS {
            let pairs: BTreeSet<_> = rails[1]
                .iter()
                .map(|coordinates| {
                    let values = [
                        coordinates.authority,
                        coordinates.readiness,
                        coordinates.time_budget,
                        coordinates.mode,
                    ];
                    (values[left], values[right])
                })
                .collect();
            assert_eq!(pairs.len(), WIDTH.pow(2));
        }
    }

    for omitted in 0..DIMENSIONS {
        let triples: BTreeSet<_> = rails[2]
            .iter()
            .map(|coordinates| {
                let values = [
                    coordinates.authority,
                    coordinates.readiness,
                    coordinates.time_budget,
                    coordinates.mode,
                ];
                let kept: Vec<_> = values
                    .into_iter()
                    .enumerate()
                    .filter_map(|(index, value)| (index != omitted).then_some(value))
                    .collect();
                [kept[0], kept[1], kept[2]]
            })
            .collect();
        assert_eq!(triples.len(), WIDTH.pow(3));
    }

    assert_eq!(
        rails[3].iter().copied().collect::<BTreeSet<_>>().len(),
        CELL_COUNT
    );
});

chicago_tdd_tools::test!(exhaustive_cube_is_cyclic_gray_and_coordinate_complete, {
    let cells = cells().expect("cube");
    assert_eq!(cells.len(), CELL_COUNT);
    assert_eq!(
        cells
            .iter()
            .map(|cell| cell.coordinates)
            .collect::<BTreeSet<_>>()
            .len(),
        CELL_COUNT
    );
    for cell in &cells {
        assert_eq!(cell.coordinates.packed(), cell.gray);
        assert_eq!(ungray(cell.gray), cell.ordinal);
    }
    for pair in cells.windows(2) {
        assert_eq!((pair[0].gray ^ pair[1].gray).count_ones(), 1);
    }
    assert_eq!(
        (cells[0].gray ^ cells[CELL_COUNT - 1].gray).count_ones(),
        1
    );
});

chicago_tdd_tools::test!(all_4096_cells_execute_the_real_auto_select_contract, {
    let results = run_matrix().expect("all real selection judgments must satisfy the oracle");
    let receipt = matrix_receipt(&results).expect("matrix receipt");
    assert_eq!(results.len(), CELL_COUNT);
    assert_eq!(receipt.cells, CELL_COUNT);
    assert_eq!(receipt.selected, 1_670);
    assert_eq!(receipt.refused_no_eligible, 1_264);
    assert_eq!(receipt.refused_no_ready, 1_162);
    assert_eq!(receipt.authorization_receipts, receipt.selected);
    assert_eq!(
        receipt.selected + receipt.refused_no_eligible + receipt.refused_no_ready,
        CELL_COUNT
    );
});

chicago_tdd_tools::test!(pinned_scenarios_have_independent_expected_outcomes, {
    let cases = [
        (
            Coordinates {
                authority: 0,
                readiness: 0,
                time_budget: 0,
                mode: 0,
            },
            OutcomeClass::RefusedNoEligible,
            None,
            None,
            0,
            0,
        ),
        (
            Coordinates {
                authority: 1,
                readiness: 0,
                time_budget: 0,
                mode: 0,
            },
            OutcomeClass::RefusedNoReady,
            None,
            None,
            1,
            0,
        ),
        (
            Coordinates {
                authority: 1,
                readiness: 1,
                time_budget: 0,
                mode: 0,
            },
            OutcomeClass::Selected,
            Some(1),
            Some(100),
            1,
            1,
        ),
        (
            Coordinates {
                authority: 1,
                readiness: 2,
                time_budget: 7,
                mode: 0,
            },
            OutcomeClass::Selected,
            Some(8),
            Some(6_400),
            129,
            128,
        ),
        (
            Coordinates {
                authority: 1,
                readiness: 1,
                time_budget: 0,
                mode: 4,
            },
            OutcomeClass::Selected,
            Some(1),
            Some(6_400),
            1,
            1,
        ),
    ];

    for (coordinates, outcome, tool, mass, eligible_mask, ready_mask) in cases {
        let gray = coordinates.packed();
        let cell = Cell {
            ordinal: ungray(gray),
            gray,
            coordinates,
        };
        let evidence = execute_cell(cell).expect("pinned scenario must execute");
        assert_eq!(evidence.outcome, outcome, "{}", evidence.identity);
        assert_eq!(evidence.selected_tool, tool, "{}", evidence.identity);
        assert_eq!(evidence.selected_mass, mass, "{}", evidence.identity);
        assert_eq!(evidence.eligible_mask, eligible_mask, "{}", evidence.identity);
        assert_eq!(evidence.ready_mask, ready_mask, "{}", evidence.identity);
    }
});

chicago_tdd_tools::test!(reverse_policy_exercises_the_first_lane_tie_break, {
    let policy = policy(true).expect("reverse tie policy");
    let request = 選択要求 {
        取得権限: 7,
        準備完了: 7,
        最大時間: 200,
        決定性必須: false,
        受領証必須: false,
        時刻: 1,
    };
    let mut workspace = 作業領域::新規();
    match 選択する(&policy, request, &mut workspace) {
        選択結果::選択(proposal) => {
            assert_eq!(proposal.道具, 1);
            assert_eq!(proposal.質量, 80_u16 * 80_u16);
        }
        選択結果::拒否 { 理由, .. } => panic!("tie policy refused: {理由:?}"),
    }
});

chicago_tdd_tools::test!(blue_river_dam_refuses_each_independent_authorization_defect, {
    let proposal = known_selected_proposal();
    let now = 100_u64;

    let broker = 仲介者 {
        必要権限: BROKER_AUTHORITY,
        方策要約: proposal.方策要約,
    };
    assert!(matches!(
        broker.許可する(
            proposal,
            能力札 {
                許可道具: proposal.道具.wrapping_add(1),
                権限: BROKER_AUTHORITY,
                有効期限: now,
            },
            now,
        ),
        成否::失敗(許可拒否::道具不一致)
    ));
    assert!(matches!(
        broker.許可する(
            proposal,
            能力札 {
                許可道具: proposal.道具,
                権限: BROKER_AUTHORITY,
                有効期限: now - 1,
            },
            now,
        ),
        成否::失敗(許可拒否::期限切れ)
    ));
    assert!(matches!(
        broker.許可する(
            proposal,
            能力札 {
                許可道具: proposal.道具,
                権限: 0,
                有効期限: now,
            },
            now,
        ),
        成否::失敗(許可拒否::権限なし)
    ));

    let wrong_policy = 仲介者 {
        必要権限: BROKER_AUTHORITY,
        方策要約: 要約値([0xff; 32]),
    };
    assert!(matches!(
        wrong_policy.許可する(
            proposal,
            能力札 {
                許可道具: proposal.道具,
                権限: BROKER_AUTHORITY,
                有効期限: now,
            },
            now,
        ),
        成否::失敗(許可拒否::方策不一致)
    ));
});

chicago_tdd_tools::test!(eight_way_sharding_is_deterministic_total_and_balanced, {
    let results = run_matrix().expect("matrix");
    let first: Vec<_> = results
        .iter()
        .map(|result| rendezvous_shard(&result.identity, 8).expect("shard"))
        .collect();
    let second: Vec<_> = results
        .iter()
        .map(|result| rendezvous_shard(&result.identity, 8).expect("shard"))
        .collect();
    assert_eq!(first, second);

    let mut loads = [0_usize; 8];
    for owner in first {
        loads[owner] += 1;
    }
    assert!(loads.iter().all(|load| *load > 0));
    let minimum = loads.iter().copied().min().expect("minimum");
    let maximum = loads.iter().copied().max().expect("maximum");
    assert!(maximum - minimum < CELL_COUNT / 8);
});

chicago_tdd_tools::test!(merkle_root_is_replay_stable_and_mutation_sensitive, {
    let results = run_matrix().expect("matrix");
    let leaves = evidence_leaves(&results).expect("leaves");
    let first = merkle_root("tcps/8pow4-root/v2", &leaves).expect("root");
    let second = merkle_root("tcps/8pow4-root/v2", &leaves).expect("replay root");
    assert_eq!(first, second);

    let mut mutated = leaves;
    mutated[2_048] = domain_digest(
        "tcps/8pow4-adversarial-mutation/v1",
        &[mutated[2_048].as_bytes()],
    );
    let mutation_root = merkle_root("tcps/8pow4-root/v2", &mutated).expect("mutation root");
    assert_ne!(mutation_root, first);
});

chicago_tdd_tools::test!(invalid_coordinate_is_refused_before_execution, {
    let invalid = Coordinates {
        authority: 8,
        readiness: 0,
        time_budget: 0,
        mode: 0,
    };
    assert!(matches!(
        invalid.validate(),
        Err(EvidenceError::InvalidData { message, .. }) if message.starts_with("INVALID_AXIS_STATE:")
    ));
});

chicago_tdd_tools::test!(artifact_bundle_contains_parseable_complete_evidence, {
    let results = run_matrix().expect("matrix");
    let receipt = matrix_receipt(&results).expect("receipt");
    let receipt_json = serialize_json(&receipt).expect("receipt JSON");
    let scenarios = scenario_json_lines(&results).expect("scenario JSONL");
    let plan = scenario_plan(&results);
    let coverage = coverage_plan(&coverage_rails());
    let output = tempfile::TempDir::new().expect("tempdir");

    std::fs::write(output.path().join("matrix-receipt.json"), &receipt_json)
        .expect("write receipt");
    std::fs::write(output.path().join("scenario-evidence.jsonl"), &scenarios)
        .expect("write scenarios");
    std::fs::write(output.path().join("scenario-plan.tsv"), &plan).expect("write plan");
    std::fs::write(output.path().join("coverage-rails.tsv"), &coverage)
        .expect("write coverage");
    std::fs::write(
        output.path().join("merkle-root.txt"),
        format!("{}\n", receipt.merkle_root),
    )
    .expect("write root");

    let decoded: JsonValue = serde_json::from_slice(&receipt_json).expect("parse receipt JSON");
    assert_eq!(decoded["cells"].as_u64(), Some(CELL_COUNT as u64));
    assert_eq!(scenarios.lines().count(), CELL_COUNT);
    assert!(scenarios
        .lines()
        .all(|line| serde_json::from_str::<JsonValue>(line).is_ok()));
    assert_eq!(plan.lines().count(), CELL_COUNT + 1);
    assert_eq!(coverage.lines().count(), 1 + 8 + 64 + 512 + 4_096);
});
