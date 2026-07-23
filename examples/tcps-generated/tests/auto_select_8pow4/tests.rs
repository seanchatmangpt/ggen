fn assert_refusal<const N: usize>(
    policy: &方策<N>,
    request: 選択要求,
    expected: 拒否理由,
) -> (u64, u64, 作業領域<N>) {
    let mut workspace = 作業領域::新規();
    match 選択する(policy, request, &mut workspace) {
        選択結果::拒否 {
            理由,
            適格候補,
            準備候補,
            受領証,
        } => {
            assert_eq!(理由, expected);
            assert_eq!(受領証.種別, 受領種別::拒否);
            assert_eq!(受領証.理由, 有無::有る(expected.番号()));
            (適格候補, 準備候補, workspace)
        }
        選択結果::選択(proposal) => {
            panic!("expected {expected:?}, selected tool {}", proposal.道具)
        }
    }
}

fn one_candidate_policy(candidate: 候補) -> 方策<1> {
    let mut policy = 方策::空(77, 要約値([0x77; 32]));
    assert!(matches!(
        policy.候補を追加する(candidate),
        成否::成功(0)
    ));
    policy
}

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
    assert_eq!((cells[0].gray ^ cells[CELL_COUNT - 1].gray).count_ones(), 1);
});

chicago_tdd_tools::test!(heterogeneous_measures_make_the_mass_oracle_falsifiable, {
    for lane in 0..LANES {
        let measures = candidate(lane, false).測度;
        let fields = measure_fields(measures);
        let minimum = *fields.iter().min().expect("seven measure fields");
        let controlling: Vec<_> = fields
            .iter()
            .enumerate()
            .filter_map(|(index, value)| (*value == minimum).then_some(index))
            .collect();
        assert_eq!(controlling, vec![lane % 7], "lane {lane}");
        assert!(fields.iter().any(|value| *value != minimum));
        assert_eq!(measures.乗法質量(), oracle_mass(measures));
    }

    let original = heterogeneous_measures(20, 0);
    let mut non_minimum_changed = original;
    non_minimum_changed.証拠適合 = 250;
    assert_eq!(
        oracle_mass(non_minimum_changed),
        oracle_mass(original),
        "non-controlling measure must not change min-squared mass"
    );

    let mut minimum_changed = original;
    minimum_changed.意味適合 = 21;
    assert_ne!(oracle_mass(minimum_changed), oracle_mass(original));
    assert_eq!(
        minimum_changed.乗法質量(),
        oracle_mass(minimum_changed),
        "production and independent min-squared laws diverged"
    );
});

chicago_tdd_tools::test!(all_4096_cells_preserve_every_refusal_distinction, {
    let results = run_matrix().expect("all real selection judgments must satisfy the oracle");
    let receipt = matrix_receipt(results).expect("matrix receipt");
    assert_eq!(results.len(), CELL_COUNT);
    assert_eq!(receipt.schema, "tcps-auto-select-8pow4/v4");
    assert_eq!(receipt.cells, CELL_COUNT);
    assert_eq!(receipt.selected, 1_670);
    assert_eq!(receipt.refused_authority, 512);
    assert_eq!(receipt.refused_determinism, 256);
    assert_eq!(receipt.refused_receipt, 256);
    assert_eq!(receipt.refused_time, 240);
    assert_eq!(receipt.refused_no_ready, 1_162);
    assert_eq!(receipt.authorization_receipts, receipt.selected);
    assert_eq!(
        receipt.selected
            + receipt.refused_authority
            + receipt.refused_determinism
            + receipt.refused_receipt
            + receipt.refused_time
            + receipt.refused_no_ready,
        CELL_COUNT
    );
});

chicago_tdd_tools::test!(pinned_scenarios_have_independent_stage_outcomes, {
    let cases = [
        (
            Coordinates {
                authority: 0,
                readiness: 0,
                time_budget: 0,
                mode: 0,
            },
            OutcomeClass::RefusedAuthority,
            None,
            None,
            0,
            0,
        ),
        (
            Coordinates {
                authority: 2,
                readiness: 0,
                time_budget: 0,
                mode: 1,
            },
            OutcomeClass::RefusedDeterminism,
            None,
            None,
            0,
            0,
        ),
        (
            Coordinates {
                authority: 4,
                readiness: 0,
                time_budget: 0,
                mode: 2,
            },
            OutcomeClass::RefusedReceipt,
            None,
            None,
            0,
            0,
        ),
        (
            Coordinates {
                authority: 2,
                readiness: 0,
                time_budget: 0,
                mode: 0,
            },
            OutcomeClass::RefusedTime,
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
        assert_eq!(
            evidence.eligible_mask, eligible_mask,
            "{}",
            evidence.identity
        );
        assert_eq!(evidence.ready_mask, ready_mask, "{}", evidence.identity);
    }
});

chicago_tdd_tools::test!(empty_policy_and_each_hard_gate_have_distinct_refusals, {
    let base_request = 選択要求 {
        取得権限: 1,
        準備完了: 1,
        最大時間: 200,
        決定性必須: false,
        受領証必須: false,
        時刻: 1,
    };

    let empty = 方策::<1>::空(1, 要約値([1; 32]));
    assert_refusal(&empty, base_request, 拒否理由::適格候補なし);

    let mut denied = candidate(0, false);
    denied.必要権限 = 2;
    assert_refusal(
        &one_candidate_policy(denied),
        base_request,
        拒否理由::権限不足,
    );

    let mut nondeterministic = candidate(0, false);
    nondeterministic.決定的 = false;
    assert_refusal(
        &one_candidate_policy(nondeterministic),
        選択要求 {
            決定性必須: true,
            ..base_request
        },
        拒否理由::決定性不足,
    );

    let mut no_receipt = candidate(0, false);
    no_receipt.受領証対応 = false;
    assert_refusal(
        &one_candidate_policy(no_receipt),
        選択要求 {
            受領証必須: true,
            ..base_request
        },
        拒否理由::受領証不足,
    );

    let mut too_slow = candidate(0, false);
    too_slow.予測時間 = 1;
    assert_refusal(
        &one_candidate_policy(too_slow),
        選択要求 {
            最大時間: 0,
            ..base_request
        },
        拒否理由::時間超過,
    );

    let not_ready = candidate(0, false);
    assert_refusal(
        &one_candidate_policy(not_ready),
        選択要求 {
            準備完了: 0,
            ..base_request
        },
        拒否理由::準備候補なし,
    );
});

chicago_tdd_tools::test!(malformed_policies_are_refused_before_mask_construction, {
    let base = candidate(0, false);
    let request = 選択要求 {
        取得権限: u64::MAX,
        準備完了: u64::MAX,
        最大時間: u32::MAX,
        決定性必須: false,
        受領証必須: false,
        時刻: 9,
    };

    let hole = 方策 {
        方策番号: 1,
        候補: [有無::無い],
        件数: 1,
        方策要約: 要約値([1; 32]),
    };
    assert_refusal(&hole, request, 拒否理由::方策矛盾);

    let hidden_tail = 方策 {
        方策番号: 2,
        候補: [有無::有る(base)],
        件数: 0,
        方策要約: 要約値([2; 32]),
    };
    assert_refusal(&hidden_tail, request, 拒否理由::方策矛盾);

    let count_over_capacity = 方策 {
        方策番号: 3,
        候補: [有無::有る(base)],
        件数: 2,
        方策要約: 要約値([3; 32]),
    };
    assert_refusal(&count_over_capacity, request, 拒否理由::方策矛盾);

    let mask_overflow = 方策 {
        方策番号: 4,
        候補: [有無::有る(base); 65],
        件数: 65,
        方策要約: 要約値([4; 32]),
    };
    assert_refusal(&mask_overflow, request, 拒否理由::方策矛盾);
});

chicago_tdd_tools::test!(policy_constructor_enforces_the_u64_mask_capacity, {
    let mut policy = 方策::<65>::空(5, 要約値([5; 32]));
    for expected in 0..64 {
        assert!(matches!(
            policy.候補を追加する(candidate(expected % LANES, false)),
            成否::成功(index) if index == expected
        ));
    }
    assert!(matches!(
        policy.候補を追加する(candidate(0, false)),
        成否::失敗(())
    ));
    assert_eq!(policy.件数, 64);
    assert!(matches!(policy.候補[64], 有無::無い));
});

chicago_tdd_tools::test!(caller_scratch_is_cleared_before_every_judgment, {
    let policy = policy(false).expect("policy");
    let mut workspace = 作業領域::新規();

    let selected = 選択する(
        &policy,
        選択要求 {
            取得権限: 7,
            準備完了: 7,
            最大時間: 200,
            決定性必須: false,
            受領証必須: false,
            時刻: 1,
        },
        &mut workspace,
    );
    assert!(matches!(selected, 選択結果::選択(_)));
    assert!(workspace.質量.iter().any(|mass| *mass > 0));

    let refused = 選択する(
        &policy,
        選択要求 {
            取得権限: 0,
            準備完了: 0,
            最大時間: 0,
            決定性必須: false,
            受領証必須: false,
            時刻: 2,
        },
        &mut workspace,
    );
    assert!(matches!(
        refused,
        選択結果::拒否 {
            理由: 拒否理由::権限不足,
            ..
        }
    ));
    assert_eq!(workspace.質量, [0; LANES]);
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

chicago_tdd_tools::test!(
    blue_river_dam_refuses_each_independent_authorization_defect,
    {
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
    }
);

chicago_tdd_tools::test!(eight_way_sharding_is_deterministic_and_total, {
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
    assert_eq!(loads.iter().sum::<usize>(), CELL_COUNT);
});

chicago_tdd_tools::test!(merkle_root_is_replay_stable_and_mutation_sensitive, {
    let results = run_matrix().expect("matrix");
    let leaves = evidence_leaves(results).expect("leaves");
    let first = merkle_root("tcps/8pow4-root/v4", &leaves).expect("root");
    let second = merkle_root("tcps/8pow4-root/v4", &leaves).expect("replay root");
    assert_eq!(first, second);

    let mut mutated = leaves;
    mutated[2_048] = domain_digest(
        "tcps/8pow4-adversarial-mutation/v1",
        &[mutated[2_048].as_bytes()],
    );
    let mutation_root = merkle_root("tcps/8pow4-root/v4", &mutated).expect("mutation root");
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
    let receipt = matrix_receipt(results).expect("receipt");
    let receipt_json = serialize_json(&receipt).expect("receipt JSON");
    let scenarios = scenario_json_lines(results).expect("scenario JSONL");
    let plan = scenario_plan(results);
    let coverage = coverage_plan(&coverage_rails());
    let output = tempfile::TempDir::new().expect("tempdir");

    std::fs::write(output.path().join("matrix-receipt.json"), &receipt_json)
        .expect("write receipt");
    std::fs::write(output.path().join("scenario-evidence.jsonl"), &scenarios)
        .expect("write scenarios");
    std::fs::write(output.path().join("scenario-plan.tsv"), &plan).expect("write plan");
    std::fs::write(output.path().join("coverage-rails.tsv"), &coverage).expect("write coverage");
    std::fs::write(
        output.path().join("merkle-root.txt"),
        format!("{}\n", receipt.merkle_root),
    )
    .expect("write root");

    let decoded: JsonValue = serde_json::from_slice(&receipt_json).expect("parse receipt JSON");
    assert_eq!(
        decoded["schema"].as_str(),
        Some("tcps-auto-select-8pow4/v4")
    );
    assert_eq!(decoded["cells"].as_u64(), Some(CELL_COUNT as u64));
    assert_eq!(scenarios.lines().count(), CELL_COUNT);
    assert!(scenarios
        .lines()
        .all(|line| serde_json::from_str::<JsonValue>(line).is_ok()));
    assert_eq!(plan.lines().count(), CELL_COUNT + 1);
    assert_eq!(coverage.lines().count(), 1 + 8 + 64 + 512 + 4_096);
});
