fn candidate(lane: usize, reverse_tie: bool) -> 候補 {
    const REQUIRED_AUTHORITY: [u64; LANES] = [1, 2, 4, 3, 5, 6, 7, 1];
    const REQUIRED_READINESS: [u64; LANES] = [1, 2, 4, 3, 5, 6, 7, 2];
    const DETERMINISTIC: [bool; LANES] = [true, false, true, false, true, true, false, true];
    const RECEIPT_CAPABLE: [bool; LANES] = [true, true, false, true, false, true, true, true];
    const PREDICTED_TIME: [u32; LANES] = [0, 1, 5, 10, 20, 50, 100, 200];
    const FORWARD_FLOOR: [u8; LANES] = [10, 20, 30, 40, 50, 60, 70, 80];
    const REVERSE_TIE_FLOOR: [u8; LANES] = [80, 80, 70, 60, 50, 40, 30, 20];

    let floor = if reverse_tie {
        REVERSE_TIE_FLOOR[lane]
    } else {
        FORWARD_FLOOR[lane]
    };
    let measures = heterogeneous_measures(floor, lane % 7);
    let kind = match lane % 5 {
        0 => 認知品種::正確なグラフ検索,
        1 => 認知品種::決定的規則推論,
        2 => 認知品種::近似意味検索,
        3 => 認知品種::計画器,
        _ => 認知品種::検証器,
    };

    候補 {
        道具: u16::try_from(lane + 1).expect("lane fits tool id"),
        経路: u16::try_from(100 + lane).expect("lane fits route id"),
        品種: kind,
        必要権限: REQUIRED_AUTHORITY[lane],
        必要準備: REQUIRED_READINESS[lane],
        決定的: DETERMINISTIC[lane],
        受領証対応: RECEIPT_CAPABLE[lane],
        予測時間: PREDICTED_TIME[lane],
        測度: measures,
    }
}

fn heterogeneous_measures(floor: u8, controlling_field: usize) -> 測度 {
    let mut fields = [
        floor.saturating_add(11),
        floor.saturating_add(13),
        floor.saturating_add(17),
        floor.saturating_add(19),
        floor.saturating_add(23),
        floor.saturating_add(29),
        floor.saturating_add(31),
    ];
    fields[controlling_field] = floor;
    測度 {
        意味適合: fields[0],
        証拠適合: fields[1],
        権限適合: fields[2],
        時間適合: fields[3],
        後工程適合: fields[4],
        信頼度: fields[5],
        費用適合: fields[6],
    }
}

fn measure_fields(measures: 測度) -> [u8; 7] {
    [
        measures.意味適合,
        measures.証拠適合,
        measures.権限適合,
        measures.時間適合,
        measures.後工程適合,
        measures.信頼度,
        measures.費用適合,
    ]
}

fn policy(reverse_tie: bool) -> Result<方策<LANES>, EvidenceError> {
    let policy_number = if reverse_tie { 2 } else { 1 };
    let policy_digest = 要約値([u8::from(reverse_tie); 32]);
    let mut policy = 方策::空(policy_number, policy_digest);
    for lane in 0..LANES {
        match policy.候補を追加する(candidate(lane, reverse_tie)) {
            成否::成功(index) if index == lane => {}
            成否::成功(index) => {
                return Err(EvidenceError::InvalidData {
                    identity: "tcps-policy-construction".to_owned(),
                    message: format!("candidate inserted at {index}, expected {lane}"),
                });
            }
            成否::失敗(()) => {
                return Err(EvidenceError::InvalidData {
                    identity: "tcps-policy-construction".to_owned(),
                    message: format!("candidate lane {lane} refused"),
                });
            }
        }
    }
    Ok(policy)
}

fn request(cell: Cell) -> 選択要求 {
    let mode = RequestMode::from_state(cell.coordinates.mode);
    選択要求 {
        取得権限: u64::from(cell.coordinates.authority),
        準備完了: u64::from(cell.coordinates.readiness),
        最大時間: TIME_BUDGETS[usize::from(cell.coordinates.time_budget)],
        決定性必須: mode.deterministic_required,
        受領証必須: mode.receipt_required,
        時刻: u64::from(cell.ordinal) + 1,
    }
}

fn oracle_mass(measures: 測度) -> u16 {
    let mut minimum = u8::MAX;
    for value in measure_fields(measures) {
        minimum = minimum.min(value);
    }
    u16::from(minimum) * u16::from(minimum)
}

fn oracle_request_digest(request: 選択要求) -> 要約値 {
    let mut hasher = Sha256::new();
    hasher.update(request.取得権限.to_le_bytes());
    hasher.update(request.準備完了.to_le_bytes());
    hasher.update(request.最大時間.to_le_bytes());
    hasher.update([u8::from(request.決定性必須)]);
    hasher.update([u8::from(request.受領証必須)]);
    hasher.update(request.時刻.to_le_bytes());
    要約値(hasher.finalize().into())
}

fn validate_decision_receipt(
    receipt: 受領証,
    policy: &方策<LANES>,
    request: 選択要求,
    expected_kind: 受領種別,
    expected_tool: 有無<u16>,
    expected_reason: 有無<u16>,
) -> Result<(), EvidenceError> {
    if receipt.受領番号 != request.時刻
        || receipt.種別 != expected_kind
        || receipt.工程 != 0
        || receipt.時刻 != request.時刻
        || receipt.標準番号 != policy.方策番号
        || receipt.異常 != 有無::無い
        || receipt.道具 != expected_tool
        || receipt.理由 != expected_reason
        || receipt.前要約 != oracle_request_digest(request)
        || receipt.後要約 != policy.方策要約
    {
        return Err(EvidenceError::InvalidData {
            identity: "tcps-decision-receipt".to_owned(),
            message: format!("receipt fields violate the decision contract: {receipt:?}"),
        });
    }
    Ok(())
}

fn expected(policy: &方策<LANES>, request: 選択要求) -> ExpectedOutcome {
    if policy.件数 == 0 {
        return ExpectedOutcome::Refused {
            reason: 拒否理由::適格候補なし,
            eligible_mask: 0,
            ready_mask: 0,
        };
    }

    let mut authority_mask = 0_u64;
    let mut determinism_mask = 0_u64;
    let mut receipt_mask = 0_u64;
    let mut eligible_mask = 0_u64;
    let mut ready_mask = 0_u64;
    let mut winner = None;
    let mut winner_mass = 0_u16;

    for index in 0..policy.件数 {
        let 有無::有る(candidate) = policy.候補[index] else {
            return ExpectedOutcome::Refused {
                reason: 拒否理由::方策矛盾,
                eligible_mask: 0,
                ready_mask: 0,
            };
        };
        let bit = 1_u64 << index;
        if (request.取得権限 & candidate.必要権限) == candidate.必要権限 {
            authority_mask |= bit;
            if !request.決定性必須 || candidate.決定的 {
                determinism_mask |= bit;
                if !request.受領証必須 || candidate.受領証対応 {
                    receipt_mask |= bit;
                    if candidate.予測時間 <= request.最大時間 {
                        eligible_mask |= bit;
                        if (request.準備完了 & candidate.必要準備)
                            == candidate.必要準備
                        {
                            ready_mask |= bit;
                            let mass = oracle_mass(candidate.測度);
                            if winner.is_none() || mass > winner_mass {
                                winner = Some(index);
                                winner_mass = mass;
                            }
                        }
                    }
                }
            }
        }
    }

    let reason = if authority_mask == 0 {
        Some(拒否理由::権限不足)
    } else if request.決定性必須 && determinism_mask == 0 {
        Some(拒否理由::決定性不足)
    } else if request.受領証必須 && receipt_mask == 0 {
        Some(拒否理由::受領証不足)
    } else if eligible_mask == 0 {
        Some(拒否理由::時間超過)
    } else if ready_mask == 0 {
        Some(拒否理由::準備候補なし)
    } else {
        None
    };

    match (reason, winner) {
        (Some(reason), _) => ExpectedOutcome::Refused {
            reason,
            eligible_mask,
            ready_mask,
        },
        (None, Some(index)) => ExpectedOutcome::Selected {
            index,
            mass: winner_mass,
            eligible_mask,
            ready_mask,
        },
        (None, None) => ExpectedOutcome::Refused {
            reason: 拒否理由::方策矛盾,
            eligible_mask,
            ready_mask,
        },
    }
}

fn receipt_digest(receipt: 受領証) -> EvidenceDigest {
    let kind = match receipt.種別 {
        受領種別::生産 => 0,
        受領種別::停止 => 1,
        受領種別::改善 => 2,
        受領種別::標準更新 => 3,
        受領種別::引取り => 4,
        受領種別::補充 => 5,
        受領種別::選択 => 6,
        受領種別::許可 => 7,
        受領種別::実行 => 8,
        受領種別::拒否 => 9,
    };
    let (tool_tag, tool_value) = match receipt.道具 {
        有無::有る(value) => (1_u8, value),
        有無::無い => (0_u8, 0),
    };
    let (reason_tag, reason_value) = match receipt.理由 {
        有無::有る(value) => (1_u8, value),
        有無::無い => (0_u8, 0),
    };
    let abnormal = u8::from(matches!(receipt.異常, 有無::有る(_)));

    domain_digest(
        "tcps/receipt/v2",
        &[
            &receipt.受領番号.to_le_bytes(),
            &[kind],
            &receipt.工程.to_le_bytes(),
            &receipt.時刻.to_le_bytes(),
            &receipt.標準番号.to_le_bytes(),
            &[abnormal, tool_tag, reason_tag],
            &tool_value.to_le_bytes(),
            &reason_value.to_le_bytes(),
            &receipt.前要約.0,
            &receipt.後要約.0,
        ],
    )
}

fn validate_workspace(
    policy: &方策<LANES>,
    expected: ExpectedOutcome,
    workspace: &作業領域<LANES>,
) -> Result<(), EvidenceError> {
    let ready_mask = match expected {
        ExpectedOutcome::Selected { ready_mask, .. }
        | ExpectedOutcome::Refused { ready_mask, .. } => ready_mask,
    };
    for index in 0..LANES {
        let expected_mass = if ready_mask & (1_u64 << index) != 0 {
            match policy.候補[index] {
                有無::有る(candidate) => oracle_mass(candidate.測度),
                有無::無い => 0,
            }
        } else {
            0
        };
        if workspace.質量[index] != expected_mass {
            return Err(EvidenceError::InvalidData {
                identity: "tcps-auto-select-workspace".to_owned(),
                message: format!(
                    "lane {index}: expected mass {expected_mass}, found {}",
                    workspace.質量[index]
                ),
            });
        }
    }
    Ok(())
}

fn authorize(proposal: 選択提案, now: u64) -> Result<受領証, EvidenceError> {
    let broker = 仲介者 {
        必要権限: BROKER_AUTHORITY,
        方策要約: proposal.方策要約,
    };
    let capability = 能力札 {
        許可道具: proposal.道具,
        権限: BROKER_AUTHORITY,
        有効期限: now,
    };
    match broker.許可する(proposal, capability, now) {
        成否::成功(authorized) => {
            let receipt = authorized.許可受領証();
            if authorized.道具() != proposal.道具
                || authorized.提案() != proposal
                || authorized.能力札() != capability
                || receipt.受領番号 != now
                || receipt.種別 != 受領種別::許可
                || receipt.工程 != 0
                || receipt.時刻 != now
                || receipt.標準番号 != proposal.方策番号
                || receipt.異常 != 有無::無い
                || receipt.道具 != 有無::有る(proposal.道具)
                || receipt.理由 != 有無::無い
                || receipt.前要約 != proposal.受領証.後要約
                || receipt.後要約 != proposal.方策要約
            {
                return Err(EvidenceError::InvalidData {
                    identity: "tcps-blue-river-dam".to_owned(),
                    message: "authorization output violates the broker contract".to_owned(),
                });
            }
            Ok(receipt)
        }
        成否::失敗(reason) => Err(EvidenceError::InvalidData {
            identity: "tcps-blue-river-dam".to_owned(),
            message: format!("matching capability was refused: {reason:?}"),
        }),
    }
}
