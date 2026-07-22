fn candidate(lane: usize, reverse_tie: bool) -> 候補 {
    const REQUIRED_AUTHORITY: [u64; LANES] = [1, 2, 4, 3, 5, 6, 7, 1];
    const REQUIRED_READINESS: [u64; LANES] = [1, 2, 4, 3, 5, 6, 7, 2];
    const DETERMINISTIC: [bool; LANES] = [true, false, true, false, true, true, false, true];
    const RECEIPT_CAPABLE: [bool; LANES] = [true, true, false, true, false, true, true, true];
    const PREDICTED_TIME: [u32; LANES] = [0, 1, 5, 10, 20, 50, 100, 200];
    const FORWARD_SCORE: [u8; LANES] = [10, 20, 30, 40, 50, 60, 70, 80];
    const REVERSE_TIE_SCORE: [u8; LANES] = [80, 80, 70, 60, 50, 40, 30, 20];

    let score = if reverse_tie {
        REVERSE_TIE_SCORE[lane]
    } else {
        FORWARD_SCORE[lane]
    };
    let measures = 測度 {
        意味適合: score,
        証拠適合: score,
        権限適合: score,
        時間適合: score,
        後工程適合: score,
        信頼度: score,
        費用適合: score,
    };
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
    let fields = [
        measures.意味適合,
        measures.証拠適合,
        measures.権限適合,
        measures.時間適合,
        measures.後工程適合,
        measures.信頼度,
        measures.費用適合,
    ];
    let mut minimum = u8::MAX;
    let mut maximum = u8::MIN;
    for value in fields {
        minimum = minimum.min(value);
        maximum = maximum.max(value);
    }
    u16::from(minimum) * u16::from(maximum)
}

fn expected(policy: &方策<LANES>, request: 選択要求) -> ExpectedOutcome {
    let mut eligible_mask = 0_u64;
    let mut ready_mask = 0_u64;
    let mut winner = None;
    let mut winner_mass = 0_u16;

    for index in 0..policy.件数 {
        let 有無::有る(candidate) = policy.候補[index] else {
            continue;
        };
        let eligible = (request.取得権限 & candidate.必要権限) == candidate.必要権限
            && (!request.決定性必須 || candidate.決定的)
            && (!request.受領証必須 || candidate.受領証対応)
            && candidate.予測時間 <= request.最大時間;
        if eligible {
            eligible_mask |= 1_u64 << index;
        }
        let ready =
            eligible && (request.準備完了 & candidate.必要準備) == candidate.必要準備;
        if ready {
            ready_mask |= 1_u64 << index;
            let mass = oracle_mass(candidate.測度);
            if winner.is_none() || mass > winner_mass {
                winner = Some(index);
                winner_mass = mass;
            }
        }
    }

    match winner {
        Some(index) => ExpectedOutcome::Selected {
            index,
            mass: winner_mass,
            eligible_mask,
            ready_mask,
        },
        None => ExpectedOutcome::Refused {
            reason: if eligible_mask == 0 {
                拒否理由::適格候補なし
            } else {
                拒否理由::準備候補なし
            },
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
            if receipt.種別 != 受領種別::許可
                || receipt.道具 != 有無::有る(proposal.道具)
                || receipt.理由 != 有無::無い
            {
                return Err(EvidenceError::InvalidData {
                    identity: "tcps-blue-river-dam".to_owned(),
                    message: "authorization receipt does not bind the selected tool".to_owned(),
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
