fn execute_cell(cell: Cell) -> Result<ScenarioEvidence, EvidenceError> {
    let mode = RequestMode::from_state(cell.coordinates.mode);
    let policy = policy(mode.reverse_tie_policy)?;
    let request = request(cell);
    let expected = expected(&policy, request);
    let mut workspace = 作業領域::新規();
    let actual = 選択する(&policy, request, &mut workspace);
    validate_workspace(&policy, expected, &workspace)?;

    let identity = cell.coordinates.identity();
    let (
        outcome,
        eligible_mask,
        ready_mask,
        selected_tool,
        selected_mass,
        selection_receipt,
        authorization_receipt,
    ) = match (expected, actual) {
        (
            ExpectedOutcome::Selected {
                index,
                mass,
                eligible_mask,
                ready_mask,
            },
            選択結果::選択(proposal),
        ) => {
            let 有無::有る(expected_candidate) = policy.候補[index] else {
                return Err(EvidenceError::InvalidData {
                    identity,
                    message: "oracle winner lane is empty".to_owned(),
                });
            };
            if proposal.道具 != expected_candidate.道具
                || proposal.経路 != expected_candidate.経路
                || proposal.質量 != mass
                || proposal.適格候補 != eligible_mask
                || proposal.準備候補 != ready_mask
                || proposal.方策番号 != policy.方策番号
                || proposal.方策要約 != policy.方策要約
            {
                return Err(EvidenceError::InvalidData {
                    identity,
                    message: "selected proposal violates the public selection contract".to_owned(),
                });
            }
            validate_decision_receipt(
                proposal.受領証,
                &policy,
                request,
                受領種別::選択,
                有無::有る(proposal.道具),
                有無::無い,
            )?;
            let authorization = authorize(proposal, request.時刻)?;
            (
                OutcomeClass::Selected,
                eligible_mask,
                ready_mask,
                Some(proposal.道具),
                Some(proposal.質量),
                proposal.受領証,
                Some(authorization),
            )
        }
        (
            ExpectedOutcome::Refused {
                reason,
                eligible_mask,
                ready_mask,
            },
            選択結果::拒否 {
                理由,
                適格候補,
                準備候補,
                受領証,
            },
        ) => {
            if 理由 != reason || 適格候補 != eligible_mask || 準備候補 != ready_mask {
                return Err(EvidenceError::InvalidData {
                    identity,
                    message: format!(
                        "refusal violates public contract: expected {reason:?}/{eligible_mask:#x}/{ready_mask:#x}, found {理由:?}/{適格候補:#x}/{準備候補:#x}"
                    ),
                });
            }
            validate_decision_receipt(
                受領証,
                &policy,
                request,
                受領種別::拒否,
                有無::無い,
                有無::有る(reason.番号()),
            )?;
            (
                OutcomeClass::from_refusal(reason)?,
                eligible_mask,
                ready_mask,
                None,
                None,
                受領証,
                None,
            )
        }
        (expected, actual) => {
            return Err(EvidenceError::InvalidData {
                identity,
                message: format!("oracle/implementation mismatch: {expected:?} vs {actual:?}"),
            });
        }
    };

    let selection_digest = receipt_digest(selection_receipt);
    let authorization_digest = authorization_receipt.map(receipt_digest);
    let tool = selected_tool.unwrap_or_default();
    let mass = selected_mass.unwrap_or_default();
    let authorization_bytes = authorization_digest
        .map(|digest| *digest.as_bytes())
        .unwrap_or([0; 32]);
    let evidence_digest = domain_digest(
        "tcps/8pow4-scenario/v4",
        &[
            &cell.ordinal.to_le_bytes(),
            &cell.gray.to_le_bytes(),
            &[
                cell.coordinates.authority,
                cell.coordinates.readiness,
                cell.coordinates.time_budget,
                cell.coordinates.mode,
                outcome.code(),
            ],
            &eligible_mask.to_le_bytes(),
            &ready_mask.to_le_bytes(),
            &tool.to_le_bytes(),
            &mass.to_le_bytes(),
            selection_digest.as_bytes(),
            &authorization_bytes,
        ],
    );

    Ok(ScenarioEvidence {
        ordinal: cell.ordinal,
        gray: cell.gray,
        identity: cell.coordinates.identity(),
        coordinates: cell.coordinates,
        outcome,
        eligible_mask,
        ready_mask,
        selected_tool,
        selected_mass,
        selection_receipt: selection_digest.to_hex(),
        authorization_receipt: authorization_digest.map(EvidenceDigest::to_hex),
        evidence_digest: evidence_digest.to_hex(),
    })
}

static MATRIX: OnceLock<Result<Vec<ScenarioEvidence>, EvidenceError>> = OnceLock::new();

fn run_matrix() -> Result<&'static [ScenarioEvidence], EvidenceError> {
    match MATRIX.get_or_init(|| cells()?.into_iter().map(execute_cell).collect()) {
        Ok(results) => Ok(results.as_slice()),
        Err(error) => Err(error.clone()),
    }
}

fn evidence_leaves(results: &[ScenarioEvidence]) -> Result<Vec<EvidenceDigest>, EvidenceError> {
    results
        .iter()
        .map(|result| {
            let bytes = hex::decode(&result.evidence_digest).map_err(|error| {
                EvidenceError::InvalidData {
                    identity: result.identity.clone(),
                    message: error.to_string(),
                }
            })?;
            let bytes: [u8; 32] = bytes.try_into().map_err(|_| EvidenceError::InvalidData {
                identity: result.identity.clone(),
                message: "scenario digest is not 32 bytes".to_owned(),
            })?;
            Ok(EvidenceDigest::from_bytes(bytes))
        })
        .collect()
}

fn count_outcome(results: &[ScenarioEvidence], outcome: OutcomeClass) -> usize {
    results
        .iter()
        .filter(|result| result.outcome == outcome)
        .count()
}

fn matrix_receipt(results: &[ScenarioEvidence]) -> Result<MatrixReceipt, EvidenceError> {
    let leaves = evidence_leaves(results)?;
    let root = merkle_root("tcps/8pow4-root/v4", &leaves)?;
    Ok(MatrixReceipt {
        schema: "tcps-auto-select-8pow4/v4",
        cells: results.len(),
        selected: count_outcome(results, OutcomeClass::Selected),
        refused_authority: count_outcome(results, OutcomeClass::RefusedAuthority),
        refused_determinism: count_outcome(results, OutcomeClass::RefusedDeterminism),
        refused_receipt: count_outcome(results, OutcomeClass::RefusedReceipt),
        refused_time: count_outcome(results, OutcomeClass::RefusedTime),
        refused_no_ready: count_outcome(results, OutcomeClass::RefusedNoReady),
        authorization_receipts: results
            .iter()
            .filter(|result| result.authorization_receipt.is_some())
            .count(),
        merkle_root: root.to_hex(),
    })
}

fn known_selected_proposal() -> 選択提案 {
    let policy = policy(false).expect("policy");
    let request = 選択要求 {
        取得権限: 7,
        準備完了: 7,
        最大時間: 200,
        決定性必須: false,
        受領証必須: false,
        時刻: 100,
    };
    let mut workspace = 作業領域::新規();
    match 選択する(&policy, request, &mut workspace) {
        選択結果::選択(proposal) => proposal,
        選択結果::拒否 { 理由, .. } => panic!("known selectable policy refused: {理由:?}"),
    }
}

fn scenario_json_lines(results: &[ScenarioEvidence]) -> Result<String, EvidenceError> {
    let mut output = String::new();
    for result in results {
        let line = serde_json::to_string(result).map_err(|error| EvidenceError::InvalidData {
            identity: result.identity.clone(),
            message: error.to_string(),
        })?;
        output.push_str(&line);
        output.push('\n');
    }
    Ok(output)
}

fn scenario_plan(results: &[ScenarioEvidence]) -> String {
    let mut output = String::from(
        "ordinal\tgray\tauthority\treadiness\ttime_budget\tmode\toutcome\ttool\tmass\n",
    );
    for result in results {
        let tool = result
            .selected_tool
            .map_or_else(|| "-".to_owned(), |value| value.to_string());
        let mass = result
            .selected_mass
            .map_or_else(|| "-".to_owned(), |value| value.to_string());
        output.push_str(&format!(
            "{}\t{}\t{}\t{}\t{}\t{}\t{:?}\t{}\t{}\n",
            result.ordinal,
            result.gray,
            AUTHORITY_LABELS[usize::from(result.coordinates.authority)],
            READINESS_LABELS[usize::from(result.coordinates.readiness)],
            TIME_BUDGETS[usize::from(result.coordinates.time_budget)],
            MODE_LABELS[usize::from(result.coordinates.mode)],
            result.outcome,
            tool,
            mass,
        ));
    }
    output
}

fn coverage_plan(rails: &[Vec<Coordinates>; DIMENSIONS]) -> String {
    let mut output = String::from("strength\trow\tauthority\treadiness\ttime_budget\tmode\n");
    for (index, rail) in rails.iter().enumerate() {
        for (row, coordinates) in rail.iter().enumerate() {
            output.push_str(&format!(
                "{}\t{}\t{}\t{}\t{}\t{}\n",
                index + 1,
                row,
                AUTHORITY_LABELS[usize::from(coordinates.authority)],
                READINESS_LABELS[usize::from(coordinates.readiness)],
                TIME_BUDGETS[usize::from(coordinates.time_budget)],
                MODE_LABELS[usize::from(coordinates.mode)],
            ));
        }
    }
    output
}
