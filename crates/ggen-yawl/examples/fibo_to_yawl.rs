//! FIBO to YAWL Transformation Example
//!
//! This example demonstrates how to transform FIBO (Financial Industry Business Ontology)
//! concepts into YAWL workflow specifications.

use ggen_yawl::{
    template::{FlowContext, TaskContext, TemplateContext, VariableContext},
    YawlXmlGenerator,
};

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== FIBO to YAWL Transformation Examples ===\n");

    // Example 1: Loan Approval Workflow
    println!("1. Loan Approval Workflow (FIBO LCC)");
    let loan_approval = loan_approval_workflow();
    println!("{}", loan_approval);
    println!();

    // Example 2: Trade Settlement Workflow
    println!("2. Trade Settlement Workflow (FIBO SEC)");
    let trade_settlement = trade_settlement_workflow();
    println!("{}", trade_settlement);
    println!();

    // Example 3: Credit Risk Assessment Workflow
    println!("3. Credit Risk Assessment Workflow (FIBO CR)");
    let credit_risk = credit_risk_workflow();
    println!("{}", credit_risk);
    println!();

    // Example 4: Regulatory Reporting Workflow
    println!("4. Regulatory Reporting Workflow (FIBO IND)");
    let regulatory_reporting = regulatory_reporting_workflow();
    println!("{}", regulatory_reporting);
    println!();

    Ok(())
}

/// FIBO Loan Approval Workflow
///
/// Maps FIBO Loan Contract and Credit Assessment concepts to a YAWL workflow.
///
/// FIBO Concepts:
/// - fibo-loan:LoanAgreement - The loan contract being processed
/// - fibo-loan:CreditAssessment - Creditworthiness evaluation
/// - fibo-loan:CollateralEvaluation - Asset assessment
/// - fibo-loan:LoanDisbursement - Fund transfer
fn loan_approval_workflow() -> String {
    let context = TemplateContext {
        workflow_name: "FIBO_LoanApproval".to_string(),
        description: "Loan approval workflow based on FIBO LCC ontology".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "receive_application".to_string(),
                name: "Receive Loan Application".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "credit_assessment".to_string(),
                name: "Credit Assessment".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "collateral_evaluation".to_string(),
                name: "Collateral Evaluation".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "risk_evaluation".to_string(),
                name: "Risk Evaluation".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "approve_loan".to_string(),
                name: "Approve Loan".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "reject_loan".to_string(),
                name: "Reject Loan".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "disburse_funds".to_string(),
                name: "Disburse Funds".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "receive_application".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            // Parallel execution of assessments
            FlowContext {
                source: "receive_application".to_string(),
                target: "credit_assessment".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "receive_application".to_string(),
                target: "collateral_evaluation".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            // Risk evaluation after assessments complete
            FlowContext {
                source: "credit_assessment".to_string(),
                target: "risk_evaluation".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "collateral_evaluation".to_string(),
                target: "risk_evaluation".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            // Conditional routing based on risk
            FlowContext {
                source: "risk_evaluation".to_string(),
                target: "approve_loan".to_string(),
                condition: Some("/risk_score <= 30".to_string()),
                predicate: Some("risk_score <= 30".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "risk_evaluation".to_string(),
                target: "reject_loan".to_string(),
                condition: Some("/risk_score > 70".to_string()),
                predicate: Some("risk_score > 70".to_string()),
                is_default: false,
            },
            // Manual review for medium risk
            FlowContext {
                source: "risk_evaluation".to_string(),
                target: "approve_loan".to_string(),
                condition: Some(
                    "/risk_score > 30 AND /risk_score <= 70 AND /manual_approved = true"
                        .to_string(),
                ),
                predicate: Some(
                    "risk_score > 30 && risk_score <= 70 && manual_approved == true".to_string(),
                ),
                is_default: false,
            },
            FlowContext {
                source: "risk_evaluation".to_string(),
                target: "reject_loan".to_string(),
                condition: Some(
                    "/risk_score > 30 AND /risk_score <= 70 AND /manual_approved = false"
                        .to_string(),
                ),
                predicate: Some(
                    "risk_score > 30 && risk_score <= 70 && manual_approved == false".to_string(),
                ),
                is_default: false,
            },
            // Disbursement after approval
            FlowContext {
                source: "approve_loan".to_string(),
                target: "disburse_funds".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "disburse_funds".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "reject_loan".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![
            VariableContext {
                name: "risk_score".to_string(),
                var_type: "integer".to_string(),
                default: Some("50".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "manual_approved".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "loan_amount".to_string(),
                var_type: "decimal".to_string(),
                default: None,
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "collateral_value".to_string(),
                var_type: "decimal".to_string(),
                default: Some("0".to_string()),
                scope: "workflow".to_string(),
            },
        ],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// FIBO Trade Settlement Workflow
///
/// Maps FIBO Securities and Settlement concepts to a YAWL workflow.
///
/// FIBO Concepts:
/// - fibo-sec:Security - Financial instrument being traded
/// - fibo-sec:Trade - The trade execution
/// - fibo-sec:Settlement - The settlement process
/// - fibo-sec:Clearing - Central counterparty clearing
fn trade_settlement_workflow() -> String {
    let context = TemplateContext {
        workflow_name: "FIBO_TradeSettlement".to_string(),
        description: "Trade settlement workflow based on FIBO SEC ontology".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "capture_trade".to_string(),
                name: "Capture Trade".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "enrich_trade".to_string(),
                name: "Enrich Trade Data".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "validate_trade".to_string(),
                name: "Validate Trade".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "clearing".to_string(),
                name: "Central Counterparty Clearing".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "settlement".to_string(),
                name: "Settlement".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "confirmation".to_string(),
                name: "Trade Confirmation".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "capture_trade".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "capture_trade".to_string(),
                target: "enrich_trade".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "enrich_trade".to_string(),
                target: "validate_trade".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "validate_trade".to_string(),
                target: "clearing".to_string(),
                condition: Some("/requires_clearing = true".to_string()),
                predicate: Some("requires_clearing == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "validate_trade".to_string(),
                target: "settlement".to_string(),
                condition: Some("/requires_clearing = false".to_string()),
                predicate: Some("requires_clearing == false".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "clearing".to_string(),
                target: "settlement".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "settlement".to_string(),
                target: "confirmation".to_string(),
                condition: Some("/settlement_status = 'settled'".to_string()),
                predicate: Some("settlement_status == 'settled'".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "confirmation".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![
            VariableContext {
                name: "requires_clearing".to_string(),
                var_type: "boolean".to_string(),
                default: Some("true".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "settlement_status".to_string(),
                var_type: "string".to_string(),
                default: Some("pending".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "security_id".to_string(),
                var_type: "string".to_string(),
                default: None,
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "settlement_date".to_string(),
                var_type: "date".to_string(),
                default: None,
                scope: "workflow".to_string(),
            },
        ],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// FIBO Credit Risk Assessment Workflow
///
/// Maps FIBO Credit Risk concepts to a YAWL workflow.
///
/// FIBO Concepts:
/// - fibo-cr:CreditRisk - Credit risk assessment
/// - fibo-cr:Counterparty - The entity being assessed
/// - fibo-cr:Exposure - Credit exposure amount
/// - fibo-cr:Mitigation - Risk mitigation techniques
fn credit_risk_workflow() -> String {
    let context = TemplateContext {
        workflow_name: "FIBO_CreditRiskAssessment".to_string(),
        description: "Credit risk assessment workflow based on FIBO CR ontology".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "identify_counterparty".to_string(),
                name: "Identify Counterparty".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "gather_financials".to_string(),
                name: "Gather Financial Information".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "calculate_exposure".to_string(),
                name: "Calculate Credit Exposure".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "assess_mitigation".to_string(),
                name: "Assess Risk Mitigation".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "determine_rating".to_string(),
                name: "Determine Credit Rating".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "set_limits".to_string(),
                name: "Set Credit Limits".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "identify_counterparty".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "identify_counterparty".to_string(),
                target: "gather_financials".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "identify_counterparty".to_string(),
                target: "calculate_exposure".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "gather_financials".to_string(),
                target: "assess_mitigation".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "calculate_exposure".to_string(),
                target: "assess_mitigation".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "assess_mitigation".to_string(),
                target: "determine_rating".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "determine_rating".to_string(),
                target: "set_limits".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "set_limits".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![
            VariableContext {
                name: "counterparty_id".to_string(),
                var_type: "string".to_string(),
                default: None,
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "exposure_amount".to_string(),
                var_type: "decimal".to_string(),
                default: Some("0".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "credit_rating".to_string(),
                var_type: "string".to_string(),
                default: Some("BBB".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "credit_limit".to_string(),
                var_type: "decimal".to_string(),
                default: Some("0".to_string()),
                scope: "workflow".to_string(),
            },
        ],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}

/// FIBO Regulatory Reporting Workflow
///
/// Maps FIBO Indicators and Reporting concepts to a YAWL workflow.
///
/// FIBO Concepts:
/// - fibo-ind:RegulatoryReport - Report submission
/// - fibo-ind:ReportableEvent - Event requiring reporting
/// - fibo-ind:ReportingObligation - Legal reporting requirement
/// - fibo-ind:ReportSubmission - The submission process
fn regulatory_reporting_workflow() -> String {
    let context = TemplateContext {
        workflow_name: "FIBO_RegulatoryReporting".to_string(),
        description: "Regulatory reporting workflow based on FIBO IND ontology".to_string(),
        version: "1.0.0".to_string(),
        tasks: vec![
            TaskContext {
                id: "detect_reportable_event".to_string(),
                name: "Detect Reportable Event".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: true,
                decomposition_id: None,
            },
            TaskContext {
                id: "determine_obligation".to_string(),
                name: "Determine Reporting Obligation".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "collect_data".to_string(),
                name: "Collect Required Data".to_string(),
                split_type: "AND".to_string(),
                join_type: "AND".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "validate_data".to_string(),
                name: "Validate Report Data".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "generate_report".to_string(),
                name: "Generate Regulatory Report".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "submit_report".to_string(),
                name: "Submit Report".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
            TaskContext {
                id: "record_submission".to_string(),
                name: "Record Submission".to_string(),
                split_type: "XOR".to_string(),
                join_type: "XOR".to_string(),
                is_auto: false,
                decomposition_id: None,
            },
        ],
        flows: vec![
            FlowContext {
                source: "input".to_string(),
                target: "detect_reportable_event".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "detect_reportable_event".to_string(),
                target: "determine_obligation".to_string(),
                condition: Some("/is_reportable = true".to_string()),
                predicate: Some("is_reportable == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "detect_reportable_event".to_string(),
                target: "output".to_string(),
                condition: Some("/is_reportable = false".to_string()),
                predicate: Some("is_reportable == false".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "determine_obligation".to_string(),
                target: "collect_data".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "collect_data".to_string(),
                target: "validate_data".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "validate_data".to_string(),
                target: "generate_report".to_string(),
                condition: Some("/validation_passed = true".to_string()),
                predicate: Some("validation_passed == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "validate_data".to_string(),
                target: "collect_data".to_string(),
                condition: Some("/validation_passed = false".to_string()),
                predicate: Some("validation_passed == false".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "generate_report".to_string(),
                target: "submit_report".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
            FlowContext {
                source: "submit_report".to_string(),
                target: "record_submission".to_string(),
                condition: Some("/submission_success = true".to_string()),
                predicate: Some("submission_success == true".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "submit_report".to_string(),
                target: "submit_report".to_string(),
                condition: Some("/submission_success = false AND /retry_count < 3".to_string()),
                predicate: Some("submission_success == false && retry_count < 3".to_string()),
                is_default: false,
            },
            FlowContext {
                source: "record_submission".to_string(),
                target: "output".to_string(),
                condition: None,
                predicate: None,
                is_default: true,
            },
        ],
        input_condition: None,
        output_condition: None,
        variables: vec![
            VariableContext {
                name: "is_reportable".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "report_type".to_string(),
                var_type: "string".to_string(),
                default: Some("trade_report".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "validation_passed".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "submission_success".to_string(),
                var_type: "boolean".to_string(),
                default: Some("false".to_string()),
                scope: "workflow".to_string(),
            },
            VariableContext {
                name: "retry_count".to_string(),
                var_type: "integer".to_string(),
                default: Some("0".to_string()),
                scope: "workflow".to_string(),
            },
        ],
    };

    YawlXmlGenerator::generate(&context).unwrap_or_else(|e| format!("Error: {}", e))
}
