//! Chicago TDD tests for CI/CD workflow utilities
//!
//! These tests use REAL system operations where possible:
//! - REAL workflow status parsing
//! - REAL enum conversions
//! - Test data structures

use ggen_cli::domain::ci::workflow::*;

#[test]
fn test_workflow_status_from_string() {
    // Test all valid statuses
    assert_eq!(
        WorkflowStatus::from_str("queued"),
        Some(WorkflowStatus::Queued)
    );
    assert_eq!(
        WorkflowStatus::from_str("in_progress"),
        Some(WorkflowStatus::InProgress)
    );
    assert_eq!(
        WorkflowStatus::from_str("completed"),
        Some(WorkflowStatus::Completed)
    );
    assert_eq!(
        WorkflowStatus::from_str("failed"),
        Some(WorkflowStatus::Failed)
    );
    assert_eq!(
        WorkflowStatus::from_str("cancelled"),
        Some(WorkflowStatus::Cancelled)
    );

    // Test case insensitivity
    assert_eq!(
        WorkflowStatus::from_str("QUEUED"),
        Some(WorkflowStatus::Queued)
    );
    assert_eq!(
        WorkflowStatus::from_str("In_Progress"),
        Some(WorkflowStatus::InProgress)
    );

    // Test invalid status
    assert_eq!(WorkflowStatus::from_str("unknown"), None);
    assert_eq!(WorkflowStatus::from_str(""), None);
    assert_eq!(WorkflowStatus::from_str("running"), None);
}

#[test]
fn test_workflow_status_to_string() {
    assert_eq!(WorkflowStatus::Queued.as_str(), "queued");
    assert_eq!(WorkflowStatus::InProgress.as_str(), "in_progress");
    assert_eq!(WorkflowStatus::Completed.as_str(), "completed");
    assert_eq!(WorkflowStatus::Failed.as_str(), "failed");
    assert_eq!(WorkflowStatus::Cancelled.as_str(), "cancelled");
}

#[test]
fn test_workflow_status_round_trip() {
    let statuses = [
        WorkflowStatus::Queued,
        WorkflowStatus::InProgress,
        WorkflowStatus::Completed,
        WorkflowStatus::Failed,
        WorkflowStatus::Cancelled,
    ];

    for status in &statuses {
        let str_repr = status.as_str();
        let parsed = WorkflowStatus::from_str(str_repr);
        assert_eq!(
            parsed,
            Some(*status),
            "Round trip failed for {:?}",
            status
        );
    }
}

#[test]
fn test_workflow_status_is_active() {
    // Active statuses
    assert!(WorkflowStatus::Queued.is_active());
    assert!(WorkflowStatus::InProgress.is_active());

    // Inactive statuses
    assert!(!WorkflowStatus::Completed.is_active());
    assert!(!WorkflowStatus::Failed.is_active());
    assert!(!WorkflowStatus::Cancelled.is_active());
}

#[test]
fn test_workflow_info_structure() {
    let info = WorkflowInfo {
        id: "12345".to_string(),
        name: "CI Build".to_string(),
        status: WorkflowStatus::Completed,
        conclusion: Some("success".to_string()),
        created_at: "2024-01-01T10:00:00Z".to_string(),
        updated_at: "2024-01-01T10:30:00Z".to_string(),
        html_url: "https://github.com/org/repo/actions/runs/12345".to_string(),
    };

    assert_eq!(info.id, "12345");
    assert_eq!(info.name, "CI Build");
    assert_eq!(info.status, WorkflowStatus::Completed);
    assert_eq!(info.conclusion, Some("success".to_string()));
    assert!(!info.html_url.is_empty());
}

#[test]
fn test_job_info_structure() {
    let job = JobInfo {
        id: "job-1".to_string(),
        name: "Build and Test".to_string(),
        status: WorkflowStatus::Completed,
        conclusion: Some("success".to_string()),
        started_at: Some("2024-01-01T10:00:00Z".to_string()),
        completed_at: Some("2024-01-01T10:15:00Z".to_string()),
    };

    assert_eq!(job.id, "job-1");
    assert_eq!(job.name, "Build and Test");
    assert!(job.started_at.is_some());
    assert!(job.completed_at.is_some());
}

#[test]
fn test_workflow_list_result() {
    let workflows = vec![
        WorkflowInfo {
            id: "1".to_string(),
            name: "Build".to_string(),
            status: WorkflowStatus::Completed,
            conclusion: Some("success".to_string()),
            created_at: "2024-01-01T10:00:00Z".to_string(),
            updated_at: "2024-01-01T10:30:00Z".to_string(),
            html_url: "https://github.com/runs/1".to_string(),
        },
        WorkflowInfo {
            id: "2".to_string(),
            name: "Test".to_string(),
            status: WorkflowStatus::InProgress,
            conclusion: None,
            created_at: "2024-01-01T11:00:00Z".to_string(),
            updated_at: "2024-01-01T11:00:00Z".to_string(),
            html_url: "https://github.com/runs/2".to_string(),
        },
    ];

    let result = WorkflowListResult {
        workflows: workflows.clone(),
        total_count: 2,
    };

    assert_eq!(result.workflows.len(), 2);
    assert_eq!(result.total_count, 2);
    assert_eq!(result.workflows[0].status, WorkflowStatus::Completed);
    assert_eq!(result.workflows[1].status, WorkflowStatus::InProgress);
}

#[test]
fn test_workflow_status_result() {
    let workflow = WorkflowInfo {
        id: "123".to_string(),
        name: "Deploy".to_string(),
        status: WorkflowStatus::Failed,
        conclusion: Some("failure".to_string()),
        created_at: "2024-01-01T10:00:00Z".to_string(),
        updated_at: "2024-01-01T10:30:00Z".to_string(),
        html_url: "https://github.com/runs/123".to_string(),
    };

    let jobs = vec![
        JobInfo {
            id: "job-1".to_string(),
            name: "Setup".to_string(),
            status: WorkflowStatus::Completed,
            conclusion: Some("success".to_string()),
            started_at: Some("2024-01-01T10:00:00Z".to_string()),
            completed_at: Some("2024-01-01T10:05:00Z".to_string()),
        },
        JobInfo {
            id: "job-2".to_string(),
            name: "Deploy".to_string(),
            status: WorkflowStatus::Failed,
            conclusion: Some("failure".to_string()),
            started_at: Some("2024-01-01T10:05:00Z".to_string()),
            completed_at: Some("2024-01-01T10:10:00Z".to_string()),
        },
    ];

    let result = WorkflowStatusResult {
        workflow: workflow.clone(),
        jobs: jobs.clone(),
    };

    assert_eq!(result.workflow.id, "123");
    assert_eq!(result.workflow.status, WorkflowStatus::Failed);
    assert_eq!(result.jobs.len(), 2);
    assert_eq!(result.jobs[0].status, WorkflowStatus::Completed);
    assert_eq!(result.jobs[1].status, WorkflowStatus::Failed);
}

#[test]
fn test_workflow_logs_result() {
    let result = WorkflowLogsResult {
        logs: "Build started\nRunning tests\nTests passed\n".to_string(),
        workflow_id: "456".to_string(),
    };

    assert!(!result.logs.is_empty());
    assert_eq!(result.workflow_id, "456");
    assert!(result.logs.contains("Build started"));
    assert!(result.logs.contains("Tests passed"));
}

#[test]
fn test_workflow_status_equality() {
    assert_eq!(WorkflowStatus::Queued, WorkflowStatus::Queued);
    assert_eq!(WorkflowStatus::Completed, WorkflowStatus::Completed);
    assert_ne!(WorkflowStatus::Queued, WorkflowStatus::InProgress);
    assert_ne!(WorkflowStatus::Completed, WorkflowStatus::Failed);
}

#[test]
fn test_multiple_workflow_filtering() {
    let workflows = vec![
        WorkflowInfo {
            id: "1".to_string(),
            name: "Build".to_string(),
            status: WorkflowStatus::Completed,
            conclusion: Some("success".to_string()),
            created_at: "2024-01-01T10:00:00Z".to_string(),
            updated_at: "2024-01-01T10:30:00Z".to_string(),
            html_url: "https://github.com/runs/1".to_string(),
        },
        WorkflowInfo {
            id: "2".to_string(),
            name: "Test".to_string(),
            status: WorkflowStatus::InProgress,
            conclusion: None,
            created_at: "2024-01-01T11:00:00Z".to_string(),
            updated_at: "2024-01-01T11:00:00Z".to_string(),
            html_url: "https://github.com/runs/2".to_string(),
        },
        WorkflowInfo {
            id: "3".to_string(),
            name: "Deploy".to_string(),
            status: WorkflowStatus::Queued,
            conclusion: None,
            created_at: "2024-01-01T12:00:00Z".to_string(),
            updated_at: "2024-01-01T12:00:00Z".to_string(),
            html_url: "https://github.com/runs/3".to_string(),
        },
    ];

    // Filter active workflows
    let active: Vec<_> = workflows
        .iter()
        .filter(|w| w.status.is_active())
        .collect();

    assert_eq!(active.len(), 2);
    assert!(active.iter().any(|w| w.id == "2"));
    assert!(active.iter().any(|w| w.id == "3"));
}

#[test]
fn test_job_info_optional_fields() {
    // Job that hasn't started yet
    let pending_job = JobInfo {
        id: "job-pending".to_string(),
        name: "Pending Job".to_string(),
        status: WorkflowStatus::Queued,
        conclusion: None,
        started_at: None,
        completed_at: None,
    };

    assert!(pending_job.started_at.is_none());
    assert!(pending_job.completed_at.is_none());
    assert!(pending_job.conclusion.is_none());

    // Job in progress
    let running_job = JobInfo {
        id: "job-running".to_string(),
        name: "Running Job".to_string(),
        status: WorkflowStatus::InProgress,
        conclusion: None,
        started_at: Some("2024-01-01T10:00:00Z".to_string()),
        completed_at: None,
    };

    assert!(running_job.started_at.is_some());
    assert!(running_job.completed_at.is_none());
}
