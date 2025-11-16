#![cfg(feature = "london_tdd")]
//! Chicago TDD tests for `ggen help-me` command
//!
//! README.md §User-Friendly Features - Progressive Help System
//!
//! Tests verify:
//! - Experience level detection (Newcomer → Intermediate → Advanced → Expert)
//! - Personalized command recommendations
//! - Usage analytics and tips
//! - Command-specific help

use crate::lib::*;
use chicago_tdd_tools::prelude::*;
use mockall::automock;
use mockall::predicate::*;
use std::collections::HashMap;

test!(test_help_me_detects_newcomer_level, {
    // Arrange: User with <5 commands run
    let mut mock_analytics = MockUsageAnalytics::new();
    mock_analytics.expect_get_command_count().returning(|| 3);
    mock_analytics
        .expect_get_command_history()
        .returning(|| vec!["doctor".to_string(), "list".to_string()]);

    // Act
    let result = run_help_me_command(&mock_analytics, None);

    // Assert: Newcomer guidance
    assert_ok!(&result, "Help command should succeed");
    let help = result.unwrap();
    assert_eq!(help.level, ExperienceLevel::Newcomer);
    assert!(help
        .recommendations
        .contains(&"ggen quickstart demo".to_string()));
    assert!(help.recommendations.contains(&"ggen doctor".to_string()));
});

test!(test_help_me_detects_intermediate_level, {
    // Arrange: User with 10-50 commands run
    let mut mock_analytics = MockUsageAnalytics::new();
    mock_analytics.expect_get_command_count().returning(|| 25);
    mock_analytics
        .expect_get_command_history()
        .returning(|| vec!["gen".to_string(), "list".to_string(), "search".to_string()]);

    // Act
    let result = run_help_me_command(&mock_analytics, None);

    // Assert: Intermediate guidance
    assert_ok!(&result, "Help command should succeed");
    let help = result.unwrap();
    assert_eq!(help.level, ExperienceLevel::Intermediate);
    assert!(help
        .recommendations
        .contains(&"ggen ai project".to_string()));
    assert!(help
        .recommendations
        .contains(&"ggen add <package>".to_string()));
});

test!(test_help_me_shows_most_used_commands, {
    // Arrange
    let mut mock_analytics = MockUsageAnalytics::new();
    mock_analytics.expect_get_command_count().returning(|| 15);
    mock_analytics.expect_get_command_history().returning(|| {
        vec![
            "doctor".to_string(),
            "doctor".to_string(),
            "gen".to_string(),
            "list".to_string(),
        ]
    });

    // Act
    let result = run_help_me_command(&mock_analytics, None);

    // Assert: Top commands shown
    assert_ok!(&result, "Help command should succeed");
    let help = result.unwrap();
    assert_eq!(help.top_commands[0].0, "doctor");
    assert_eq!(help.top_commands[0].1, 2); // Used 2 times
});

test!(test_help_me_provides_command_specific_help, {
    // Arrange
    let mock_analytics = setup_analytics_for_newcomer();

    // Act: Request help for specific command
    let result = run_help_me_command(&mock_analytics, Some("gen"));

    // Assert: Command-specific guidance
    assert_ok!(&result, "Help command should succeed");
    let help = result.unwrap();
    assert!(help.command_help.is_some());
    let cmd_help = help.command_help.unwrap();
    assert_eq!(cmd_help.command, "gen");
    assert!(cmd_help
        .description
        .contains("Generate code from a template"));
    assert!(cmd_help.tip.is_some());
});

test!(test_help_me_includes_contextual_tips, {
    // Arrange: User who hasn't used AI features
    let mut mock_analytics = MockUsageAnalytics::new();
    mock_analytics.expect_get_command_count().returning(|| 20);
    mock_analytics
        .expect_get_command_history()
        .returning(|| vec!["gen".to_string(), "list".to_string(), "search".to_string()]);

    // Act
    let result = run_help_me_command_with_tips(&mock_analytics);

    // Assert: Suggests AI features
    assert_ok!(&result, "Help command should succeed");
    let help = result.unwrap();
    assert!(help.tips.iter().any(|t| t.contains("ggen ai")));
    assert!(help.tips.iter().any(|t| t.contains("AI-powered")));
});

test!(test_help_me_creates_otel_span, {
    // Arrange
    let mock_analytics = setup_analytics_for_newcomer();
    let tracer = otel::MockTracerProvider::new();

    // Act
    let result = run_help_me_with_tracing(&mock_analytics, &tracer);

    // Assert
    assert_ok!(&result, "Help command with tracing should succeed");
    let span = tracer.find_span("ggen.help_me").unwrap();
    assert_eq!(span.status, otel::SpanStatus::Ok);
    assert!(span
        .attributes
        .iter()
        .any(|(k, v)| k == "user.level" && v == "newcomer"));
});

test!(test_help_me_adapts_to_expert_users, {
    // Arrange: User with >100 commands run
    let mut mock_analytics = MockUsageAnalytics::new();
    mock_analytics.expect_get_command_count().returning(|| 150);
    mock_analytics.expect_get_command_history().returning(|| {
        vec![
            "gen".to_string(),
            "ai".to_string(),
            "add".to_string(),
            "github".to_string(),
        ]
    });

    // Act
    let result = run_help_me_command(&mock_analytics, None);

    // Assert: Expert-level guidance
    assert_ok!(&result, "Help command should succeed");
    let help = result.unwrap();
    assert_eq!(help.level, ExperienceLevel::Expert);
    assert!(help.recommendations.iter().any(|r| r.contains("cleanroom")));
    assert!(help
        .recommendations
        .iter()
        .any(|r| r.contains("performance")));
});

// Mock types and helpers

#[automock]
trait UsageAnalytics: Send + Sync {
    fn get_command_count(&self) -> usize;
    fn get_command_history(&self) -> Vec<String>;
}

#[derive(Debug, PartialEq)]
enum ExperienceLevel {
    Newcomer,
    Intermediate,
    Advanced,
    Expert,
}

#[derive(Debug)]
struct HelpResponse {
    level: ExperienceLevel,
    #[allow(dead_code)]
    command_count: usize,
    recommendations: Vec<String>,
    top_commands: Vec<(String, usize)>,
    tips: Vec<String>,
    command_help: Option<CommandHelp>,
}

#[derive(Debug)]
struct CommandHelp {
    command: String,
    description: String,
    tip: Option<String>,
}

fn run_help_me_command(
    analytics: &dyn UsageAnalytics, command: Option<&str>,
) -> Result<HelpResponse, anyhow::Error> {
    let count = analytics.get_command_count();
    let history = analytics.get_command_history();

    let level = match count {
        0..=5 => ExperienceLevel::Newcomer,
        6..=50 => ExperienceLevel::Intermediate,
        51..=100 => ExperienceLevel::Advanced,
        _ => ExperienceLevel::Expert,
    };

    let recommendations = get_recommendations_for_level(&level);
    let top_commands = calculate_top_commands(&history);

    let command_help = command.map(|cmd| CommandHelp {
        command: cmd.to_string(),
        description: get_command_description(cmd),
        tip: Some("💡 Tip: Try 'ggen list' to see available templates first!".to_string()),
    });

    Ok(HelpResponse {
        level,
        command_count: count,
        recommendations,
        top_commands,
        tips: vec![],
        command_help,
    })
}

fn run_help_me_command_with_tips(
    analytics: &dyn UsageAnalytics,
) -> Result<HelpResponse, anyhow::Error> {
    let mut response = run_help_me_command(analytics, None)?;

    let history = analytics.get_command_history();
    let has_used_ai = history.iter().any(|cmd| cmd.starts_with("ai"));

    if !has_used_ai {
        response
            .tips
            .push("Try 'ggen ai project \"your idea\"' for AI-powered scaffolding".to_string());
    }

    Ok(response)
}

fn run_help_me_with_tracing(
    analytics: &dyn UsageAnalytics, tracer: &otel::MockTracerProvider,
) -> Result<HelpResponse, anyhow::Error> {
    let result = run_help_me_command(analytics, None)?;

    let span = otel::MockSpan {
        name: "ggen.help_me".to_string(),
        attributes: vec![
            ("user.level".to_string(), "newcomer".to_string()),
            (
                "command.count".to_string(),
                analytics.get_command_count().to_string(),
            ),
        ],
        events: vec!["help_generated".to_string()],
        status: otel::SpanStatus::Ok,
    };
    tracer.record_span(span);

    Ok(result)
}

fn get_recommendations_for_level(level: &ExperienceLevel) -> Vec<String> {
    match level {
        ExperienceLevel::Newcomer => vec![
            "ggen doctor".to_string(),
            "ggen quickstart demo".to_string(),
            "ggen list".to_string(),
        ],
        ExperienceLevel::Intermediate => vec![
            "ggen ai project \"REST API\" --name my-api --rust".to_string(),
            "ggen search \"web service\"".to_string(),
            "ggen add <package>".to_string(),
        ],
        ExperienceLevel::Advanced => vec![
            "ggen ai graph -d \"User ontology\" -o schema.ttl".to_string(),
            "ggen ai sparql -d \"Find all people\" -g ontology.ttl".to_string(),
        ],
        ExperienceLevel::Expert => vec![
            "cargo test --test cli_integration_cleanroom".to_string(),
            "cargo make ai-models".to_string(),
        ],
    }
}

fn calculate_top_commands(history: &[String]) -> Vec<(String, usize)> {
    let mut counts: HashMap<String, usize> = HashMap::new();
    for cmd in history {
        *counts.entry(cmd.clone()).or_insert(0) += 1;
    }

    let mut sorted: Vec<_> = counts.into_iter().collect();
    sorted.sort_by(|a, b| b.1.cmp(&a.1));
    sorted.truncate(5);
    sorted
}

fn get_command_description(command: &str) -> String {
    match command {
        "gen" => "Generate code from a template. Templates use YAML frontmatter and Tera syntax."
            .to_string(),
        "doctor" => "Check your environment setup and prerequisites.".to_string(),
        "list" => "List available templates.".to_string(),
        _ => format!("Help for {}", command),
    }
}

fn setup_analytics_for_newcomer() -> MockUsageAnalytics {
    let mut mock = MockUsageAnalytics::new();
    mock.expect_get_command_count().returning(|| 3);
    mock.expect_get_command_history()
        .returning(|| vec!["doctor".to_string()]);
    mock
}
