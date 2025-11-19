//! Developer Feedback Loops
//!
//! Implements "Respect for People" through continuous feedback:
//! - Developer satisfaction tracking
//! - Pain point collection
//! - Suggestion capture and implementation
//! - Voice of the developer (VOD)

use super::*;
use std::collections::BTreeMap;

/// Developer feedback loop manager
pub struct FeedbackLoop {
    /// Feedback entries
    feedback: Vec<FeedbackEntry>,

    /// Developer satisfaction history
    satisfaction_history: Vec<SatisfactionSnapshot>,

    /// Collected suggestions
    suggestions: Vec<Suggestion>,

    /// Pain points database
    pain_points: Vec<PainPoint>,

    /// Voice of developer insights
    vod_insights: Vec<VodInsight>,
}

/// A feedback entry from a developer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FeedbackEntry {
    /// Entry ID
    pub id: String,

    /// Developer ID (anonymous or identified)
    pub developer_id: String,

    /// Timestamp
    pub timestamp: String,

    /// Feedback type
    pub feedback_type: FeedbackType,

    /// Context (what they were doing)
    pub context: String,

    /// The feedback content
    pub content: String,

    /// Sentiment (positive, neutral, negative)
    pub sentiment: Sentiment,

    /// Priority assigned
    pub priority: Priority,

    /// Status
    pub status: FeedbackStatus,

    /// Response/Action taken
    pub response: Option<String>,
}

/// Type of feedback
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum FeedbackType {
    /// General comment
    Comment,

    /// Bug report
    Bug,

    /// Feature request
    FeatureRequest,

    /// Process improvement
    ProcessImprovement,

    /// Pain point
    PainPoint,

    /// Praise/Positive
    Praise,

    /// Question
    Question,
}

/// Sentiment analysis
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Sentiment {
    /// Very positive
    VeryPositive,

    /// Positive
    Positive,

    /// Neutral
    Neutral,

    /// Negative
    Negative,

    /// Very negative
    VeryNegative,
}

/// Priority level
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum Priority {
    /// Critical - blocks work
    Critical,

    /// High - significant impact
    High,

    /// Medium - moderate impact
    Medium,

    /// Low - minor impact
    Low,
}

/// Status of feedback
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum FeedbackStatus {
    /// Received but not reviewed
    New,

    /// Under review
    Reviewing,

    /// Accepted for action
    Accepted,

    /// Being worked on
    InProgress,

    /// Implemented/Resolved
    Resolved,

    /// Not going to address
    Declined,

    /// Duplicate of another feedback
    Duplicate,
}

/// Developer satisfaction snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SatisfactionSnapshot {
    /// Timestamp
    pub timestamp: String,

    /// Overall satisfaction score (1-10)
    pub overall_score: f64,

    /// Satisfaction by category
    pub category_scores: BTreeMap<SatisfactionCategory, f64>,

    /// Number of responses
    pub response_count: usize,

    /// Net Promoter Score (NPS)
    pub nps: i32,
}

/// Satisfaction categories
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum SatisfactionCategory {
    /// Ease of use
    EaseOfUse,

    /// Performance
    Performance,

    /// Reliability
    Reliability,

    /// Documentation
    Documentation,

    /// Support
    Support,

    /// Features
    Features,
}

/// A suggestion from a developer
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Suggestion {
    /// Suggestion ID
    pub id: String,

    /// Who suggested
    pub suggested_by: String,

    /// When suggested
    pub suggested_at: String,

    /// Title
    pub title: String,

    /// Description
    pub description: String,

    /// Expected benefit
    pub expected_benefit: String,

    /// Implementation effort estimate
    pub effort_estimate: EffortEstimate,

    /// Number of votes/endorsements
    pub votes: usize,

    /// Status
    pub status: SuggestionStatus,

    /// If implemented, reference to Kaizen event
    pub kaizen_event_id: Option<String>,
}

/// Effort estimate for suggestion
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum EffortEstimate {
    /// Less than 1 day
    Trivial,

    /// 1-3 days
    Small,

    /// 1-2 weeks
    Medium,

    /// 2-4 weeks
    Large,

    /// More than 4 weeks
    VeryLarge,
}

/// Status of suggestion
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum SuggestionStatus {
    /// Submitted, awaiting review
    Submitted,

    /// Under evaluation
    Evaluating,

    /// Approved for implementation
    Approved,

    /// Being implemented
    Implementing,

    /// Implemented
    Implemented,

    /// Rejected
    Rejected,

    /// Deferred for later
    Deferred,
}

/// Voice of Developer insight
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VodInsight {
    /// Insight ID
    pub id: String,

    /// Timestamp
    pub timestamp: String,

    /// Theme/Category
    pub theme: String,

    /// Insight description
    pub insight: String,

    /// Supporting evidence (feedback IDs)
    pub evidence: Vec<String>,

    /// Recommended actions
    pub recommended_actions: Vec<String>,

    /// Impact if addressed
    pub impact: Impact,
}

/// Impact level
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub enum Impact {
    /// Transformational change
    Transformational,

    /// Major improvement
    Major,

    /// Moderate improvement
    Moderate,

    /// Minor improvement
    Minor,
}

impl FeedbackLoop {
    /// Create new feedback loop manager
    pub fn new() -> Self {
        Self {
            feedback: Vec::new(),
            satisfaction_history: Vec::new(),
            suggestions: Vec::new(),
            pain_points: Vec::new(),
            vod_insights: Vec::new(),
        }
    }

    /// Collect feedback from developer
    pub fn collect_feedback(
        &mut self,
        developer_id: String,
        feedback_type: FeedbackType,
        context: String,
        content: String,
    ) -> String {
        let id = format!("feedback-{}", uuid::Uuid::new_v4());

        // Simple sentiment analysis based on keywords
        let sentiment = Self::analyze_sentiment(&content);

        // Determine priority
        let priority = match feedback_type {
            FeedbackType::Bug => Priority::High,
            FeedbackType::PainPoint => Priority::High,
            FeedbackType::FeatureRequest => Priority::Medium,
            FeedbackType::ProcessImprovement => Priority::Medium,
            _ => Priority::Low,
        };

        let entry = FeedbackEntry {
            id: id.clone(),
            developer_id,
            timestamp: chrono::Utc::now().to_rfc3339(),
            feedback_type,
            context,
            content,
            sentiment,
            priority,
            status: FeedbackStatus::New,
            response: None,
        };

        self.feedback.push(entry);
        id
    }

    /// Simple sentiment analysis
    fn analyze_sentiment(text: &str) -> Sentiment {
        let text_lower = text.to_lowercase();

        let positive_words = ["good", "great", "excellent", "love", "awesome", "perfect", "helpful"];
        let negative_words = ["bad", "terrible", "awful", "hate", "broken", "useless", "frustrating", "slow"];

        let positive_count = positive_words.iter().filter(|w| text_lower.contains(*w)).count();
        let negative_count = negative_words.iter().filter(|w| text_lower.contains(*w)).count();

        match positive_count.cmp(&negative_count) {
            std::cmp::Ordering::Greater => {
                if positive_count > negative_count + 2 {
                    Sentiment::VeryPositive
                } else {
                    Sentiment::Positive
                }
            }
            std::cmp::Ordering::Less => {
                if negative_count > positive_count + 2 {
                    Sentiment::VeryNegative
                } else {
                    Sentiment::Negative
                }
            }
            std::cmp::Ordering::Equal => Sentiment::Neutral,
        }
    }

    /// Record satisfaction scores
    pub fn record_satisfaction(
        &mut self,
        overall_score: f64,
        category_scores: BTreeMap<SatisfactionCategory, f64>,
        response_count: usize,
        nps: i32,
    ) {
        let snapshot = SatisfactionSnapshot {
            timestamp: chrono::Utc::now().to_rfc3339(),
            overall_score,
            category_scores,
            response_count,
            nps,
        };

        self.satisfaction_history.push(snapshot);
    }

    /// Submit a suggestion
    pub fn submit_suggestion(
        &mut self,
        suggested_by: String,
        title: String,
        description: String,
        expected_benefit: String,
        effort_estimate: EffortEstimate,
    ) -> String {
        let id = format!("suggestion-{}", uuid::Uuid::new_v4());

        let suggestion = Suggestion {
            id: id.clone(),
            suggested_by,
            suggested_at: chrono::Utc::now().to_rfc3339(),
            title,
            description,
            expected_benefit,
            effort_estimate,
            votes: 0,
            status: SuggestionStatus::Submitted,
            kaizen_event_id: None,
        };

        self.suggestions.push(suggestion);
        id
    }

    /// Vote on a suggestion
    pub fn vote_suggestion(&mut self, suggestion_id: &str) {
        if let Some(suggestion) = self.suggestions.iter_mut().find(|s| s.id == suggestion_id) {
            suggestion.votes += 1;
        }
    }

    /// Get top suggestions by votes
    pub fn get_top_suggestions(&self, limit: usize) -> Vec<&Suggestion> {
        let mut suggestions: Vec<_> = self.suggestions.iter().collect();
        suggestions.sort_by(|a, b| b.votes.cmp(&a.votes));
        suggestions.into_iter().take(limit).collect()
    }

    /// Add pain point
    pub fn add_pain_point(&mut self, pain_point: PainPoint) {
        self.pain_points.push(pain_point);
    }

    /// Get pain points by severity
    pub fn get_pain_points_by_severity(&self) -> Vec<&PainPoint> {
        let mut points: Vec<_> = self.pain_points.iter().collect();
        points.sort_by(|a, b| b.severity.cmp(&a.severity));
        points
    }

    /// Analyze feedback to generate VOD insights
    pub fn generate_vod_insights(&mut self) {
        // Group feedback by theme
        let mut themes: BTreeMap<String, Vec<String>> = BTreeMap::new();

        for entry in &self.feedback {
            // Simple theme extraction based on keywords
            let theme = Self::extract_theme(&entry.content);
            themes
                .entry(theme)
                .or_default()
                .push(entry.id.clone());
        }

        // Create insights for themes with multiple feedback entries
        for (theme, evidence) in themes {
            if evidence.len() >= 3 {
                // Need at least 3 pieces of evidence
                let insight_id = format!("vod-{}", uuid::Uuid::new_v4());

                let insight = VodInsight {
                    id: insight_id,
                    timestamp: chrono::Utc::now().to_rfc3339(),
                    theme: theme.clone(),
                    insight: format!(
                        "Multiple developers reporting issues with {}",
                        theme
                    ),
                    evidence: evidence.clone(),
                    recommended_actions: vec![
                        format!("Investigate {} issues", theme),
                        "Start PDCA cycle".to_string(),
                    ],
                    impact: if evidence.len() > 10 {
                        Impact::Major
                    } else {
                        Impact::Moderate
                    },
                };

                self.vod_insights.push(insight);
            }
        }
    }

    /// Extract theme from feedback content
    fn extract_theme(content: &str) -> String {
        let content_lower = content.to_lowercase();

        let themes = [
            ("performance", vec!["slow", "performance", "speed", "lag"]),
            ("usability", vec!["confusing", "difficult", "hard", "unclear"]),
            ("reliability", vec!["crash", "error", "fail", "broken"]),
            ("documentation", vec!["documentation", "docs", "help", "guide"]),
            ("features", vec!["feature", "functionality", "capability"]),
        ];

        for (theme_name, keywords) in themes {
            if keywords.iter().any(|k| content_lower.contains(k)) {
                return theme_name.to_string();
            }
        }

        "general".to_string()
    }

    /// Get satisfaction trend
    pub fn get_satisfaction_trend(&self) -> Option<SatisfactionTrend> {
        if self.satisfaction_history.len() < 2 {
            return None;
        }

        let first = self.satisfaction_history.first()?;
        let last = self.satisfaction_history.last()?;

        let score_change = last.overall_score - first.overall_score;
        let nps_change = last.nps - first.nps;

        Some(SatisfactionTrend {
            score_change,
            nps_change,
            is_improving: score_change > 0.0,
        })
    }

    /// Get all feedback
    pub fn get_feedback(&self) -> &[FeedbackEntry] {
        &self.feedback
    }

    /// Get all pain points
    pub fn get_pain_points(&self) -> &[PainPoint] {
        &self.pain_points
    }

    /// Get all VOD insights
    pub fn get_vod_insights(&self) -> &[VodInsight] {
        &self.vod_insights
    }
}

impl Default for FeedbackLoop {
    fn default() -> Self {
        Self::new()
    }
}

/// Satisfaction trend
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SatisfactionTrend {
    /// Change in overall score
    pub score_change: f64,

    /// Change in NPS
    pub nps_change: i32,

    /// Whether satisfaction is improving
    pub is_improving: bool,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_feedback_collection() {
        let mut feedback_loop = FeedbackLoop::new();

        let id = feedback_loop.collect_feedback(
            "dev1".to_string(),
            FeedbackType::Bug,
            "Template rendering".to_string(),
            "Template rendering is very slow and frustrating".to_string(),
        );

        assert_eq!(feedback_loop.feedback.len(), 1);
        assert_eq!(feedback_loop.feedback[0].sentiment, Sentiment::Negative);
        assert_eq!(feedback_loop.feedback[0].priority, Priority::High);
    }

    #[test]
    fn test_suggestion_voting() {
        let mut feedback_loop = FeedbackLoop::new();

        let id = feedback_loop.submit_suggestion(
            "dev1".to_string(),
            "Add caching".to_string(),
            "Cache SPARQL results".to_string(),
            "Faster queries".to_string(),
            EffortEstimate::Small,
        );

        feedback_loop.vote_suggestion(&id);
        feedback_loop.vote_suggestion(&id);

        assert_eq!(feedback_loop.suggestions[0].votes, 2);
    }

    #[test]
    fn test_sentiment_analysis() {
        assert_eq!(
            FeedbackLoop::analyze_sentiment("This is great and awesome!"),
            Sentiment::Positive
        );

        assert_eq!(
            FeedbackLoop::analyze_sentiment("This is terrible and awful"),
            Sentiment::Negative
        );

        assert_eq!(
            FeedbackLoop::analyze_sentiment("This is okay"),
            Sentiment::Neutral
        );
    }

    #[test]
    fn test_vod_insights() {
        let mut feedback_loop = FeedbackLoop::new();

        // Add multiple pieces of feedback with same theme
        for i in 0..5 {
            feedback_loop.collect_feedback(
                format!("dev{}", i),
                FeedbackType::PainPoint,
                "SPARQL".to_string(),
                "SPARQL queries are very slow".to_string(),
            );
        }

        feedback_loop.generate_vod_insights();

        assert!(!feedback_loop.vod_insights.is_empty());
    }
}
