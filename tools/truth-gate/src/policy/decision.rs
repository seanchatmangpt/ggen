//! Stable admission decision shared by hooks, CI, and future service adapters.
use serde::Serialize;
use super::Violation;

#[derive(Debug, Clone, Serialize)]
#[serde(rename_all="SCREAMING_SNAKE_CASE")]
pub enum DecisionKind { Admit, Refuse }

#[derive(Debug, Clone, Serialize)]
pub struct Decision {
    pub kind: DecisionKind,
    pub subject: String,
    pub violations: Vec<Violation>,
    pub replay_hint: Option<String>,
}

impl Decision {
    pub fn from_violations(subject: impl Into<String>, violations: Vec<Violation>) -> Self {
        let kind=if violations.is_empty(){DecisionKind::Admit}else{DecisionKind::Refuse};
        Self{kind,subject:subject.into(),violations,replay_hint:None}
    }
    pub fn refused(&self)->bool { matches!(self.kind,DecisionKind::Refuse) }
    pub fn with_replay(mut self, hint:impl Into<String>)->Self { self.replay_hint=Some(hint.into()); self }
}
