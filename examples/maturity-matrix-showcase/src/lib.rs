use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum MaturityLevel {
    Initial,
    Repeatable,
    Defined,
    Managed,
    Optimizing,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Assessment {
    pub category: String,
    pub level: MaturityLevel,
    pub score: f32,
}

impl Assessment {
    pub fn new(category: String, level: MaturityLevel) -> Self {
        let score = match level {
            MaturityLevel::Initial => 1.0,
            MaturityLevel::Repeatable => 2.0,
            MaturityLevel::Defined => 3.0,
            MaturityLevel::Managed => 4.0,
            MaturityLevel::Optimizing => 5.0,
        };
        Self { category, level, score }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_maturity_levels() {
        let a = Assessment::new("Testing".to_string(), MaturityLevel::Managed);
        assert_eq!(a.level, MaturityLevel::Managed);
        assert_eq!(a.score, 4.0);
    }

    #[test]
    fn test_all_levels() {
        for (level, expected_score) in &[
            (MaturityLevel::Initial, 1.0),
            (MaturityLevel::Repeatable, 2.0),
            (MaturityLevel::Defined, 3.0),
            (MaturityLevel::Managed, 4.0),
            (MaturityLevel::Optimizing, 5.0),
        ] {
            let a = Assessment::new("Test".to_string(), level.clone());
            assert_eq!(a.score, *expected_score);
        }
    }
}
