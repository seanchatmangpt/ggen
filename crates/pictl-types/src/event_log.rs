use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Attribute value types for event data
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
#[serde(tag = "tag", content = "value")]
pub enum AttributeValue {
    String(String),
    Int(i64),
    Float(f64),
    Date(String), // ISO 8601
    Boolean(bool),
    List(Vec<AttributeValue>),
    Container(HashMap<String, AttributeValue>),
}

impl AttributeValue {
    #[inline]
    pub fn as_string(&self) -> Option<&str> {
        match self {
            AttributeValue::String(s) => Some(s.as_str()),
            _ => None,
        }
    }

    #[inline]
    pub fn as_i64(&self) -> Option<i64> {
        match self {
            AttributeValue::Int(i) => Some(*i),
            _ => None,
        }
    }

    #[inline]
    pub fn as_f64(&self) -> Option<f64> {
        match self {
            AttributeValue::Float(f) => Some(*f),
            _ => None,
        }
    }

    #[inline]
    pub fn as_bool(&self) -> Option<bool> {
        match self {
            AttributeValue::Boolean(b) => Some(*b),
            _ => None,
        }
    }
}

pub type Attributes = HashMap<String, AttributeValue>;

/// A single event in a trace
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Event {
    pub attributes: Attributes,
}

impl Event {
    pub fn new(attributes: Attributes) -> Self {
        Event { attributes }
    }

    pub fn get_activity(&self, key: &str) -> Option<String> {
        self.attributes
            .get(key)
            .and_then(|v| v.as_string().map(|s| s.to_string()))
    }

    pub fn get_timestamp(&self, key: &str) -> Option<String> {
        match self.attributes.get(key)? {
            AttributeValue::Date(d) => Some(d.clone()),
            AttributeValue::String(s) => Some(s.clone()),
            _ => None,
        }
    }
}

/// A trace (sequence of events for one case)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Trace {
    pub case_id: String,
    pub events: Vec<Event>,
}

impl Trace {
    pub fn new(case_id: String, events: Vec<Event>) -> Self {
        Trace { case_id, events }
    }

    pub fn len(&self) -> usize {
        self.events.len()
    }

    pub fn is_empty(&self) -> bool {
        self.events.is_empty()
    }

    pub fn activities(&self, activity_key: &str) -> Vec<String> {
        self.events
            .iter()
            .filter_map(|e| e.get_activity(activity_key))
            .collect()
    }
}

/// An event log (collection of traces)
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct EventLog {
    pub traces: Vec<Trace>,
    pub attributes: Attributes,
    #[serde(default)]
    pub format: String,
    #[serde(default)]
    pub source_hash: String,
}

impl EventLog {
    pub fn new(traces: Vec<Trace>, attributes: Attributes) -> Self {
        EventLog {
            traces,
            attributes,
            format: String::new(),
            source_hash: String::new(),
        }
    }

    pub fn with_format(mut self, format: String) -> Self {
        self.format = format;
        self
    }

    pub fn with_source_hash(mut self, hash: String) -> Self {
        self.source_hash = hash;
        self
    }

    pub fn len(&self) -> usize {
        self.traces.len()
    }

    pub fn is_empty(&self) -> bool {
        self.traces.is_empty()
    }

    pub fn event_count(&self) -> usize {
        self.traces.iter().map(|t| t.len()).sum()
    }

    pub fn get_activities(&self, activity_key: &str) -> Vec<String> {
        let mut activities = Vec::new();
        for trace in &self.traces {
            for activity in trace.activities(activity_key) {
                if !activities.contains(&activity) {
                    activities.push(activity);
                }
            }
        }
        activities.sort();
        activities
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_event_creation() {
        let mut attrs = HashMap::new();
        attrs.insert(
            "concept:name".to_string(),
            AttributeValue::String("A".to_string()),
        );
        let event = Event::new(attrs);
        assert_eq!(event.get_activity("concept:name"), Some("A".to_string()));
    }

    #[test]
    fn test_trace_activities() {
        let mut attrs1 = HashMap::new();
        attrs1.insert(
            "concept:name".to_string(),
            AttributeValue::String("A".to_string()),
        );
        let mut attrs2 = HashMap::new();
        attrs2.insert(
            "concept:name".to_string(),
            AttributeValue::String("B".to_string()),
        );

        let trace = Trace::new(
            "case1".to_string(),
            vec![Event::new(attrs1), Event::new(attrs2)],
        );
        assert_eq!(trace.activities("concept:name"), vec!["A", "B"]);
    }
}
