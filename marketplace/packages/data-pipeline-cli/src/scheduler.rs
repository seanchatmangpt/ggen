//! Scheduler module - Pipeline scheduling

use serde::{Deserialize, Serialize};

#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum ScheduleType {
    Cron(String),
    Interval(std::time::Duration),
    Event(String),
}

pub struct Scheduler {
    schedule: ScheduleType,
}

impl Scheduler {
    pub fn new(schedule: ScheduleType) -> Self {
        Self { schedule }
    }
}
