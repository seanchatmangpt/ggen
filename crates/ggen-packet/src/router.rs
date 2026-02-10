use crate::{Priority, WorkOrder, WorkOrderStatus};
use std::collections::HashMap;
use thiserror::Error;

/// Routing errors
#[derive(Debug, Error)]
pub enum RoutingError {
    /// No channel found for the given route
    #[error("No channel found for route: {0}")]
    NoChannelFound(String),

    /// Invalid work order state
    #[error("Invalid work order state: {0}")]
    InvalidState(String),

    /// Routing rule violated
    #[error("Routing rule violation: {0}")]
    RuleViolation(String),
}

/// Channel identifier for packet routing
#[derive(Debug, Clone, Hash, Eq, PartialEq)]
pub struct ChannelId(String);

impl ChannelId {
    /// Create a new channel identifier
    #[must_use]
    pub fn new(id: String) -> Self {
        Self(id)
    }

    /// Get the channel ID as a string slice
    #[must_use]
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl From<String> for ChannelId {
    fn from(s: String) -> Self {
        Self(s)
    }
}

impl From<&str> for ChannelId {
    fn from(s: &str) -> Self {
        Self(s.to_string())
    }
}

/// Routing strategy for work orders
pub trait RoutingStrategy {
    /// Determine the channel for a work order
    ///
    /// # Errors
    ///
    /// Returns `RoutingError` if routing fails
    fn route(&self, work_order: &WorkOrder) -> Result<ChannelId, RoutingError>;
}

/// Priority-based routing strategy
pub struct PriorityRouter {
    channel_map: HashMap<Priority, ChannelId>,
    default_channel: ChannelId,
}

impl PriorityRouter {
    /// Create a new priority-based router
    #[must_use]
    pub fn new(default_channel: ChannelId) -> Self {
        Self {
            channel_map: HashMap::new(),
            default_channel,
        }
    }

    /// Add a priority-to-channel mapping
    pub fn with_mapping(mut self, priority: Priority, channel: ChannelId) -> Self {
        self.channel_map.insert(priority, channel);
        self
    }
}

impl RoutingStrategy for PriorityRouter {
    fn route(&self, work_order: &WorkOrder) -> Result<ChannelId, RoutingError> {
        Ok(self
            .channel_map
            .get(&work_order.priority)
            .cloned()
            .unwrap_or_else(|| self.default_channel.clone()))
    }
}

/// Status-based routing strategy
pub struct StatusRouter {
    channel_map: HashMap<WorkOrderStatus, ChannelId>,
    default_channel: ChannelId,
}

impl StatusRouter {
    /// Create a new status-based router
    #[must_use]
    pub fn new(default_channel: ChannelId) -> Self {
        Self {
            channel_map: HashMap::new(),
            default_channel,
        }
    }

    /// Add a status-to-channel mapping
    pub fn with_mapping(mut self, status: WorkOrderStatus, channel: ChannelId) -> Self {
        self.channel_map.insert(status, channel);
        self
    }
}

impl RoutingStrategy for StatusRouter {
    fn route(&self, work_order: &WorkOrder) -> Result<ChannelId, RoutingError> {
        Ok(self
            .channel_map
            .get(&work_order.status)
            .cloned()
            .unwrap_or_else(|| self.default_channel.clone()))
    }
}

/// Tag-based routing strategy
pub struct TagRouter {
    tag_map: HashMap<String, ChannelId>,
    default_channel: ChannelId,
}

impl TagRouter {
    /// Create a new tag-based router
    #[must_use]
    pub fn new(default_channel: ChannelId) -> Self {
        Self {
            tag_map: HashMap::new(),
            default_channel,
        }
    }

    /// Add a tag-to-channel mapping
    pub fn with_mapping(mut self, tag: String, channel: ChannelId) -> Self {
        self.tag_map.insert(tag, channel);
        self
    }
}

impl RoutingStrategy for TagRouter {
    fn route(&self, work_order: &WorkOrder) -> Result<ChannelId, RoutingError> {
        // Find first matching tag
        for tag in &work_order.tags {
            if let Some(channel) = self.tag_map.get(tag) {
                return Ok(channel.clone());
            }
        }

        Ok(self.default_channel.clone())
    }
}

/// Composite router that chains multiple strategies
pub struct CompositeRouter {
    strategies: Vec<Box<dyn RoutingStrategy>>,
    default_channel: ChannelId,
}

impl CompositeRouter {
    /// Create a new composite router
    #[must_use]
    pub fn new(default_channel: ChannelId) -> Self {
        Self {
            strategies: Vec::new(),
            default_channel,
        }
    }

    /// Add a routing strategy to the chain
    pub fn add_strategy(mut self, strategy: Box<dyn RoutingStrategy>) -> Self {
        self.strategies.push(strategy);
        self
    }
}

impl RoutingStrategy for CompositeRouter {
    fn route(&self, work_order: &WorkOrder) -> Result<ChannelId, RoutingError> {
        for strategy in &self.strategies {
            let channel = strategy.route(work_order)?;
            if channel.as_str() != self.default_channel.as_str() {
                return Ok(channel);
            }
        }

        Ok(self.default_channel.clone())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{WorkOrder, Priority, WorkOrderStatus};
    use std::collections::HashSet;

    #[test]
    fn test_priority_router() {
        let router = PriorityRouter::new("default".into())
            .with_mapping(Priority::Critical, "critical-queue".into())
            .with_mapping(Priority::High, "high-queue".into());

        let mut wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap()
        .with_priority(Priority::Critical)
        .ok()
        .unwrap();

        let channel = router.route(&wo).ok().unwrap();
        assert_eq!(channel.as_str(), "critical-queue");

        wo = wo.with_priority(Priority::Normal).ok().unwrap();
        let channel = router.route(&wo).ok().unwrap();
        assert_eq!(channel.as_str(), "default");
    }

    #[test]
    fn test_status_router() {
        let router = StatusRouter::new("default".into())
            .with_mapping(WorkOrderStatus::InProgress, "active-queue".into())
            .with_mapping(WorkOrderStatus::Blocked, "blocked-queue".into());

        let mut wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let channel = router.route(&wo).ok().unwrap();
        assert_eq!(channel.as_str(), "default");

        wo.transition_to(WorkOrderStatus::InProgress).ok().unwrap();
        let channel = router.route(&wo).ok().unwrap();
        assert_eq!(channel.as_str(), "active-queue");
    }

    #[test]
    fn test_tag_router() {
        let router = TagRouter::new("default".into())
            .with_mapping(String::from("backend"), "backend-queue".into())
            .with_mapping(String::from("frontend"), "frontend-queue".into());

        let mut tags = HashSet::new();
        tags.insert(String::from("backend"));

        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap()
        .with_tags(tags)
        .ok()
        .unwrap();

        let channel = router.route(&wo).ok().unwrap();
        assert_eq!(channel.as_str(), "backend-queue");
    }

    #[test]
    fn test_tag_router_no_match() {
        let router = TagRouter::new("default".into())
            .with_mapping(String::from("backend"), "backend-queue".into());

        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap();

        let channel = router.route(&wo).ok().unwrap();
        assert_eq!(channel.as_str(), "default");
    }

    #[test]
    fn test_channel_id_creation() {
        let id1: ChannelId = "test".into();
        let id2 = ChannelId::new(String::from("test"));
        assert_eq!(id1, id2);
    }

    #[test]
    fn test_composite_router() {
        let priority_router = PriorityRouter::new("default".into())
            .with_mapping(Priority::Critical, "critical-queue".into());

        let tag_router = TagRouter::new("default".into())
            .with_mapping(String::from("urgent"), "urgent-queue".into());

        let composite = CompositeRouter::new("default".into())
            .add_strategy(Box::new(priority_router))
            .add_strategy(Box::new(tag_router));

        let wo = WorkOrder::new(
            String::from("Task"),
            String::from("owner"),
        )
        .ok()
        .unwrap()
        .with_priority(Priority::Critical)
        .ok()
        .unwrap();

        let channel = composite.route(&wo).ok().unwrap();
        assert_eq!(channel.as_str(), "critical-queue");
    }
}
