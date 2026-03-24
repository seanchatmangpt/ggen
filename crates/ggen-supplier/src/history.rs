use crate::{QualityMetrics, Result, Supplier};
use crate::scorer::{QualityScore, QualityScorer};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HistoryEntry {
    pub timestamp: DateTime<Utc>,
    pub metrics: QualityMetrics,
    pub score: QualityScore,
}

impl HistoryEntry {
    pub fn new(metrics: QualityMetrics) -> Result<Self> {
        let score = QualityScorer::score(&metrics)?;
        Ok(Self {
            timestamp: Utc::now(),
            metrics,
            score,
        })
    }

    pub fn with_timestamp(metrics: QualityMetrics, timestamp: DateTime<Utc>) -> Result<Self> {
        let score = QualityScorer::score(&metrics)?;
        Ok(Self {
            timestamp,
            metrics,
            score,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplierHistory {
    pub supplier_id: String,
    pub entries: Vec<HistoryEntry>,
}

impl SupplierHistory {
    pub fn new(supplier_id: impl Into<String>) -> Self {
        Self {
            supplier_id: supplier_id.into(),
            entries: Vec::new(),
        }
    }

    pub fn add_entry(&mut self, entry: HistoryEntry) {
        self.entries.push(entry);
        self.entries.sort_by(|a, b| a.timestamp.cmp(&b.timestamp));
    }

    pub fn latest_entry(&self) -> Option<&HistoryEntry> {
        self.entries.last()
    }

    pub fn get_entries_since(&self, since: DateTime<Utc>) -> Vec<&HistoryEntry> {
        self.entries
            .iter()
            .filter(|e| e.timestamp >= since)
            .collect()
    }

    pub fn average_defect_rate(&self) -> f64 {
        if self.entries.is_empty() {
            return 0.0;
        }

        let total: f64 = self.entries.iter().map(|e| e.score.defect_rate).sum();
        total / self.entries.len() as f64
    }

    pub fn trend_analysis(&self, min_entries: usize) -> Option<Trend> {
        if self.entries.len() < min_entries {
            return None;
        }

        let mid = self.entries.len() / 2;
        let first_half: f64 = self.entries[..mid]
            .iter()
            .map(|e| e.score.defect_rate)
            .sum::<f64>() / mid as f64;

        let second_half: f64 = self.entries[mid..]
            .iter()
            .map(|e| e.score.defect_rate)
            .sum::<f64>() / (self.entries.len() - mid) as f64;

        let direction = if second_half > first_half + 1.0 {
            TrendDirection::Worsening
        } else if second_half < first_half - 1.0 {
            TrendDirection::Improving
        } else {
            TrendDirection::Stable
        };

        Some(Trend {
            direction,
            change_rate: second_half - first_half,
        })
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum TrendDirection {
    Improving,
    Stable,
    Worsening,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trend {
    pub direction: TrendDirection,
    pub change_rate: f64,
}

pub struct HistoryTracker {
    histories: HashMap<String, SupplierHistory>,
}

impl HistoryTracker {
    pub fn new() -> Self {
        Self {
            histories: HashMap::new(),
        }
    }

    pub fn record(&mut self, supplier: &Supplier, metrics: QualityMetrics) -> Result<()> {
        supplier.validate()?;

        let entry = HistoryEntry::new(metrics)?;
        let history = self.histories
            .entry(supplier.id.clone())
            .or_insert_with(|| SupplierHistory::new(supplier.id.clone()));

        history.add_entry(entry);
        Ok(())
    }

    pub fn get_history(&self, supplier_id: &str) -> Option<&SupplierHistory> {
        self.histories.get(supplier_id)
    }

    pub fn get_history_mut(&mut self, supplier_id: &str) -> Option<&mut SupplierHistory> {
        self.histories.get_mut(supplier_id)
    }

    pub fn get_latest_metrics(&self, supplier_id: &str) -> Option<&QualityMetrics> {
        self.histories
            .get(supplier_id)
            .and_then(|h| h.latest_entry())
            .map(|e| &e.metrics)
    }

    pub fn get_average_defect_rate(&self, supplier_id: &str) -> Option<f64> {
        self.histories
            .get(supplier_id)
            .map(|h| h.average_defect_rate())
    }

    pub fn compare_suppliers(&self, supplier_ids: &[&str]) -> Vec<SupplierComparison> {
        supplier_ids
            .iter()
            .filter_map(|id| {
                self.histories.get(*id).map(|history| {
                    SupplierComparison {
                        supplier_id: id.to_string(),
                        average_defect_rate: history.average_defect_rate(),
                        total_entries: history.entries.len(),
                        latest_score: history.latest_entry().map(|e| e.score.clone()),
                    }
                })
            })
            .collect()
    }

    pub fn clear_history(&mut self, supplier_id: &str) -> bool {
        self.histories.remove(supplier_id).is_some()
    }

    pub fn supplier_count(&self) -> usize {
        self.histories.len()
    }
}

impl Default for HistoryTracker {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SupplierComparison {
    pub supplier_id: String,
    pub average_defect_rate: f64,
    pub total_entries: usize,
    pub latest_score: Option<QualityScore>,
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_supplier(id: &str) -> Supplier {
        Supplier::new(id, format!("Supplier {}", id))
    }

    #[test]
    fn test_history_entry_new() {
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();
        let entry = HistoryEntry::new(metrics).unwrap();

        assert_eq!(entry.metrics.total_defects(), 10);
        assert_eq!(entry.score.defect_rate, 10.0);
    }

    #[test]
    fn test_supplier_history_new() {
        let history = SupplierHistory::new("s1");
        assert_eq!(history.supplier_id, "s1");
        assert!(history.entries.is_empty());
    }

    #[test]
    fn test_supplier_history_add_entry() {
        let mut history = SupplierHistory::new("s1");
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();
        let entry = HistoryEntry::new(metrics).unwrap();

        history.add_entry(entry);
        assert_eq!(history.entries.len(), 1);
    }

    #[test]
    fn test_supplier_history_latest_entry() {
        let mut history = SupplierHistory::new("s1");

        assert!(history.latest_entry().is_none());

        let metrics1 = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();
        let entry1 = HistoryEntry::new(metrics1).unwrap();
        history.add_entry(entry1);

        let metrics2 = QualityMetrics::new(10, 5, 3, 2, 100).unwrap();
        let entry2 = HistoryEntry::new(metrics2).unwrap();
        history.add_entry(entry2);

        let latest = history.latest_entry().unwrap();
        assert_eq!(latest.score.defect_rate, 20.0);
    }

    #[test]
    fn test_supplier_history_average_defect_rate() {
        let mut history = SupplierHistory::new("s1");

        let metrics1 = QualityMetrics::new(10, 0, 0, 0, 100).unwrap();
        let entry1 = HistoryEntry::new(metrics1).unwrap();
        history.add_entry(entry1);

        let metrics2 = QualityMetrics::new(20, 0, 0, 0, 100).unwrap();
        let entry2 = HistoryEntry::new(metrics2).unwrap();
        history.add_entry(entry2);

        assert_eq!(history.average_defect_rate(), 15.0);
    }

    #[test]
    fn test_supplier_history_average_defect_rate_empty() {
        let history = SupplierHistory::new("s1");
        assert_eq!(history.average_defect_rate(), 0.0);
    }

    #[test]
    fn test_supplier_history_trend_improving() {
        let mut history = SupplierHistory::new("s1");

        // Add entries with decreasing defect rates
        for i in (5..=10).rev() {
            let metrics = QualityMetrics::new(i, 0, 0, 0, 100).unwrap();
            let entry = HistoryEntry::new(metrics).unwrap();
            history.add_entry(entry);
        }

        let trend = history.trend_analysis(4).unwrap();
        assert_eq!(trend.direction, TrendDirection::Improving);
    }

    #[test]
    fn test_supplier_history_trend_worsening() {
        let mut history = SupplierHistory::new("s1");

        // Add entries with increasing defect rates
        for i in 5..=10 {
            let metrics = QualityMetrics::new(i, 0, 0, 0, 100).unwrap();
            let entry = HistoryEntry::new(metrics).unwrap();
            history.add_entry(entry);
        }

        let trend = history.trend_analysis(4).unwrap();
        assert_eq!(trend.direction, TrendDirection::Worsening);
    }

    #[test]
    fn test_supplier_history_trend_stable() {
        let mut history = SupplierHistory::new("s1");

        // Add entries with stable defect rates
        for _ in 0..6 {
            let metrics = QualityMetrics::new(10, 0, 0, 0, 100).unwrap();
            let entry = HistoryEntry::new(metrics).unwrap();
            history.add_entry(entry);
        }

        let trend = history.trend_analysis(4).unwrap();
        assert_eq!(trend.direction, TrendDirection::Stable);
    }

    #[test]
    fn test_supplier_history_trend_insufficient_data() {
        let mut history = SupplierHistory::new("s1");

        let metrics = QualityMetrics::new(10, 0, 0, 0, 100).unwrap();
        let entry = HistoryEntry::new(metrics).unwrap();
        history.add_entry(entry);

        assert!(history.trend_analysis(4).is_none());
    }

    #[test]
    fn test_history_tracker_new() {
        let tracker = HistoryTracker::new();
        assert_eq!(tracker.supplier_count(), 0);
    }

    #[test]
    fn test_history_tracker_record() {
        let mut tracker = HistoryTracker::new();
        let supplier = create_test_supplier("s1");
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();

        assert!(tracker.record(&supplier, metrics).is_ok());
        assert_eq!(tracker.supplier_count(), 1);
    }

    #[test]
    fn test_history_tracker_get_history() {
        let mut tracker = HistoryTracker::new();
        let supplier = create_test_supplier("s1");
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();

        tracker.record(&supplier, metrics).unwrap();

        let history = tracker.get_history("s1");
        assert!(history.is_some());
        assert_eq!(history.unwrap().entries.len(), 1);
    }

    #[test]
    fn test_history_tracker_get_latest_metrics() {
        let mut tracker = HistoryTracker::new();
        let supplier = create_test_supplier("s1");

        let metrics1 = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();
        tracker.record(&supplier, metrics1).unwrap();

        let metrics2 = QualityMetrics::new(10, 5, 3, 2, 100).unwrap();
        tracker.record(&supplier, metrics2.clone()).unwrap();

        let latest = tracker.get_latest_metrics("s1").unwrap();
        assert_eq!(latest.total_defects(), 20);
    }

    #[test]
    fn test_history_tracker_get_average_defect_rate() {
        let mut tracker = HistoryTracker::new();
        let supplier = create_test_supplier("s1");

        let metrics1 = QualityMetrics::new(10, 0, 0, 0, 100).unwrap();
        tracker.record(&supplier, metrics1).unwrap();

        let metrics2 = QualityMetrics::new(20, 0, 0, 0, 100).unwrap();
        tracker.record(&supplier, metrics2).unwrap();

        let avg = tracker.get_average_defect_rate("s1").unwrap();
        assert_eq!(avg, 15.0);
    }

    #[test]
    fn test_history_tracker_compare_suppliers() {
        let mut tracker = HistoryTracker::new();

        let s1 = create_test_supplier("s1");
        let metrics1 = QualityMetrics::new(10, 0, 0, 0, 100).unwrap();
        tracker.record(&s1, metrics1).unwrap();

        let s2 = create_test_supplier("s2");
        let metrics2 = QualityMetrics::new(20, 0, 0, 0, 100).unwrap();
        tracker.record(&s2, metrics2).unwrap();

        let comparisons = tracker.compare_suppliers(&["s1", "s2"]);
        assert_eq!(comparisons.len(), 2);
        assert!(comparisons[0].average_defect_rate < comparisons[1].average_defect_rate);
    }

    #[test]
    fn test_history_tracker_clear_history() {
        let mut tracker = HistoryTracker::new();
        let supplier = create_test_supplier("s1");
        let metrics = QualityMetrics::new(5, 3, 2, 0, 100).unwrap();

        tracker.record(&supplier, metrics).unwrap();
        assert_eq!(tracker.supplier_count(), 1);

        assert!(tracker.clear_history("s1"));
        assert_eq!(tracker.supplier_count(), 0);
    }

    #[test]
    fn test_history_tracker_clear_nonexistent() {
        let mut tracker = HistoryTracker::new();
        assert!(!tracker.clear_history("nonexistent"));
    }

    #[test]
    fn test_history_get_entries_since() {
        let mut history = SupplierHistory::new("s1");

        let now = Utc::now();
        let past = now - chrono::Duration::hours(2);

        let metrics1 = QualityMetrics::new(10, 0, 0, 0, 100).unwrap();
        let entry1 = HistoryEntry::with_timestamp(metrics1, past).unwrap();
        history.add_entry(entry1);

        let metrics2 = QualityMetrics::new(20, 0, 0, 0, 100).unwrap();
        let entry2 = HistoryEntry::with_timestamp(metrics2, now).unwrap();
        history.add_entry(entry2);

        let since = now - chrono::Duration::hours(1);
        let entries = history.get_entries_since(since);
        assert_eq!(entries.len(), 1);
    }
}
