pub mod scorer;
pub mod limiter;
pub mod history;

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use thiserror::Error;

#[derive(Error, Debug)]
pub enum SupplierError {
    #[error("Supplier rate limited: defect rate {0:.2}% exceeds threshold")]
    RateLimited(f64),
    #[error("Invalid supplier ID: {0}")]
    InvalidSupplierId(String),
    #[error("Invalid metric value: {0}")]
    InvalidMetric(String),
    #[error("History error: {0}")]
    HistoryError(String),
}

pub type Result<T> = std::result::Result<T, SupplierError>;

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct Supplier {
    pub id: String,
    pub name: String,
    pub created_at: DateTime<Utc>,
}

impl Supplier {
    pub fn new(id: impl Into<String>, name: impl Into<String>) -> Self {
        Self {
            id: id.into(),
            name: name.into(),
            created_at: Utc::now(),
        }
    }

    pub fn validate(&self) -> Result<()> {
        if self.id.is_empty() {
            return Err(SupplierError::InvalidSupplierId("ID cannot be empty".to_string()));
        }
        if self.name.is_empty() {
            return Err(SupplierError::InvalidSupplierId("Name cannot be empty".to_string()));
        }
        Ok(())
    }
}

#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct QualityMetrics {
    pub incomplete_packets: u32,
    pub churn: u32,
    pub urgency_inflation: u32,
    pub coordination_dumping: u32,
    pub total_deliveries: u32,
}

impl QualityMetrics {
    pub fn new(
        incomplete_packets: u32,
        churn: u32,
        urgency_inflation: u32,
        coordination_dumping: u32,
        total_deliveries: u32,
    ) -> Result<Self> {
        let metrics = Self {
            incomplete_packets,
            churn,
            urgency_inflation,
            coordination_dumping,
            total_deliveries,
        };
        metrics.validate()?;
        Ok(metrics)
    }

    pub fn validate(&self) -> Result<()> {
        let total_defects = self.incomplete_packets
            + self.churn
            + self.urgency_inflation
            + self.coordination_dumping;

        if total_defects > self.total_deliveries {
            return Err(SupplierError::InvalidMetric(
                format!("Total defects ({}) cannot exceed total deliveries ({})",
                    total_defects, self.total_deliveries)
            ));
        }
        Ok(())
    }

    pub fn total_defects(&self) -> u32 {
        self.incomplete_packets + self.churn + self.urgency_inflation + self.coordination_dumping
    }
}

impl Default for QualityMetrics {
    fn default() -> Self {
        Self {
            incomplete_packets: 0,
            churn: 0,
            urgency_inflation: 0,
            coordination_dumping: 0,
            total_deliveries: 0,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_supplier_new() {
        let supplier = Supplier::new("s1", "Acme Corp");
        assert_eq!(supplier.id, "s1");
        assert_eq!(supplier.name, "Acme Corp");
    }

    #[test]
    fn test_supplier_validate_success() {
        let supplier = Supplier::new("s1", "Acme Corp");
        assert!(supplier.validate().is_ok());
    }

    #[test]
    fn test_supplier_validate_empty_id() {
        let supplier = Supplier::new("", "Acme Corp");
        assert!(matches!(supplier.validate(), Err(SupplierError::InvalidSupplierId(_))));
    }

    #[test]
    fn test_supplier_validate_empty_name() {
        let supplier = Supplier::new("s1", "");
        assert!(matches!(supplier.validate(), Err(SupplierError::InvalidSupplierId(_))));
    }

    #[test]
    fn test_quality_metrics_new_success() {
        let metrics = QualityMetrics::new(5, 3, 2, 1, 100);
        assert!(metrics.is_ok());
        let m = metrics.unwrap();
        assert_eq!(m.total_defects(), 11);
    }

    #[test]
    fn test_quality_metrics_validate_defects_exceed_deliveries() {
        let metrics = QualityMetrics::new(50, 30, 20, 10, 100);
        assert!(metrics.is_err());
    }

    #[test]
    fn test_quality_metrics_total_defects() {
        let metrics = QualityMetrics {
            incomplete_packets: 5,
            churn: 3,
            urgency_inflation: 2,
            coordination_dumping: 1,
            total_deliveries: 100,
        };
        assert_eq!(metrics.total_defects(), 11);
    }

    #[test]
    fn test_quality_metrics_default() {
        let metrics = QualityMetrics::default();
        assert_eq!(metrics.total_defects(), 0);
        assert_eq!(metrics.total_deliveries, 0);
    }
}
