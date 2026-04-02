//! Grafana dashboard JSON generation
//!
//! Generates dashboard JSON payloads for Grafana visualization of Kaizen metrics
//! Includes panels for:
//! - Jidoka status (failures/min, circuit open %)
//! - Kanban status (queue depth, latency P99, throughput)
//! - Andon status (alerts/min, MTTD)
//! - Heijunka status (load balance, worker utilization)

use serde_json::{json, Value};

/// Dashboard panel configuration
#[derive(Clone, Debug, serde::Serialize, serde::Deserialize)]
pub struct DashboardPanel {
    /// Panel title
    pub title: String,
    /// Metric query
    pub query: String,
    /// Panel type (graph, stat, gauge, etc.)
    pub panel_type: String,
    /// Y-axis label
    pub ylabel: Option<String>,
    /// Min value for gauge
    pub min: Option<f64>,
    /// Max value for gauge
    pub max: Option<f64>,
}

/// Grafana dashboard
#[derive(Clone, Debug)]
pub struct Dashboard {
    /// Dashboard title
    pub title: String,
    /// Dashboard description
    pub description: String,
    /// Time window (e.g., "24h", "7d")
    pub time_range: String,
    /// Panels
    pub panels: Vec<DashboardPanel>,
}

impl Dashboard {
    /// Create default Kaizen dashboard
    pub fn kaizen_default() -> Self {
        let panels = vec![
            // Jidoka panels
            DashboardPanel {
                title: "Jidoka: Circuit Open %".to_string(),
                query: "jidoka_circuit_open_percent".to_string(),
                panel_type: "gauge".to_string(),
                ylabel: Some("Percent".to_string()),
                min: Some(0.0),
                max: Some(100.0),
            },
            DashboardPanel {
                title: "Jidoka: Failure Rate (per min)".to_string(),
                query: "jidoka_failure_rate_per_min".to_string(),
                panel_type: "graph".to_string(),
                ylabel: Some("Failures/min".to_string()),
                min: None,
                max: None,
            },
            DashboardPanel {
                title: "Jidoka: Recovery Time (secs)".to_string(),
                query: "jidoka_recovery_time_secs".to_string(),
                panel_type: "stat".to_string(),
                ylabel: Some("Seconds".to_string()),
                min: None,
                max: None,
            },
            // Kanban panels
            DashboardPanel {
                title: "Kanban: Queue Depth".to_string(),
                query: "kanban_queue_depth".to_string(),
                panel_type: "graph".to_string(),
                ylabel: Some("Tasks".to_string()),
                min: Some(0.0),
                max: None,
            },
            DashboardPanel {
                title: "Kanban: Latency P99 (ms)".to_string(),
                query: "kanban_latency_p99_ms".to_string(),
                panel_type: "graph".to_string(),
                ylabel: Some("Milliseconds".to_string()),
                min: Some(0.0),
                max: None,
            },
            DashboardPanel {
                title: "Kanban: Throughput (tasks/min)".to_string(),
                query: "kanban_throughput_per_min".to_string(),
                panel_type: "graph".to_string(),
                ylabel: Some("Tasks/min".to_string()),
                min: Some(0.0),
                max: None,
            },
            DashboardPanel {
                title: "Kanban: Queue Saturation %".to_string(),
                query: "kanban_queue_saturation_percent".to_string(),
                panel_type: "gauge".to_string(),
                ylabel: Some("Percent".to_string()),
                min: Some(0.0),
                max: Some(100.0),
            },
            // Andon panels
            DashboardPanel {
                title: "Andon: Alerts per Minute".to_string(),
                query: "andon_alerts_per_min".to_string(),
                panel_type: "graph".to_string(),
                ylabel: Some("Alerts/min".to_string()),
                min: Some(0.0),
                max: None,
            },
            DashboardPanel {
                title: "Andon: Mean Time To Detection (secs)".to_string(),
                query: "andon_mttd_secs".to_string(),
                panel_type: "stat".to_string(),
                ylabel: Some("Seconds".to_string()),
                min: None,
                max: None,
            },
            DashboardPanel {
                title: "Andon: Critical Alerts".to_string(),
                query: "andon_critical_alerts".to_string(),
                panel_type: "gauge".to_string(),
                ylabel: Some("Count".to_string()),
                min: Some(0.0),
                max: None,
            },
            // Heijunka panels
            DashboardPanel {
                title: "Heijunka: Load Balance Coefficient".to_string(),
                query: "heijunka_load_balance_coeff".to_string(),
                panel_type: "gauge".to_string(),
                ylabel: Some("Coefficient".to_string()),
                min: Some(0.0),
                max: Some(1.0),
            },
            DashboardPanel {
                title: "Heijunka: Worker Utilization %".to_string(),
                query: "heijunka_worker_utilization_percent".to_string(),
                panel_type: "gauge".to_string(),
                ylabel: Some("Percent".to_string()),
                min: Some(0.0),
                max: Some(100.0),
            },
            DashboardPanel {
                title: "Heijunka: Active Workers".to_string(),
                query: "heijunka_workers_active".to_string(),
                panel_type: "stat".to_string(),
                ylabel: Some("Count".to_string()),
                min: None,
                max: None,
            },
        ];

        Self {
            title: "TPS Kaizen - Continuous Improvement Metrics".to_string(),
            description: "Real-time monitoring of Jidoka, Kanban, Andon, and Heijunka metrics".to_string(),
            time_range: "24h".to_string(),
            panels,
        }
    }

    /// Generate Grafana dashboard JSON
    pub fn to_grafana_json(&self) -> Result<Value, serde_json::Error> {
        let mut panel_objects = Vec::new();

        for (idx, panel) in self.panels.iter().enumerate() {
            let grid_pos = self.calculate_grid_pos(idx);

            let panel_json = match panel.panel_type.as_str() {
                "gauge" => self.create_gauge_panel(panel, grid_pos)?,
                "graph" => self.create_graph_panel(panel, grid_pos)?,
                "stat" => self.create_stat_panel(panel, grid_pos)?,
                _ => self.create_graph_panel(panel, grid_pos)?,
            };

            panel_objects.push(panel_json);
        }

        let dashboard = json!({
            "annotations": {
                "list": [
                    {
                        "builtIn": 1,
                        "datasource": "-- Grafana --",
                        "enable": true,
                        "hide": true,
                        "iconColor": "rgba(0, 211, 255, 1)",
                        "name": "Annotations & Alerts",
                        "type": "dashboard"
                    }
                ]
            },
            "description": self.description,
            "editable": true,
            "gnetId": null,
            "graphTooltip": 0,
            "id": null,
            "links": [],
            "panels": panel_objects,
            "refresh": "30s",
            "schemaVersion": 26,
            "style": "dark",
            "tags": ["tps", "kaizen", "metrics"],
            "templating": {
                "list": []
            },
            "time": {
                "from": "now-24h",
                "to": "now"
            },
            "timepicker": {},
            "timezone": "UTC",
            "title": self.title,
            "uid": "tps-kaizen-dashboard",
            "version": 1
        });

        Ok(dashboard)
    }

    /// Create gauge panel JSON
    fn create_gauge_panel(&self, panel: &DashboardPanel, grid_pos: Value) -> Result<Value, serde_json::Error> {
        Ok(json!({
            "datasource": "Prometheus",
            "fieldConfig": {
                "defaults": {
                    "mappings": [],
                    "max": panel.max.unwrap_or(100.0),
                    "min": panel.min.unwrap_or(0.0),
                    "thresholds": {
                        "mode": "absolute",
                        "steps": [
                            {
                                "color": "green",
                                "value": null
                            },
                            {
                                "color": "red",
                                "value": 80.0
                            }
                        ]
                    },
                    "unit": "percent"
                },
                "overrides": []
            },
            "gridPos": grid_pos,
            "id": uuid::Uuid::new_v4().to_string(),
            "options": {
                "orientation": "auto",
                "reduceOptions": {
                    "values": false,
                    "fields": "",
                    "calcs": ["lastNotNull"]
                },
                "showThresholdLabels": false,
                "showThresholdMarkers": true
            },
            "pluginVersion": "7.0.0",
            "targets": [
                {
                    "expr": panel.query.clone(),
                    "legendFormat": "{{ instance }}",
                    "refId": "A"
                }
            ],
            "title": panel.title.clone(),
            "type": "gauge"
        }))
    }

    /// Create graph panel JSON
    fn create_graph_panel(&self, panel: &DashboardPanel, grid_pos: Value) -> Result<Value, serde_json::Error> {
        Ok(json!({
            "datasource": "Prometheus",
            "fieldConfig": {
                "defaults": {
                    "custom": {},
                    "unit": "short"
                },
                "overrides": []
            },
            "gridPos": grid_pos,
            "id": uuid::Uuid::new_v4().to_string(),
            "options": {
                "legend": {
                    "calcs": [],
                    "displayMode": "list",
                    "placement": "bottom"
                },
                "tooltip": {
                    "mode": "single"
                }
            },
            "pluginVersion": "7.0.0",
            "targets": [
                {
                    "expr": panel.query.clone(),
                    "legendFormat": "{{ instance }}",
                    "refId": "A"
                }
            ],
            "title": panel.title.clone(),
            "type": "timeseries"
        }))
    }

    /// Create stat panel JSON
    fn create_stat_panel(&self, panel: &DashboardPanel, grid_pos: Value) -> Result<Value, serde_json::Error> {
        Ok(json!({
            "datasource": "Prometheus",
            "fieldConfig": {
                "defaults": {
                    "mappings": [],
                    "thresholds": {
                        "mode": "absolute",
                        "steps": [
                            {
                                "color": "green",
                                "value": null
                            }
                        ]
                    },
                    "unit": "short"
                },
                "overrides": []
            },
            "gridPos": grid_pos,
            "id": uuid::Uuid::new_v4().to_string(),
            "options": {
                "colorMode": "background",
                "graphMode": "area",
                "justifyMode": "auto",
                "orientation": "auto",
                "reduceOptions": {
                    "values": false,
                    "fields": "",
                    "calcs": ["lastNotNull"]
                },
                "text": {}
            },
            "pluginVersion": "7.0.0",
            "targets": [
                {
                    "expr": panel.query.clone(),
                    "legendFormat": "{{ instance }}",
                    "refId": "A"
                }
            ],
            "title": panel.title.clone(),
            "type": "stat"
        }))
    }

    /// Calculate grid position for panel (auto-layout)
    fn calculate_grid_pos(&self, index: usize) -> Value {
        let panels_per_row = 3;
        let panel_width = 8;
        let panel_height = 8;

        let row = (index / panels_per_row) as i32;
        let col = (index % panels_per_row) as i32;

        json!({
            "h": panel_height,
            "w": panel_width,
            "x": col * panel_width,
            "y": row * panel_height
        })
    }
}

impl Default for Dashboard {
    fn default() -> Self {
        Self::kaizen_default()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_dashboard_creation() {
        let dashboard = Dashboard::kaizen_default();
        assert_eq!(dashboard.title, "TPS Kaizen - Continuous Improvement Metrics");
        assert!(!dashboard.panels.is_empty());
    }

    #[test]
    fn test_grafana_json_export() {
        let dashboard = Dashboard::kaizen_default();
        let json = dashboard.to_grafana_json().expect("Failed to generate JSON");

        assert!(json.get("title").is_some());
        assert!(json.get("panels").is_some());

        let panels = json.get("panels").and_then(|p| p.as_array());
        assert!(panels.is_some());
        assert!(panels.unwrap().len() > 0);
    }

    #[test]
    fn test_panel_types() {
        let dashboard = Dashboard::kaizen_default();

        let gauge_panels: Vec<_> = dashboard
            .panels
            .iter()
            .filter(|p| p.panel_type == "gauge")
            .collect();

        let graph_panels: Vec<_> = dashboard
            .panels
            .iter()
            .filter(|p| p.panel_type == "graph")
            .collect();

        assert!(!gauge_panels.is_empty());
        assert!(!graph_panels.is_empty());
    }
}
