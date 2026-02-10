#[cfg(test)]
mod tests {
    use osiris_sensors::{SensorManager, SensorDataType, PrivacyLevel};

    #[tokio::test]
    async fn test_sensor_manager_creation() {
        let manager = SensorManager::new();
        assert!(manager.get_sensors().await.is_empty());
    }

    #[tokio::test]
    async fn test_add_sensor() {
        let manager = SensorManager::new();
        let config = osiris_sensors::SensorConfig {
            sensor_type: "test".to_string(),
            enabled: true,
            frequency_ms: 100,
            batch_size: 10,
            privacy_level: PrivacyLevel::Low,
        };

        let result = manager.add_sensor("test-sensor".to_string(), config).await;
        assert!(result.is_ok());

        let sensors = manager.get_sensors().await;
        assert_eq!(sensors.len(), 1);
        assert!(sensors.contains_key("test-sensor"));
    }

    #[tokio::test]
    async fn test_sensor_data_buffer() {
        let manager = SensorManager::new();
        let config = osiris_sensors::SensorConfig {
            sensor_type: "test".to_string(),
            enabled: true,
            frequency_ms: 100,
            batch_size: 10,
            privacy_level: PrivacyLevel::Low,
        };

        manager.add_sensor("test-sensor".to_string(), config).await.unwrap();

        let sensor_data = SensorDataType::Accelerometer { x: 1.0, y: 2.0, z: 3.0 };
        let result = manager.add_sensor_data("test-sensor", sensor_data).await;
        assert!(result.is_ok());

        let data = manager.get_sensor_data("test-sensor").await.unwrap();
        assert_eq!(data.len(), 1);
    }
}