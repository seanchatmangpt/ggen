use chicago_tdd_tools::prelude::*;
use ggen_utils::app_config::*;
use std::sync::Once;

static INIT: Once = Once::new();

pub fn initialize() {
    INIT.call_once(|| {
        // Initialize configuration
        let config_contents = include_str!("resources/test_config.toml");
        AppConfig::init(Some(config_contents)).unwrap();
    });
}

test!(fetch_config, {
    // Arrange
    initialize();

    // Act
    let config = AppConfig::fetch().unwrap();

    // Assert
    assert_eq!(config.debug, false);
    assert_eq!(config.database.url, "custom database url");
});

test!(verify_get, {
    // Arrange
    initialize();

    // Act
    let debug_value = AppConfig::get::<bool>("debug").unwrap();
    let db_url = AppConfig::get::<String>("database.url").unwrap();

    // Assert
    assert_eq!(debug_value, false);
    assert_eq!(db_url, "custom database url");
});

test!(verify_set, {
    // Arrange
    initialize();

    // Act
    AppConfig::set("database.variable", "new value").unwrap();
    let config = AppConfig::fetch().unwrap();

    // Assert
    assert_eq!(config.database.variable, "new value");
});
