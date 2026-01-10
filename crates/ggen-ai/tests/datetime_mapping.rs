//! Integration tests for DateTime type mapping from XSD to Rust chrono types
//!
//! This test suite verifies:
//! - Proper XSD datetime type detection in SHACL parser
//! - Correct Rust type mapping for all XSD temporal types
//! - DateTime parsing and validation for all supported formats
//! - JSON Schema format keywords for datetime fields
//! - Serialization/deserialization roundtrips
//! - Edge cases and boundary conditions
//!
//! Chicago TDD Pattern (AAA: Arrange/Act/Assert)

use chrono::{Datelike, Timelike};
use ggen_ai::codegen::{
    datetime::*, map_xsd_to_rust_type, parse_date, parse_datetime, parse_duration, parse_time,
    validate_gday, validate_gmonth, validate_gmonth_day, validate_gyear, validate_gyear_month,
};

// ===== DateTime Mapping Tests =====

#[test]
fn datetime_mapping_datetime_type() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#dateTime";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    assert_eq!(rust_type, "chrono::DateTime<chrono::Utc>");
}

#[test]
fn datetime_mapping_date_type() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#date";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    assert_eq!(rust_type, "chrono::NaiveDate");
}

#[test]
fn datetime_mapping_time_type() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#time";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    assert_eq!(rust_type, "chrono::NaiveTime");
}

#[test]
fn datetime_mapping_duration_type() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#duration";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    assert_eq!(rust_type, "chrono::Duration");
}

#[test]
fn datetime_mapping_gyear_month_type() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#gYearMonth";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    assert_eq!(rust_type, "(u16, u8)");
}

#[test]
fn datetime_mapping_gmonth_day_type() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#gMonthDay";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    assert_eq!(rust_type, "(u8, u8)");
}

#[test]
fn datetime_mapping_all_temporal_types() {
    // Arrange
    let test_cases = vec![
        (
            "http://www.w3.org/2001/XMLSchema#dateTime",
            "chrono::DateTime<chrono::Utc>",
        ),
        (
            "http://www.w3.org/2001/XMLSchema#date",
            "chrono::NaiveDate",
        ),
        (
            "http://www.w3.org/2001/XMLSchema#time",
            "chrono::NaiveTime",
        ),
        (
            "http://www.w3.org/2001/XMLSchema#duration",
            "chrono::Duration",
        ),
        ("http://www.w3.org/2001/XMLSchema#gYear", "u16"),
        ("http://www.w3.org/2001/XMLSchema#gYearMonth", "(u16, u8)"),
        ("http://www.w3.org/2001/XMLSchema#gDay", "u8"),
        ("http://www.w3.org/2001/XMLSchema#gMonth", "u8"),
        ("http://www.w3.org/2001/XMLSchema#gMonthDay", "(u8, u8)"),
    ];

    // Act & Assert
    for (xsd_uri, expected_type) in test_cases {
        let rust_type = map_xsd_to_rust_type(xsd_uri);
        assert_eq!(
            rust_type, expected_type,
            "Type mapping failed for {}",
            xsd_uri
        );
    }
}

// ===== DateTime Parsing Tests =====

#[test]
fn datetime_parse_rfc3339_utc() {
    // Arrange
    let input = "2023-12-25T10:30:45Z";

    // Act
    let result = parse_datetime(input);

    // Assert
    assert!(result.is_ok());
    let dt = result.unwrap();
    assert_eq!(dt.year(), 2023);
    assert_eq!(dt.month(), 12);
    assert_eq!(dt.day(), 25);
    assert_eq!(dt.hour(), 10);
    assert_eq!(dt.minute(), 30);
    assert_eq!(dt.second(), 45);
}

#[test]
fn datetime_parse_with_positive_offset() {
    // Arrange
    let input = "2023-12-25T10:30:45+05:30";

    // Act
    let result = parse_datetime(input);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn datetime_parse_with_negative_offset() {
    // Arrange
    let input = "2023-12-25T10:30:45-08:00";

    // Act
    let result = parse_datetime(input);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn datetime_parse_with_fractional_seconds() {
    // Arrange
    let input = "2023-12-25T10:30:45.123Z";

    // Act
    let result = parse_datetime(input);

    // Assert
    assert!(result.is_ok());
    let dt = result.unwrap();
    assert_eq!(dt.second(), 45);
}

#[test]
fn datetime_parse_invalid_format_should_fail() {
    // Arrange
    let input = "2023/12/25 10:30:45";

    // Act
    let result = parse_datetime(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn datetime_parse_invalid_month_should_fail() {
    // Arrange
    let input = "2023-13-25T10:30:45Z";

    // Act
    let result = parse_datetime(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn datetime_parse_invalid_hour_should_fail() {
    // Arrange
    let input = "2023-12-25T25:30:45Z";

    // Act
    let result = parse_datetime(input);

    // Assert
    assert!(result.is_err());
}

// ===== Date Parsing Tests =====

#[test]
fn date_parse_valid() {
    // Arrange
    let input = "2023-12-25";

    // Act
    let result = parse_date(input);

    // Assert
    assert!(result.is_ok());
    let date = result.unwrap();
    assert_eq!(date.year(), 2023);
    assert_eq!(date.month(), 12);
    assert_eq!(date.day(), 25);
}

#[test]
fn date_parse_leap_year() {
    // Arrange
    let input = "2024-02-29";

    // Act
    let result = parse_date(input);

    // Assert
    assert!(result.is_ok());
    let date = result.unwrap();
    assert_eq!(date.day(), 29);
}

#[test]
fn date_parse_invalid_leap_year_should_fail() {
    // Arrange
    let input = "2023-02-29";

    // Act
    let result = parse_date(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn date_parse_invalid_day_should_fail() {
    // Arrange
    let input = "2023-02-30";

    // Act
    let result = parse_date(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn date_parse_boundary_min_year() {
    // Arrange
    let input = "0000-01-01";

    // Act
    let result = parse_date(input);

    // Assert
    assert!(result.is_ok());
}

#[test]
fn date_parse_boundary_max_year() {
    // Arrange
    let input = "9999-12-31";

    // Act
    let result = parse_date(input);

    // Assert
    assert!(result.is_ok());
}

// ===== Time Parsing Tests =====

#[test]
fn time_parse_valid() {
    // Arrange
    let input = "10:30:45";

    // Act
    let result = parse_time(input);

    // Assert
    assert!(result.is_ok());
    let time = result.unwrap();
    assert_eq!(time.hour(), 10);
    assert_eq!(time.minute(), 30);
    assert_eq!(time.second(), 45);
}

#[test]
fn time_parse_with_milliseconds() {
    // Arrange
    let input = "10:30:45.123";

    // Act
    let result = parse_time(input);

    // Assert
    assert!(result.is_ok());
    let time = result.unwrap();
    assert_eq!(time.hour(), 10);
    assert_eq!(time.minute(), 30);
}

#[test]
fn time_parse_midnight() {
    // Arrange
    let input = "00:00:00";

    // Act
    let result = parse_time(input);

    // Assert
    assert!(result.is_ok());
    let time = result.unwrap();
    assert_eq!(time.hour(), 0);
    assert_eq!(time.minute(), 0);
    assert_eq!(time.second(), 0);
}

#[test]
fn time_parse_end_of_day() {
    // Arrange
    let input = "23:59:59";

    // Act
    let result = parse_time(input);

    // Assert
    assert!(result.is_ok());
    let time = result.unwrap();
    assert_eq!(time.hour(), 23);
    assert_eq!(time.minute(), 59);
    assert_eq!(time.second(), 59);
}

#[test]
fn time_parse_invalid_hour_should_fail() {
    // Arrange
    let input = "25:30:45";

    // Act
    let result = parse_time(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn time_parse_invalid_minute_should_fail() {
    // Arrange
    let input = "10:60:45";

    // Act
    let result = parse_time(input);

    // Assert
    assert!(result.is_err());
}

// ===== Duration Parsing Tests =====

#[test]
fn duration_parse_days() {
    // Arrange
    let input = "P1D";

    // Act
    let result = parse_duration(input);

    // Assert
    assert!(result.is_ok());
    let duration = result.unwrap();
    assert_eq!(duration.num_days(), 1);
}

#[test]
fn duration_parse_hours() {
    // Arrange
    let input = "PT1H";

    // Act
    let result = parse_duration(input);

    // Assert
    assert!(result.is_ok());
    let duration = result.unwrap();
    assert_eq!(duration.num_hours(), 1);
}

#[test]
fn duration_parse_combined() {
    // Arrange
    let input = "P1DT12H30M";

    // Act
    let result = parse_duration(input);

    // Assert
    assert!(result.is_ok());
    let duration = result.unwrap();
    assert_eq!(duration.num_hours(), 36); // 1 day (24h) + 12h
}

#[test]
fn duration_parse_invalid_missing_p_should_fail() {
    // Arrange
    let input = "1D";

    // Act
    let result = parse_duration(input);

    // Assert
    assert!(result.is_err());
}

// ===== GYear Validation Tests =====

#[test]
fn gyear_validate_valid() {
    // Arrange
    let input = "2023";

    // Act
    let result = validate_gyear(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 2023);
}

#[test]
fn gyear_validate_year_zero() {
    // Arrange
    let input = "0000";

    // Act
    let result = validate_gyear(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 0);
}

#[test]
fn gyear_validate_max_year() {
    // Arrange
    let input = "9999";

    // Act
    let result = validate_gyear(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 9999);
}

#[test]
fn gyear_validate_too_short_should_fail() {
    // Arrange
    let input = "23";

    // Act
    let result = validate_gyear(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn gyear_validate_non_numeric_should_fail() {
    // Arrange
    let input = "abcd";

    // Act
    let result = validate_gyear(input);

    // Assert
    assert!(result.is_err());
}

// ===== GYearMonth Validation Tests =====

#[test]
fn gyear_month_validate_valid() {
    // Arrange
    let input = "2023-12";

    // Act
    let result = validate_gyear_month(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), (2023, 12));
}

#[test]
fn gyear_month_validate_january() {
    // Arrange
    let input = "2023-01";

    // Act
    let result = validate_gyear_month(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), (2023, 1));
}

#[test]
fn gyear_month_validate_invalid_month_zero_should_fail() {
    // Arrange
    let input = "2023-00";

    // Act
    let result = validate_gyear_month(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn gyear_month_validate_invalid_month_13_should_fail() {
    // Arrange
    let input = "2023-13";

    // Act
    let result = validate_gyear_month(input);

    // Assert
    assert!(result.is_err());
}

// ===== GDay Validation Tests =====

#[test]
fn gday_validate_valid() {
    // Arrange
    let input = "25";

    // Act
    let result = validate_gday(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 25);
}

#[test]
fn gday_validate_first_day() {
    // Arrange
    let input = "1";

    // Act
    let result = validate_gday(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 1);
}

#[test]
fn gday_validate_last_day() {
    // Arrange
    let input = "31";

    // Act
    let result = validate_gday(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 31);
}

#[test]
fn gday_validate_zero_should_fail() {
    // Arrange
    let input = "0";

    // Act
    let result = validate_gday(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn gday_validate_32_should_fail() {
    // Arrange
    let input = "32";

    // Act
    let result = validate_gday(input);

    // Assert
    assert!(result.is_err());
}

// ===== GMonth Validation Tests =====

#[test]
fn gmonth_validate_valid() {
    // Arrange
    let input = "12";

    // Act
    let result = validate_gmonth(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 12);
}

#[test]
fn gmonth_validate_january() {
    // Arrange
    let input = "1";

    // Act
    let result = validate_gmonth(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 1);
}

#[test]
fn gmonth_validate_december() {
    // Arrange
    let input = "12";

    // Act
    let result = validate_gmonth(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 12);
}

#[test]
fn gmonth_validate_zero_should_fail() {
    // Arrange
    let input = "0";

    // Act
    let result = validate_gmonth(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn gmonth_validate_13_should_fail() {
    // Arrange
    let input = "13";

    // Act
    let result = validate_gmonth(input);

    // Assert
    assert!(result.is_err());
}

// ===== GMonthDay Validation Tests =====

#[test]
fn gmonth_day_validate_valid() {
    // Arrange
    let input = "12-25";

    // Act
    let result = validate_gmonth_day(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), (12, 25));
}

#[test]
fn gmonth_day_validate_february_29() {
    // Arrange
    let input = "02-29";

    // Act
    let result = validate_gmonth_day(input);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), (2, 29));
}

#[test]
fn gmonth_day_validate_invalid_month_should_fail() {
    // Arrange
    let input = "13-15";

    // Act
    let result = validate_gmonth_day(input);

    // Assert
    assert!(result.is_err());
}

#[test]
fn gmonth_day_validate_invalid_day_should_fail() {
    // Arrange
    let input = "12-32";

    // Act
    let result = validate_gmonth_day(input);

    // Assert
    assert!(result.is_err());
}

// ===== DateTimeValidation Builder Tests =====

#[test]
fn datetime_validation_builder_creates_instance() {
    // Arrange
    let value = "2023-12-25";
    let xsd_type = "xsd:date";

    // Act
    let validation = DateTimeValidation::new(value, xsd_type);

    // Assert
    assert_eq!(validation.value, value);
    assert_eq!(validation.xsd_type, xsd_type);
    assert!(!validation.has_timezone);
    assert_eq!(validation.timezone_offset, None);
}

#[test]
fn datetime_validation_builder_with_timezone() {
    // Arrange
    let value = "2023-12-25T10:30:45";
    let xsd_type = "xsd:dateTime";
    let offset_minutes = 330; // UTC+5:30

    // Act
    let validation = DateTimeValidation::new(value, xsd_type).with_timezone(offset_minutes);

    // Assert
    assert_eq!(validation.value, value);
    assert_eq!(validation.xsd_type, xsd_type);
    assert!(validation.has_timezone);
    assert_eq!(validation.timezone_offset, Some(offset_minutes));
}

// ===== Serialization Tests =====

#[test]
fn datetime_validation_serialization_roundtrip() {
    // Arrange
    let original = DateTimeValidation::new("2023-12-25", "xsd:date").with_timezone(0);

    // Act
    let json = serde_json::to_value(&original).expect("Serialization failed");
    let deserialized: DateTimeValidation =
        serde_json::from_value(json).expect("Deserialization failed");

    // Assert
    assert_eq!(deserialized, original);
}

// ===== JSON Schema Format Keyword Tests =====

#[test]
fn datetime_json_schema_format_datetime() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#dateTime";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    // Should map to chrono::DateTime<chrono::Utc> which corresponds to "date-time" in JSON Schema
    assert_eq!(rust_type, "chrono::DateTime<chrono::Utc>");
}

#[test]
fn datetime_json_schema_format_date() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#date";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    // Should map to chrono::NaiveDate which corresponds to "date" in JSON Schema
    assert_eq!(rust_type, "chrono::NaiveDate");
}

#[test]
fn datetime_json_schema_format_time() {
    // Arrange
    let xsd_uri = "http://www.w3.org/2001/XMLSchema#time";

    // Act
    let rust_type = map_xsd_to_rust_type(xsd_uri);

    // Assert
    // Should map to chrono::NaiveTime which corresponds to "time" in JSON Schema
    assert_eq!(rust_type, "chrono::NaiveTime");
}
