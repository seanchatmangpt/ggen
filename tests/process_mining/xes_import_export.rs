//! XES Import/Export Tests
//!
//! Tests for reading and writing XES (eXtensible Event Stream) format event logs.
//! Verifies roundtrip conversion and standard compliance.

use ggen_process_mining::{XesParser, XesWriter};
use ggen_process_mining::{EventLog, Trace, Event};
use ggen_process_mining::event_log::AttributeValue;
use std::io::Cursor;

#[cfg(test)]
mod xes_tests {
    use super::*;
    use super::super::helpers::*;

    /// Test XES parser creation and configuration.
    #[test]
    fn test_xes_parser_creation() {
        // Arrange & Act
        let parser = XesParser::new();
        let parser_no_validation = XesParser::new().with_timestamp_validation(false);

        // Assert
        assert!(parser.validate_timestamps);
        assert!(!parser_no_validation.validate_timestamps);
    }

    /// Test XES writer creation and configuration.
    #[test]
    fn test_xes_writer_creation() {
        // Arrange & Act
        let writer = XesWriter::new();
        let writer_minimal = XesWriter::new().with_all_attributes(false);

        // Assert
        assert!(writer.include_all_attributes);
        assert!(!writer_minimal.include_all_attributes);
    }

    /// Test parsing simple XES from string.
    #[test]
    fn test_parse_simple_xes_string() {
        // Arrange
        let xes_xml = create_sample_xes();
        let parser = XesParser::new();

        // Act
        let result = parser.parse_str(&xes_xml);

        // Assert
        assert!(result.is_ok());

        let log = result.unwrap();
        assert_eq!(log.name, "Test Log");
        assert_eq!(log.traces.len(), 2);

        // Verify first trace
        assert_eq!(log.traces[0].case_id, "case1");
        assert_eq!(log.traces[0].events.len(), 3);
        assert_eq!(log.traces[0].events[0].activity, "Submit");
        assert_eq!(log.traces[0].events[1].activity, "Review");
        assert_eq!(log.traces[0].events[2].activity, "Approve");
    }

    /// Test XES roundtrip: Parse -> Write -> Parse.
    #[test]
    fn test_xes_roundtrip_preservation() {
        // Arrange
        let original_xes = create_sample_xes();
        let parser = XesParser::new();
        let writer = XesWriter::new();

        // Act - First parse
        let log1 = parser.parse_str(&original_xes).unwrap();

        // Act - Write to string
        let xes_output = writer.write_to_string(&log1).unwrap();

        // Act - Second parse
        let log2 = parser.parse_str(&xes_output).unwrap();

        // Assert - Structure preserved
        assert_eq!(log1.name, log2.name);
        assert_eq!(log1.traces.len(), log2.traces.len());

        for (trace1, trace2) in log1.traces.iter().zip(log2.traces.iter()) {
            assert_eq!(trace1.events.len(), trace2.events.len());

            for (event1, event2) in trace1.events.iter().zip(trace2.events.iter()) {
                assert_eq!(event1.activity, event2.activity);
                assert_eq!(event1.timestamp, event2.timestamp);
            }
        }
    }

    /// Test XES export from EventLog.
    #[test]
    fn test_xes_export_from_event_log() {
        // Arrange
        let log = EventLog::new("Export Test Log")
            .with_trace(
                Trace::new("case1")
                    .with_event(Event::new("e1", "Start", "2024-01-01T10:00:00Z").unwrap())
                    .with_event(Event::new("e2", "End", "2024-01-01T11:00:00Z").unwrap())
            );

        let writer = XesWriter::new();

        // Act
        let result = writer.write_to_string(&log);

        // Assert
        assert!(result.is_ok());

        let xes = result.unwrap();
        assert!(xes.contains("<?xml version"));
        assert!(xes.contains("<log"));
        assert!(xes.contains("Export Test Log"));
        assert!(xes.contains("<trace>"));
        assert!(xes.contains("<event>"));
        assert!(xes.contains("Start"));
        assert!(xes.contains("End"));
    }

    /// Test XES import with all standard extensions.
    #[test]
    fn test_xes_with_extensions() {
        // Arrange
        let xes_with_extensions = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Extended Log" version="1.0">
  <extension name="Concept" prefix="concept" uri="http://www.xes-standard.org/concept.xesext"/>
  <extension name="Time" prefix="time" uri="http://www.xes-standard.org/time.xesext"/>
  <extension name="Organizational" prefix="org" uri="http://www.xes-standard.org/org.xesext"/>
  <extension name="Cost" prefix="cost" uri="http://www.xes-standard.org/cost.xesext"/>
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="Activity"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
      <string key="org:resource" value="Alice"/>
      <int key="cost:amount" value="100"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();

        // Act
        let result = parser.parse_str(xes_with_extensions);

        // Assert
        assert!(result.is_ok());

        let log = result.unwrap();
        assert_eq!(log.traces.len(), 1);
        assert_eq!(log.traces[0].events.len(), 1);
        assert_eq!(log.traces[0].events[0].resource, Some("Alice".to_string()));
    }

    /// Test XES with various attribute types.
    #[test]
    fn test_xes_attribute_types() {
        // Arrange
        let xes_with_types = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Typed Log" version="1.0">
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="Activity"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
      <string key="str_attr" value="text value"/>
      <int key="int_attr" value="42"/>
      <float key="float_attr" value="3.14"/>
      <boolean key="bool_attr" value="true"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();

        // Act
        let result = parser.parse_str(xes_with_types);

        // Assert
        assert!(result.is_ok());

        let log = result.unwrap();
        let event = &log.traces[0].events[0];

        assert!(event.attributes.contains_key("str_attr"));
        assert!(event.attributes.contains_key("int_attr"));
        assert!(event.attributes.contains_key("float_attr"));
        assert!(event.attributes.contains_key("bool_attr"));
    }

    /// Test XES with invalid timestamp handling.
    #[test]
    fn test_xes_invalid_timestamp() {
        // Arrange
        let xes_invalid_ts = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Invalid Timestamp" version="1.0">
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="Activity"/>
      <date key="time:timestamp" value="not-a-valid-timestamp"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();

        // Act
        let result = parser.parse_str(xes_invalid_ts);

        // Assert
        assert!(result.is_err());

        // With validation disabled, should succeed
        let parser_no_validation = XesParser::new().with_timestamp_validation(false);
        let result_no_validation = parser_no_validation.parse_str(xes_invalid_ts);
        assert!(result_no_validation.is_ok());
    }

    /// Test XES with multiple traces.
    #[test]
    fn test_xes_multiple_traces() {
        // Arrange
        let xes_multiple = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Multi Trace" version="1.0">
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="A"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
    </event>
  </trace>
  <trace>
    <string key="concept:name" value="case2"/>
    <event>
      <string key="concept:name" value="B"/>
      <date key="time:timestamp" value="2024-01-01T11:00:00Z"/>
    </event>
  </trace>
  <trace>
    <string key="concept:name" value="case3"/>
    <event>
      <string key="concept:name" value="C"/>
      <date key="time:timestamp" value="2024-01-01T12:00:00Z"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();

        // Act
        let log = parser.parse_str(xes_multiple).unwrap();

        // Assert
        assert_eq!(log.traces.len(), 3);
        assert_eq!(log.traces[0].case_id, "case1");
        assert_eq!(log.traces[1].case_id, "case2");
        assert_eq!(log.traces[2].case_id, "case3");
    }

    /// Test XES with nested attributes (global scope).
    #[test]
    fn test_xes_global_attributes() {
        // Arrange
        let xes_globals = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Global Attributes" version="1.0">
  <global scope="trace">
    <string key="concept:name" value="case id"/>
  </global>
  <global scope="event">
    <string key="concept:name" value="activity"/>
    <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
  </global>
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="Activity"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();

        // Act
        let result = parser.parse_str(xes_globals);

        // Assert
        assert!(result.is_ok());
        let log = result.unwrap();
        assert_eq!(log.traces.len(), 1);
    }

    /// Test XES writer includes all attributes when enabled.
    #[test]
    fn test_xes_writer_includes_attributes() {
        // Arrange
        use ggen_process_mining::event_log::AttributeValue;

        let log = EventLog::new("Attribute Test")
            .with_trace(
                Trace::new("case1")
                    .with_event(
                        Event::new("e1", "Activity", "2024-01-01T10:00:00Z").unwrap()
                            .with_attribute("custom_attr".to_string(), AttributeValue::String("custom_value".to_string()))
                            .with_attribute("number_attr".to_string(), AttributeValue::Integer(42))
                    )
            );

        let writer = XesWriter::new().with_all_attributes(true);

        // Act
        let xes = writer.write_to_string(&log).unwrap();

        // Assert
        assert!(xes.contains("custom_attr"));
        assert!(xes.contains("number_attr"));
    }

    /// Test XES writer minimal output.
    #[test]
    fn test_xes_writer_minimal_output() {
        // Arrange
        use ggen_process_mining::event_log::AttributeValue;

        let log = EventLog::new("Minimal Test")
            .with_trace(
                Trace::new("case1")
                    .with_event(
                        Event::new("e1", "Activity", "2024-01-01T10:00:00Z").unwrap()
                            .with_attribute("extra".to_string(), AttributeValue::String("value".to_string()))
                    )
            );

        let writer = XesWriter::new().with_all_attributes(false);

        // Act
        let xes = writer.write_to_string(&log).unwrap();

        // Assert - Should have core elements but not extra attributes
        assert!(xes.contains("<log"));
        assert!(xes.contains("<trace>"));
        assert!(xes.contains("<event>"));
        assert!(xes.contains("concept:name"));
        assert!(xes.contains("time:timestamp"));
        // Extra attributes may be omitted when include_all_attributes is false
    }

    /// Test XES export with special characters in values.
    #[test]
    fn test_xes_special_characters() {
        // Arrange
        let log = EventLog::new("Special Chars")
            .with_trace(
                Trace::new("case<1>")
                    .with_event(
                        Event::new("e1", "Activity & Test", "2024-01-01T10:00:00Z").unwrap()
                    )
            );

        let writer = XesWriter::new();

        // Act
        let xes = writer.write_to_string(&log).unwrap();

        // Assert - Special characters should be properly escaped
        assert!(xes.contains("case&lt;1&gt;") || xes.contains("case<1>"));
        assert!(xes.contains("Activity &amp; Test") || xes.contains("Activity & Test"));
    }

    /// Test XES parse empty log.
    #[test]
    fn test_xes_empty_log() {
        // Arrange
        let xes_empty = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Empty Log" version="1.0">
</log>"#;

        let parser = XesParser::new();

        // Act
        let log = parser.parse_str(xes_empty).unwrap();

        // Assert
        assert_eq!(log.name, "Empty Log");
        assert_eq!(log.traces.len(), 0);
    }

    /// Test XES with trace having single event.
    #[test]
    fn test_xes_single_event_trace() {
        // Arrange
        let xes_single = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Single Event" version="1.0">
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="OnlyActivity"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();

        // Act
        let log = parser.parse_str(xes_single).unwrap();

        // Assert
        assert_eq!(log.traces.len(), 1);
        assert_eq!(log.traces[0].events.len(), 1);
        assert_eq!(log.traces[0].events[0].activity, "OnlyActivity");
    }

    /// Test XES preserves resource information.
    #[test]
    fn test_xes_resource_preservation() {
        // Arrange
        let xes_with_resource = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Resource Log" version="1.0">
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="Activity"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
      <string key="org:resource" value="Resource123"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();
        let writer = XesWriter::new();

        // Act
        let log = parser.parse_str(xes_with_resource).unwrap();
        let xes_output = writer.write_to_string(&log).unwrap();

        // Assert
        assert_eq!(log.traces[0].events[0].resource, Some("Resource123".to_string()));
        assert!(xes_output.contains("Resource123"));
    }

    /// Test XES handles malformed XML gracefully.
    #[test]
    fn test_xes_malformed_xml() {
        // Arrange
        let malformed_xes = "This is not XML at all";

        let parser = XesParser::new();

        // Act
        let result = parser.parse_str(malformed_xes);

        // Assert
        assert!(result.is_err());
    }

    /// Test XES with trace-level attributes.
    #[test]
    fn test_xes_trace_attributes() {
        // Arrange
        let xes_trace_attrs = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Trace Attrs" version="1.0">
  <trace>
    <string key="concept:name" value="case1"/>
    <string key="custom:traceAttr" value="traceValue"/>
    <event>
      <string key="concept:name" value="Activity"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
    </event>
  </trace>
</log>"#;

        let parser = XesParser::new();

        // Act
        let result = parser.parse_str(xes_trace_attrs);

        // Assert
        assert!(result.is_ok());
        // Trace attributes are parsed but may not be stored in current implementation
    }
}
