//! XES (eXtensible Event Stream) format support.
//!
//! This module provides parsing and writing functionality for the XES event log format,
//! which is the standard format for process mining event logs.

use crate::error::{Error, Result};
use crate::event_log::AttributeValue;
use crate::event_log::{Event, EventLog, Trace};
use chrono::{DateTime, Utc};
use quick_xml::events::{BytesDecl, BytesEnd, BytesStart, Event as XmlEvent};
use quick_xml::Writer;
use std::fs::File;
use std::io::{BufReader, Read, Write};
use std::path::Path;

/// XES event log parser.
#[derive(Debug, Clone)]
pub struct XesParser {
    /// Whether to validate timestamps during parsing.
    pub validate_timestamps: bool,
}

impl Default for XesParser {
    fn default() -> Self {
        Self::new()
    }
}

impl XesParser {
    /// Create a new XES parser.
    #[must_use]
    pub fn new() -> Self {
        Self {
            validate_timestamps: true,
        }
    }

    /// Set whether to validate timestamps.
    #[must_use]
    pub fn with_timestamp_validation(mut self, validate: bool) -> Self {
        self.validate_timestamps = validate;
        self
    }

    /// Parse a XES file into an event log.
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - The file cannot be read
    /// - The XES format is invalid
    /// - Timestamps are invalid (when validation is enabled)
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::XesParser;
    ///
    /// let parser = XesParser::new();
    /// let log = parser.parse_file("log.xes")?;
    /// ```
    pub fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<EventLog> {
        let file = File::open(path.as_ref()).map_err(|e| Error::EventLogRead {
            path: path.as_ref().to_path_buf(),
            source: e,
        })?;

        let reader = BufReader::new(file);
        self.parse_reader(reader)
    }

    /// Parse XES from a string.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::XesParser;
    ///
    /// let parser = XesParser::new();
    /// let log = parser.parse_str(xes_string)?;
    /// ```
    pub fn parse_str(&self, xml: &str) -> Result<EventLog> {
        self.parse_reader(xml.as_bytes())
    }

    /// Parse XES from any reader.
    fn parse_reader<R: Read>(&self, reader: R) -> Result<EventLog> {
        let reader = BufReader::new(reader);
        let mut parser = quick_xml::Reader::from_reader(reader);

        let mut log = EventLog::new("Unnamed");
        let mut current_trace: Option<Trace> = None;
        let mut current_event: Option<Event> = None;
        let mut current_attribute: Option<(String, AttributeValue)> = None;

        let mut buf = Vec::new();

        loop {
            match parser.read_event_into(&mut buf) {
                Ok(XmlEvent::Start(ref e)) => match e.name().as_ref() {
                    b"log" => {
                        if let Some(name) = get_attribute(e, b"name") {
                            log.name = name;
                        }
                    }
                    b"trace" => {
                        current_trace = Some(Trace::new(generate_case_id()));
                    }
                    b"event" => {
                        current_event =
                            Some(Event::new(generate_event_id(), "", "1970-01-01T00:00:00Z")?);
                    }
                    b"string" | b"int" | b"float" | b"boolean" | b"date" => {
                        if let Some(key) = get_attribute(e, b"key") {
                            if let Some(value) = get_attribute(e, b"value") {
                                let attr_value = match e.name().as_ref() {
                                    b"string" => AttributeValue::String(value),
                                    b"int" => {
                                        let i = value.parse().unwrap_or(0);
                                        AttributeValue::Integer(i)
                                    }
                                    b"float" => {
                                        let f = value.parse().unwrap_or(0.0);
                                        AttributeValue::Float(f)
                                    }
                                    b"boolean" => {
                                        let b = value.parse().unwrap_or(false);
                                        AttributeValue::Boolean(b)
                                    }
                                    b"date" => {
                                        if self.validate_timestamps {
                                            let dt =
                                                value.parse::<DateTime<Utc>>().map_err(|_| {
                                                    Error::invalid_timestamp(value.clone())
                                                })?;
                                            AttributeValue::Timestamp(dt)
                                        } else {
                                            AttributeValue::String(value)
                                        }
                                    }
                                    _ => AttributeValue::String(value),
                                };
                                current_attribute = Some((key, attr_value));
                            }
                        }
                    }
                    _ => {}
                },
                Ok(XmlEvent::End(ref e)) => match e.name().as_ref() {
                    b"event" => {
                        if let (Some(trace), Some(event)) =
                            (&mut current_trace, current_event.take())
                        {
                            trace.events.push(event);
                        }
                    }
                    b"trace" => {
                        if let Some(trace) = current_trace.take() {
                            log.traces.push(trace);
                        }
                    }
                    b"string" | b"int" | b"float" | b"boolean" | b"date" => {
                        if let Some((key, value)) = current_attribute.take() {
                            if let Some(event) = &mut current_event {
                                match key.as_str() {
                                    "concept:name" => {
                                        if let AttributeValue::String(name) = value {
                                            event.activity = name;
                                        }
                                    }
                                    "time:timestamp" => {
                                        if let AttributeValue::Timestamp(ts) = value {
                                            event.timestamp = ts;
                                        }
                                    }
                                    "org:resource" => {
                                        if let AttributeValue::String(resource) = value {
                                            event.resource = Some(resource);
                                        }
                                    }
                                    _ => {
                                        event.attributes.insert(key, value);
                                    }
                                }
                            }
                        }
                    }
                    _ => {}
                },
                Ok(XmlEvent::Eof) => break,
                Err(e) => {
                    return Err(Error::XesParse {
                        line: parser.buffer_position() as usize,
                        message: e.to_string(),
                    });
                }
                _ => {}
            }
            buf.clear();
        }

        Ok(log)
    }
}

/// XES event log writer.
#[derive(Debug, Clone)]
pub struct XesWriter {
    /// Whether to include all event attributes in output.
    pub include_all_attributes: bool,
}

impl Default for XesWriter {
    fn default() -> Self {
        Self::new()
    }
}

impl XesWriter {
    /// Create a new XES writer.
    #[must_use]
    pub fn new() -> Self {
        Self {
            include_all_attributes: true,
        }
    }

    /// Set whether to include all attributes.
    #[must_use]
    pub fn with_all_attributes(mut self, include: bool) -> Self {
        self.include_all_attributes = include;
        self
    }

    /// Write an event log to a XES file.
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be written.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::XesWriter;
    ///
    /// let writer = XesWriter::new();
    /// writer.write_file("output.xes", &log)?;
    /// ```
    pub fn write_file<P: AsRef<Path>>(&self, path: P, log: &EventLog) -> Result<()> {
        let file = File::create(path.as_ref()).map_err(|e| Error::EventLogWrite {
            path: path.as_ref().to_path_buf(),
            source: e,
        })?;

        self.write_writer(file, log)
    }

    /// Write an event log to a string.
    ///
    /// # Examples
    ///
    /// ```rust,ignore
    /// use ggen_process_mining::XesWriter;
    ///
    /// let writer = XesWriter::new();
    /// let xes_string = writer.write_to_string(&log)?;
    /// ```
    pub fn write_to_string(&self, log: &EventLog) -> Result<String> {
        let mut buffer = Vec::new();
        self.write_writer(&mut buffer, log)?;
        String::from_utf8(buffer).map_err(|e| Error::EventLogWrite {
            path: "<string>".into(),
            source: std::io::Error::new(std::io::ErrorKind::InvalidData, e),
        })
    }

    /// Write an event log to any writer.
    fn write_writer<W: Write>(&self, writer: W, log: &EventLog) -> Result<()> {
        let mut writer = Writer::new_with_indent(writer, b' ', 2);

        // Write XES header
        let decl = BytesDecl::new("1.0", None, None);
        writer.write_event(XmlEvent::Decl(decl))?;

        // Start log element
        let mut log_start = BytesStart::new("log");
        log_start.push_attribute(("xmlns", "http://www.xes-standard.org/"));
        log_start.push_attribute(("name", log.name.as_str()));
        log_start.push_attribute(("version", "1.0"));
        writer.write_event(XmlEvent::Start(log_start))?;

        // Write extensions
        write_extensions(&mut writer)?;

        // Write traces
        for trace in &log.traces {
            write_trace(&mut writer, trace, self.include_all_attributes)?;
        }

        // End log element
        writer.write_event(XmlEvent::End(BytesEnd::new("log")))?;

        Ok(())
    }
}

/// Write XES extensions.
fn write_extensions<W: Write>(writer: &mut Writer<W>) -> Result<()> {
    writer.write_event(XmlEvent::Start(BytesStart::new("extensions")))?;

    // Concept extension
    writer.write_event(XmlEvent::Start(BytesStart::new("extension")))?;
    writer.write_event(XmlEvent::End(BytesEnd::new("extension")))?;

    // Time extension
    writer.write_event(XmlEvent::Start(BytesStart::new("extension")))?;
    writer.write_event(XmlEvent::End(BytesEnd::new("extension")))?;

    writer.write_event(XmlEvent::End(BytesEnd::new("extensions")))?;

    Ok(())
}

/// Write a single trace.
fn write_trace<W: Write>(writer: &mut Writer<W>, trace: &Trace, include_all: bool) -> Result<()> {
    writer.write_event(XmlEvent::Start(BytesStart::new("trace")))?;

    // Write case ID
    let mut string_start = BytesStart::new("string");
    string_start.push_attribute(("key", "concept:name"));
    string_start.push_attribute(("value", trace.case_id.as_str()));
    writer.write_event(XmlEvent::Empty(string_start))?;

    // Write events
    for event in &trace.events {
        write_event(writer, event, include_all)?;
    }

    writer.write_event(XmlEvent::End(BytesEnd::new("trace")))?;

    Ok(())
}

/// Write a single event.
fn write_event<W: Write>(writer: &mut Writer<W>, event: &Event, include_all: bool) -> Result<()> {
    writer.write_event(XmlEvent::Start(BytesStart::new("event")))?;

    // Write activity name
    let mut string_start = BytesStart::new("string");
    string_start.push_attribute(("key", "concept:name"));
    string_start.push_attribute(("value", event.activity.as_str()));
    writer.write_event(XmlEvent::Empty(string_start))?;

    // Write timestamp
    let mut date_start = BytesStart::new("date");
    date_start.push_attribute(("key", "time:timestamp"));
    date_start.push_attribute(("value", event.timestamp.to_rfc3339().as_str()));
    writer.write_event(XmlEvent::Empty(date_start))?;

    // Write resource if present
    if let Some(ref resource) = event.resource {
        let mut string_start = BytesStart::new("string");
        string_start.push_attribute(("key", "org:resource"));
        string_start.push_attribute(("value", resource.as_str()));
        writer.write_event(XmlEvent::Empty(string_start))?;
    }

    // Write additional attributes if enabled
    if include_all {
        for (key, value) in &event.attributes {
            write_attribute(writer, key, value)?;
        }
    }

    writer.write_event(XmlEvent::End(BytesEnd::new("event")))?;

    Ok(())
}

/// Write an attribute value.
fn write_attribute<W: Write>(
    writer: &mut Writer<W>, key: &str, value: &AttributeValue,
) -> Result<()> {
    match value {
        AttributeValue::String(s) => {
            let mut elem = BytesStart::new("string");
            elem.push_attribute(("key", key));
            elem.push_attribute(("value", s.as_str()));
            writer.write_event(XmlEvent::Empty(elem))?;
        }
        AttributeValue::Integer(i) => {
            let mut elem = BytesStart::new("int");
            elem.push_attribute(("key", key));
            elem.push_attribute(("value", i.to_string().as_str()));
            writer.write_event(XmlEvent::Empty(elem))?;
        }
        AttributeValue::Float(f) => {
            let mut elem = BytesStart::new("float");
            elem.push_attribute(("key", key));
            elem.push_attribute(("value", f.to_string().as_str()));
            writer.write_event(XmlEvent::Empty(elem))?;
        }
        AttributeValue::Boolean(b) => {
            let mut elem = BytesStart::new("boolean");
            elem.push_attribute(("key", key));
            elem.push_attribute(("value", b.to_string().as_str()));
            writer.write_event(XmlEvent::Empty(elem))?;
        }
        AttributeValue::Timestamp(dt) => {
            let mut elem = BytesStart::new("date");
            elem.push_attribute(("key", key));
            elem.push_attribute(("value", dt.to_rfc3339().as_str()));
            writer.write_event(XmlEvent::Empty(elem))?;
        }
        AttributeValue::List(_) => {
            // Lists not directly supported in XES - skip or flatten
        }
    }

    Ok(())
}

/// Get an attribute value from an XML element.
fn get_attribute(event: &BytesStart<'_>, name: &[u8]) -> Option<String> {
    event
        .attributes()
        .find(|a| {
            a.as_ref()
                .ok()
                .map(|a| a.key.as_ref() == name)
                .unwrap_or(false)
        })
        .and_then(|a| a.ok())
        .and_then(|a| std::str::from_utf8(a.value.as_ref()).ok().map(String::from))
}

/// Generate a unique case ID.
fn generate_case_id() -> String {
    use std::sync::atomic;
    static COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(1);
    format!("case-{}", COUNTER.fetch_add(1, atomic::Ordering::SeqCst))
}

/// Generate a unique event ID.
fn generate_event_id() -> String {
    use std::sync::atomic;
    static COUNTER: atomic::AtomicUsize = atomic::AtomicUsize::new(1);
    format!("event-{}", COUNTER.fetch_add(1, atomic::Ordering::SeqCst))
}

#[cfg(test)]
mod tests {
    use super::*;

    const SIMPLE_XES: &str = r#"<?xml version="1.0" encoding="UTF-8"?>
<log xmlns="http://www.xes-standard.org/" name="Test Log" version="1.0">
  <trace>
    <string key="concept:name" value="case1"/>
    <event>
      <string key="concept:name" value="A"/>
      <date key="time:timestamp" value="2024-01-01T10:00:00Z"/>
    </event>
    <event>
      <string key="concept:name" value="B"/>
      <date key="time:timestamp" value="2024-01-01T11:00:00Z"/>
    </event>
  </trace>
</log>"#;

    #[test]
    fn test_xes_parser_creation() {
        let parser = XesParser::new();
        assert!(parser.validate_timestamps);
    }

    #[test]
    fn test_xes_parser_with_config() {
        let parser = XesParser::new().with_timestamp_validation(false);
        assert!(!parser.validate_timestamps);
    }

    #[test]
    fn test_parse_simple_xes() {
        let parser = XesParser::new();
        let result = parser.parse_str(SIMPLE_XES);

        assert!(result.is_ok());

        let log = result.unwrap();
        assert_eq!(log.name, "Test Log");
        assert_eq!(log.traces.len(), 1);
        assert_eq!(log.traces[0].events.len(), 2);
        assert_eq!(log.traces[0].events[0].activity, "A");
    }

    #[test]
    fn test_parse_invalid_xes() {
        let parser = XesParser::new();
        let result = parser.parse_str("not xml");

        assert!(result.is_err());
    }

    #[test]
    fn test_xes_writer_creation() {
        let writer = XesWriter::new();
        assert!(writer.include_all_attributes);
    }

    #[test]
    fn test_write_and_parse_roundtrip() {
        let original_log = EventLog::new("Test").with_trace(
            Trace::new("case1").with_event(
                Event::new("e1", "A", "2024-01-01T10:00:00Z")
                    .unwrap()
                    .with_resource("user1"),
            ),
        );

        let writer = XesWriter::new();
        let xes_string = writer.write_to_string(&original_log).unwrap();

        let parser = XesParser::new();
        let parsed_log = parser.parse_str(&xes_string).unwrap();

        assert_eq!(parsed_log.name, original_log.name);
        assert_eq!(parsed_log.traces.len(), original_log.traces.len());
    }

    #[test]
    fn test_generate_unique_ids() {
        let id1 = generate_case_id();
        let id2 = generate_case_id();
        let id3 = generate_event_id();

        assert_ne!(id1, id2);
        assert!(id1.starts_with("case-"));
        assert!(id3.starts_with("event-"));
    }
}
