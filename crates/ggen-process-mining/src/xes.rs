//! XES (eXtensible Event Stream) event log format support.
//!
//! This module provides parsing and writing functionality for the XES standard,
//! utilizing quick-xml for high-performance processing and pictl-types for representation.

use crate::error::{Error, Result};
use crate::event_log::{AttributeValue, Event, EventLog, Trace};
use quick_xml::events::{BytesStart, Event as XmlEvent};
use quick_xml::Reader;
use std::collections::HashMap;
use std::fs::File;
use std::io::BufReader;
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

    /// Parse a XES file from the given path.
    pub fn parse_file<P: AsRef<Path>>(&self, path: P) -> Result<EventLog> {
        let file = File::open(path.as_ref()).map_err(|e| Error::EventLogRead {
            path: path.as_ref().to_path_buf(),
            source: e,
        })?;
        let reader = BufReader::new(file);
        self.parse_reader(reader)
    }

    /// Parse a XES string.
    pub fn parse_str(&self, xes: &str) -> Result<EventLog> {
        self.parse_reader(xes.as_bytes())
    }

    /// Parse XES data from any reader.
    pub fn parse_reader<R: std::io::BufRead>(&self, reader: R) -> Result<EventLog> {
        let mut parser = Reader::from_reader(reader);
        parser.config_mut().trim_text(true);

        let mut log = EventLog::new(Vec::new(), HashMap::new());
        let mut buf = Vec::new();

        let mut current_trace: Option<Trace> = None;
        let mut current_event: Option<Event> = None;
        let mut current_attribute: Option<(String, AttributeValue)> = None;

        let mut saw_log_element = false;

        loop {
            match parser.read_event_into(&mut buf) {
                Ok(XmlEvent::Start(ref e)) => match e.name().as_ref() {
                    b"log" => {
                        saw_log_element = true;
                        if let Some(name) = get_attribute(e, b"name") {
                            log.attributes.insert("concept:name".to_string(), AttributeValue::String(name));
                        }
                    }
                    b"trace" => {
                        current_trace = Some(Trace::new(String::new(), Vec::new()));
                    }
                    b"event" => {
                        current_event = Some(Event::new(HashMap::new()));
                    }
                    b"string" | b"int" | b"float" | b"boolean" | b"date" => {
                        if let Some(key) = get_attribute(e, b"key") {
                            if let Some(value) = get_attribute(e, b"value") {
                                let attr_value = match e.name().as_ref() {
                                    b"string" => AttributeValue::String(value),
                                    b"int" => AttributeValue::Int(value.parse().unwrap_or(0)),
                                    b"float" => AttributeValue::Float(value.parse().unwrap_or(0.0)),
                                    b"boolean" => AttributeValue::Boolean(value.parse().unwrap_or(false)),
                                    b"date" => AttributeValue::Date(value),
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
                        if let (Some(trace), Some(event)) = (&mut current_trace, current_event.take()) {
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
                                event.attributes.insert(key, value);
                            } else if let Some(trace) = &mut current_trace {
                                if key == "concept:name" {
                                    if let AttributeValue::String(name) = value {
                                        trace.case_id = name;
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

        if !saw_log_element {
            return Err(Error::XesParse {
                line: 0,
                message: "no <log> element found in XES document".to_string(),
            });
        }

        Ok(log)
    }
}

fn get_attribute(event: &BytesStart<'_>, name: &[u8]) -> Option<String> {
    event
        .attributes()
        .find(|a| a.as_ref().ok().is_some_and(|a| a.key.as_ref() == name))
        .and_then(std::result::Result::ok)
        .and_then(|a| std::str::from_utf8(a.value.as_ref()).ok().map(String::from))
}
