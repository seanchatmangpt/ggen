// HL7 v2 Message Parser and Generator
// Implements HL7 v2.x message parsing, validation, and generation

use std::collections::HashMap;

// ============================================================================
// HL7 Message Structures
// ============================================================================

#[derive(Debug, Clone)]
pub struct HL7Message {
    pub message_type: String,
    pub trigger_event: String,
    pub segments: Vec<HL7Segment>,
}

#[derive(Debug, Clone)]
pub struct HL7Segment {
    pub segment_id: String,
    pub fields: Vec<String>,
}

#[derive(Debug)]
pub struct HL7Parser {
    field_separator: char,
    component_separator: char,
    repetition_separator: char,
    escape_character: char,
    subcomponent_separator: char,
}

impl Default for HL7Parser {
    fn default() -> Self {
        Self {
            field_separator: '|',
            component_separator: '^',
            repetition_separator: '~',
            escape_character: '\\',
            subcomponent_separator: '&',
        }
    }
}

impl HL7Parser {
    pub fn new() -> Self {
        Self::default()
    }

    // Parse HL7 message from string
    pub fn parse(&mut self, message: &str) -> Result<HL7Message, String> {
        let lines: Vec<&str> = message.lines()
            .map(|l| l.trim())
            .filter(|l| !l.is_empty())
            .collect();

        if lines.is_empty() {
            return Err("Empty message".to_string());
        }

        // First segment must be MSH
        let first_line = lines[0];
        if !first_line.starts_with("MSH") {
            return Err("Message must start with MSH segment".to_string());
        }

        // Extract encoding characters from MSH-2
        if first_line.len() < 9 {
            return Err("Invalid MSH segment".to_string());
        }

        let encoding_chars = &first_line[4..9];
        self.component_separator = encoding_chars.chars().nth(0).unwrap_or('^');
        self.repetition_separator = encoding_chars.chars().nth(1).unwrap_or('~');
        self.escape_character = encoding_chars.chars().nth(2).unwrap_or('\\');
        self.subcomponent_separator = encoding_chars.chars().nth(3).unwrap_or('&');

        let mut segments = Vec::new();

        for line in lines {
            let segment = self.parse_segment(line)?;
            segments.push(segment);
        }

        // Extract message type and trigger event from MSH-9
        let (message_type, trigger_event) = self.extract_message_type(&segments)?;

        Ok(HL7Message {
            message_type,
            trigger_event,
            segments,
        })
    }

    fn parse_segment(&self, line: &str) -> Result<HL7Segment, String> {
        if line.len() < 3 {
            return Err("Segment too short".to_string());
        }

        let segment_id = &line[0..3];

        let fields: Vec<String> = if segment_id == "MSH" {
            // MSH is special - field separator is between MSH and encoding characters
            let mut msh_fields = vec![segment_id.to_string()];
            msh_fields.push(self.field_separator.to_string());

            let rest = &line[4..];
            let remaining_fields: Vec<String> = rest
                .split(self.field_separator)
                .map(|s| s.to_string())
                .collect();

            msh_fields.extend(remaining_fields);
            msh_fields
        } else {
            line.split(self.field_separator)
                .map(|s| s.to_string())
                .collect()
        };

        Ok(HL7Segment {
            segment_id: segment_id.to_string(),
            fields,
        })
    }

    fn extract_message_type(&self, segments: &[HL7Segment]) -> Result<(String, String), String> {
        let msh = segments.first()
            .ok_or_else(|| "No MSH segment".to_string())?;

        // MSH-9 contains message type
        if msh.fields.len() < 9 {
            return Err("MSH segment missing message type field".to_string());
        }

        let msh9 = &msh.fields[8];
        let parts: Vec<&str> = msh9.split(self.component_separator).collect();

        let message_type = parts.get(0).unwrap_or(&"").to_string();
        let trigger_event = parts.get(1).unwrap_or(&"").to_string();

        Ok((message_type, trigger_event))
    }

    // Get field value by path (e.g., "PID-3.1" for patient ID)
    pub fn get_field(&self, message: &HL7Message, path: &str) -> Option<String> {
        let parts: Vec<&str> = path.split('-').collect();
        if parts.len() != 2 {
            return None;
        }

        let segment_id = parts[0];
        let field_spec = parts[1];

        let segment = message.segments.iter()
            .find(|s| s.segment_id == segment_id)?;

        let field_parts: Vec<&str> = field_spec.split('.').collect();
        let field_num: usize = field_parts[0].parse().ok()?;

        if field_num >= segment.fields.len() {
            return None;
        }

        let field_value = &segment.fields[field_num];

        if field_parts.len() == 1 {
            return Some(field_value.clone());
        }

        // Handle component access
        let component_num: usize = field_parts[1].parse().ok()?;
        let components: Vec<&str> = field_value.split(self.component_separator).collect();

        components.get(component_num.saturating_sub(1))
            .map(|s| s.to_string())
    }

    // Generate HL7 message from structure
    pub fn generate(&self, message: &HL7Message) -> String {
        let mut lines = Vec::new();

        for segment in &message.segments {
            let line = if segment.segment_id == "MSH" {
                // MSH is special
                format!("MSH{}{}",
                    self.field_separator,
                    segment.fields[2..].join(&self.field_separator.to_string()))
            } else {
                segment.fields.join(&self.field_separator.to_string())
            };

            lines.push(line);
        }

        lines.join("\r\n")
    }

    // Create ACK message
    pub fn create_ack(
        &self,
        original_message: &HL7Message,
        ack_code: &str,
        text_message: Option<&str>
    ) -> Result<HL7Message, String> {
        let original_msh = original_message.segments.first()
            .ok_or_else(|| "No MSH segment in original message".to_string())?;

        // Create MSH segment for ACK
        let msh_fields = vec![
            "MSH".to_string(),
            self.field_separator.to_string(),
            format!("{}{}{}{}", self.component_separator, self.repetition_separator,
                    self.escape_character, self.subcomponent_separator),
            self.get_field_value(&original_msh.fields, 4), // Sending App <- Receiving App
            self.get_field_value(&original_msh.fields, 5), // Sending Facility
            self.get_field_value(&original_msh.fields, 2), // Receiving App <- Sending App
            self.get_field_value(&original_msh.fields, 3), // Receiving Facility
            self.format_timestamp(&chrono::Utc::now()),
            String::new(),
            "ACK".to_string(),
            uuid::Uuid::new_v4().to_string(),
            self.get_field_value(&original_msh.fields, 11), // Processing ID
        ];

        // Create MSA segment
        let message_control_id = self.get_field_value(&original_msh.fields, 9);
        let msa_fields = vec![
            "MSA".to_string(),
            ack_code.to_string(),
            message_control_id,
            text_message.unwrap_or("").to_string(),
        ];

        Ok(HL7Message {
            message_type: "ACK".to_string(),
            trigger_event: String::new(),
            segments: vec![
                HL7Segment { segment_id: "MSH".to_string(), fields: msh_fields },
                HL7Segment { segment_id: "MSA".to_string(), fields: msa_fields },
            ],
        })
    }

    fn get_field_value(&self, fields: &[String], index: usize) -> String {
        fields.get(index).cloned().unwrap_or_default()
    }

    fn format_timestamp(&self, dt: &chrono::DateTime<chrono::Utc>) -> String {
        dt.format("%Y%m%d%H%M%S").to_string()
    }

    // Validate message structure
    pub fn validate(&self, message: &HL7Message) -> Result<(), Vec<String>> {
        let mut errors = Vec::new();

        // Check for MSH segment
        if message.segments.is_empty() || message.segments[0].segment_id != "MSH" {
            errors.push("Message must start with MSH segment".to_string());
        }

        // Validate required fields based on message type
        match (message.message_type.as_str(), message.trigger_event.as_str()) {
            ("ADT", "A01") | ("ADT", "A04") => {
                if !message.segments.iter().any(|s| s.segment_id == "PID") {
                    errors.push("ADT message must contain PID segment".to_string());
                }
                if !message.segments.iter().any(|s| s.segment_id == "PV1") {
                    errors.push("ADT message must contain PV1 segment".to_string());
                }
            }
            ("ORM", "O01") => {
                if !message.segments.iter().any(|s| s.segment_id == "ORC") {
                    errors.push("ORM message must contain ORC segment".to_string());
                }
            }
            ("ORU", "R01") => {
                if !message.segments.iter().any(|s| s.segment_id == "OBR") {
                    errors.push("ORU message must contain OBR segment".to_string());
                }
            }
            _ => {}
        }

        if errors.is_empty() {
            Ok(())
        } else {
            Err(errors)
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_adt_message() {
        let mut parser = HL7Parser::new();

        let message = "MSH|^~\\&|SENDING_APP|SENDING_FACILITY|RECEIVING_APP|RECEIVING_FACILITY|20240115120000||ADT^A01|MSG00001|P|2.5\r\n\
                      PID|1||12345^^^MRN||Doe^John^A||19800101|M|||123 Main St^^Springfield^IL^62701||(555)123-4567\r\n\
                      PV1|1|I|ICU^101^1|||||||MED||||||||V1234^^^VN";

        let result = parser.parse(message);
        assert!(result.is_ok());

        let msg = result.unwrap();
        assert_eq!(msg.message_type, "ADT");
        assert_eq!(msg.trigger_event, "A01");
        assert_eq!(msg.segments.len(), 3);

        // Test field extraction
        let patient_id = parser.get_field(&msg, "PID-3");
        assert!(patient_id.is_some());
        assert!(patient_id.unwrap().contains("12345"));
    }

    #[test]
    fn test_generate_message() {
        let parser = HL7Parser::new();

        let message = HL7Message {
            message_type: "ACK".to_string(),
            trigger_event: String::new(),
            segments: vec![
                HL7Segment {
                    segment_id: "MSH".to_string(),
                    fields: vec![
                        "MSH".to_string(),
                        "|".to_string(),
                        "^~\\&".to_string(),
                        "APP".to_string(),
                    ],
                },
            ],
        };

        let output = parser.generate(&message);
        assert!(output.starts_with("MSH|"));
    }

    #[test]
    fn test_validate_message() {
        let parser = HL7Parser::new();

        let valid_message = HL7Message {
            message_type: "ADT".to_string(),
            trigger_event: "A01".to_string(),
            segments: vec![
                HL7Segment { segment_id: "MSH".to_string(), fields: vec![] },
                HL7Segment { segment_id: "PID".to_string(), fields: vec![] },
                HL7Segment { segment_id: "PV1".to_string(), fields: vec![] },
            ],
        };

        assert!(parser.validate(&valid_message).is_ok());

        let invalid_message = HL7Message {
            message_type: "ADT".to_string(),
            trigger_event: "A01".to_string(),
            segments: vec![
                HL7Segment { segment_id: "MSH".to_string(), fields: vec![] },
            ],
        };

        assert!(parser.validate(&invalid_message).is_err());
    }
}
