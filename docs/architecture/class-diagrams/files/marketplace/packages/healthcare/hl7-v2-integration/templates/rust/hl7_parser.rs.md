# `marketplace/packages/healthcare/hl7-v2-integration/templates/rust/hl7_parser.rs`

Source SHA-256: `4480b70e3af5bc36b4445b3d98b1bdeb34a515e744ff5460b861ae171d3274d5`

```mermaid
classDiagram
    class struct_HL7Message {
      <<struct>>
      +"message_type: String"
      +"trigger_event: String"
      +"segments: Vec~HL7Segment~"
    }
    class struct_HL7Segment {
      <<struct>>
      +"segment_id: String"
      +"fields: Vec~String~"
    }
    class struct_HL7Parser {
      <<struct>>
      +"field_separator: char"
      +"component_separator: char"
      +"repetition_separator: char"
      +"escape_character: char"
      +"subcomponent_separator: char"
    }
    class mod_tests {
      <<mod>>
    }
    note "Default for HL7Parser"
    note "HL7Parser"
```

## Dependencies

- `std::collections::HashMap`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
