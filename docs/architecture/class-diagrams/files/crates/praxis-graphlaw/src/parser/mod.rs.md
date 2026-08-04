# `crates/praxis-graphlaw/src/parser/mod.rs`

Source SHA-256: `9fc6884ebce1902144f0d0c821cf662c1cec21c939d142051e74771d1b1cd796`

```mermaid
classDiagram
    class mod_iri_resolve {
      <<mod>>
    }
    class mod_n3_terms {
      <<mod>>
    }
    class mod_n3rule_parser {
      <<mod>>
    }
    class struct_Parser {
      <<struct>>
    }
    class enum_Syntax {
      <<enum>>
    }
    class mod_test {
      <<mod>>
    }
    note "Parser"
```

## Dependencies

- `crate::{BodyLiteral, Rule, Triple, VarOrTerm}`
- `rio_api::parser::{QuadsParser, TriplesParser}`
- `rio_turtle::{NQuadsParser, NTriplesParser, TriGParser, TurtleError, TurtleParser}`
- `rio_xml::RdfXmlParser`
- `super::*`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
