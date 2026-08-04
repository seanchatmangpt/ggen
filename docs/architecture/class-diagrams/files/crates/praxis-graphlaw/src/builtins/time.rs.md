# `crates/praxis-graphlaw/src/builtins/time.rs`

Source SHA-256: `008be9cd8ac4c8b10d4ea3011ebe866d866329301d1879297505691a8717e700`

```mermaid
classDiagram
    class struct_DateTimeParts {
      <<struct>>
      +"year: i64"
      +"month: i64"
      +"day: i64"
      +"hour: i64"
      +"minute: i64"
      +"second: f64"
      +"tz_str: String"
      +"tz_offset_secs: i64"
    }
    class fn_parse_datetime {
      <<fn>>
    }
    class fn_days_from_civil {
      <<fn>>
    }
    class fn_epoch_seconds {
      <<fn>>
    }
    class fn_day_of_week {
      <<fn>>
    }
    class fn_eval_component {
      <<fn>>
    }
    class fn_eval_year {
      <<fn>>
    }
    class fn_eval_month {
      <<fn>>
    }
    class fn_eval_day {
      <<fn>>
    }
    class fn_eval_hour {
      <<fn>>
    }
    class fn_eval_minute {
      <<fn>>
    }
    class fn_eval_second {
      <<fn>>
    }
    class fn_eval_day_of_week {
      <<fn>>
    }
    class fn_eval_in_seconds {
      <<fn>>
    }
    class fn_eval_time_zone {
      <<fn>>
    }
    class fn_eval_local_time {
      <<fn>>
    }
```

## Dependencies

- `crate::{Binding, Triple}`
- `super::{eval_functional, intern_number, intern_string, lexical_value, resolve_operand}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
