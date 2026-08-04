# `crates/praxis-graphlaw/tests/self_monitoring_hook_actuation.rs`

Source SHA-256: `817c4d5349ae921cbbd511d4fdfbf39c9613cb6e970f4c813072c61680279fe7`

```mermaid
classDiagram
    class fn_escalation_obligations {
      <<fn>>
    }
    class fn_run_fixture {
      <<fn>>
    }
    class fn_hook_fires_on_repeat_grounding_question_after_survey_only_response {
      <<fn>>
    }
    class fn_hook_does_not_fire_when_prior_response_was_a_run_not_a_survey {
      <<fn>>
    }
    class fn_hook_does_not_fire_when_the_second_grounding_question_is_a_different_topic {
      <<fn>>
    }
```

## Dependencies

- `praxis_graphlaw::TripleStore`
- `praxis_graphlaw::parser::Syntax`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
