# `crates/praxis-graphlaw/src/observer.rs`

Source SHA-256: `f8f3b2b3ad86c2ae5ef02479a07a7b7b21c542f9c7e7c5480f8af7aca79f18a1`

```mermaid
classDiagram
    class trait_IObserver {
      <<trait>>
      +"update(&self, new: Vec~i32~) -~ Vec~i32~"
    }
    class trait_ISubject {
      <<trait>>
      +"attach(&mut self, observer: &'a T)"
      +"detach(&mut self, observer: &'a T)"
      +"notify_observers(&mut self)"
      +"add_data(&mut self, data: i32)"
    }
    class struct_Subject {
      <<struct>>
      +"observers: Vec~&'a T~"
      +"data: Vec~i32~"
    }
    class struct_ConcreteObserver {
      <<struct>>
      +"id: i32"
      +"data: i32"
    }
    class fn_test_observer {
      <<fn>>
    }
    note "ConcreteObserver"
    note "IObserver for ConcreteObserver"
    note "ISubject~"
    note "Subject~"
```

## Dependencies

- None observed.

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
