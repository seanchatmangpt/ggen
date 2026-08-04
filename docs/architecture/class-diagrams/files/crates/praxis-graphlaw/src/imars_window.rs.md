# `crates/praxis-graphlaw/src/imars_window.rs`

Source SHA-256: `413444fb5a9a7ec02dda965a3c94165109c8f2e1e8bb9e0480042ef96af41467`

```mermaid
classDiagram
    class trait_WindowConsumer {
      <<trait>>
      +"update(&mut self, new: Vec~(i32, Rc~T~)"
    }
    class struct_SimpleWindowConsumer {
      <<struct>>
      +"windows: Vec~Box~ImarsWindow~T~~~"
      +"new: Vec~(i32"
      +"old: Vec~(i32"
    }
    class struct_ImarsWindow {
      <<struct>>
      +"content: LinkedList~(i32"
      +"consumers: Vec~Rc~RefCell~dyn WindowConsumer~T~~~~"
      +"width: i32"
      +"slide: i32"
      +"time: i32"
      +"pending_adds: Vec~(i32"
      +"index: HashMap~Rc~T~"
    }
    class mod_imars_window_test {
      <<mod>>
    }
    note "Default for SimpleWindowConsumer~T~"
    note "ImarsWindow~T~"
    note "SimpleWindowConsumer~T~"
    note "WindowConsumer~T~ for SimpleWindowConsumer~T~"
```

## Dependencies

- `deepmesa_collections::LinkedList`
- `deepmesa_collections::linkedlist::NodeHandle as Node`
- `std::cell::RefCell`
- `std::cmp`
- `std::collections::HashMap`
- `std::hash::Hash`
- `std::rc::Rc`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
