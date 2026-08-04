# `boilerplate/crates/domain/src/ports.rs`

Source SHA-256: `abd85e8327420db9df6522258a325ca2c7281d8beaf0ef8944d2264bc73cdb64`

```mermaid
classDiagram
    class trait_ItemRepository {
      <<trait>>
      +"find_by_id(&self, id: &ItemId) -~ Result~Item~"
      +"list(&self, page: Page) -~ Result~PagedResult~Item~~"
      +"save(&self, item: &Item) -~ Result~()~"
      +"delete(&self, id: &ItemId) -~ Result~()~"
    }
    class trait_ItemService {
      <<trait>>
      +"create_item(&self, name: String, description: Option~String~) -~ Result~Item~"
      +"get_item(&self, id: &ItemId) -~ Result~Item~"
      +"list_items(&self, page: Page) -~ Result~PagedResult~Item~~"
      +"delete_item(&self, id: &ItemId) -~ Result~()~"
    }
```

## Dependencies

- `async_trait::async_trait`
- `bp_core::{ pagination::{Page, PagedResult}, Result, }`
- `crate::entities::{Item, ItemId}`

## Standing

- Structural parse: `ALIVE`
- Runtime behavior: `UNKNOWN`
