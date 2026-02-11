<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Craftplan Entity Quick Reference Catalog](#craftplan-entity-quick-reference-catalog)
  - [Catalog Domain](#catalog-domain)
    - [Product](#product)
    - [BOM (Bill of Materials)](#bom-bill-of-materials)
    - [BOMComponent](#bomcomponent)
    - [LaborStep](#laborstep)
    - [BOMRollup](#bomrollup)
  - [Orders Domain](#orders-domain)
    - [Order](#order)
    - [OrderItem](#orderitem)
    - [ProductionBatch](#productionbatch)
    - [OrderItemBatchAllocation](#orderitembatchallocation)
    - [OrderItemLot](#orderitemlot)
  - [Inventory Domain](#inventory-domain)
    - [Material](#material)
    - [Lot](#lot)
    - [Movement](#movement)
    - [Allergen](#allergen)
    - [NutritionalFact](#nutritionalfact)
    - [MaterialAllergen (Join)](#materialallergen-join)
    - [MaterialNutritionalFact (Join)](#materialnutritionalfact-join)
    - [Supplier](#supplier)
    - [PurchaseOrder](#purchaseorder)
    - [PurchaseOrderItem](#purchaseorderitem)
  - [CRM Domain](#crm-domain)
    - [Customer](#customer)
  - [Accounts Domain](#accounts-domain)
    - [User](#user)
    - [ApiKey](#apikey)
    - [Token](#token)
  - [Summary by Domain](#summary-by-domain)
    - [Catalog (6 entities)](#catalog-6-entities)
    - [Orders (5 entities)](#orders-5-entities)
    - [Inventory (10 entities)](#inventory-10-entities)
    - [CRM (1 entity)](#crm-1-entity)
    - [Accounts (3 entities)](#accounts-3-entities)
    - [Settings (not detailed)](#settings-not-detailed)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Craftplan Entity Quick Reference Catalog

**Generated**: 2026-02-04
**Purpose**: Quick lookup of all entities, attributes, and relationships

---

## Catalog Domain

### Product
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| name | string | required, min:2, max:100 |
| sku | string | required, unique, min:2, max:50 |
| status | enum | draft, testing, active, paused, discontinued, archived |
| price | decimal | required |
| photos | array(string) | default [] |
| featured_photo | string | optional |
| selling_availability | atom | available, preorder, off |
| max_daily_quantity | integer | min:0 |

**Relationships**:
- has_many :boms → BOM
- has_one :active_bom → BOM (where status=:active)
- has_many :items → OrderItem

**Calculations**: materials_cost, bom_unit_cost, markup_percentage, gross_profit, allergens, nutritional_facts

---

### BOM (Bill of Materials)
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| product_id | uuid | FK required |
| name | string | optional |
| notes | string | optional |
| status | atom | draft, active, archived |
| version | integer | auto-increment |
| published_at | utc_datetime | optional |

**Relationships**:
- belongs_to :product → Product
- has_many :components → BOMComponent
- has_many :labor_steps → LaborStep
- has_one :rollup → BOMRollup

**Constraints**: One active BOM per product

---

### BOMComponent
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| bom_id | uuid | FK required |
| component_type | atom | material, product |
| quantity | decimal | min:0 |
| position | integer | min:0 |
| waste_percent | decimal | min:0 |
| notes | string | optional |
| material_id | uuid | FK optional |
| product_id | uuid | FK optional |

**Relationships**:
- belongs_to :bom → BOM
- belongs_to :material → Material (if type=material)
- belongs_to :product → Product (if type=product)

**Validation**: Exactly one of material_id or product_id

---

### LaborStep
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| bom_id | uuid | FK required |
| name | string | required, min:1 |
| sequence | integer | min:0 |
| duration_minutes | decimal | min:0 |
| rate_override | decimal | optional |
| units_per_run | decimal | min:1 |
| notes | string | optional |

**Relationships**:
- belongs_to :bom → BOM

---

### BOMRollup
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| bom_id | uuid | FK required, unique |
| product_id | uuid | FK required |
| material_cost | decimal | default 0 |
| labor_cost | decimal | default 0 |
| overhead_cost | decimal | default 0 |
| unit_cost | decimal | default 0 |
| components_map | map | JSONB {} |

**Relationships**:
- belongs_to :bom → BOM
- belongs_to :product → Product

---

## Orders Domain

### Order
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| reference | string | auto, pattern OR_YYYY_MM_DD_XXXXXX |
| customer_id | uuid | FK required |
| status | enum | unconfirmed, confirmed, in_progress, ready, delivered, completed, cancelled |
| payment_status | enum | pending, partial, paid, refunded |
| delivery_date | utc_datetime | required |
| currency | atom | USD |
| invoice_number | string | optional |
| invoice_status | atom | none, issued, paid |
| invoiced_at | utc_datetime | optional |
| paid_at | utc_datetime | optional |
| payment_method | atom | cash, card, bank_transfer, other |
| discount_type | atom | none, percent, fixed |
| discount_value | decimal | default 0 |
| delivery_method | atom | pickup, delivery |
| subtotal | decimal | default 0 |
| tax_total | decimal | default 0 |
| shipping_total | decimal | default 0 |
| discount_total | decimal | default 0 |
| total | decimal | default 0 |

**Relationships**:
- belongs_to :customer → Customer
- has_many :items → OrderItem

**Aggregates**: total_items, total_cost

---

### OrderItem
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| order_id | uuid | FK required |
| product_id | uuid | FK required |
| bom_id | uuid | FK optional |
| production_batch_id | uuid | FK optional |
| quantity | decimal | required |
| unit_price | decimal | required |
| status | enum | todo, in_progress, done |
| consumed_at | utc_datetime | optional |
| batch_code | string | optional |
| material_cost | decimal | default 0 |
| labor_cost | decimal | default 0 |
| overhead_cost | decimal | default 0 |
| unit_cost | decimal | default 0 |

**Relationships**:
- belongs_to :order → Order
- belongs_to :product → Product
- belongs_to :bom → BOM
- belongs_to :production_batch → ProductionBatch
- has_many :order_item_lots → OrderItemLot
- has_many :allocations → OrderItemBatchAllocation

**Calculations**: cost = quantity * unit_price
**Aggregates**: planned_qty_sum, completed_qty_sum

---

### ProductionBatch
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| batch_code | string | unique, required |
| product_id | uuid | FK required |
| bom_id | uuid | FK optional |
| planned_qty | decimal | min:0 |
| produced_qty | decimal | min:0 |
| scrap_qty | decimal | min:0 |
| status | atom | open, in_progress, completed, canceled |
| notes | string | optional |
| bom_version | integer | optional |
| components_map | map | JSONB {} |
| started_at | utc_datetime | optional |
| completed_at | utc_datetime | optional |

**Relationships**:
- belongs_to :product → Product
- belongs_to :bom → BOM
- has_many :order_items → OrderItem
- has_many :allocations → OrderItemBatchAllocation
- has_many :batch_lots → ProductionBatchLot

---

### OrderItemBatchAllocation
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| production_batch_id | uuid | FK required |
| order_item_id | uuid | FK required |
| planned_qty | decimal | min:0 |
| completed_qty | decimal | min:0 |

**Relationships**:
- belongs_to :production_batch → ProductionBatch
- belongs_to :order_item → OrderItem

**Constraints**:
- Unique pair (batch_id, item_id)
- completed_qty <= planned_qty
- Sum planned_qty per item <= item.quantity

---

### OrderItemLot
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| order_item_id | uuid | FK required |
| lot_id | uuid | FK required |
| quantity | decimal | required |

**Relationships**:
- belongs_to :order_item → OrderItem
- belongs_to :lot → Lot

---

## Inventory Domain

### Material
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| name | string | required, min:2, max:50 |
| sku | string | required, unique, min:2, max:50 |
| unit | atom | Unit enum (kg, g, lb, oz, l, ml, gal, each, etc.) |
| price | decimal | required |
| minimum_stock | decimal | min:0 |
| maximum_stock | decimal | min:0 |

**Relationships**:
- has_many :movements → Movement
- has_many :material_allergens → MaterialAllergen
- has_many :material_nutritional_facts → MaterialNutritionalFact
- many_to_many :allergens → Allergen
- many_to_many :nutritional_facts → NutritionalFact

**Aggregates**: current_stock (sum of movements)

---

### Lot
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| lot_code | string | unique, required |
| material_id | uuid | FK required |
| supplier_id | uuid | FK optional |
| expiry_date | date | optional |
| received_at | utc_datetime | optional |

**Relationships**:
- belongs_to :material → Material
- belongs_to :supplier → Supplier
- has_many :movements → Movement

**Aggregates**: current_stock (sum of movements)

---

### Movement
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| material_id | uuid | FK required |
| lot_id | uuid | FK optional |
| quantity | decimal | required |
| reason | string | max:255 |
| occurred_at | utc_datetime | required |

**Relationships**:
- belongs_to :material → Material
- belongs_to :lot → Lot

**Note**: Positive = receive, Negative = consume

---

### Allergen
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| name | string | unique, required |

**Relationships**:
- many_to_many :materials → Material

---

### NutritionalFact
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| name | string | unique, required |

**Relationships**:
- many_to_many :materials → Material

---

### MaterialAllergen (Join)
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| material_id | uuid | FK required |
| allergen_id | uuid | FK required |

**Relationships**:
- belongs_to :material → Material
- belongs_to :allergen → Allergen

---

### MaterialNutritionalFact (Join)
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| material_id | uuid | FK required |
| nutritional_fact_id | uuid | FK required |

**Relationships**:
- belongs_to :material → Material
- belongs_to :nutritional_fact → NutritionalFact

---

### Supplier
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| name | string | required |
| contact_name | string | optional |
| contact_email | string | optional |
| contact_phone | string | optional |
| notes | string | max:2000 |

**Relationships**:
- has_many :lots → Lot
- has_many :purchase_orders → PurchaseOrder

---

### PurchaseOrder
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| reference | string | auto, pattern PO_YYYY_MM_DD_XXXXXX |
| supplier_id | uuid | FK required |
| status | enum | draft, ordered, received |
| ordered_at | utc_datetime | optional |
| received_at | utc_datetime | optional |

**Relationships**:
- belongs_to :supplier → Supplier
- has_many :items → PurchaseOrderItem

---

### PurchaseOrderItem
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| purchase_order_id | uuid | FK required |
| material_id | uuid | FK required |
| quantity | decimal | required |

**Relationships**:
- belongs_to :purchase_order → PurchaseOrder
- belongs_to :material → Material

---

## CRM Domain

### Customer
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| reference | string | auto, pattern CUS_XXXXXXXXXXXX |
| type | atom | individual, company |
| first_name | string | required, min:1 |
| last_name | string | required, min:1 |
| email | string | regex:@, unique, optional |
| phone | string | max:15, unique, optional |
| billing_address | Address (embedded) | optional |
| shipping_address | Address (embedded) | optional |

**Address Type**: line1, line2, city, state, postal_code, country

**Relationships**:
- has_many :orders → Order

**Calculations**: full_name = first_name + " " + last_name
**Aggregates**: total_orders, total_orders_value

---

## Accounts Domain

### User
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| email | ci_string | unique, required |
| hashed_password | string | sensitive, required |
| role | enum | customer, staff, admin |

**Actions**: register, sign_in, password_reset

---

### ApiKey
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| name | string | required |
| key | string | auto-generated |
| scopes | array(atom) | [] |

---

### Token
| Attribute | Type | Constraints |
|-----------|------|-------------|
| id | uuid | PK, auto |
| user_id | uuid | FK required |
| token | string | required |
| context | string | required |
| expires_at | utc_datetime | optional |

**Relationships**:
- belongs_to :user → User

---

## Summary by Domain

### Catalog (6 entities)
- Product, BOM, BOMComponent, LaborStep, BOMRollup, ProductStatus (enum)

### Orders (5 entities)
- Order, OrderItem, ProductionBatch, OrderItemBatchAllocation, OrderItemLot

### Inventory (10 entities)
- Material, Lot, Movement, Allergen, NutritionalFact, MaterialAllergen, MaterialNutritionalFact, Supplier, PurchaseOrder, PurchaseOrderItem

### CRM (1 entity)
- Customer

### Accounts (3 entities)
- User, ApiKey, Token

### Settings (not detailed)
- Application settings

**Total**: 25 core entities + enums + settings

---

**END OF CATALOG**
