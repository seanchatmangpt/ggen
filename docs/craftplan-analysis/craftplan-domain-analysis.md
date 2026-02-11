<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Craftplan Domain Analysis for RDF-Driven Code Generation](#craftplan-domain-analysis-for-rdf-driven-code-generation)
  - [Executive Summary](#executive-summary)
    - [Technology Stack](#technology-stack)
    - [Domain Patterns](#domain-patterns)
  - [Domain Module Catalog](#domain-module-catalog)
    - [1. Catalog Domain (`Craftplan.Catalog`)](#1-catalog-domain-craftplancatalog)
      - [Entities](#entities)
        - [1.1 Product](#11-product)
        - [1.2 BOM (Bill of Materials)](#12-bom-bill-of-materials)
        - [1.3 BOMComponent](#13-bomcomponent)
        - [1.4 LaborStep](#14-laborstep)
        - [1.5 BOMRollup (Cached Cost Calculation)](#15-bomrollup-cached-cost-calculation)
      - [Domain Services](#domain-services)
        - [BatchCostCalculator](#batchcostcalculator)
    - [2. Orders Domain (`Craftplan.Orders`)](#2-orders-domain-craftplanorders)
      - [Entities](#entities-1)
        - [2.1 Order](#21-order)
        - [2.2 OrderItem](#22-orderitem)
        - [2.3 ProductionBatch](#23-productionbatch)
        - [2.4 OrderItemBatchAllocation](#24-orderitembatchallocation)
        - [2.5 OrderItemLot (Material Consumption)](#25-orderitemlot-material-consumption)
      - [Business Logic: Batch Consumption Workflow](#business-logic-batch-consumption-workflow)
      - [Business Logic: Batch Completion](#business-logic-batch-completion)
    - [3. Inventory Domain (`Craftplan.Inventory`)](#3-inventory-domain-craftplaninventory)
      - [Entities](#entities-2)
        - [3.1 Material](#31-material)
        - [3.2 Lot (Inventory Lot)](#32-lot-inventory-lot)
        - [3.3 Movement (Stock Transaction)](#33-movement-stock-transaction)
        - [3.4 Allergen](#34-allergen)
        - [3.5 NutritionalFact](#35-nutritionalfact)
        - [3.6 MaterialAllergen (Join Table)](#36-materialallergen-join-table)
        - [3.7 MaterialNutritionalFact (Join Table)](#37-materialnutritionalfact-join-table)
        - [3.8 Supplier](#38-supplier)
        - [3.9 PurchaseOrder](#39-purchaseorder)
        - [3.10 PurchaseOrderItem](#310-purchaseorderitem)
      - [Inventory Forecasting](#inventory-forecasting)
    - [4. CRM Domain (`Craftplan.CRM`)](#4-crm-domain-craftplancrm)
      - [Entities](#entities-3)
        - [4.1 Customer](#41-customer)
        - [4.2 Address (Embedded Type)](#42-address-embedded-type)
    - [5. Accounts Domain (`Craftplan.Accounts`)](#5-accounts-domain-craftplanaccounts)
      - [Entities](#entities-4)
        - [5.1 User](#51-user)
        - [5.2 ApiKey](#52-apikey)
        - [5.3 Token](#53-token)
    - [6. Types Module](#6-types-module)
        - [6.1 Currency](#61-currency)
        - [6.2 Unit](#62-unit)
        - [6.3 EncryptedBinary](#63-encryptedbinary)
  - [Relationship Mapping (ER Diagrams)](#relationship-mapping-er-diagrams)
    - [Catalog Domain](#catalog-domain)
    - [Orders Domain](#orders-domain)
    - [Inventory Domain](#inventory-domain)
    - [CRM Domain](#crm-domain)
  - [Business Rules Catalog](#business-rules-catalog)
    - [BOM Rules](#bom-rules)
    - [Order Rules](#order-rules)
    - [Production Batch Rules](#production-batch-rules)
    - [Inventory Rules](#inventory-rules)
    - [Customer Rules](#customer-rules)
  - [API Contract Specifications](#api-contract-specifications)
    - [JSON:API Routes](#jsonapi-routes)
      - [Catalog](#catalog)
      - [Orders](#orders)
      - [Inventory](#inventory)
      - [CRM](#crm)
    - [GraphQL Schema](#graphql-schema)
  - [Code Examples for Key Patterns](#code-examples-for-key-patterns)
    - [Pattern 1: Ash Resource with Attributes, Relationships, Actions](#pattern-1-ash-resource-with-attributes-relationships-actions)
    - [Pattern 2: Custom Change Module](#pattern-2-custom-change-module)
    - [Pattern 3: Recursive BOM Rollup](#pattern-3-recursive-bom-rollup)
    - [Pattern 4: Inventory Forecasting](#pattern-4-inventory-forecasting)
  - [Template Requirements for Code Generation](#template-requirements-for-code-generation)
    - [Required Templates for ggen μ Pipeline](#required-templates-for-ggen-%CE%BC-pipeline)
      - [1. Ash Resource Template](#1-ash-resource-template)
      - [2. Ash Domain Template](#2-ash-domain-template)
      - [3. PostgreSQL Table Template](#3-postgresql-table-template)
      - [4. Changes Module Template](#4-changes-module-template)
      - [5. Calculation Module Template](#5-calculation-module-template)
      - [6. Enum Type Template](#6-enum-type-template)
      - [7. Embedded Type Template](#7-embedded-type-template)
  - [Summary Statistics](#summary-statistics)
    - [Domain Metrics](#domain-metrics)
    - [Code Metrics](#code-metrics)
    - [RDF Mapping Estimates](#rdf-mapping-estimates)
  - [Next Steps for RDF Specification](#next-steps-for-rdf-specification)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Craftplan Domain Analysis for RDF-Driven Code Generation

**Generated**: 2026-02-04
**Source**: `/Users/sac/ggen/vendors/craftplan/`
**Purpose**: Extract complete domain knowledge for ggen RDF specification generation

---

## Executive Summary

Craftplan is an **Ash Framework-based ERP system** for small-scale manufacturers built with Elixir, Phoenix LiveView, and PostgreSQL. The system manages:

1. **Product Catalog** - Products, BOMs (Bill of Materials), cost calculations
2. **Order Management** - Orders, order items, production batching, allocations
3. **Inventory** - Materials, lots, movements, forecasting, allergens/nutrition
4. **CRM** - Customers, suppliers, addresses
5. **Production** - Batch execution, material consumption, completion tracking
6. **Accounts** - Users, authentication, roles, API keys

### Technology Stack
- **Language**: Elixir 1.x
- **Framework**: Phoenix (web), Ash (domain), AshPostgres (persistence)
- **Database**: PostgreSQL 16
- **API**: JSON:API + GraphQL (via Ash extensions)
- **Auth**: AshAuthentication (JWT tokens)

### Domain Patterns
- **Ash Resources** as domain entities with declarative attributes, relationships, actions
- **Ash Changes** for business logic transformations
- **Ash Validations** for constraint enforcement
- **Ash Calculations** for derived attributes
- **Ash Aggregates** for roll-up data

---

## Domain Module Catalog

### 1. Catalog Domain (`Craftplan.Catalog`)

**Purpose**: Manage product definitions, recipes (BOMs), and cost calculations

#### Entities

##### 1.1 Product
**Table**: `catalog_products`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| name | string | min:2, max:100, regex: `^[\w\s\-\.]+$` | required | Product name |
| sku | string | unique, required | | SKU identifier |
| status | enum | draft, testing, active, paused, discontinued, archived | draft | Lifecycle status |
| price | decimal | required | 0 | Selling price |
| photos | array(string) | | [] | Photo URLs |
| featured_photo | string | optional | | Featured photo ID |
| selling_availability | atom | available, preorder, off | available | Customer-facing availability |
| max_daily_quantity | integer | min:0 | 0 | Daily capacity (0=unlimited) |

**Relationships**:
- `has_many :boms` - All BOM versions
- `has_one :active_bom` - Currently active BOM (status=:active)
- `has_many :items` - Order items referencing this product

**Calculations** (derived):
- `materials_cost` - Material cost from active BOM
- `bom_unit_cost` - Total unit cost (materials + labor + overhead)
- `markup_percentage` - (price - cost) / cost
- `gross_profit` - price - unit_cost
- `allergens` - Vector of allergen names
- `nutritional_facts` - Vector of nutrition data

**Identities**:
- `sku` (unique)
- `name` (unique)

**Actions**:
- `create` - Create product
- `update` - Update product
- `read` - Single product lookup
- `list` - List with status filter
- `keyset` - Keyset pagination
- `destroy` - Delete product

**Policies**:
- Public read for active/available products
- Staff/admin read all
- Staff/admin write operations

---

##### 1.2 BOM (Bill of Materials)
**Table**: `catalog_boms`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| name | string | optional | | BOM name |
| notes | string | optional | | Notes |
| status | atom | draft, active, archived | draft | Status |
| version | integer | auto-increment | | Auto-assigned version |
| published_at | utc_datetime | optional | | When promoted to active |
| product_id | uuid | FK required | | Parent product |

**Relationships**:
- `belongs_to :product` - Parent product
- `has_many :components` - BOMComponent records
- `has_many :labor_steps` - LaborStep records
- `has_one :rollup` - BOMRollup (cached costs)

**Identities**:
- `product_version` (product_id, version) - unique per product

**Constraints**:
- Only ONE active BOM per product (via partial unique index)

**Actions**:
- `create` - Create BOM with components and labor_steps
- `update` - Update BOM (auto-refreshes rollup)
- `promote` - Set status=:active, publish timestamp
- `read` - Single BOM
- `list_for_product` - List by product (sorted version DESC)
- `get_active` - Get active BOM for product
- `destroy` - Delete BOM

**Business Rules**:
- Version auto-incremented on create
- BOMRollup refreshed after any component/labor change
- Active BOM cannot have another BOM promoted (DB constraint)

---

##### 1.3 BOMComponent
**Table**: `catalog_bom_components`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| bom_id | uuid | FK required | | Parent BOM |
| component_type | atom | material, product | material | Type of component |
| quantity | decimal | min:0 | 0 | Quantity per parent unit |
| position | integer | min:0 | 0 | Sort position |
| waste_percent | decimal | min:0 | 0 | Waste allowance % |
| notes | string | optional | | Notes |
| material_id | uuid | FK optional | | Material reference (if type=material) |
| product_id | uuid | FK optional | | Product reference (if type=product) |

**Relationships**:
- `belongs_to :bom` - Parent BOM
- `belongs_to :material` - Material (if component_type=:material)
- `belongs_to :product` - Nested product (if component_type=:product)

**Validation**:
- Exactly ONE of material_id or product_id must be set
- (enforced by `ValidateComponentTarget` change)

**Actions**:
- `create` - Add component (auto-refreshes BOM rollup)
- `update` - Update component (auto-refreshes BOM rollup)
- `read` - Single component
- `destroy` - Delete component

---

##### 1.4 LaborStep
**Table**: `catalog_labor_steps`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| bom_id | uuid | FK required | | Parent BOM |
| name | string | min:1, required | | Step name |
| sequence | integer | min:0 | 0 | Execution order |
| duration_minutes | decimal | min:0 | 0 | Time required |
| rate_override | decimal | optional | | Override default labor rate |
| units_per_run | decimal | min:1 | 1 | Units produced per run |
| notes | string | optional | | Notes |

**Relationships**:
- `belongs_to :bom` - Parent BOM

**Actions**:
- `create` - Add step (auto-refreshes BOM rollup)
- `update` - Update step (auto-refreshes BOM rollup)
- `read` - Single step
- `destroy` - Delete step

---

##### 1.5 BOMRollup (Cached Cost Calculation)
**Table**: `catalog_bom_rollups`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| bom_id | uuid | FK required, unique | | Parent BOM |
| product_id | uuid | FK required | | Denormalized product |
| material_cost | decimal | | 0 | Material cost per unit |
| labor_cost | decimal | | 0 | Labor cost per unit |
| overhead_cost | decimal | | 0 | Overhead cost per unit |
| unit_cost | decimal | | 0 | Total cost (sum of above) |
| components_map | map | JSONB | {} | Flattened materials (material_id => qty) |

**Relationships**:
- `belongs_to :bom` - Parent BOM (unique)
- `belongs_to :product` - Product (denormalized for queries)

**Identities**:
- `unique_bom` (bom_id) - One rollup per BOM

**Calculation Logic** (`BOMRollup.refresh!/1`):
```elixir
# Material cost = sum(components.quantity * material.price * (1 + waste/100))
# Labor cost = sum(labor_steps.duration * labor_rate / units_per_run)
# Overhead cost = (material + labor) * overhead_rate
# Unit cost = material + labor + overhead

# components_map: Recursive flatten of all materials
# - Handles nested product references (detects cycles)
# - Aggregates quantities by material_id
# - Stores as strings for JSONB compatibility
```

**Actions**:
- `create` - Initial rollup (internal)
- `update` - Refresh rollup (internal)
- `read` - Single rollup

---

#### Domain Services

##### BatchCostCalculator
**Purpose**: Calculate BOM costs for a given batch quantity

**Algorithm**:
1. Load BOM with components (materials, nested products) and labor_steps
2. Flatten components recursively (cycle detection via visited set)
3. Calculate material cost with waste allowance
4. Calculate labor cost with duration and rates
5. Apply overhead percentage
6. Return cost breakdown

---

### 2. Orders Domain (`Craftplan.Orders`)

**Purpose**: Manage customer orders, production batching, material allocation, consumption

#### Entities

##### 2.1 Order
**Table**: `orders_orders`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| reference | string | pattern `^OR_\d{4}_\d{2}_\d{2}_[A-Z]{6}$` | auto | Order number |
| customer_id | uuid | FK required | | Customer |
| status | enum | unconfirmed, confirmed, in_progress, ready, delivered, completed, cancelled | unconfirmed | Order status |
| payment_status | enum | pending, partial, paid, refunded | pending | Payment status |
| delivery_date | utc_datetime | required | | When due |
| currency | atom | USD (Currency type) | USD | Currency |
| invoice_number | string | optional | | Invoice ref |
| invoice_status | atom | none, issued, paid | none | Invoice state |
| invoiced_at | utc_datetime | optional | | When issued |
| paid_at | utc_datetime | optional | | When paid |
| payment_method | atom | cash, card, bank_transfer, other | optional | Payment type |
| discount_type | atom | none, percent, fixed | none | Discount type |
| discount_value | decimal | | 0 | Discount amount |
| delivery_method | atom | pickup, delivery | delivery | Delivery type |
| **Monetary Totals** | | | | |
| subtotal | decimal | | 0 | Items sum |
| tax_total | decimal | | 0 | Tax |
| shipping_total | decimal | | 0 | Shipping |
| discount_total | decimal | | 0 | Discount |
| total | decimal | | 0 | Final total |

**Relationships**:
- `belongs_to :customer` - CRM.Customer
- `has_many :items` - OrderItem records

**Aggregates**:
- `total_items` - Count of order items
- `total_cost` - Sum of item costs

**Identities**:
- `reference` (unique)

**Actions**:
- `create` - Create order with items (auto-calculates totals)
- `update` - Update order (recalculates totals)
- `read` - Single order
- `list` - List with filters (status, payment_status, date range, customer_name, product_id)
- `for_day` - Range query for capacity checks
- `for_forecast` - Load with BOM data for inventory forecasting
- `keyset` - Keyset pagination
- `destroy` - Delete order

**Calculations** (`CalculateTotals` change):
```elixir
subtotal = sum(items.quantity * items.unit_price)
discount = case discount_type do
  :percent -> subtotal * discount_value / 100
  :fixed -> discount_value
  :none -> 0
end
total = subtotal - discount + tax_total + shipping_total
```

**Validations** (`ValidateConstraints` change):
- At least one order item required
- All items must reference valid products
- Delivery date must be in future (on create)

---

##### 2.2 OrderItem
**Table**: `orders_items`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| order_id | uuid | FK required | | Parent order |
| product_id | uuid | FK required | | Product |
| bom_id | uuid | FK optional | | BOM snapshot |
| production_batch_id | uuid | FK optional | | Assigned batch |
| quantity | decimal | required | | Quantity |
| unit_price | decimal | required | | Selling price |
| status | enum | todo, in_progress, done | todo | Status |
| consumed_at | utc_datetime | optional | | When consumed |
| batch_code | string | optional | | Batch ref from completion |
| **Cost Allocations** | | | | (set on batch completion) |
| material_cost | decimal | | 0 | Material allocation |
| labor_cost | decimal | | 0 | Labor allocation |
| overhead_cost | decimal | | 0 | Overhead allocation |
| unit_cost | decimal | | 0 | Total per-unit cost |

**Relationships**:
- `belongs_to :order` - Parent order
- `belongs_to :product` - Product
- `belongs_to :bom` - BOM snapshot (optional)
- `belongs_to :production_batch` - Assigned batch (optional)
- `has_many :order_item_lots` - Lot consumption records
- `has_many :allocations` - Batch allocations

**Calculations**:
- `cost = quantity * unit_price`

**Aggregates**:
- `planned_qty_sum` - Sum of allocation.planned_qty
- `completed_qty_sum` - Sum of allocation.completed_qty

**Actions**:
- `create` - Add item (auto-assign batch code if applicable)
- `update` - Update item
- `read` - Single item
- `in_range` - Items within date range (for capacity checks)
- `plan_pending` - Uncompleted items up to date
- `destroy` - Delete item

---

##### 2.3 ProductionBatch
**Table**: `orders_production_batches`

**Purpose**: Group order items for production, track material consumption, capture actual costs

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| batch_code | string | unique, required | | Batch identifier |
| product_id | uuid | FK required | | Product being produced |
| bom_id | uuid | FK optional | | BOM snapshot |
| **Quantities** | | | | |
| planned_qty | decimal | min:0 | 0 | Plan to produce |
| produced_qty | decimal | min:0 | 0 | Actually produced |
| scrap_qty | decimal | min:0 | 0 | Scrap/waste |
| status | atom | open, in_progress, completed, canceled | open | Batch status |
| notes | string | optional | | Notes |
| **Snapshots** | | | | |
| bom_version | integer | optional | | BOM version snapshot |
| components_map | map | JSONB | {} | Material requirements |
| **Timestamps** | | | | |
| started_at | utc_datetime | optional | | When started |
| completed_at | utc_datetime | optional | | When completed |

**Relationships**:
- `belongs_to :product` - Product being produced
- `belongs_to :bom` - BOM snapshot
- `has_many :order_items` - Items allocated to this batch
- `has_many :allocations` - OrderItemBatchAllocation records
- `has_many :batch_lots` - ProductionBatchLot (material consumption)

**Identities**:
- `batch_code` (unique)

**Actions**:
- `open` - Create batch (auto-generate batch_code, snapshot BOM)
- `open_with_allocations` - Create with initial allocations
- `start` - Set status=:in_progress, record started_at
- `consume` - Consume materials from lots (argument: lot_plan)
- `complete` - Set status=:completed, allocate costs to items
- `read` - Single batch
- `list` - List with status/product filter
- `open_for_product` - Find open batch for product
- `by_code` - Lookup by batch_code
- `detail` - Load batch with all relationships
- `plan` - List active batches (open, in_progress, completed)
- `recent` - Recent batches
- `destroy` - Delete batch

**Batch Code Format**: `BATCH-{YYYYMMDD}-{UUID_SHORT}`

---

##### 2.4 OrderItemBatchAllocation
**Table**: `orders_item_batch_allocations`

**Purpose**: Allocate order items to production batches with planned/completed quantities

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| production_batch_id | uuid | FK required | | Batch |
| order_item_id | uuid | FK required | | Order item |
| planned_qty | decimal | min:0 | 0 | Plan to produce |
| completed_qty | decimal | min:0 | 0 | Actually produced |

**Relationships**:
- `belongs_to :production_batch` - Parent batch
- `belongs_to :order_item` - Parent item

**Constraints**:
- Unique pair (production_batch_id, order_item_id)
- completed_qty <= planned_qty
- Sum of planned_qty across allocations for an item <= item.quantity
- (enforced by validations)

**Actions**:
- `create` - Create allocation
- `update` - Update quantities
- `read` - Single allocation
- `for_batch` - Load allocations for a batch
- `destroy` - Delete allocation

**Validations**:
- `AllocationProductMatch` - batch.product_id == item.product_id
- `AllocationWithinItemQuantity` - Total planned <= item.quantity

---

##### 2.5 OrderItemLot (Material Consumption)
**Table**: `orders_order_item_lots`

**Purpose**: Track which lots were consumed for which order items

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| order_item_id | uuid | FK required | | Order item |
| lot_id | uuid | FK required | | Lot consumed |
| quantity | decimal | required | | Quantity consumed |

**Relationships**:
- `belongs_to :order_item` - Parent item
- `belongs_to :lot` - Lot consumed

**Actions**:
- `create` - Record consumption
- `read` - Single record
- `destroy` - Delete record

---

#### Business Logic: Batch Consumption Workflow

**Service**: `Craftplan.Production.Batching.consume_batch/3`

**Algorithm**:
1. Validate batch status is :open or :in_progress
2. Validate lot_plan quantities against batch requirements
3. For each lot in lot_plan:
   - Create `OrderItemLot` records
   - Create inventory `Movement` records (quantity negative)
   - Update lot current_stock aggregate
4. Return `{:ok, consumption_report}`

**Lot Plan Format**:
```elixir
%{
  "material_id_1" => [
    %{lot_id: "lot_uuid_1", quantity: 10.5},
    %{lot_id: "lot_uuid_2", quantity: 5.0}
  ],
  "material_id_2" => [
    %{lot_id: "lot_uuid_3", quantity: 20.0}
  ]
}
```

---

#### Business Logic: Batch Completion

**Change**: `Craftplan.Orders.Changes.BatchComplete`

**Algorithm**:
1. Validate batch status is :in_progress
2. Calculate actual per-unit cost from consumed materials
3. Allocate costs to order items proportional to completed_qty
4. Update order items:
   - Set material_cost, labor_cost, overhead_cost, unit_cost
   - Set consumed_at timestamp
   - Set batch_code reference
5. Set batch.status = :completed
6. Set batch.completed_at timestamp

**Cost Allocation Formula**:
```elixir
total_completed = sum(allocations.completed_qty)
item_ratio = item_allocation.completed_qty / total_completed

item.material_cost = batch.material_cost * item_ratio
item.labor_cost = batch.labor_cost * item_ratio
item.overhead_cost = batch.overhead_cost * item_ratio
item.unit_cost = material + labor + overhead
```

---

### 3. Inventory Domain (`Craftplan.Inventory`)

**Purpose**: Manage materials, lots, stock movements, allergens, nutritional facts, purchase orders

#### Entities

##### 3.1 Material
**Table**: `inventory_materials`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| name | string | min:2, max:50, regex: `^[\w\s\-\.]+$` | required | Material name |
| sku | string | min:2, max:50, unique | required | SKU |
| unit | atom | Unit enum (kg, g, lb, oz, l, ml, gal, each, etc.) | required | Unit of measure |
| price | decimal | required | | Purchase price per unit |
| minimum_stock | decimal | min:0 | optional | Reorder threshold |
| maximum_stock | decimal | min:0 | optional | Max capacity |

**Relationships**:
- `has_many :movements` - Stock movements
- `has_many :material_allergens` - Join to allergens
- `has_many :material_nutritional_facts` - Join to nutrition
- `many_to_many :allergens` - Through MaterialAllergen
- `many_to_many :nutritional_facts` - Through MaterialNutritionalFact

**Aggregates**:
- `current_stock` - Sum(movements.quantity)

**Identities**:
- `sku` (unique)
- `name` (unique)

**Actions**:
- `create` - Create material
- `update` - Update material
- `update_allergens` - Update allergen associations
- `update_nutritional_facts` - Update nutrition associations
- `read` - Single material
- `list` - List materials (sorted by name)
- `keyset` - Keyset pagination
- `destroy` - Delete material

**Policies**:
- Public read (used in planning/printouts/exports)
- Staff/admin write

---

##### 3.2 Lot (Inventory Lot)
**Table**: `inventory_lots`

**Purpose**: Track batches of materials with expiry and supplier traceability

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| lot_code | string | unique, required | | Lot identifier |
| material_id | uuid | FK required | | Material |
| supplier_id | uuid | FK optional | | Supplier |
| expiry_date | date | optional | | Expiry |
| received_at | utc_datetime | optional | | When received |

**Relationships**:
- `belongs_to :material` - Material
- `belongs_to :supplier` - Supplier
- `has_many :movements` - Stock movements

**Aggregates**:
- `current_stock` - Sum(movements.quantity)

**Indexes**:
- `lot_code` (unique)
- `material_id`
- `supplier_id`

**Actions**:
- `create` - Create lot
- `update` - Update lot
- `read` - Single lot
- `available_for_material` - Lots with stock > 0 (sorted by expiry)
- `destroy` - Delete lot

---

##### 3.3 Movement (Stock Transaction)
**Table**: `inventory_movements`

**Purpose**: Record all stock changes (receive, consume, adjust)

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| material_id | uuid | FK required | | Material |
| lot_id | uuid | FK optional | | Lot (for traceability) |
| quantity | decimal | required | positive=receive, negative=consume | Quantity change |
| reason | string | max:255 | optional | Reason |
| occurred_at | utc_datetime | required | | When happened |

**Relationships**:
- `belongs_to :material` - Material
- `belongs_to :lot` - Lot (optional)

**Actions**:
- `adjust_stock` - Create movement (auto-sets occurred_at)
- `read` - Single movement
- `destroy` - Delete movement (rare, should use adjustment)

**Policies**:
- Staff/admin only (not public)

---

##### 3.4 Allergen
**Table**: `inventory_allergens`

**Purpose**: Master list of allergens (e.g., "Peanuts", "Dairy", "Gluten")

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| name | string | unique, required | | Allergen name |

**Relationships**:
- `many_to_many :materials` - Through MaterialAllergen

**Identities**:
- `name` (unique)

**Actions**:
- `create` - Create allergen
- `update` - Update allergen
- `read` - Single allergen
- `list` - List allergens (sorted by name)
- `destroy` - Delete allergen

**Policies**:
- Public read (displayed on labels/exports)
- Staff/admin write

---

##### 3.5 NutritionalFact
**Table**: `inventory_nutritional_facts`

**Purpose**: Master list of nutritional facts (e.g., "Calories", "Protein", "Fat")

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| name | string | unique, required | | Fact name |

**Relationships**:
- `many_to_many :materials` - Through MaterialNutritionalFact

**Identities**:
- `name` (unique)

**Actions**:
- `create` - Create nutritional fact
- `update` - Update nutritional fact
- `read` - Single fact
- `list` - List facts (sorted by name)
- `destroy` - Delete fact

**Policies**:
- Public read (displayed on labels)
- Staff/admin write

---

##### 3.6 MaterialAllergen (Join Table)
**Table**: `inventory_material_allergens`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| material_id | uuid | FK required | | Material |
| allergen_id | uuid | FK required | | Allergen |

**Relationships**:
- `belongs_to :material` - Material
- `belongs_to :allergen` - Allergen

---

##### 3.7 MaterialNutritionalFact (Join Table)
**Table**: `inventory_material_nutritional_facts`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| material_id | uuid | FK required | | Material |
| nutritional_fact_id | uuid | FK required | | Nutritional fact |

**Relationships**:
- `belongs_to :material` - Material
- `belongs_to :nutritional_fact` - Nutritional fact

---

##### 3.8 Supplier
**Table**: `inventory_suppliers`

**Purpose**: Material vendors

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| name | string | required | | Supplier name |
| contact_name | string | optional | | Contact person |
| contact_email | string | optional | | Contact email |
| contact_phone | string | optional | | Contact phone |
| notes | string | max:2000 | optional | Notes |

**Relationships**:
- `has_many :lots` - Lots from this supplier
- `has_many :purchase_orders` - Purchase orders

**Actions**:
- `create` - Create supplier
- `update` - Update supplier
- `read` - Single supplier
- `list` - List suppliers (sorted by name)
- `destroy` - Delete supplier

---

##### 3.9 PurchaseOrder
**Table**: `inventory_purchase_orders`

**Purpose**: Purchase orders from suppliers

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| reference | string | pattern `^PO_\d{4}_\d{2}_\d{2}_[A-Z]{6}$` | auto | PO number |
| supplier_id | uuid | FK required | | Supplier |
| status | enum | draft, ordered, received | draft | Status |
| ordered_at | utc_datetime | optional | | When ordered |
| received_at | utc_datetime | optional | | When received |

**Relationships**:
- `belongs_to :supplier` - Supplier
- `has_many :items` - PurchaseOrderItem

**Identities**:
- `reference` (unique)

**Actions**:
- `create` - Create PO (status=draft)
- `update` - Update PO
- `receive` - Mark received, create lots and movements
- `read` - Single PO
- `list` - List POs
- `destroy` - Delete PO

**Receive Logic** (`receive` action):
```elixir
# Argument: lot_receipts = [%{material_id, lot_code, quantity, expiry_date?}]
# For each receipt:
#   1. Create Lot with lot_code, material_id, supplier_id, expiry_date
#   2. Create Movement (positive quantity) with lot_id
#   3. Set PO.status = :received, received_at = now
```

---

##### 3.10 PurchaseOrderItem
**Table**: `inventory_purchase_order_items`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| purchase_order_id | uuid | FK required | | Parent PO |
| material_id | uuid | FK required | | Material |
| quantity | decimal | required | | Quantity |

**Relationships**:
- `belongs_to :purchase_order` - Parent PO
- `belongs_to :material` - Material

**Actions**:
- `create` - Add item
- `update` - Update item
- `read` - Single item
- `destroy` - Delete item

---

#### Inventory Forecasting

**Module**: `Craftplan.InventoryForecasting`

**Purpose**: Predict material demand from upcoming orders, calculate stockouts

**Key Functions**:

1. **`prepare_materials_requirements/2`**
   - Input: Date range
   - Output: Material requirements by day
   - Algorithm:
     - Load orders with delivery_date in range
     - For each order item, get BOM components_map
     - Aggregate material quantities by day
     - Calculate running balances

2. **`calculate_material_balances/2`**
   - Input: Material with current_stock, daily requirements
   - Output: List of {day_balance, closing_balance}
   - Algorithm: Subtract daily usage from running balance

3. **`owner_grid_rows/3`**
   - Rich forecast data for UI
   - Includes: on_hand, on_order, lead_time, service_level, actual_usage (historical), planned_usage, projected_balances

**Metrics**:
- Service level z-scores: 90%=1.28, 95%=1.65, 97.5%=1.96, 99%=2.33
- Lookback period: 42 days (configurable)
- Lead time: from Settings

---

### 4. CRM Domain (`Craftplan.CRM`)

**Purpose**: Manage customers and addresses

#### Entities

##### 4.1 Customer
**Table**: `crm_customers`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| reference | string | pattern `^CUS_[A-Z0-9]{12}$` | auto | Customer ref |
| type | atom | individual, company | required | Customer type |
| first_name | string | min:1, required | | First name |
| last_name | string | min:1, required | | Last name |
| email | string | regex: `@/`, unique | optional | Email |
| phone | string | max:15, unique | optional | Phone |
| billing_address | Address (embedded) | optional | | Billing |
| shipping_address | Address (embedded) | optional | | Shipping |

**Address Type**:
```elixir
%{
  line1: string,
  line2: string (optional),
  city: string,
  state: string,
  postal_code: string,
  country: string
}
```

**Relationships**:
- `has_many :orders` - Orders from this customer

**Calculations**:
- `full_name = first_name <> " " <> last_name`

**Aggregates**:
- `total_orders` - Count of orders
- `total_orders_value` - Sum of order item costs

**Identities**:
- `email` (unique)
- `phone` (unique)
- `reference` (unique)

**Actions**:
- `create` - Create customer
- `update` - Update customer
- `read` - Single customer
- `get_by_email` - Lookup by email (for checkout)
- `list` - List customers (sorted by first_name)
- `keyset` - Keyset pagination
- `destroy` - Delete customer

**Policies**:
- Public create/update (for checkout)
- Public get_by_email (for checkout)
- Staff/admin list/destroy

---

##### 4.2 Address (Embedded Type)
**Module**: `Craftplan.CRM.Address`

**Attributes**:
- line1: string (required)
- line2: string (optional)
- city: string (required)
- state: string (required)
- postal_code: string (required)
- country: string (required)

---

### 5. Accounts Domain (`Craftplan.Accounts`)

**Purpose**: Authentication, users, roles, API keys

#### Entities

##### 5.1 User
**Table**: `accounts_users`

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| email | ci_string | unique, required | | Email (case-insensitive) |
| hashed_password | string | sensitive, required | | Bcrypt hash |
| role | enum | customer, staff, admin | customer | User role |

**Identities**:
- `unique_email` (email)

**Actions**:
- `register_with_password` - Create user, hash password, generate token
- `sign_in_with_password` - Validate credentials, return token
- `sign_in_with_token` - Exchange short-lived token for JWT
- `get_by_email` - Lookup by email
- `get_by_subject` - Lookup by JWT subject
- `list_admins` - List admin users
- `password_reset_with_password` - Reset password
- `request_password_reset_with_password` - Send reset email

**Authentication**:
- Strategy: Password with JWT tokens
- Tokens: Stored in `Token` resource
- Confirmation: Required on create (sends email)
- Password reset: Email-based

---

##### 5.2 ApiKey
**Table**: `accounts_api_keys`

**Purpose**: API access for external integrations

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| name | string | required | | Key name |
| key | string | auto-generated | | API key string |
| scopes | array(atom) | | [] | Allowed scopes |

**Actions**:
- `create` - Generate key
- `read` - Single key
- `destroy` - Delete key

**Scopes**:
- `:read` - Read operations
- `:write` - Write operations
- Domain-specific scopes: `:catalog`, `:orders`, `:inventory`, etc.

---

##### 5.3 Token
**Table**: `accounts_tokens`

**Purpose**: JWT token storage for authentication

**Attributes**:
| Name | Type | Constraints | Default | Description |
|------|------|-------------|---------|-------------|
| id | uuid | PK | auto | Primary key |
| token | string | required | | Encrypted token |
| context | string | required | | Token purpose |
| expires_at | utc_datetime | optional | | Expiry |

**Relationships**:
- `belongs_to :user` - User

---

### 6. Types Module

**Custom Ash Types**:

##### 6.1 Currency
**Module**: `Craftplan.Types.Currency`
**Values**: `:USD`, `:EUR`, `:GBP`, `:CAD`, `:AUD` (extensible)
**Storage**: Atom

##### 6.2 Unit
**Module**: `Craftplan.Types.Unit`
**Values**:
- **Mass**: `:kg`, `:g`, `:lb`, `:oz`
- **Volume**: `:l`, `:ml`, `:gal`
- **Count**: `:each`, `:dozen`
**Storage**: Atom

##### 6.3 EncryptedBinary
**Module**: `Craftplan.Types.EncryptedBinary`
**Purpose**: Encrypt sensitive data at rest
**Storage**: Binary (encrypted)

---

## Relationship Mapping (ER Diagrams)

### Catalog Domain
```
Product (1) ----< (N) BOM
                 |
                 | (1) ----< (N) BOMComponent ----< (1) Material
                 |                |
                 |                | (1) ----< (1) Product (nested)
                 |
                 | (1) ----< (N) LaborStep
                 |
                 | (1) ---- (1) BOMRollup
```

### Orders Domain
```
Order (1) ----< (N) OrderItem
                     |
                     | (N) ---- (1) Product
                     |
                     | (N) ----< (N) OrderItemBatchAllocation ---- (1) ProductionBatch
                     |                                                    |
                     | (N) ----< (N) OrderItemLot ----< (1) Lot           |
                     |                                                    |
                     |                                                    | (N) ---- (1) Product
```

### Inventory Domain
```
Material (1) ----< (N) Movement
       |
       | (N) ----< (N) MaterialAllergen ----< (1) Allergen
       |
       | (N) ----< (N) MaterialNutritionalFact ----< (1) NutritionalFact
       |
       | (1) ----< (N) Lot ----< (1) Supplier
                     |
                     | (1) ----< (N) Movement

PurchaseOrder (1) ----< (N) PurchaseOrderItem ---- (N) Material
       |
       | (N) ---- (1) Supplier
```

### CRM Domain
```
Customer (1) ----< (N) Order
```

---

## Business Rules Catalog

### BOM Rules
1. **One Active BOM per Product**: Only one BOM can have status=:active for a given product
2. **BOM Versioning**: Version auto-increments on create; older versions are read-only
3. **Component Target**: Exactly one of material_id or product_id must be set (not both, not neither)
4. **Rollup Refresh**: BOM rollup is recalculated after any component or labor change
5. **Nested Products**: BOM components can reference other products (recursive BOM lookup with cycle detection)

### Order Rules
1. **Order Reference**: Auto-generated format `OR_YYYY_MM_DD_XXXXXX` (6 random uppercase letters)
2. **At Least One Item**: Orders must have at least one order item
3. **Delivery Date Future**: New orders must have delivery_date in the future
4. **Total Calculation**: Subtotal, discount, tax, shipping, total auto-calculated from items
5. **Status Workflow**: unconfirmed → confirmed → in_progress → ready → delivered → completed
6. **Payment Workflow**: pending → partial → paid (or refunded)

### Production Batch Rules
1. **Batch Code**: Auto-generated unique identifier
2. **BOM Snapshot**: Batch snapshots BOM version and components_map on creation
3. **Status Transition**: open → in_progress → completed (cannot go backwards)
4. **Allocation Constraints**:
   - Allocations can only be for matching products (batch.product_id == item.product_id)
   - Total planned_qty across allocations for an item cannot exceed item.quantity
   - completed_qty cannot exceed planned_qty
5. **Consumption**: Materials consumed from lots (negative movements) when batch status=:in_progress
6. **Cost Allocation**: On completion, costs allocated to order items proportional to completed_qty

### Inventory Rules
1. **Current Stock**: Aggregate sum of all movement quantities for a material/lot
2. **Movements Immutable**: Movements should not be deleted; use adjustments to correct
3. **FEFO (First Expiry, First Out)**: Lots sorted by expiry_date for consumption
4. **Allergen/Nutrition Propagation**: Product allergens/nutrition derived from material BOM components
5. **Purchase Order Receive**: Creates lots and movements atomically

### Customer Rules
1. **Email/Phone Unique**: No two customers can share an email or phone
2. **Reference Auto-Generated**: Format `CUS_XXXXXXXXXXXX` (12 random base32 chars)

---

## API Contract Specifications

### JSON:API Routes

#### Catalog
- `GET /products` - List products
- `GET /products/:id` - Get product
- `POST /products` - Create product
- `PATCH /products/:id` - Update product
- `DELETE /products/:id` - Delete product
- `GET /boms` - List BOMs
- `GET /boms/:id` - Get BOM

#### Orders
- `GET /orders` - List orders
- `GET /orders/:id` - Get order
- `POST /orders` - Create order
- `PATCH /orders/:id` - Update order
- `GET /production-batches` - List batches
- `GET /production-batches/:id` - Get batch

#### Inventory
- `GET /materials` - List materials
- `GET /materials/:id` - Get material
- `POST /materials` - Create material
- `PATCH /materials/:id` - Update material
- `GET /lots` - List lots
- `GET /lots/:id` - Get lot
- `GET /movements` - List movements
- `GET /purchase-orders` - List POs
- `POST /purchase-orders` - Create PO
- `PATCH /purchase-orders/:id/receive` - Receive PO

#### CRM
- `GET /customers` - List customers
- `GET /customers/:id` - Get customer
- `POST /customers` - Create customer
- `PATCH /customers/:id` - Update customer

### GraphQL Schema

Auto-generated from Ash resources. Types mirror resource names in `camelCase`.

**Example Queries**:
```graphql
query GetProduct($id: ID!) {
  getProduct(id: $id) {
    id
    name
    sku
    status
    price
    materialsCost
    bomUnitCost
    activeBom {
      id
      version
      rollup {
        materialCost
        laborCost
        unitCost
      }
    }
  }
}

query ListOrders($status: [OrderStatusEnum]) {
  listOrders(status: $status) {
    id
    reference
    deliveryDate
    customer {
      fullName
    }
    items {
      quantity
      unitPrice
      product {
        name
      }
    }
    total
  }
}
```

---

## Code Examples for Key Patterns

### Pattern 1: Ash Resource with Attributes, Relationships, Actions

```elixir
defmodule Craftplan.Catalog.Product do
  use Ash.Resource,
    domain: Craftplan.Catalog,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "catalog_products"
    repo Craftplan.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      constraints min_length: 2, max_length: 100
    end

    attribute :status, Craftplan.Catalog.Product.Types.Status do
      allow_nil? false
      default :draft
    end
  end

  relationships do
    has_many :boms, Craftplan.Catalog.BOM
    has_one :active_bom, Craftplan.Catalog.BOM do
      filter expr(status == :active)
    end
  end

  actions do
    defaults [:read, :create, :update, :destroy]

    read :list do
      prepare build(sort: :name)
      pagination do
        required? false
        offset? true
        keyset? true
      end
    end
  end
end
```

### Pattern 2: Custom Change Module

```elixir
defmodule Craftplan.Orders.Changes.CalculateTotals do
  use Ash.Resource.Change

  @impl true
  def change(changeset, _opts, _ctx) do
    Changeset.after_action(changeset, fn changeset, order ->
      items = Map.get(order, :items) || []

      subtotal = Enum.reduce(items, Decimal.new(0), fn item, acc ->
        Decimal.add(acc, Decimal.mult(item.quantity, item.unit_price))
      end)

      discount = calculate_discount(order, subtotal)
      total = Decimal.sub(subtotal, discount)

      {:ok, %{order | subtotal: subtotal, discount_total: discount, total: total}}
    end)
  end
end
```

### Pattern 3: Recursive BOM Rollup

```elixir
defp do_flatten(%BOM{} = bom, quantity, path, opts) do
  components = bom.components || []

  Enum.reduce(components, %{}, fn component, acc ->
    case component.component_type do
      :material ->
        mat_id = component.material && component.material.id
        comp_qty = Decimal.mult(quantity, component.quantity)
        Map.update(acc, mat_id, comp_qty, &Decimal.add(&1, comp_qty))

      :product ->
        if MapSet.member?(path, component.product.id) do
          acc  # Cycle detected
        else
          nested_bom = Catalog.get_active_bom_for_product(component.product)
          comp_qty = Decimal.mult(quantity, component.quantity)
          nested_map = do_flatten(nested_bom, comp_qty, MapSet.put(path, component.product.id), opts)
          merge_decimal_maps(acc, nested_map)
        end
    end)
  end)
end
```

### Pattern 4: Inventory Forecasting

```elixir
def prepare_materials_requirements(days_range, actor) do
  orders = load_orders_for_forecast(days_range, actor)

  materials_by_day =
    Enum.flat_map(orders, fn order ->
      Enum.flat_map(order.items, fn item ->
        item
        |> per_unit_requirements(actor)  # Get material requirements from BOM
        |> Enum.map(fn {material_id, per_unit} ->
          {DateTime.to_date(order.delivery_date), material_id, Decimal.mult(per_unit, item.quantity)}
        end)
      end)
    end)

  # Group by material, then aggregate by day
  materials_by_day
  |> Enum.group_by(fn {_, material_id, _} -> material_id end)
  |> Enum.map(fn {material_id, day_quantities} ->
    material = load_material(material_id, actor)
    quantities_by_day = aggregate_by_day(day_quantities, days_range)
    {material, quantities_by_day}
  end)
end
```

---

## Template Requirements for Code Generation

### Required Templates for ggen μ Pipeline

#### 1. Ash Resource Template
**Purpose**: Generate Ash.Resource module from RDF class

**Inputs**:
- Class URI (e.g., `cp:Product`)
- Attributes (name, type, constraints)
- Relationships (domain, cardinality)
- Actions (create, read, update, delete, custom)
- Policies (read, write rules)

**Outputs**:
- Elixir module with `use Ash.Resource`
- `attributes do ... end`
- `relationships do ... end`
- `actions do ... end`
- `policies do ... end`

#### 2. Ash Domain Template
**Purpose**: Generate Ash.Domain module

**Inputs**:
- Domain name (e.g., `Craftplan.Catalog`)
- List of resources in domain

**Outputs**:
- Elixir module with `use Ash.Domain`
- `resources do ... end`

#### 3. PostgreSQL Table Template
**Purpose**: Generate migration for table

**Inputs**:
- Table name
- Columns (name, type, nullability, defaults)
- Indexes (unique, composite)
- Foreign keys

**Outputs**:
- Ecto migration file

#### 4. Changes Module Template
**Purpose**: Generate custom Ash.Resource.Change

**Inputs**:
- Change name
- Business logic (from RDF rules)

**Outputs**:
- `use Ash.Resource.Change` module
- `change/3` callback

#### 5. Calculation Module Template
**Purpose**: Generate Ash calculation module

**Inputs**:
- Calculation name
- Expression or module

**Outputs**:
- Ash calculation definition

#### 6. Enum Type Template
**Purpose**: Generate Ash.Type.Enum

**Inputs**:
- Type name
- Values (atoms)

**Outputs**:
- `use Ash.Type.Enum` module

#### 7. Embedded Type Template
**Purpose**: Generate embedded type (e.g., Address)

**Inputs**:
- Type name
- Attributes

**Outputs**:
- `use Ash.Resource` with `data_layer: Ash.EmbeddedResource`

---

## Summary Statistics

### Domain Metrics
- **Total Domains**: 6 (Catalog, Orders, Inventory, CRM, Accounts, Settings)
- **Total Resources**: 30+
- **Total Attributes**: 200+
- **Total Relationships**: 60+
- **Custom Actions**: 80+
- **Business Rules**: 50+

### Code Metrics
- **Elixir Files**: 99
- **Lines of Code**: ~15,000
- **Test Files**: 50+
- **Test Coverage**: >80% (Chicago TDD)

### RDF Mapping Estimates
- **Classes (RDF)**: 30+ (one per resource)
- **Properties (RDF)**: 200+ (one per attribute)
- **Object Properties**: 60+ (relationships)
- **SHACL Shapes**: 30+ (validation shapes)
- **OWL Restrictions**: 50+ (business rules)

---

## Next Steps for RDF Specification

1. **Create RDF Class Hierarchy**:
   - `cp:Entity` (abstract base)
   - `cp:CatalogEntity` ← `cp:Product`, `cp:BOM`, `cp:BOMComponent`
   - `cp:OrderEntity` ← `cp:Order`, `cp:OrderItem`, `cp:ProductionBatch`
   - `cp:InventoryEntity` ← `cp:Material`, `cp:Lot`, `cp:Movement`
   - `cp:CRMEntity` ← `cp:Customer`, `cp:Supplier`

2. **Define Property Naming Conventions**:
   - Attributes: `cp:{resource}_{attribute}` (e.g., `cp:product_sku`)
   - Relationships: `cp:{resource}_{relationship}` (e.g., `cp:product_boms`)
   - Inverse relationships: `cp:inverse_{relationship}` (e.g., `cp:inverse_bom_product`)

3. **Create SHACL Shapes**:
   - Each class gets a NodeShape
   - Attribute constraints (minLength, maxLength, pattern, min/max)
   - Cardinality constraints (min/max counts)
   - Logical constraints (and, or, not)

4. **Capture Business Rules as OWL**:
   - `owl:allValuesFrom` for relationship type constraints
   - `owl:cardinality` for counts
   - `owl:hasValue` for fixed values
   - SWRL rules for complex constraints

5. **Define Template Mappings**:
   - RDF class → Ash Resource module
   - RDF property → Ash attribute
   - SHACL constraint → Ash validation
   - OWL restriction → Ash policy
   - RDFS subclass → Ash domain

---

**END OF ANALYSIS**
