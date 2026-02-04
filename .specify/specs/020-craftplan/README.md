# Craftplan Ontology Design Documentation

**Specification ID:** 020-craftplan
**Version:** 1.0.0
**Created:** 2026-02-04
**Status:** Draft

## Overview

This document describes the RDF ontology for the **craftplan** domain - a manufacturing ERP system for small-scale producers. The ontology captures the complete domain model including Products, BOMs (Bill of Materials), Orders, Production Batches, Inventory (Materials & Lots), and CRM (Customers & Suppliers).

## Ontology Files

- **`core.ttl`** - Domain ontology with classes, properties, and SHACL validation shapes
- **`entities.ttl`** - Entity instances and example data
- **`plan.ttl`** - Code generation plan for Ash Framework implementation

## Design Principles

### 1. Ash Framework Alignment

The ontology is designed to map directly to Ash Framework resources:

- **RDF Classes** → **Ash Resources** (e.g., `craftplan:Product` → `Craftplan.Catalog.Product`)
- **RDF Properties** → **Ash Attributes** (e.g., `craftplan:productName` → `attribute :name, :string`)
- **Object Properties** → **Ash Relationships** (e.g., `craftplan:hasActiveBOM` → `belongs_to :active_bom`)
- **SHACL Shapes** → **Ash Validations** (e.g., `sh:minInclusive 0` → `constraints min: 0`)

### 2. Decimal Precision for Financial Data

All monetary and quantity fields use `xsd:decimal` type, which maps to PostgreSQL's NUMERIC type and Elixir's Decimal struct:

```turtle
craftplan:unitPrice a rdf:Property ;
    rdfs:domain craftplan:Product ;
    rdfs:range xsd:decimal .  # PostgreSQL NUMERIC, Elixir Decimal
```

**Rationale:** Prevents floating-point rounding errors in financial calculations. See `plan.ttl` Decision:PostgresDecimal.

### 3. Audit Trail via Lot Tracking

Material consumption is tracked through **Lots** (inventory batches) and **Movements** (transactions):

```
Customer Order
  → Order Items
    → Production Batch (allocated)
      → Material Consumption (OrderItemLot)
        → Inventory Lot (source)
          → Supplier (origin)
```

This provides complete supply chain traceability from supplier to customer delivery.

### 4. BOM Versioning

Bill of Materials support versioning:

- Each BOM has a version number (`craftplan:bomVersion`)
- Only the latest version is editable (`craftplan:isLatestVersion`)
- Historical versions are read-only for audit purposes
- Products reference their active BOM (`craftplan:hasActiveBOM`)

### 5. Production Batching

Multiple order items can be grouped into a **Production Batch** for efficiency:

- Batch has a product and BOM
- Order items are allocated to batches (`OrderItemBatchAllocation`)
- Material lots are staged for batches (`ProductionBatchLot`)
- Consumption tracks which lot quantities were used

## Domain Model

### Catalog (Products & BOMs)

#### Product

**URI:** `craftplan:Product`

**Purpose:** Manufacturable product with pricing, cost, and BOM relationships

**Key Properties:**
- `productSku` (string) - Unique stock keeping unit
- `productName` (string) - Human-readable name
- `unitPrice` (decimal) - Selling price per unit
- `unitCost` (decimal) - Manufacturing cost per unit (calculated from BOM)
- `maxDailyQuantity` (integer) - Production capacity constraint
- `hasActiveBOM` (BOM) - Reference to active Bill of Materials

**Status Enum:** Active, Draft, Discontinued, Archived

**Example:**
```turtle
craftplan:Product-SourdoughBread a craftplan:Product ;
    craftplan:productSku "PROD-001" ;
    craftplan:productName "Sourdough Bread" ;
    craftplan:unitPrice "8.50"^^xsd:decimal ;
    craftplan:unitCost "3.20"^^xsd:decimal ;
    craftplan:maxDailyQuantity 100 .
```

#### BOM (Bill of Materials)

**URI:** `craftplan:BOM`

**Purpose:** Recipe for manufacturing a product

**Components:**
- **BOMComponent** - Material line items (material, quantity, unit, cost)
- **LaborStep** - Work operations (name, minutes, cost, description)
- **BOMRollup** - Calculated summary (material cost, labor cost, overhead cost, total cost)

**Relationships:**
- `hasBOMComponent` → BOMComponent (1 or more)
- `hasLaborStep` → LaborStep (0 or more)
- `hasRollup` → BOMRollup (1)

**Example:**
```turtle
craftplan:BOM-SourdoughBread-v1 a craftplan:BOM ;
    craftplan:bomVersion 1 ;
    craftplan:bomProduct craftplan:Product-SourdoughBread ;
    craftplan:hasBOMComponent craftplan:BOMComponent-Flour ;
    craftplan:hasLaborStep craftplan:LaborStep-Mixing .
```

### Orders

#### Order

**URI:** `craftplan:Order`

**Purpose:** Customer purchase order with delivery date and line items

**Key Properties:**
- `orderReference` (string) - Human-readable order number
- `orderDeliveryDate` (date) - Requested delivery date
- `orderStatus` (OrderStatus) - Draft, Confirmed, InProgress, Completed, Cancelled
- `orderCustomer` (Customer) - Customer who placed order
- `hasOrderItem` (OrderItem) - Line items

**Status Enum:** OrderDraft, OrderConfirmed, OrderInProgress, OrderCompleted, OrderCancelled

#### OrderItem

**URI:** `craftplan:OrderItem`

**Purpose:** Individual line item in an order

**Key Properties:**
- `orderItemProduct` (Product) - Product ordered
- `orderItemQuantity` (decimal) - Quantity ordered
- `orderItemStatus` (OrderItemStatus) - Todo, InProgress, Completed
- `orderItemUnitPrice` (decimal) - Price per unit (snapshot)
- `orderItemUnitCost` (decimal) - Manufacturing cost per unit
- `orderItemMaterialCost` (decimal) - Total material cost (calculated)
- `orderItemLaborCost` (decimal) - Total labor cost (calculated)
- `orderItemOverheadCost` (decimal) - Total overhead cost (calculated)
- `orderItemConsumedAt` (dateTime) - Production timestamp
- `orderItemBatchCode` (string) - Batch code assigned after production

**Cost Calculation:**
```
orderItemMaterialCost = orderItemQuantity × BOM.totalMaterialCost
orderItemLaborCost = orderItemQuantity × BOM.totalLaborCost
orderItemOverheadCost = orderItemQuantity × BOM.totalOverheadCost
```

### Production

#### ProductionBatch

**URI:** `craftplan:ProductionBatch`

**Purpose:** Production run grouping multiple order items

**Key Properties:**
- `batchCode` (string) - Unique batch identifier
- `batchProduct` (Product) - Product being produced
- `batchBOM` (BOM) - BOM version used
- `batchStatus` (BatchStatus) - Open, Started, Completed
- `batchProducedAt` (dateTime) - Completion timestamp
- `batchQuantity` (decimal) - Total quantity produced
- `hasBatchAllocation` (OrderItemBatchAllocation) - Order items in batch
- `hasBatchLot` (ProductionBatchLot) - Material lots staged

**Workflow:**
1. Create batch with status=Open
2. Allocate order items to batch (OrderItemBatchAllocation)
3. Stage material lots (ProductionBatchLot)
4. Start batch (status=Started) - consumes materials via Movements
5. Complete batch (status=Completed) - updates OrderItem status and costs

#### OrderItemBatchAllocation

**URI:** `craftplan:OrderItemBatchAllocation`

**Purpose:** Many-to-many link between order items and batches

**Properties:**
- `allocationOrderItem` (OrderItem)
- `allocationBatch` (ProductionBatch)

### Inventory

#### Material

**URI:** `craftplan:Material`

**Purpose:** Raw material or ingredient used in production

**Key Properties:**
- `materialSku` (string) - Unique material identifier
- `materialName` (string) - Human-readable name
- `materialUnit` (string) - Unit of measurement (g, kg, ml, L, each)
- `materialUnitCost` (decimal) - Cost per unit (from latest purchase)
- `materialSupplier` (Supplier) - Preferred supplier
- `hasLot` (Lot) - Inventory lots

#### Lot

**URI:** `craftplan:Lot`

**Purpose:** Inventory lot with expiry tracking and cost

**Key Properties:**
- `lotCode` (string) - Unique lot identifier
- `lotExpiryDate` (date) - Expiry date for perishables
- `lotReceivedAt` (dateTime) - Receiving timestamp
- `lotCurrentStock` (decimal) - Current available quantity
- `lotUnitCost` (decimal) - Cost per unit for this specific lot

**FIFO/LIFO Logic:**
- FIFO (First In, First Out) - Use oldest lots first
- FEFO (First Expired, First Out) - Use lots expiring soonest first

#### Movement

**URI:** `craftplan:Movement`

**Purpose:** Stock transaction (consume, receive, adjust)

**Key Properties:**
- `movementType` (MovementType) - Consume, Receive, Adjust
- `movementQuantity` (decimal) - Quantity moved (negative for consume)
- `movementLot` (Lot) - Lot affected
- `movementTimestamp` (dateTime) - When movement occurred
- `movementReason` (string) - Reason for adjustment

**Types:**
- **Consume** - Material consumed in production (negative quantity)
- **Receive** - Material received from supplier (positive quantity)
- **Adjust** - Manual inventory correction (positive or negative)

### CRM

#### Customer

**URI:** `craftplan:Customer`

**Purpose:** Customer who places orders

**Key Properties:**
- `customerFullName` (string)
- `customerEmail` (string)
- `customerPhone` (string)
- `customerAddress` (Address)

#### Supplier

**URI:** `craftplan:Supplier`

**Purpose:** Material supplier

**Key Properties:**
- `supplierName` (string) - Company name
- `supplierContact` (string) - Primary contact person
- `supplierEmail` (string)
- `supplierPhone` (string)

#### Address

**URI:** `craftplan:Address`

**Purpose:** Postal address

**Properties:**
- `addressStreet` (string)
- `addressCity` (string)
- `addressState` (string)
- `addressPostalCode` (string)
- `addressCountry` (string)

## Validation Rules (SHACL Shapes)

### Product Shape

```turtle
craftplan:ProductShape a sh:NodeShape ;
    sh:targetClass craftplan:Product ;
    sh:property [
        sh:path craftplan:productSku ;
        sh:datatype xsd:string ;
        sh:minLength 1 ;
        sh:maxLength 255 ;
        sh:uniqueLang true ;     # Enforces uniqueness
    ] ;
    sh:property [
        sh:path craftplan:unitPrice ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;      # Non-negative
    ] .
```

**Validations:**
- SKU required, max 255 chars, unique
- Name required
- Unit price non-negative decimal

### BOM Shape

```turtle
craftplan:BOMShape a sh:NodeShape ;
    sh:targetClass craftplan:BOM ;
    sh:property [
        sh:path craftplan:bomVersion ;
        sh:datatype xsd:integer ;
        sh:minInclusive 1 ;      # Version >= 1
    ] ;
    sh:property [
        sh:path craftplan:hasBOMComponent ;
        sh:class craftplan:BOMComponent ;
        sh:minCount 1 ;          # At least 1 component
    ] .
```

**Validations:**
- Version number >= 1
- At least 1 BOM component required

### Lot Shape

```turtle
craftplan:LotShape a sh:NodeShape ;
    sh:targetClass craftplan:Lot ;
    sh:property [
        sh:path craftplan:lotCurrentStock ;
        sh:datatype xsd:decimal ;
        sh:minInclusive 0 ;      # Non-negative stock
    ] .
```

**Validations:**
- Lot code unique
- Current stock non-negative

## SPARQL Query Patterns

### 1. Calculate Material Requirements for an Order

**Use Case:** Determine which materials and quantities are needed to fulfill an order.

```sparql
PREFIX craftplan: <http://craftplan.example.org/ontology#>

SELECT ?material ?sumQuantity
WHERE {
    ?order a craftplan:Order ;
        craftplan:orderReference "ORD-2024-001" ;
        craftplan:hasOrderItem ?orderItem .

    ?orderItem craftplan:orderItemProduct ?product ;
                craftplan:orderItemQuantity ?orderItemQuantity .

    ?product craftplan:hasActiveBOM ?bom .

    ?bom craftplan:hasBOMComponent ?bomComponent .

    ?bomComponent craftplan:componentMaterial ?material ;
                 craftplan:componentQuantity ?componentQuantity .

    BIND (?componentQuantity * ?orderItemQuantity AS ?needed)
}
GROUP BY ?material
```

### 2. Find Expiring Lots

**Use Case:** Alert for materials expiring within 30 days.

```sparql
PREFIX craftplan: <http://craftplan.example.org/ontology#>
PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

SELECT ?lotCode ?material ?expiryDate ?currentStock
WHERE {
    ?lot a craftplan:Lot ;
        craftplan:lotCode ?lotCode ;
        craftplan:lotExpiryDate ?expiryDate ;
        craftplan:lotCurrentStock ?currentStock .

    ?lot craftplan:hasLotMaterial ?material .

    FILTER (?expiryDate < NOW() + "P30D"^^xsd:duration)
    FILTER (?currentStock > 0)
}
ORDER BY ?expiryDate
```

### 3. Trace Lot to Orders

**Use Case:** Full traceability from lot to final customer delivery.

```sparql
PREFIX craftplan: <http://craftplan.example.org/ontology#>

SELECT ?lotCode ?orderReference ?customerName ?quantityUsed
WHERE {
    ?lot a craftplan:Lot ;
        craftplan:lotCode "LOT-2024-001" .

    ?movement a craftplan:Movement ;
        craftplan:movementLot ?lot ;
        craftplan:movementQuantity ?quantityUsed .

    # ... (join to batch, order item, order, customer)
}
```

## Code Generation Plan

See `plan.ttl` for complete code generation strategy.

### Target Stack

- **Language:** Elixir 1.15+
- **Framework:** Ash 3.0+ (Resource-based domain modeling)
- **Web:** Phoenix 1.7+ with LiveView
- **Database:** PostgreSQL 16 with NUMERIC type

### Generation Workflow

1. **Validate RDF** - Ensure ontology files are valid Turtle and SHACL shapes pass
2. **Generate Resources** - Create Ash Resource modules from ontology classes
3. **Generate Domains** - Create Ash Domain modules grouping related resources
4. **Generate Migrations** - Use Ash's built-in migration generator
5. **Generate Tests** - Create factory and test cases from SHACL shapes
6. **Format Code** - Run `mix format` with Styler, Spark.Formatter
7. **Run Tests** - Verify all tests pass

### Template Mappings

| RDF Concept | Ash Concept | Example |
|-------------|-------------|---------|
| `rdfs:Class` | `Ash.Resource` | `craftplan:Product` → `Craftplan.Catalog.Product` |
| `rdf:Property` (range xsd:) | `Ash.Attribute` | `craftplan:productName` → `attribute :name, :string` |
| `rdf:Property` (range rdfs:Class) | `Ash.Relationship` | `craftplan:hasActiveBOM` → `belongs_to :active_bom` |
| `sh:NodeShape` | `Ash.Validation` | `sh:minInclusive 0` → `constraints min: 0` |
| `owl:hasKey` | Unique constraint | `owl:hasKey (productSku)` → `allow_nil? false, unique? true` |

### Example Generated Resource

```elixir
defmodule Craftplan.Catalog.Product do
  use Ash.Resource,
    data_layer: AshPostgres.DataLayer

  postgres do
    table "products"
    repo Craftplan.Repo
  end

  attributes do
    uuid_primary_key :id

    attribute :sku, :string do
      allow_nil? false
      public? true
    end

    attribute :name, :string do
      allow_nil? false
      public? true
    end

    attribute :unit_price, :decimal do
      allow_nil? false
      public? true
      constraints min: 0
    end

    attribute :unit_cost, :decimal do
      allow_nil? false
      public? true
      constraints min: 0
    end

    create_timestamp :inserted_at
    update_timestamp :updated_at
  end

  relationships do
    belongs_to :active_bom, Craftplan.Catalog.BOM do
      source_attribute :active_bom_id
      destination_attribute :id
    end
  end

  actions do
    defaults [:read, :destroy, :update]

    create :create do
      primary? true
    end
  end
end
```

## Business Rules

1. **Product Must Have Active BOM**
   - Every active product must reference an active BOM version
   - Constraint: `minCount(1)` on `hasActiveBOM`

2. **Positive Quantities Required**
   - Order item quantities must be > 0
   - BOM component quantities must be > 0
   - Constraint: `minInclusive(0.01)` for quantities, `minInclusive(0.001)` for BOM components

3. **Lot Expiry Future**
   - Material lot expiry date must be in the future at receiving time
   - Constraint: `expiryDate > receivedAt`

4. **Batch Capacity Limit**
   - Production batch quantity cannot exceed product's max daily quantity
   - Constraint: `batchQuantity <= product.maxDailyQuantity`

5. **Delivery Date Future**
   - Order delivery date must be today or in the future
   - Constraint: `deliveryDate >= today`

## Risks & Mitigations

### RDF Model Drift from Code

**Risk:** Manual code edits diverge from RDF ontology

**Mitigation:**
- CI check to compare RDF timestamp vs. generated file timestamp
- Run `ggen validate` in CI to detect drift
- Use merge markers for manual customizations
- Document all manual edits in RDF annotations

### Decimal Precision Overflow

**Risk:** DECIMAL type overflow with extreme values

**Mitigation:**
- Use PostgreSQL NUMERIC(20,6) for all decimal columns
- Add CHECK constraints for reasonable ranges
- Validate in Ash changeset before DB insert
- Test with extreme values (1e10, 1e-6)

### Ash Framework Version Breaks Templates

**Risk:** Ash DSL changes break code generation templates

**Mitigation:**
- Pin Ash version in mix.exs (e.g., `~> 3.0`)
- Test templates on each Ash minor version
- Maintain template version history in RDF
- Include Ash version in generation metadata

## Success Criteria

1. **All Resources Generated** - 100% of ontology classes have corresponding Ash Resource files
2. **Tests Pass** - 100% test pass rate (`mix test: 0 failures`)
3. **Code Formatted** - 0 formatting violations (`mix format --check-formatted`)
4. **No Warnings** - 0 compiler warnings (`mix compile`)
5. **Validations Enforced** - All SHACL shapes map to Ash validations

## References

- **Ash Framework Documentation:** https://ash-hq.org/
- **RDF/Turtle Specification:** https://www.w3.org/TR/turtle/
- **SHACL Specification:** https://www.w3.org/TR/shacl/
- **ggen Documentation:** `/Users/sac/ggen/CLAUDE.md`

## Changelog

### v1.0.0 (2026-02-04)

- Initial ontology design
- Core domain model: Catalog, Orders, Production, Inventory, CRM
- SHACL validation shapes for all entities
- SPARQL query patterns for common operations
- Code generation plan for Ash Framework
- Business rules and constraints
- Risk analysis and mitigations
