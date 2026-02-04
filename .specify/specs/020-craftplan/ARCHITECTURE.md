# Craftplan Architecture Overview

## System Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    PHOENIX LIVEVIEW (UI)                    │
│  Real-time UI with forms, tables, pagination, filtering     │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│                      ASH DOMAIN LAYER                       │
│  ┌──────────┐  ┌──────────┐  ┌──────────┐  ┌──────────┐  │
│  │ Catalog  │  │  Orders  │  │Inventory │  │   CRM    │  │
│  └──────────┘  └──────────┘  └──────────┘  └──────────┘  │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│                    ASH RESOURCE LAYER                       │
│  ┌─────────┐ ┌─────────┐ ┌──────────┐ ┌─────────┐        │
│  │ Product │ │   BOM   │ │   Order   │ │Material │        │
│  └─────────┘ └─────────┘ └──────────┘ └─────────┘        │
│  ┌─────────┐ ┌─────────┐ ┌──────────┐ ┌─────────┐        │
│  │   Lot   │ │  Batch  │ │ Customer │ │Supplier │        │
│  └─────────┘ └─────────┘ └──────────┘ └─────────┘        │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│                 POSTGRESQL DATABASE                         │
│  Tables: products, boms, orders, materials, lots, etc.     │
│  Types: UUID, VARCHAR, NUMERIC(20,6), TIMESTAMP, DATE      │
└─────────────────────────────────────────────────────────────┘
```

## Domain Model Relationships

```
Customer ──────< Order ──────< OrderItem ──────> Product
                    │                                    │
                    │                                    │
                    ▼                                    ▼
                (link)                              hasActiveBOM
                    │                                    │
                    └──────────> ProductionBatch <────────┘
                                      │
                                      │
                        hasBatchAllocation │
                                      │
                                      ▼
                                OrderItemLot
                                      │
                                      │
                                    (uses)
                                      │
                                      ▼
                                Lot ──────────> Material
                                                                │
                                                                │
                                                              (from)
                                                                │
                                                                ▼
                                                          Supplier
```

## Supply Chain Traceability Flow

```
Supplier
    │ (supplies)
    ▼
Material (raw ingredient)
    │ (stored in)
    ▼
Lot (inventory batch with expiry)
    │ (consumed by)
    ▼
Movement (transaction: consume/receive/adjust)
    │ (allocated to)
    ▼
ProductionBatch (production run)
    │ (fulfills)
    ▼
OrderItem (line item)
    │ (part of)
    ▼
Order (customer order)
    │ (placed by)
    ▼
Customer
```

## Code Generation Flow

```
RDF Ontology (core.ttl, entities.ttl)
         │
         │ ggen generate ash_resource
         ▼
   Ash Resource Modules
   (lib/craftplan/**/*.ex)
         │
         │ mix ash.generate_migrations
         ▼
   Database Migrations
   (priv/repo/migrations/*.exs)
         │
         │ mix ecto.migrate
         ▼
   PostgreSQL Schema
```

## Business Logic Flow (Order Fulfillment)

```
1. CREATE ORDER
   Customer places order with delivery date
   → Order created with status=Draft
   → OrderItems created with status=Todo

2. PLAN PRODUCTION
   Group OrderItems by Product and delivery date
   → Create ProductionBatches (max_daily_quantity constraint)
   → Allocate OrderItems to Batches

3. CHECK INVENTORY
   Calculate material requirements from BOMs
   → Query Lots for required materials
   → Check expiry dates (FEFO: First Expired, First Out)
   → Check available quantities

4. STAGE MATERIALS
   Allocate Lots to ProductionBatches
   → Create ProductionBatchLot records
   → Reserve quantities (no actual consumption yet)

5. EXECUTE PRODUCTION
   Update ProductionBatch status: Open → Started
   → Consume materials (Movement records with type=Consume)
   → Update Lot.currentStock (decrement)
   → Track quantities used per Lot (OrderItemLot records)

6. COMPLETE BATCH
   Update ProductionBatch status: Started → Completed
   → Update OrderItem.status: Todo → Completed
   → Update OrderItem.consumedAt timestamp
   → Assign batch code to OrderItem
   → Calculate costs (material, labor, overhead)

7. FULFILL ORDER
   When all OrderItems completed:
   → Update Order.status: InProgress → Completed
   → Generate invoice/packing list
```

## Data Types Mapping

| RDF Type | PostgreSQL | Elixir | Description |
|----------|------------|--------|-------------|
| xsd:string | VARCHAR | String | Text fields (names, descriptions) |
| xsd:decimal | NUMERIC(20,6) | Decimal | Financial data, quantities |
| xsd:integer | INTEGER | Integer | Counts, version numbers |
| xsd:date | DATE | Date | Delivery dates, expiry dates |
| xsd:dateTime | TIMESTAMPTZ | DateTime | Created/updated timestamps |
| xsd:boolean | BOOLEAN | Boolean | Status flags |
| rdfs:Class | TABLE | Module | Domain entities |
| rdf:Property | COLUMN | Attribute/Relationship | Properties and relationships |

## Validation Layers

```
┌─────────────────────────────────────────────────────────────┐
│ LAYER 1: ASH CHANGESET VALIDATION                          │
│  - Required fields (allow_nil?: false)                      │
│  - Type constraints (string, decimal, integer)              │
│  - Value constraints (min:, max:, pattern:)                 │
│  - Custom changes (validate_changeset)                      │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│ LAYER 2: SHACL VALIDATION (pre-generation)                 │
│  - RDF ontology validation                                  │
│  - Shape-based constraints                                  │
│  - Cardinality constraints                                  │
│  - Datatype constraints                                     │
└──────────────────────┬──────────────────────────────────────┘
                       │
                       ▼
┌─────────────────────────────────────────────────────────────┐
│ LAYER 3: POSTGRESQL CONSTRAINTS                            │
│  - NOT NULL constraints                                     │
│  - CHECK constraints (positive values, reasonable ranges)   │
│  - UNIQUE constraints (SKU, lot codes, order references)   │
│  - FOREIGN KEY constraints (referential integrity)         │
└─────────────────────────────────────────────────────────────┘
```

## Cost Calculation Flow

```
Product.unitCost = BOM.totalCost
                  ↓
BOM.totalCost = materialCost + laborCost + overheadCost

materialCost = Σ(BOMComponent.componentCost)
              = Σ(componentQuantity × Material.unitCost)

laborCost = Σ(LaborStep.laborCost)
           = Σ(laborStepMinutes × laborRatePerMinute)

overheadCost = (materialCost + laborCost) × overheadRate

OrderItem.materialCost = orderItemQuantity × BOM.materialCost
OrderItem.laborCost = orderItemQuantity × BOM.laborCost
OrderItem.overheadCost = orderItemQuantity × BOM.overheadCost

Order.totalCost = Σ(OrderItem.materialCost + laborCost + overheadCost)

OrderProfit = Σ(OrderItem.quantity × (unitPrice - unitCost))

OrderMargin = (OrderRevenue - OrderCost) / OrderRevenue
```

## Key Design Decisions

### 1. Decimal Precision for Financial Data
**Decision:** Use PostgreSQL NUMERIC and Elixir Decimal for all monetary/quantity fields
**Rationale:** Prevent floating-point rounding errors in financial calculations
**Trade-off:** Slightly slower than float, but correctness is non-negotiable

### 2. Ash DSL-First Code Generation
**Decision:** Define domain in Ash DSL, generate boilerplate only
**Rationale:** Ash DSL is expressive and concise; no need to abstract it away
**Trade-off:** Manual edits to generated resources (tracked with merge markers)

### 3. Phoenix LiveView for UI
**Decision:** Use LiveView for all user interfaces
**Rationale:** Real-time, server-rendered UI without writing JavaScript
**Trade-off:** Higher server memory usage (per-connection process)

### 4. Lot-Based Inventory Tracking
**Decision:** Track materials through Lots (not aggregate counts)
**Rationale:** Enables full traceability and FEFO/FIFO selection
**Trade-off:** More complex consumption logic (but necessary for ERP)

### 5. BOM Versioning
**Decision:** Support multiple BOM versions per product
**Rationale:** Audit trail for recipe changes over time
**Trade-off:** Additional joins to resolve active BOM

## Performance Considerations

### Database Indexes
- Primary keys: UUID (all tables)
- Unique constraints: SKU, lot_code, order_reference
- Foreign keys: All belongs_to relationships
- Query indexes: delivery_date, expiry_date, status

### N+1 Query Prevention
- Use Ash.Query.load() for preloading relationships
- Use Ash.Query.aggregate() for rollups
- Use :stream for large lists (not :for)

### Caching Strategy
- ETS cache for Material prices (update on receiving)
- ETS cache for Product.active_bom (update on BOM changes)
- No caching for Order/OrderItem (transactional data)

### Scalability Targets
- 10,000+ Products
- 100,000+ Orders
- 1,000,000+ Lots
- 100+ concurrent users
- <5s response time for 95% of queries

## Security Model

### Ash Policies (Authorization)
- User-based access control (actor: user)
- Role-based permissions (admin, staff, customer)
- Field-level encryption for sensitive data
- Row-level security (customers see own orders only)

### Data Protection
- PII encryption (customer emails, phones)
- Audit logging (all movements, batch allocations)
- GDPR compliance (right to deletion, export)
- SOC 2 compliance controls (if applicable)

## Monitoring & Observability

### Metrics
- Order fulfillment rate
- Material waste percentage
- Batch success rate
- Inventory turnover
- On-time delivery rate

### Logging
- Structured logging (Logger.metadata)
- Audit trail (who changed what, when)
- Error tracking (Sentry integration)
- Performance monitoring (telemetry metrics)

## Deployment Architecture

```
┌─────────────────────────────────────────────────────────────┐
│                    LOAD BALANCER                            │
│                   (SSL Termination)                         │
└──────────────────────┬──────────────────────────────────────┘
                       │
        ┌──────────────┴──────────────┐
        │                             │
        ▼                             ▼
┌───────────────┐             ┌───────────────┐
│  Phoenix Node 1│             │  Phoenix Node 2│
│  (LiveView UI) │             │  (LiveView UI) │
└───────┬───────┘             └───────┬───────┘
        │                             │
        └──────────────┬──────────────┘
                       │
                       ▼
        ┌──────────────────────────┐
        │  PostgreSQL Primary      │
        │  (Ash.DataLayer)         │
        └───────┬──────────────────┘
                │
                ▼
        ┌──────────────────────────┐
        │  PostgreSQL Replica      │
        │  (Read-only queries)     │
        └──────────────────────────┘
```

## Technology Stack Versions

- **Elixir:** 1.15+ (OTP 26+)
- **Ash Framework:** 3.0+ (with AshPostgres 2.0+)
- **Phoenix:** 1.7+ (with LiveView 1.0+)
- **PostgreSQL:** 16+ (NUMERIC type support)
- **Node.js:** 20+ (for assets/build tools)
- **Docker:** 24+ (for development containers)

## Development Workflow

```bash
# 1. Generate Ash Resources from RDF
ggen generate ash_resource --output lib/craftplan/

# 2. Generate Database Migrations
mix ash.generate_migrations

# 3. Run Migrations
mix ecto.migrate

# 4. Start Development Server
mix phx.server

# 5. Run Tests
mix test

# 6. Format Code
mix format

# 7. Type Check (optional)
mix dialyzer
```

## Testing Strategy

### Unit Tests
- Ash Resource validation tests
- Calculation logic tests (BOM rollups, costs)
- Changeset tests (create, update, destroy)

### Integration Tests
- Order fulfillment workflow
- Material consumption tracking
- Batch allocation logic
- Lot expiry handling

### LiveView Tests
- Form validation
- Real-time updates
- Pagination
- Filtering and search

## Future Enhancements

1. **Multi-Warehouse Support** - Track inventory across locations
2. **Purchase Order Automation** - Auto-generate POs from material shortages
3. **Production Scheduling** - Optimize batch scheduling with capacity constraints
4. **Quality Control** - Add QC checks and lot holds
5. **Shipping Integration** - Track shipments and generate labels
6. **Accounting Integration** - Export to accounting software (QuickBooks, Xero)
7. **API Endpoints** - JSON API and GraphQL for mobile apps
8. **Reporting** - BI dashboards, sales reports, production analytics
9. **Barcode Scanning** - Mobile barcode scanner for inventory
10. **Mobile App** - React Native app for warehouse and production floor
