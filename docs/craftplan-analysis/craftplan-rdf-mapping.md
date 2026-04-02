# Craftplan Elixir to RDF Mapping Guide

**Purpose**: Define systematic mapping from Elixir/Ash constructs to RDF/OWL/SHACL for ggen code generation

**Last Updated**: 2026-02-04

---

## 1. Type System Mapping

### Elixir Types → RDF/XSD Datatypes

| Elixir Type | RDF Datatype | Example |
|------------|--------------|---------|
| `:uuid` | `xsd:string` (pattern) | `"550e8400-e29b-41d4-a716-446655440000"` |
| `:string` | `xsd:string` | `"Product Name"` |
| `:integer` | `xsd:integer` | `42` |
| `:decimal` | `xsd:decimal` | `"19.99"`^^xsd:decimal |
| `:boolean` | `xsd:boolean` | `true` |
| `:atom` | `xsd:string` (enumeration) | `"active"` |
| `:utc_datetime` | `xsd:dateTime` | `"2026-02-04T10:30:00Z"`^^xsd:dateTime |
| `:date` | `xsd:date` | `"2026-02-04"`^^xsd:date |
| `:array` | `rdf:List` or repeat property | `(1, 2, 3)` |
| `:map` | `rdf:JSON` or custom structure | `{"key": "value"}` |
| `Ash.Type.Enum` | `owl:Class` with `owl:oneOf` | Status enum |

### Custom Ash Types → RDF

| Custom Type | RDF Representation |
|-------------|-------------------|
| `Craftplan.Types.Currency` | `cp:Currency` enum (`:USD`, `:EUR`, etc.) |
| `Craftplan.Types.Unit` | `cp:Unit` enum (`:kg`, `:g`, `:l`, etc.) |
| `Craftplan.CRM.Address` | `cp:Address` class (embedded) |

---

## 2. Resource Class Mapping

### Elixir Ash.Resource → RDF Class

**Elixir**:
```elixir
defmodule Craftplan.Catalog.Product do
  use Ash.Resource,
    domain: Craftplan.Catalog,
    data_layer: AshPostgres.DataLayer

  attributes do
    uuid_primary_key :id
    attribute :name, :string
    attribute :sku, :string
    attribute :status, Craftplan.Catalog.Product.Types.Status
    attribute :price, :decimal
  end
end
```

**RDF (Turtle)**:
```turtle
cp:Product a owl:Class ;
    rdfs:subClassOf cp:CatalogEntity ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" .

cp:Product_id a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:string ;
    rdfs:pattern "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" .

cp:Product_name a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:string ;
    sh:minLength 2 ;
    sh:maxLength 100 ;
    sh:pattern "^[\\w\\s\\-\\.]+$" .

cp:Product_sku a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:string ;
    sh:minLength 2 ;
    sh:maxLength 50 .

cp:Product_status a owl:ObjectProperty ;
    rdfs:domain cp:Product ;
    rdfs:range cp:ProductStatus .

cp:Product_price a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:decimal .
```

### Enum Type Mapping

**Elixir**:
```elixir
defmodule Craftplan.Catalog.Product.Types.Status do
  use Ash.Type.Enum,
    values: [
      :draft,
      :testing,
      :active,
      :paused,
      :discontinued,
      :archived
    ]
end
```

**RDF (Turtle)**:
```turtle
cp:ProductStatus a owl:Class ;
    owl:equivalentClass [
        a owl:Class ;
        owl:oneOf (cp:status_draft cp:status_testing cp:status_active
                   cp:status_paused cp:status_discontinued cp:status_archived)
    ] .

cp:status_draft a cp:ProductStatus ;
    rdfs:label "draft" .
cp:status_testing a cp:ProductStatus ;
    rdfs:label "testing" .
cp:status_active a cp:ProductStatus ;
    rdfs:label "active" .
cp:status_paused a cp:ProductStatus ;
    rdfs:label "paused" .
cp:status_discontinued a cp:ProductStatus ;
    rdfs:label "discontinued" .
cp:status_archived a cp:ProductStatus ;
    rdfs:label "archived" .
```

---

## 3. Attribute Constraint Mapping

### Elixir Constraints → SHACL Constraints

| Elixir Constraint | SHACL Constraint | Example |
|-------------------|------------------|---------|
| `allow_nil?: false` | `sh:minCount 1` | Required field |
| `allow_nil?: true` | `sh:minCount 0` | Optional field |
| `default: value` | `sh:defaultValue` | Default value |
| `generated?: true` | `sh:defaultValue` with auto-gen pattern | Auto-generated |
| `writable?: false` | Custom property `cp:readOnly "true"` | Read-only |
| `public?: true` | Custom property `cp:publicRead "true"` | Publicly readable |

#### String Constraints

| Elixir Constraint | SHACL Constraint |
|-------------------|------------------|
| `min_length: n` | `sh:minLength n` |
| `max_length: n` | `sh:maxLength n` |
| `match: ~r/pattern/` | `sh:pattern "pattern"` |
| `allow_empty?: false` | `sh:minLength 1` |

**Example**:
```elixir
attribute :name, :string do
  allow_nil? false
  constraints min_length: 2, max_length: 100, match: ~r/^[\w\s\-\.]+$/
end
```

```turtle
cp:Product_name sh:datatype xsd:string ;
    sh:minCount 1 ;
    sh:minLength 2 ;
    sh:maxLength 100 ;
    sh:pattern "^[\\w\\s\\-\\.]+$" .
```

#### Numeric Constraints

| Elixir Constraint | SHACL Constraint |
|-------------------|------------------|
| `min: n` | `sh:minInclusive n` |
| `max: n` | `sh:maxInclusive n` |
| `greater_than: n` | `sh:minExclusive n` |
| `less_than: n` | `sh:maxExclusive n` |

**Example**:
```elixir
attribute :quantity, :decimal do
  allow_nil? false
  constraints min: 0
end
```

```turtle
cp:BOMComponent_quantity sh:datatype xsd:decimal ;
    sh:minCount 1 ;
    sh:minInclusive 0.0 .
```

#### Enum Constraints

| Elixir Constraint | OWL Representation |
|-------------------|--------------------|
| `constraints one_of: [:a, :b, :c]` | `owl:oneOf` or SHACL `sh:in` |

**Example**:
```elixir
attribute :status, :atom do
  constraints one_of: [:draft, :active, :archived]
end
```

```turtle
cp:BOM_status sh:datatype xsd:string ;
    sh:in ("draft" "active" "archived") .
```

---

## 4. Relationship Mapping

### Elixir Relationships → RDF Object Properties

#### belongs_to

**Elixir**:
```elixir
belongs_to :product, Craftplan.Catalog.Product do
  allow_nil? false
end
```

**RDF**:
```turtle
cp:BOMComponent_product a owl:ObjectProperty ;
    rdfs:domain cp:BOMComponent ;
    rdfs:range cp:Product ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    owl:inverseOf cp:Product_components .
```

#### has_many

**Elixir**:
```elixir
has_many :boms, Craftplan.Catalog.BOM
```

**RDF**:
```turtle
cp:Product_boms a owl:ObjectProperty ;
    rdfs:domain cp:Product ;
    rdfs:range cp:BOM ;
    sh:minCount 0 ;
    owl:inverseOf cp:BOM_product .
```

#### has_one

**Elixir**:
```elixir
has_one :active_bom, Craftplan.Catalog.BOM do
  filter expr(status == :active)
end
```

**RDF**:
```turtle
cp:Product_activeBom a owl:ObjectProperty ;
    rdfs:domain cp:Product ;
    rdfs:range cp:BOM ;
    sh:minCount 0 ;
    sh:maxCount 1 ;
    sh:property [
        sh:path cp:BOM_status ;
        sh:hasValue cp:status_active
    ] .
```

#### many_to_many

**Elixir**:
```elixir
many_to_many :allergens, Craftplan.Inventory.Allergen,
  through: Craftplan.Inventory.MaterialAllergen
```

**RDF** (with explicit join entity):
```turtle
cp:Material_allergens a owl:ObjectProperty ;
    rdfs:domain cp:Material ;
    rdfs:range cp:Allergen ;
    sh:minCount 0 .

cp:MaterialAllergen a owl:Class ;
    rdfs:subClassOf [
        a owl:Restriction ;
        owl:onProperty cp:MaterialAllergen_material ;
        owl:someValuesFrom cp:Material
    ] ;
    rdfs:subClassOf [
        a owl:Restriction ;
        owl:onProperty cp:MaterialAllergen_allergen ;
        owl:someValuesFrom cp:Allergen
    ] .
```

---

## 5. Identity/Uniqueness Mapping

### Elixir Identities → SHACL Unique Keys

**Elixir**:
```elixir
identities do
  identity :sku, [:sku]
  identity :name, [:name]
end
```

**RDF**:
```turtle
cp:Product_shape a sh:NodeShape ;
    sh:targetClass cp:Product ;
    sh:property [
        sh:path cp:Product_sku ;
        sh:uniqueLang true ;  # Marker for uniqueness
        sh:maxCount 1
    ] ;
    sh:property [
        sh:path cp:Product_name ;
        sh:uniqueLang true ;
        sh:maxCount 1
    ] .
```

**Custom RDF for uniqueness constraint**:
```turtle
cp:uniqueSku a sh:NodeShape ;
    sh:targetClass cp:Product ;
    sh:sparql """
        SELECT $this ($sku as $value)
        WHERE {
            $this cp:Product_sku $sku .
            FILTER NOT EXISTS {
                $other cp:Product_sku $sku .
                FILTER ($other != $this)
            }
        }
    """ .
```

---

## 6. Action Mapping

### Elixir Actions → RDF Operations

#### CRUD Actions

**Elixir**:
```elixir
actions do
  defaults [:read, :create, :update, :destroy]
end
```

**RDF** (using SHACL-AF or custom operations ontology):
```turtle
cp:Product a owl:Class ;
    cp:canCreate true ;
    cp:canRead true ;
    cp:canUpdate true ;
    cp:canDelete true .
```

#### Custom Actions

**Elixir**:
```elixir
update :promote do
  change set_attribute(:status, :active)
  change fn cs, _ ->
    Ash.Changeset.change_attribute(cs, :published_at, DateTime.utc_now())
  end
end
```

**RDF**:
```turtle
cp:BOM_promote a cp:Action ;
    rdfs:domain cp:BOM ;
    cp:stateTransition cp:status_draft, cp:status_active ;
    cp:sideEffect """
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
        INSERT {
            ?bom cp:BOM_published_at ?now .
        }
        WHERE {
            ?bom a cp:BOM .
            BIND(NOW() as ?now)
        }
    """ .
```

---

## 7. Policy/Authorization Mapping

### Elixir Policies → RDF Access Control

**Elixir**:
```elixir
policies do
  policy always() do
    authorize_if {Craftplan.Accounts.Checks.ApiScopeCheck, []}
  end

  policy action_type(:read) do
    authorize_if expr(status == :active)
    authorize_if expr(^actor(:role) in [:staff, :admin])
  end

  policy action_type([:create, :update, :destroy]) do
    authorize_if expr(^actor(:role) in [:staff, :admin])
  end
end
```

**RDF** (using WebAC or custom ontology):
```turtle
cp:ProductReadPolicy a cp:AuthorizationPolicy ;
    cp:appliesTo cp:Product ;
    cp:action cp:Read ;
    cp:condition [
        a cp:OrCondition ;
        cp:operand cp:ProductStatusActive ;
        cp:operand cp:ActorRoleStaffOrAdmin
    ] .

cp:ProductWritePolicy a cp:AuthorizationPolicy ;
    cp:appliesTo cp:Product ;
    cp:action cp:Create ;
    cp:action cp:Update ;
    cp:action cp:Delete ;
    cp:condition cp:ActorRoleStaffOrAdmin .

cp:ActorRoleStaffOrAdmin a cp:Condition ;
    sh:sparql """
        SELECT $this ($actor as $value)
        WHERE {
            $actor cp:User_role ?role .
            FILTER (?role IN (cp:role_staff, cp:role_admin))
        }
    """ .
```

---

## 8. Validation Mapping

### Elixir Validations → SHACL/OWL Constraints

#### Inline Validations

**Elixir**:
```elixir
validations do
  validate compare(:completed_qty, less_than_or_equal_to: :planned_qty) do
    message "completed quantity must be less than or equal to planned quantity"
  end
end
```

**RDF**:
```turtle
cp:OrderItemBatchAllocation_validation a sh:NodeShape ;
    sh:targetClass cp:OrderItemBatchAllocation ;
    sh:sparql """
        SELECT $this (?completed as ?value)
        WHERE {
            $this cp:OrderItemBatchAllocation_completedQty ?completed ;
                  cp:OrderItemBatchAllocation_plannedQty ?planned .
            FILTER (?completed > ?planned)
        }
        """ ;
    sh:message "completed quantity must be less than or equal to planned quantity" .
```

#### Custom Validation Module

**Elixir**:
```elixir
validate {Craftplan.Orders.Validations.AllocationProductMatch, []}
```

**RDF**:
```turtle
cp:AllocationProductMatch a sh:SPARQLConstraint ;
    sh:message "Product must match between batch and order item" ;
    sh:sparql """
        SELECT $this (?batchProduct as ?value)
        WHERE {
            $this cp:OrderItemBatchAllocation_productionBatch ?batch ;
                  cp:OrderItemBatchAllocation_orderItem ?item ;
                  cp:OrderItemBatchAllocation_productionBatch cp:ProductionBatch_product ?batchProduct ;
                  cp:OrderItemBatchAllocation_orderItem cp:OrderItem_product ?itemProduct .
            FILTER (?batchProduct != ?itemProduct)
        }
    """ .
```

---

## 9. Calculation Mapping

### Elixir Calculations → RDF Derived Properties

**Elixir**:
```elixir
calculate :cost, :decimal, expr(quantity * unit_price)
```

**RDF**:
```turtle
cp:OrderItem_cost a owl:DatatypeProperty ;
    rdfs:domain cp:OrderItem ;
    rdfs:range xsd:decimal ;
    cp:derivedFrom [
        a cp:ArithmeticExpression ;
        cp:operator cp:Multiply ;
        cp:operand cp:OrderItem_quantity ;
        cp:operand cp:OrderItem_unitPrice
    ] .
```

### Complex Calculation (Module)

**Elixir**:
```elixir
calculate :materials_cost, :decimal,
  Craftplan.Catalog.Product.Calculations.MaterialCost
```

**RDF**:
```turtle
cp:Product_materialsCost a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:decimal ;
    cp:calculatedBy cp:MaterialCostCalculator ;
    cp:calculation """
        PREFIX cp: <http://craftplan.org/ontology#>
        SELECT ?product (?sum as ?cost)
        WHERE {
            ?product cp:Product_activeBom ?bom .
            ?bom cp:BOM_component ?component .
            ?component cp:BOMComponent_quantity ?qty .
            ?component cp:BOMComponent_material ?material .
            ?material cp:Material_price ?price .
            ?bom cp:BOMComponent_wastePercent ?waste .
            BIND(?qty * ?price * (1 + ?waste) as ?sum)
        }
        GROUP BY ?product
    """ .
```

---

## 10. Aggregate Mapping

### Elixir Aggregates → RDF Roll-up Properties

**Elixir**:
```elixir
aggregates do
  count :total_items, :items
  sum :total_cost, :items, :cost
end
```

**RDF**:
```turtle
cp:Order_totalItems a owl:DatatypeProperty ;
    rdfs:domain cp:Order ;
    rdfs:range xsd:integer ;
    cp:aggregateType cp:Count ;
    cp:aggregatePath cp:Order_items .

cp:Order_totalCost a owl:DatatypeProperty ;
    rdfs:domain cp:Order ;
    rdfs:range xsd:decimal ;
    cp:aggregateType cp:Sum ;
    cp:aggregatePath cp:Order_items ;
    cp:aggregateProperty cp:OrderItem_cost .
```

---

## 11. Change/Transformation Mapping

### Elixir Change Modules → RDF Rules

**Elixir**:
```elixir
defmodule Craftplan.Orders.Changes.CalculateTotals do
  use Ash.Resource.Change

  def change(changeset, _opts, _ctx) do
    Changeset.after_action(changeset, fn changeset, order ->
      subtotal = calculate_subtotal(order)
      discount = calculate_discount(order, subtotal)
      total = Decimal.sub(subtotal, discount)

      {:ok, %{order | subtotal: subtotal, discount_total: discount, total: total}}
    end)
  end
end
```

**RDF** (SWRL rule):
```turtle
cp:CalculateTotalsRule a swrl:Rule ;
    swrl:body [
        swrl:ClassAtom(<http://craftplan.org/ontology#Order> swrl:this) ;
        swrl:DataRangeAtom(<http://craftplan.org/ontology#Order> cp:Order_subtotal ?subtotal) ;
        swrl:DataRangeAtom(<http://craftplan.org/ontology#Order> cp:Order_discount_total ?discount)
    ] ;
    swrl:head [
        swrl:DataRangeAtom(<http://craftplan.org/ontology#Order> cp:Order_total ?total)
    ] ;
    swrl:condition "BIND(?subtotal - ?discount AS ?total)" .
```

---

## 12. Domain Organization

### Elixir Ash.Domain → RDF Module Grouping

**Elixir**:
```elixir
defmodule Craftplan.Catalog do
  use Ash.Domain

  resources do
    resource Craftplan.Catalog.Product
    resource Craftplan.Catalog.BOM
    resource Craftplan.Catalog.BOMComponent
    resource Craftplan.Catalog.LaborStep
    resource Craftplan.Catalog.BOMRollup
  end
end
```

**RDF**:
```turtle
cp:Catalog a owl:Module ;
    rdfs:label "Catalog Domain" ;
    cp:containsResource cp:Product ;
    cp:containsResource cp:BOM ;
    cp:containsResource cp:BOMComponent ;
    cp:containsResource cp:LaborStep ;
    cp:containsResource cp:BOMRollup .

cp:CatalogEntity a owl:Class ;
    rdfs:label "Catalog Entity" ;
    owl:equivalentClass [
        a owl:Class ;
        owl:unionOf (cp:Product cp:BOM cp:BOMComponent cp:LaborStep cp:BOMRollup)
    ] .
```

---

## 13. Complete Example: Product Resource

### Elixir Source
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
      constraints min_length: 2, max_length: 100, match: ~r/^[\w\s\-\.]+$/
    end

    attribute :sku, :string do
      allow_nil? false
    end

    attribute :status, Craftplan.Catalog.Product.Types.Status do
      allow_nil? false
      default :draft
    end

    attribute :price, :decimal do
      allow_nil? false
    end

    attribute :photos, {:array, :string} do
      default []
    end

    attribute :max_daily_quantity, :integer do
      allow_nil? false
      default 0
      constraints min: 0
    end
  end

  relationships do
    has_many :boms, Craftplan.Catalog.BOM

    has_one :active_bom, Craftplan.Catalog.BOM do
      filter expr(status == :active)
    end

    has_many :items, Craftplan.Orders.OrderItem
  end

  calculations do
    calculate :materials_cost, :decimal, Craftplan.Catalog.Product.Calculations.MaterialCost
    calculate :bom_unit_cost, :decimal, Craftplan.Catalog.Product.Calculations.UnitCost
    calculate :gross_profit, :decimal, Craftplan.Catalog.Product.Calculations.GrossProfit
  end

  identities do
    identity :sku, [:sku]
    identity :name, [:name]
  end

  actions do
    defaults [:read, :create, :update, :destroy]

    read :list do
      prepare build(sort: :name)
    end
  end

  policies do
    policy action_type(:read) do
      authorize_if expr(status == :active or selling_availability != :off)
      authorize_if expr(^actor(:role) in [:staff, :admin])
    end

    policy action_type([:create, :update, :destroy]) do
      authorize_if expr(^actor(:role) in [:staff, :admin])
    end
  end
end
```

### RDF Equivalent (Turtle)

```turtle
@prefix cp: <http://craftplan.org/ontology#> .
@prefix owl: <http://www.w3.org/2002/07/owl#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

# ===== CLASS DEFINITION =====

cp:Product a owl:Class ;
    rdfs:subClassOf cp:CatalogEntity ;
    rdfs:label "Product" ;
    rdfs:comment "A product in the catalog" ;
    cp:moduleName "Craftplan.Catalog.Product" ;
    cp:tableName "catalog_products" .

# ===== ATTRIBUTES =====

cp:Product_id a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:string ;
    rdfs:label "id" ;
    sh:pattern "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}" ;
    sh:maxCount 1 ;
    cp:primaryKey true .

cp:Product_name a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:string ;
    rdfs:label "name" ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:minLength 2 ;
    sh:maxLength 100 ;
    sh:pattern "^[\\w\\s\\-\\.]+$" .

cp:Product_sku a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:string ;
    rdfs:label "SKU" ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:minLength 2 ;
    sh:maxLength 50 .

cp:Product_status a owl:ObjectProperty ;
    rdfs:domain cp:Product ;
    rdfs:range cp:ProductStatus ;
    rdfs:label "status" ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:defaultValue cp:status_draft .

cp:Product_price a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "price" ;
    sh:minCount 1 ;
    sh:maxCount 1 .

cp:Product_photos a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:string ;
    rdfs:label "photos" ;
    sh:maxCount 1 ;
    sh:defaultValue [] .

cp:Product_maxDailyQuantity a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:integer ;
    rdfs:label "max daily quantity" ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:minInclusive 0 ;
    sh:defaultValue 0 .

# ===== RELATIONSHIPS =====

cp:Product_boms a owl:ObjectProperty ;
    rdfs:domain cp:Product ;
    rdfs:range cp:BOM ;
    rdfs:label "BOMs" ;
    sh:minCount 0 ;
    owl:inverseOf cp:BOM_product .

cp:Product_activeBom a owl:ObjectProperty ;
    rdfs:domain cp:Product ;
    rdfs:range cp:BOM ;
    rdfs:label "active BOM" ;
    sh:minCount 0 ;
    sh:maxCount 1 ;
    sh:property [
        sh:path cp:BOM_status ;
        sh:hasValue cp:status_active
    ] ;
    owl:inverseOf cp:BOM_product .

cp:Product_items a owl:ObjectProperty ;
    rdfs:domain cp:Product ;
    rdfs:range cp:OrderItem ;
    rdfs:label "order items" ;
    sh:minCount 0 ;
    owl:inverseOf cp:OrderItem_product .

# ===== CALCULATIONS =====

cp:Product_materialsCost a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "materials cost" ;
    cp:derivedFrom cp:ActiveBOMRollup ;
    cp:calculation """
        PREFIX cp: <http://craftplan.org/ontology#>
        SELECT ?product (?sum as ?cost)
        WHERE {
            ?product cp:Product_activeBom ?bom .
            ?bom cp:BOM_rollup ?rollup .
            ?rollup cp:BOMRollup_materialCost ?sum .
        }
    """ .

cp:Product_bomUnitCost a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "BOM unit cost" ;
    cp:derivedFrom cp:ActiveBOMRollup ;
    cp:calculation """
        PREFIX cp: <http://craftplan.org/ontology#>
        SELECT ?product (?sum as ?cost)
        WHERE {
            ?product cp:Product_activeBom ?bom .
            ?bom cp:BOM_rollup ?rollup .
            ?rollup cp:BOMRollup_unitCost ?sum .
        }
    """ .

cp:Product_grossProfit a owl:DatatypeProperty ;
    rdfs:domain cp:Product ;
    rdfs:range xsd:decimal ;
    rdfs:label "gross profit" ;
    cp:derivedFrom cp:PriceAndCost ;
    cp:calculation "price - bomUnitCost" .

# ===== IDENTITIES =====

cp:Product_uniqueSku a sh:NodeShape ;
    sh:targetClass cp:Product ;
    sh:sparql """
        SELECT $this ($sku as $value)
        WHERE {
            $this cp:Product_sku $sku .
            FILTER NOT EXISTS {
                $other cp:Product_sku $sku .
                FILTER ($other != $this)
            }
        }
    """ .

cp:Product_uniqueName a sh:NodeShape ;
    sh:targetClass cp:Product ;
    sh:sparql """
        SELECT $this ($name as $value)
        WHERE {
            $this cp:Product_name $name .
            FILTER NOT EXISTS {
                $other cp:Product_name $name .
                FILTER ($other != $this)
            }
        }
    """ .

# ===== POLICIES =====

cp:ProductReadPolicy a cp:AuthorizationPolicy ;
    cp:appliesTo cp:Product ;
    cp:action cp:Read ;
    cp:condition [
        a cp:OrCondition ;
        cp:operand [
            a cp:OrCondition ;
            cp:operand cp:ProductStatusActive ;
            cp:operand cp:ProductSellingAvailabilityNotOff
        ] ;
        cp:operand cp:ActorRoleStaffOrAdmin
    ] .

cp:ProductWritePolicy a cp:AuthorizationPolicy ;
    cp:appliesTo cp:Product ;
    cp:action cp:Create ;
    cp:action cp:Update ;
    cp:action cp:Delete ;
    cp:condition cp:ActorRoleStaffOrAdmin .

# ===== SHACL SHAPE =====

cp:Product_shape a sh:NodeShape ;
    sh:targetClass cp:Product ;
    sh:property [
        sh:path cp:Product_id ;
        sh:maxCount 1 ;
        sh:pattern "[0-9a-f]{8}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{4}-[0-9a-f]{12}"
    ] ;
    sh:property [
        sh:path cp:Product_name ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:minLength 2 ;
        sh:maxLength 100 ;
        sh:pattern "^[\\w\\s\\-\\.]+$"
    ] ;
    sh:property [
        sh:path cp:Product_sku ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:minLength 2 ;
        sh:maxLength 50
    ] ;
    sh:property [
        sh:path cp:Product_status ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] ;
    sh:property [
        sh:path cp:Product_price ;
        sh:minCount 1 ;
        sh:maxCount 1
    ] ;
    sh:property [
        sh:path cp:Product_maxDailyQuantity ;
        sh:minCount 1 ;
        sh:maxCount 1 ;
        sh:minInclusive 0
    ] ;
    sh:property [
        sh:path cp:Product_boms ;
        sh:minCount 0
    ] ;
    sh:property [
        sh:path cp:Product_activeBom ;
        sh:minCount 0 ;
        sh:maxCount 1
    ] .
```

---

## 14. Mapping Templates for ggen

### Template 1: Class Generator

**Input**: RDF class definition
**Output**: Elixir Ash.Resource module

```bash
# ggen template
ggen generate --template ash_resource --input product.ttl --output product.ex
```

**Template Logic**:
1. Read RDF class (e.g., `cp:Product`)
2. Extract `rdfs:subClassOf` → determine `domain` if it's a domain entity
3. Extract attributes → `attributes do ... end`
4. Extract SHACL constraints → `constraints`
5. Extract object properties → `relationships do ... end`
6. Extract SHACL node shape → `validations do ... end`
7. Extract operation permissions → `policies do ... end`
8. Generate Elixir code

### Template 2: Migration Generator

**Input**: RDF class with table metadata
**Output**: Ecto migration

```bash
ggen generate --template ecto_migration --input product.ttl --output 20260204_create_products.exs
```

**Template Logic**:
1. Extract `cp:tableName`
2. Extract primary key (`cp:primaryKey`)
3. Extract all datatype properties → columns
4. Extract SHACL datatypes → Ecto types
5. Extract SHACL constraints → column constraints
6. Extract unique constraints → indexes
7. Generate migration

### Template 3: Domain Generator

**Input**: RDF module/grouping
**Output**: Ash.Domain module

```bash
ggen generate --template ash_domain --input catalog.ttl --output catalog.ex
```

**Template Logic**:
1. Read `cp:Catalog` (owl:Module)
2. Extract `cp:containsResource` statements
3. Generate list of resources
4. Generate Ash.Domain module

### Template 4: Test Generator

**Input**: RDF class with test data
**Output**: Ash test case

```bash
ggen generate --template ash_test --input product_test.ttl --output product_test.exs
```

**Template Logic**:
1. Extract test cases from RDF
2. Generate CRUD tests
3. Generate validation tests from SHACL constraints
4. Generate authorization tests from policies

---

## 15. Best Practices for RDF Modeling

### Naming Conventions

| Elixir | RDF | Example |
|--------|-----|---------|
| Module `Craftplan.Catalog.Product` | Class `cp:Product` | `cp:Product` |
| Attribute `:name` | Property `cp:Product_name` | `cp:Product_name` |
| Relationship `:boms` | Property `cp:Product_boms` | `cp:Product_boms` |
| Enum value `:active` | Individual `cp:status_active` | `cp:status_active` |

### Hierarchy Design

```turtle
cp:Entity a owl:Class .  # Abstract base

cp:CatalogEntity a owl:Class ;
    rdfs:subClassOf cp:Entity .

cp:OrderEntity a owl:Class ;
    rdfs:subClassOf cp:Entity .

cp:InventoryEntity a owl:Class ;
    rdfs:subClassOf cp:Entity .
```

### Reusable Constraints

```turtle
# Common email pattern
cp:emailPattern a sh:PropertyShape ;
    sh:datatype xsd:string ;
    sh:pattern "^[^@]+@[^@]+\\.[^@]+$" .

# Non-empty string
cp:nonEmptyString a sh:PropertyShape ;
    sh:datatype xsd:string ;
    sh:minLength 1 .

# Positive decimal
cp:positiveDecimal a sh:PropertyShape ;
    sh:datatype xsd:decimal ;
    sh:minInclusive 0.0 .
```

### Inverse Relationships

**Always define inverses** for navigation:
```turtle
cp:Product_boms owl:inverseOf cp:BOM_product .
cp:Order_items owl:inverseOf cp:OrderItem_order .
cp:Material_movements owl:inverseOf cp:Movement_material .
```

---

## 16. Validation Strategy

### Three-Level Validation

1. **SHACL Shape Validation** (static schema)
   - Required fields
   - Data types
   - Patterns
   - Ranges

2. **SPARQL Constraint Validation** (dynamic rules)
   - Cross-field validation
   - Cross-resource validation
   - Uniqueness constraints
   - Business rules

3. **Ash Validation** (runtime)
   - Authorization checks
   - Change validations
   - Custom validations

### Example: Multi-Level Validation

```turtle
# Level 1: SHACL Shape
cp:Product_shape sh:property [
    sh:path cp:Product_sku ;
    sh:minCount 1 ;
    sh:maxCount 1 ;
    sh:minLength 2
] .

# Level 2: SPARQL Constraint
cp:Product_uniqueSku a sh:SPARQLConstraint ;
    sh:sparql """
        SELECT $this ($sku as $value)
        WHERE {
            $this cp:Product_sku $sku .
            FILTER NOT EXISTS {
                $other cp:Product_sku $sku .
                FILTER ($other != $this)
            }
        }
    """ .

# Level 3: Ash Validation (in generated code)
# validations do
#   validate {unique_sku, []}
# end
```

---

## 17. Migration Strategy

### Incremental Extraction

1. **Phase 1**: Core entities (Product, Order, Material)
   - Extract basic attributes
   - Create SHACL shapes
   - Generate Ash resources

2. **Phase 2**: Relationships and aggregates
   - Extract foreign keys
   - Extract joins
   - Extract aggregations

3. **Phase 3**: Business logic
   - Extract validations
   - Extract calculations
   - Extract policies
   - Extract changes

4. **Phase 4**: Complex patterns
   - Recursive BOMs
   - Batch workflows
   - Forecasting algorithms

---

## 18. Tooling Recommendations

### RDF Editors
- **Protégé**: Visual ontology editing
- **TopBraid Composer**: Enterprise RDF development
- **Apache Jena**: Command-line RDF processing

### SHACL Validation
- **SHACL Playground**: Online validation testing
- **PySHACL**: Python SHACL engine
- **TopBraid SHACL API**: Java validation

### Code Generation
- **ggen μ Pipeline**: Five-stage transformation
- **Tera Templates**: Rust template engine
- **SPARQL Queries**: Extract RDF data

---

**END OF MAPPING GUIDE**
