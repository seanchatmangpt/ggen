# Tera Template Reference - Elixir Code Generation

Complete guide for Tera templates that generate idiomatic Elixir code from RDF specifications for the craftplan ERP system.

## Table of Contents

- [Overview](#overview)
- [Template System Architecture](#template-system-architecture)
- [Template Variables](#template-variables)
- [RDF to Elixir Mapping](#rdf-to-elixir-mapping)
- [Template Reference](#template-reference)
- [Transformation Examples](#transformation-examples)
- [Best Practices](#best-practices)

## Overview

The ggen-craftplan template system transforms RDF ontologies into production-ready Elixir code using the Tera template engine. All templates follow the craftplan ERP patterns:

- **Ash Framework**: Resources with attributes, relationships, actions, policies
- **Ecto Schemas**: Database schemas with validations
- **A2A Agents**: Agent-to-Agent protocol handlers
- **ExUnit Tests**: AAA pattern (Arrange/Act/Assert)
- **Phoenix LiveViews**: Interactive UI components

### Design Principles

1. **Determinism**: Same RDF input → identical Elixir code (byte-for-byte)
2. **Type Safety**: Full typespecs on all public functions
3. **Documentation**: Complete `@moduledoc` and `@doc` annotations
4. **Testing**: Property-based tests with PropCheck
5. **Formatting**: `mix format` compatible output

## Template System Architecture

### Template Hierarchy

```
templates/elixir/
├── ash_resource.ex.tera      # Ash.Resource definitions (300+ lines)
├── ecto_schema.ex.tera       # Ecto.Schema definitions (200+ lines)
├── agent.ex.tera              # A2A protocol handlers (400+ lines)
├── test.ex.tera               # ExUnit test suites (150+ lines)
└── module.ex.tera             # Basic Elixir modules (100+ lines)
```

### Template Processing Pipeline

1. **μ₁ (Normalize)**: RDF validation, SHACL shapes, dependency resolution
2. **μ₂ (Extract)**: SPARQL queries extract entities, attributes, relationships
3. **μ₃ (Emit)**: Tera renders templates with extracted data
4. **μ₄ (Canonicalize)**: Format output with `mix format` rules
5. **μ₅ (Receipt)**: Generate SHA-256 hash for reproducibility verification

## Template Variables

### Core Variables

| Variable | Type | Description | Example |
|----------|------|-------------|---------|
| `{{ entity_name }}` | String | Entity name (PascalCase) | `"Product"` |
| `{{ module_path }}` | String | Full module path | `"Craftplan.Catalog.Product"` |
| `{{ table_name }}` | String | Database table name | `"catalog_products"` |
| `{{ domain }}` | String | Domain name | `"catalog"` |
| `{{ doc_comments }}` | String | Documentation from RDF | `"Product catalog entity"` |
| `{{ module_prefix }}` | String | Application prefix | `"Craftplan"` |

### Attribute Variables

Each attribute in `{{ attributes }}` array contains:

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `name` | String | Attribute name (snake_case) | `"product_name"` |
| `type` | String | Elixir type | `:string`, `:decimal`, `:atom` |
| `allow_nil` | Boolean | Whether null is allowed | `false` |
| `default` | String | Default value expression | `"~U[2024-01-01 00:00:00Z]"` |
| `constraints` | Map | Type constraints | `{min_length: 2, max_length: 100}` |
| `public` | Boolean | API visibility | `true` |
| `unique` | Boolean | Whether field is unique | `true` |
| `description` | String | Field documentation | `"Product SKU"` |
| `sensitive` | Boolean | PII/redacted field | `false` |

### Relationship Variables

Each relationship in `{{ relationships }}` array contains:

| Field | Type | Description | Example |
|-------|------|-------------|---------|
| `name` | String | Relationship name | `"customer"` |
| `kind` | String | Relationship type | `"belongs_to"`, `"has_many"`, `"has_one"`, `"many_to_many"` |
| `related` | String | Related module | `"Craftplan.CRM.Customer"` |
| `destination` | String | Destination attribute (optional) | `"customer_id"` |
| `through` | String | Join module (many_to_many) | `"Craftplan.Catalog.ProductMaterial"` |
| `allow_nil` | Boolean | Whether relationship can be nil | `false` |

### Constraint Variables

SHACL constraints in `{{ constraints }}` array:

| Constraint | Type | Description | Example |
|------------|------|-------------|---------|
| `min_length` | Integer | Minimum string length | `2` |
| `max_length` | Integer | Maximum string length | `100` |
| `min` | Integer/Decimal | Minimum numeric value | `0` |
| `max` | Integer/Decimal | Maximum numeric value | `999999` |
| `pattern` | String | Regex pattern | `"^[\w\s\-\.]+$"` |
| `one_of` | Array | Enum values | `[:active, :draft, :archived]` |

### Agent Variables

For agent templates:

| Variable | Type | Description | Example |
|----------|------|-------------|---------|
| `{{ domain_name }}` | String | Domain name | `"catalog"` |
| `{{ capabilities }}` | Array | Domain capabilities | `["product", "bom", "component"]` |
| `{{ methods }}` | Array | Available methods | `["list", "get", "create", "update"]` |

## RDF to Elixir Mapping

### RDF Classes → Elixir Modules

**RDF Input**:
```turtle
:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "Catalog product entity" ;
    rdfs:subClassOf :CatalogEntity .
```

**Elixir Output**:
```elixir
defmodule Craftplan.Catalog.Product do
  @moduledoc """
  Catalog product entity
  """

  use Ash.Resource,
    otp_app: :craftplan,
    domain: Craftplan.Catalog,
    # ...
end
```

### RDF Properties → Attributes

**RDF Input**:
```turtle
:productName a rdf:Property ;
    rdfs:domain :Product ;
    rdfs:range xsd:string ;
    sh:datatypes "string" ;
    sh:minLength 2 ;
    sh:maxLength 100 ;
    sh:pattern "^[\w\s\-\.]+$" .
```

**Elixir Output**:
```elixir
attribute :name, :string do
  allow_nil? false
  public? true

  constraints min_length: 2,
              max_length: 100,
              match: ~r/^[\w\s\-\.]+$/
end
```

### RDF Object Properties → Relationships

**RDF Input**:
```turtle
:productCustomer a owl:ObjectProperty ;
    rdfs:domain :Order ;
    rdfs:range :Customer ;
    rdfs:subPropertyOf :belongsTo .
```

**Elixir Output**:
```elixir
relationships do
  belongs_to :customer, Craftplan.CRM.Customer do
    allow_nil? false
  end
end
```

### SHACL Constraints → Validations

**RDF Input**:
```turtle
:ProductShape a sh:NodeShape ;
    sh:targetClass :Product ;
    sh:property [
        sh:path :productName ;
        sh:minLength 2 ;
        sh:maxLength 100 ;
        sh:pattern "^[\w\s\-\.]+$"
    ] .
```

**Elixir Output**:
```elixir
validations do
  validate {String.Length, min: 2, max: 100, field: :name}
  validate {String.Match, pattern: ~r/^[\w\s\-\.]+$/, field: :name}
end
```

## Template Reference

### ash_resource.ex.tera

**Purpose**: Generate complete Ash.Resource definitions

**Features**:
- JSON:API and GraphQL endpoints
- PostgreSQL data layer configuration
- CRUD actions (create, read, update, destroy)
- Policy-based authorization
- Relationships (belongs_to, has_many, has_one, many_to_many)
- Calculations and aggregates
- Identities (unique constraints)

**Example Usage**:
```bash
ggen generate ash_resource \
  --ontology product.ttl \
  --output lib/craftplan/catalog/product.ex
```

**Generated Output Structure**:
```elixir
defmodule Craftplan.Catalog.Product do
  use Ash.Resource, [...]
  # JSON:API routes
  # GraphQL types
  # PostgreSQL config
  # Actions (create, read, update, destroy, list)
  # Policies (authorization)
  # Attributes (fields)
  # Relationships
  # Calculations
  # Identities
end
```

### ecto_schema.ex.tera

**Purpose**: Generate Ecto schema definitions

**Features**:
- Schema with field definitions
- Changeset with validations
- CRUD functions (create, update, delete, list, get)
- Association casting
- Unique constraints

**Example Usage**:
```bash
ggen generate ecto_schema \
  --ontology product.ttl \
  --output lib/craftplan/catalog/product.ex
```

**Generated Output Structure**:
```elixir
defmodule Craftplan.Catalog.Product do
  use Ecto.Schema

  schema "catalog_products" do
    field :name, :string
    # ... fields
    timestamps()
  end

  def changeset(product, attrs) do
    # ... validations
  end

  def create(attrs), do: ...
  def update(product, attrs), do: ...
  def delete(product), do: ...
end
```

### agent.ex.tera

**Purpose**: Generate A2A (Agent-to-Agent) protocol handlers

**Features**:
- JSON-RPC 2.0 message handling
- Domain capability routing
- Ash integration with authorization
- Error handling and response formatting
- CRUD operations for all entities
- Search functionality

**Example Usage**:
```bash
ggen generate agent \
  --domain catalog \
  --capabilities product,bom,component \
  --output lib/craftplan/agents/catalog_agent.ex
```

**Generated Output Structure**:
```elixir
defmodule Craftplan.Agents.CatalogAgent do
  @behaviour Craftplan.Agents.AgentBehaviour

  def handle("products.list", params, actor), do: ...
  def handle("products.get", params, actor), do: ...
  def handle("products.create", params, actor), do: ...
  def handle("products.update", params, actor), do: ...
  def handle("products.delete", params, actor), do: ...
  def handle("products.search", params, actor), do: ...
end
```

### test.ex.tera

**Purpose**: Generate ExUnit test suites with AAA pattern

**Features**:
- Chicago TDD: AAA pattern (Arrange/Act/Assert)
- Property-based tests with PropCheck
- Factory helpers for test data
- Authorization tests
- Validation tests
- Association tests

**Example Usage**:
```bash
ggen generate test \
  --ontology product.ttl \
  --output test/craftplan/catalog/product_test.exs
```

**Generated Output Structure**:
```elixir
defmodule Craftplan.Catalog.ProductTest do
  use Craftplan.DataCase

  describe "Product resource" do
    test "creates product with valid name" do
      # Arrange
      actor = staff_actor()
      attrs = valid_product_attrs()

      # Act
      {:ok, product} = Ash.create(Product, attrs, actor: actor)

      # Assert
      assert product.name == attrs.name
    end
  end

  describe "property-based tests" do
    use PropCheck

    property "name constraints always hold" do
      forall name <- valid_name_generator() do
        # ... property test
      end
    end
  end
end
```

### module.ex.tera

**Purpose**: Generate basic Elixir modules with CRUD operations

**Features**:
- Public API functions
- Typespecs on all functions
- Documentation with examples
- Error handling with Result types
- Association loading helpers

**Example Usage**:
```bash
ggen generate module \
  --ontology product.ttl \
  --output lib/craftplan/catalog/products.ex
```

**Generated Output Structure**:
```elixir
defmodule Craftplan.Catalog.Products do
  @type result :: {:ok, term()} | {:error, term()}

  @spec get_by_name(String.t()) :: result()
  def get_by_name(name), do: ...

  @spec create(map()) :: result()
  def create(attrs), do: ...

  @spec update(%Product{}, map()) :: result()
  def update(product, attrs), do: ...

  @spec list(keyword()) :: [%Product{}]
  def list(opts \\ []), do: ...
end
```

## Transformation Examples

### Example 1: Simple Product Resource

**Input RDF**:
```turtle
@prefix : <http://craftplan.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix sh: <http://www.w3.org/ns/shacl#> .

:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "Catalog product" ;
    rdfs:subClassOf :CatalogEntity .

:productName a rdf:Property ;
    rdfs:domain :Product ;
    rdfs:range xsd:string ;
    sh:minLength 2 ;
    sh:maxLength 100 .

:productSku a rdf:Property ;
    rdfs:domain :Product ;
    rdfs:range xsd:string ;
    sh:unique true .
```

**Output Elixir** (abbreviated):
```elixir
defmodule Craftplan.Catalog.Product do
  @moduledoc """
  Catalog product
  """

  use Ash.Resource,
    otp_app: :craftplan,
    domain: Craftplan.Catalog,
    data_layer: AshPostgres.DataLayer

  attributes do
    uuid_primary_key :id

    attribute :name, :string do
      allow_nil? false
      public? true
      constraints min_length: 2, max_length: 100
    end

    attribute :sku, :string do
      allow_nil? false
    end

    timestamps()
  end

  identities do
    identity :unique_sku, [:sku]
  end
end
```

### Example 2: Order with Relationships

**Input RDF**:
```turtle
:Order a rdfs:Class ;
    rdfs:label "Order" ;
    rdfs:comment "Customer order" ;
    rdfs:subClassOf :OrderEntity .

:orderCustomer a owl:ObjectProperty ;
    rdfs:domain :Order ;
    rdfs:range :Customer ;
    rdfs:subPropertyOf :belongsTo .

:OrderItem a rdfs:Class ;
    rdfs:label "OrderItem" ;
    rdfs:comment "Line item in order" .

:orderItems a owl:ObjectProperty ;
    rdfs:domain :Order ;
    rdfs:range :OrderItem ;
    rdfs:subPropertyOf :hasMany .
```

**Output Elixir**:
```elixir
defmodule Craftplan.Orders.Order do
  use Ash.Resource, [...]

  relationships do
    belongs_to :customer, Craftplan.CRM.Customer do
      allow_nil? false
    end

    has_many :items, Craftplan.Orders.OrderItem
  end
end
```

### Example 3: Agent Generation

**Input Configuration**:
```json
{
  "domain": "catalog",
  "capabilities": ["product", "bom", "component"],
  "actions": ["list", "get", "create", "update", "delete", "search"]
}
```

**Output Elixir** (abbreviated):
```elixir
defmodule Craftplan.Agents.CatalogAgent do
  @moduledoc """
  Agent business logic for Catalog domain.

  Handles:
    - Product
    - Bom
    - Component
  """

  def handle("products.list", _params, actor) do
    case Ash.read(Product, actor: actor, authorize?: true, action: :list) do
      {:ok, products} ->
        {:ok, %{"products" => Enum.map(products, &product_to_map/1)}}
      {:error, error} -> {:error, error}
    end
  end

  # ... more handlers
end
```

## Best Practices

### 1. Template Design

- **Modular**: Keep templates focused on single responsibility
- **Reusable**: Use template inheritance for common patterns
- **Documented**: Comment complex template logic
- **Tested**: Include example RDF → Elixir transformations

### 2. RDF Modeling

- **Semantic**: Use proper RDF/OWL semantics for relationships
- **Validated**: Include SHACL shapes for all constraints
- **Documented**: Provide `rdfs:comment` for all classes and properties
- **Consistent**: Follow naming conventions (PascalCase classes, snake_case properties)

### 3. Generated Code Quality

- **Formatted**: All output should pass `mix format`
- **Typed**: Include `@spec` and `@type` annotations
- **Tested**: Generate comprehensive tests with property-based testing
- **Idiomatic**: Follow Elixir and Ash framework conventions

### 4. Error Handling

```elixir
# Always return Result types
@spec create(map()) :: {:ok, %Product{}} | {:error, Ash.Changeset.t()}
def create(attrs) do
  # Implementation
end

# Pattern match on results
case Ash.create(Product, attrs, actor: actor) do
  {:ok, product} -> {:ok, product_to_map(product)}
  {:error, changeset} -> {:error, changeset_errors(changeset)}
end
```

### 5. Authorization

```elixir
policies do
  # Always check scope first
  policy always() do
    authorize_if Craftplan.Accounts.Checks.ApiScopeCheck
  end

  # Admin bypass
  bypass expr(^actor(:role) == :admin) do
    authorize_if always()
  end

  # Role-based access
  policy action_type(:read) do
    authorize_if expr(^actor(:role) in [:staff, :admin])
  end
end
```

### 6. Testing

```elixir
# Chicago TDD: AAA Pattern
test "creates product with valid attributes" do
  # Arrange
  actor = staff_actor()
  attrs = valid_product_attrs()

  # Act
  {:ok, product} = Ash.create(Product, attrs, actor: actor)

  # Assert
  assert product.name == attrs.name
  assert product.sku == attrs.sku
end

# Property-based testing
property "name constraints always hold" do
  forall name <- valid_name_generator() do
    # Test property
  end
end
```

## Template Filters

The template system provides custom Tera filters:

| Filter | Input | Output | Example |
|--------|-------|--------|---------|
| `elixir_module_name` | `"product"` | `"Product"` | `{{ entity_name | elixir_module_name }}` |
| `elixir_var_name` | `"Product"` | `"product"` | `{{ entity_name | elixir_var_name }}` |
| `to_snake` | `"ProductName"` | `"product_name"` | `{{ attr | to_snake }}` |
| `to_pascal` | `"product_name"` | `"ProductName"` | `{{ attr | to_pascal }}` |
| `capitalize` | `"catalog"` | `"Catalog"` | `{{ domain | capitalize }}` |
| `lower` | `"Catalog"` | `"catalog"` | `{{ domain | lower }}` |

## Deterministic Output

All templates generate deterministic output:

1. **Stable field ordering**: Attributes sorted by name
2. **Consistent formatting**: Follow `mix format` rules
3. **Predictable names**: Module names follow conventions
4. **Reproducible hashes**: Same RDF → identical SHA-256 hash

### Verification

```bash
# Generate code
ggen generate ash_resource --ontology product.ttl --output product.ex

# Verify format
mix format product.ex

# Verify reproducibility
ggen verify --ontology product.ttl --generated product.ex
```

## Troubleshooting

### Common Issues

**Issue**: Template variable not found
```bash
Error: Failed to render template: Variable 'entity_name' not found
```
**Solution**: Ensure RDF ontology includes `rdfs:label` for all classes

**Issue**: Generated code doesn't compile
```bash
Error: compilation failed due to type mismatch
```
**Solution**: Verify SHACL datatypes map correctly to Elixir types

**Issue**: Missing relationships
```bash
Error: Relationship destination not found
```
**Solution**: Ensure both domain and range are defined for object properties

## See Also

- [ggen-core Documentation](../ggen-core/README.md)
- [RDF/SHACL Specification](https://www.w3.org/TR/shacl/)
- [Ash Framework Guide](https://hexdocs.pm/ash/)
- [Elixir Style Guide](https://github.com/nccstyle/dirac-elixir-style-guide)

---

**Version**: 0.1.0
**Last Updated**: 2026-02-04
**Maintainer**: ggen Contributors
