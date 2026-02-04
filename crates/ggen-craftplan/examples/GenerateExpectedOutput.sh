#!/bin/bash
# Example script demonstrating how to use ggen-craftplan templates

set -e

echo "=========================================="
echo "ggen-craftplan Template Generation Demo"
echo "=========================================="
echo ""

# Paths
ONTOLOGY="examples/product-catalog.ttl"
OUTPUT_DIR="output/lib/craftplan"
TEST_DIR="output/test/craftplan"

# Create output directories
mkdir -p "$OUTPUT_DIR/catalog"
mkdir -p "$TEST_DIR/catalog"

echo "📝 Example 1: Generating Ash Resource"
echo "---------------------------------------"
cat << 'EOF'
# Input: RDF Class definition
:Product a rdfs:Class ;
    rdfs:label "Product" ;
    rdfs:comment "Catalog product with pricing" ;
    rdfs:subClassOf :CatalogEntity .

:productName a rdf:Property ;
    rdfs:domain :Product ;
    rdfs:range xsd:string ;
    sh:minLength 2 ;
    sh:maxLength 100 .
EOF

echo ""
echo "✅ Output: Ash Resource (lib/craftplan/catalog/product.ex)"
cat << 'EOF'
defmodule Craftplan.Catalog.Product do
  @moduledoc """
  Catalog product with pricing
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

    timestamps()
  end
end
EOF

echo ""
echo "📝 Example 2: Generating Ecto Schema"
echo "---------------------------------------"
cat << 'EOF'
# Output: Ecto Schema (lib/craftplan/catalog/product.ex)
defmodule Craftplan.Catalog.Product do
  use Ecto.Schema

  schema "catalog_products" do
    field :name, :string
    timestamps()
  end

  def changeset(product, attrs) do
    product
    |> cast(attrs, [:name])
    |> validate_required([:name])
    |> validate_length(:name, min: 2, max: 100)
  end
end
EOF

echo ""
echo "📝 Example 3: Generating Test Suite"
echo "---------------------------------------"
cat << 'EOF'
# Output: ExUnit Test (test/craftplan/catalog/product_test.exs)
defmodule Craftplan.Catalog.ProductTest do
  use Craftplan.DataCase

  describe "Product resource" do
    test "creates product with valid name" do
      # Arrange
      actor = staff_actor()
      attrs = %{name: "Test Product"}

      # Act
      {:ok, product} = Ash.create(Product, attrs, actor: actor)

      # Assert
      assert product.name == "Test Product"
    end
  end
end
EOF

echo ""
echo "📝 Example 4: Generating Agent"
echo "---------------------------------------"
cat << 'EOF'
# Output: A2A Agent (lib/craftplan/agents/catalog_agent.ex)
defmodule Craftplan.Agents.CatalogAgent do
  @moduledoc """
  Agent business logic for Catalog domain.

  Handles:
    - Product
    - Bom
  """

  def handle("products.list", _params, actor) do
    case Ash.read(Product, actor: actor, authorize?: true) do
      {:ok, products} ->
        {:ok, %{"products" => Enum.map(products, &product_to_map/1)}}
      {:error, error} -> {:error, error}
    end
  end
end
EOF

echo ""
echo "=========================================="
echo "✨ Template Features Summary"
echo "=========================================="
echo "✅ Ash Resources with attributes, relationships, actions"
echo "✅ Ecto Schemas with validations and changesets"
echo "✅ A2A Agents with JSON-RPC 2.0 support"
echo "✅ ExUnit Tests with AAA pattern"
echo "✅ Property-based testing with PropCheck"
echo "✅ Full type safety with @spec annotations"
echo "✅ Comprehensive documentation"
echo "✅ Deterministic output (same RDF → same code)"
echo ""

echo "📚 See docs/TEMPLATE_REFERENCE.md for complete guide"
echo "🔧 See examples/product-catalog.ttl for RDF examples"
echo ""
