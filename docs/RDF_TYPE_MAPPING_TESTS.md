# RDF Type Mapping Validation Tests

## Overview

This document describes the comprehensive RDF type mapping validation tests added to the Chicago TDD E2E test suite. These tests ensure that ontology changes correctly propagate to generated code across multiple target languages.

## Test Location

**File:** `tests/chicago_tdd/ontology_driven_e2e.rs`

**New Test Function:** `test_comprehensive_rdf_type_mapping_validation()`

## Test Coverage

### 1. XSD Type Mappings

The test validates correct type conversions from XSD (XML Schema Definition) types to target language types:

#### xsd:string
- **Rust:** `String`
- **TypeScript:** `string`
- **Python:** `str`

**Test Properties:**
- `name: xsd:string`
- `sku: xsd:string` (Stock Keeping Unit)
- `description: xsd:string`

#### xsd:decimal
- **Rust:** `f64`
- **TypeScript:** `number`
- **Python:** `Decimal`

**Test Properties:**
- `price: xsd:decimal`
- `rating: xsd:decimal`

#### xsd:integer
- **Rust:** `i32`
- **TypeScript:** `number`
- **Python:** `int`

**Test Properties:**
- `quantity: xsd:integer`
- `inventory_count: xsd:integer`

#### xsd:boolean
- **Rust:** `bool`
- **TypeScript:** `boolean`
- **Python:** `bool`

**Test Property:**
- `is_active: xsd:boolean`

#### xsd:dateTime
- **Rust:** `String` (with note: could use `chrono::DateTime<Utc>`)
- **TypeScript:** `string` (with note: could use `Date`)
- **Python:** `str` (with note: could use `datetime`)

**Test Property:**
- `created_at: xsd:dateTime`

### 2. RDFS Class Mappings

The test validates that `rdfs:Class` definitions generate appropriate language constructs:

- **Rust:** `struct` with `#[derive(Debug, Clone, Serialize, Deserialize)]`
- **TypeScript:** `interface` (for data) + `class` (for behavior)
- **Python:** `class` with `@dataclass` decorator

**Test Classes:**
- `Product`
- `Category`
- `Supplier`

### 3. RDF Property Mappings

The test validates that `rdf:Property` relationships generate getter methods:

#### Object Properties (relationships to other classes)

**Test Properties:**
- `category: rdf:Property` with `rdfs:range pc:Category`
- `supplier: rdf:Property` with `rdfs:range pc:Supplier`

**Generated Methods:**
- **Rust:** `pub fn get_category(&self) -> Option<Category>`
- **TypeScript:** `getCategory(): Category | null`
- **Python:** `def get_category(self) -> Optional['Category']`

### 4. Product.sku Ontology Change Validation

The test validates that when the ontology is modified to add new SKU-related properties, the changes correctly propagate to all generated code:

#### Version 1 (Initial)
- `sku: xsd:string` - Basic SKU field

#### Version 2 (Enhanced)
- `sku: xsd:string` - Original field (retained)
- `sku_prefix: xsd:string` - **NEW:** SKU prefix for categorization
- `sku_sequence: xsd:integer` - **NEW:** SKU sequence number

**Validation:**
The test ensures that after regenerating code from the v2 ontology:
- All three languages include `sku_prefix` field with correct type
- All three languages include `sku_sequence` field with correct type
- Original `sku` field is preserved

## Test Workflow

The test follows a comprehensive 5-step workflow:

### Step 1: Verify Type Mappings
- Create comprehensive ontology with all XSD types
- Query ontology using SPARQL to verify structure
- Assert at least 8 properties with different types exist

### Step 2: Generate Rust Code
- Generate Rust structs from ontology
- Validate all type mappings (xsd:* → Rust types)
- Validate rdfs:Class → struct
- Validate rdf:Property → getter methods

### Step 3: Generate TypeScript Code
- Generate TypeScript interfaces/classes from ontology
- Validate all type mappings (xsd:* → TypeScript types)
- Validate rdfs:Class → interface
- Validate rdf:Property → getter methods

### Step 4: Generate Python Code
- Generate Python dataclasses from ontology
- Validate all type mappings (xsd:* → Python types)
- Validate rdfs:Class → class
- Validate rdf:Property → getter methods

### Step 5: Test Product.sku Changes
- Modify ontology to add `sku_prefix` and `sku_sequence`
- Regenerate code for all three languages
- Validate new fields appear in all languages with correct types

## Supporting Functions

### `map_rdf_type_to_language(rdf_type: &str, language: TargetLanguage) -> String`

Enhanced type mapping function that supports multiple target languages:
- Handles all common XSD types
- Returns language-specific type strings
- Supports Rust, TypeScript, and Python

### `create_comprehensive_type_ontology(path: &PathBuf) -> Result<()>`

Creates a comprehensive RDF ontology with:
- 3 classes: Product, Category, Supplier
- 8+ data properties covering all XSD types
- 2 object properties for relationship testing

### `modify_ontology_add_extra_sku_properties(source: &PathBuf, dest: &PathBuf) -> Result<()>`

Modifies an existing ontology to add:
- `sku_prefix: xsd:string`
- `sku_sequence: xsd:integer`

### `generate_code_with_type_mapping(ontology: &PathBuf, output: &PathBuf, lang: TargetLanguage) -> Result<()>`

Generates code from ontology for specified target language:
- Queries ontology for classes and properties
- Applies language-specific type mappings
- Generates appropriate language constructs

### `generate_struct_for_class_multi_language(ontology: &PathBuf, class: &str, lang: TargetLanguage) -> Result<String>`

Generates struct/interface/class for a specific RDF class:
- Separates data properties from object properties
- Generates fields with correct types
- Generates getter methods for relationships

## Example Generated Code

### Rust
```rust
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Product {
    pub name: String,
    pub sku: String,
    pub price: f64,
    pub rating: f64,
    pub quantity: i32,
    pub inventory_count: i32,
    pub is_active: bool,
    pub created_at: String,
}

impl Product {
    pub fn get_category(&self) -> Option<Category> {
        // Fetch related object from database
        None
    }

    pub fn get_supplier(&self) -> Option<Supplier> {
        // Fetch related object from database
        None
    }
}
```

### TypeScript
```typescript
export interface Product {
  name: string;
  sku: string;
  price: number;
  rating: number;
  quantity: number;
  inventory_count: number;
  is_active: boolean;
  created_at: string;
}

export class ProductImpl implements Product {
  name: string;
  sku: string;
  price: number;
  // ... other fields

  getCategory(): Category | null {
    // Fetch related object
    return null;
  }

  getSupplier(): Supplier | null {
    // Fetch related object
    return null;
  }
}
```

### Python
```python
from decimal import Decimal
from dataclasses import dataclass
from typing import Optional

@dataclass
class Product:
    name: str
    sku: str
    price: Decimal
    rating: Decimal
    quantity: int
    inventory_count: int
    is_active: bool
    created_at: str

    def get_category(self) -> Optional['Category']:
        # Fetch related object
        return None

    def get_supplier(self) -> Optional['Supplier']:
        # Fetch related object
        return None
```

## Production Readiness Enhancements

This test suite significantly increases production readiness by:

1. **Multi-Language Support:** Validates code generation for Rust, TypeScript, and Python
2. **Type Safety:** Ensures correct type mappings prevent runtime errors
3. **Change Propagation:** Verifies ontology changes correctly update all generated code
4. **Relationship Handling:** Tests object property conversion to getter methods
5. **Comprehensive Coverage:** Tests all common XSD data types
6. **Regression Prevention:** Ensures v1 → v2 migrations don't break existing functionality

## Chicago TDD Principles

This test follows Chicago TDD (state-based testing) principles:

- ✅ **Real Systems:** Uses real RDF graphs with Oxigraph
- ✅ **Real SPARQL:** Executes actual SPARQL queries
- ✅ **Real File I/O:** Writes and reads actual files
- ✅ **Real Code Generation:** Generates actual compilable code
- ✅ **State Verification:** Validates final state of generated code
- ✅ **No Mocks:** Tests real system behavior, not mocked interactions

## Running the Tests

```bash
# Run all Chicago TDD E2E tests
cargo test --test chicago_tdd_main

# Run only the comprehensive type mapping test
cargo test --test chicago_tdd_main test_comprehensive_rdf_type_mapping_validation

# Run with output
cargo test --test chicago_tdd_main test_comprehensive_rdf_type_mapping_validation -- --nocapture
```

## Future Enhancements

Potential areas for expansion:

1. **Additional Languages:** Add support for Java, Go, C#
2. **Complex Types:** Support for arrays, nested objects, unions
3. **Validation Rules:** Generate validation code from OWL constraints
4. **Documentation Generation:** Auto-generate API docs from ontology comments
5. **Migration Scripts:** Generate database migration scripts from ontology changes
6. **GraphQL Schema:** Generate GraphQL schemas from RDF ontologies

## Related Files

- `tests/chicago_tdd/ontology_driven_e2e.rs` - Main test file
- `crates/ggen-core/src/rdf/schema.rs` - RDF schema definitions
- `crates/ggen-ai/src/rdf/generator.rs` - Code generation orchestration
- `crates/ggen-domain/src/template/render_with_rdf.rs` - Template rendering

## References

- [Chicago School TDD](https://martinfowler.com/articles/mocksArentStubs.html)
- [RDF Schema Specification](https://www.w3.org/TR/rdf-schema/)
- [XSD Datatypes](https://www.w3.org/TR/xmlschema-2/)
- [SPARQL Query Language](https://www.w3.org/TR/sparql11-query/)
