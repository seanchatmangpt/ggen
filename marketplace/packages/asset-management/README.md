# Asset Management

Enterprise asset management with tracking, depreciation, maintenance, and lifecycle management.

## Features

- **Asset Register**: Complete inventory with tracking
- **Barcode/RFID**: Physical asset tracking
- **Depreciation**: Automated depreciation schedules
- **Maintenance**: Preventive and corrective maintenance
- **Lifecycle**: Acquisition to disposal tracking
- **Warranty/Insurance**: Coverage and claims tracking

## Quick Start

```rust
use asset_management::*;

// Create asset
let asset = am.create_asset(CreateAssetRequest {
    name: "Dell Laptop".to_string(),
    serial_number: "SN12345".to_string(),
    category: "IT Equipment".to_string(),
    acquisition_cost: 1200.00,
})?;

// Track location
am.track_location(&asset.id, "Building A - Floor 2")?;

// Schedule maintenance
am.schedule_maintenance(&asset.id, MaintenanceType::Preventive)?;
```

## Documentation

- RDF Ontology: 270+ lines
- SPARQL Queries: 10 templates
- Chicago TDD Tests: 530+ lines

See full documentation at https://docs.ggen.ai/packages/asset-management
