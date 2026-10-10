# Business Intelligence & Reporting

Advanced BI platform with OLAP, dashboards, KPIs, and dimensional modeling.

## Features

- **Data Warehouse**: Dimensional modeling and ETL
- **OLAP Cubes**: Multidimensional analysis and drill-down
- **Dashboards**: Interactive visualizations
- **KPI Tracking**: Scorecards and targets
- **Scheduled Reports**: Automated report generation
- **Alert System**: Threshold-based notifications

## Quick Start

```typescript
import { BusinessIntelligence } from '@ggen/bi-reporting';

const bi = new BusinessIntelligence();

// Create OLAP cube
const cube = await bi.createCube({
  name: 'Sales Analysis',
  factTable: 'sales_fact',
  dimensions: ['time', 'geography', 'product']
});

// Create dashboard
const dashboard = await bi.createDashboard({
  name: 'Executive Dashboard',
  widgets: [
    { type: 'kpi', metric: 'monthly_revenue' },
    { type: 'chart', chartType: 'line', data: 'revenue_trend' }
  ]
});

// Set up alerts
await bi.createAlert({
  kpi: 'monthly_revenue',
  condition: 'less_than',
  threshold: 1000000,
  notify: ['ceo@example.com']
});
```

## Documentation

- RDF Ontology: 310+ lines
- SPARQL Queries: 15 templates
- Chicago TDD Tests: 650+ lines

See full documentation at https://docs.ggen.ai/packages/bi-reporting
