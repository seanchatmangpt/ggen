# Product Recommendations Package

Production-ready AI-powered recommendation engine with RDF ontology, SPARQL queries, and ML models.

## Overview

Comprehensive recommendation system with:
- **Collaborative Filtering**: User-based and item-based similarity
- **Content-Based**: Recommendations from product attributes
- **Hybrid Algorithms**: Combine multiple signals
- **Real-Time**: In-memory recommendation engine (Rust)
- **A/B Testing**: Experiment with algorithm variants

## Features

- ✅ RDF ontology (280+ lines) defining recommendation concepts
- ✅ 10 SPARQL queries for user behavior and recommendations
- ✅ Rust real-time recommendation engine (high-performance)
- ✅ TypeScript/React recommendation widgets
- ✅ Python ML model training (matrix factorization)
- ✅ Chicago TDD tests with real RDF/SPARQL

## Quick Start

### 1. Generate Recommendation Engine

```bash
ggen template render product-recommendations/templates/rust/recommendation_engine.rs \
  --output src/recommendations/engine.rs
```

### 2. Query User Preferences

```bash
ggen query execute product-recommendations/sparql/queries.rq \
  --graph behaviors.ttl \
  --query "Extract User Behavior Preferences"
```

### 3. Train ML Model

```bash
python product-recommendations/templates/python/ml_model_training.py
```

## Architecture

### Recommendation Algorithms

| Algorithm | Type | Description | Use Case |
|-----------|------|-------------|----------|
| **User-Based CF** | Collaborative | Similar users' preferences | Cold-start items |
| **Item-Based CF** | Collaborative | Similar items | Personalization |
| **Content-Based** | Attributes | Product features | Explainability |
| **Hybrid** | Combined | Weighted ensemble | Best accuracy |
| **Association Rules** | Patterns | Frequent item sets | Bundles |

### User Behavior Weights

| Event | Weight | Impact |
|-------|--------|--------|
| View | 1.0 | Minimal |
| Click | 2.0 | Low |
| Add to Cart | 5.0 | Medium |
| Purchase | 10.0 | High |
| 5-Star Rating | 10.0 | High |

## RDF Ontology Classes

- `rec:UserBehavior` - User action (view/click/purchase)
- `rec:Product` - Recommendable product
- `rec:RecommendationAlgorithm` - CF/Content-based/Hybrid
- `rec:UserSegment` - User group (new/active/high-value)
- `rec:ABTest` - Experiment with variants
- `rec:RecommendationContext` - Page type, device, time
- `rec:PersonalizationRule` - Business rules (boost/diversity)

## SPARQL Query Examples

### User-Based Collaborative Filtering
```sparql
PREFIX rec: <http://example.org/recommendations#>
SELECT ?userId1 ?userId2 ?commonProducts WHERE {
    ?user1 foaf:accountName ?userId1 .
    ?user2 foaf:accountName ?userId2 .
    ?user1 rec:hasBehavior ?b1 .
    ?user2 rec:hasBehavior ?b2 .
    ?b1 a rec:PurchaseEvent ; rec:onProduct ?product .
    ?b2 a rec:PurchaseEvent ; rec:onProduct ?product .
    FILTER(?userId1 < ?userId2)
}
GROUP BY ?userId1 ?userId2
HAVING (COUNT(?product) AS ?commonProducts)
```

### Trending Products
```sparql
PREFIX rec: <http://example.org/recommendations#>
SELECT ?productName (COUNT(?view) AS ?viewCount) WHERE {
    ?behavior a rec:ViewEvent ;
              rec:onProduct ?product ;
              rec:eventTimestamp ?timestamp .
    ?product rec:productName ?productName .
    FILTER(?timestamp >= NOW() - (24 * 60 * 60))
}
GROUP BY ?productName
ORDER BY DESC(?viewCount)
LIMIT 50
```

## Code Examples

### Rust: Real-Time Recommendations
```rust
let engine = RecommendationEngine::new();

// Record user behavior
engine.record_behavior(UserBehavior {
    user_id: "user_123".to_string(),
    product_id: "product_A".to_string(),
    behavior_type: BehaviorType::Purchase,
    timestamp: Utc::now().timestamp(),
    score: 10.0,
}).await;

// Generate recommendations
let recommendations = engine.recommend_for_user("user_123", 10).await;

// Bundle recommendations
let bundles = engine.recommend_bundle("product_A", 5).await;
```

### TypeScript: Recommendation Widgets
```tsx
import { RecommendationWidget } from './recommendation-widgets';

function ProductPage({ productId }) {
  return (
    <RecommendationWidget
      userId="user_123"
      context="product-page"
      currentProductId={productId}
      limit={6}
    />
  );
}
```

### Python: ML Model Training
```python
from ml_model_training import MatrixFactorizationModel

# Train model
model = MatrixFactorizationModel(n_factors=20, n_epochs=50)
model.fit(interactions)

# Generate recommendations
recommendations = model.recommend_for_user(
    'user_123',
    n=10,
    exclude_items={'product_A', 'product_B'}
)
```

## A/B Testing

Test different algorithms:
```python
# Control: Item-based CF
# Treatment: Hybrid (60% CF + 40% content-based)

# Metrics: CTR, conversion rate, revenue per user
```

## Testing

Run Chicago TDD tests:
```bash
cargo test --package product-recommendations
```

## License

MIT
