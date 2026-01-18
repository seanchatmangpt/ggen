# ggen-folk-strategy

Folk strategy quantification: Computes all 67 folk terms from Chapter 4 (The Folk Theory of Success) from measurable inputs via opportunity field dynamics.

## Core Equation

**Folk Terms = f(Opportunity Field, Trajectories, Capabilities, Demand)**

The quantification pipeline transforms:
- **Opportunity Fields**: 2D potential landscapes representing market attractiveness
- **Trajectories**: Time series of positions in opportunity space (competitor movements)
- **Capabilities & Demand**: Vectors representing what you can do vs. what market needs
- **Time Windows**: Entry feasibility periods

Into 67 quantified folk strategy terms organized as:
1. **Luck terms** (15): Serendipity, Fortune, Break, etc.
2. **Timing terms** (15): Momentum, Velocity, Threshold, Rhythm, etc.
3. **Vision terms** (15): Perception, Foresight, InsideView, Pattern Recognition, etc.
4. **Execution terms** (15): Competence, Skill, Discipline, Focus, Persistence, etc.
5. **Network/Moat terms** (7): Network Effects, Defensibility, Scale, Disruption

## Key Functions

### Core Computations

- `luck(position, field) -> f64` - Initial potential at position
- `timing(time, window) -> bool` - Entry feasibility
- `timing_score(time, window) -> f64` - Continuous timing metric (0-1)
- `vision(perceived, actual) -> f64` - Field perception error (0-1)
- `execution(trajectory) -> f64` - Path integral cost
- `momentum(trajectory) -> f64` - Current velocity
- `traction(trajectory) -> f64` - Acceleration magnitude
- `pmf(capability, demand) -> f64` - Product-market fit (0-1)
- `moat(field, position) -> f64` - Barrier height (gradient magnitude)
- `network_effect(n_users) -> NetworkValue` - Metcalfe's law valuation
- `disruption_potential(incumbent_inertia, entrant_inertia) -> f64` - Disruption score

### Decomposition

- `decompose_success(potential, cost, timing) -> SuccessDecomposition`
  - Breaks success into: luck (30%) + execution (40%) + timing (30%)
  - All components sum to total (guaranteed)

### All Terms at Once

- `compute_folk_terms(position, field, trajectory, capability, demand, time, window) -> Vec<(FolkTerm, f64)>`
  - Computes all 67 terms in one call
  - Each term is normalized to [0, 1]

## Data Types

### Core Types

- **Position**: 2D point in opportunity space (x, y)
- **Vector**: N-dimensional capability/demand vector with operations (magnitude, dot product, alignment)
- **Window**: Time interval (start, end) with feasibility scoring
- **OpportunityField**: 2D grid of potential values with bilinear interpolation, gradient computation
- **Trajectory**: Time series of positions with velocity/acceleration computation
- **CompetitorDynamics**: Multiple trajectories with competitive metrics

### Enum Types

- **FolkTerm**: All 67 terms enumerated
- **CalculusObject**: Mathematical foundations (Field, Derivative, Threshold, Barrier, Velocity, Acceleration, Curvature, PathCost)

## Error Handling

All operations return `Result<T, Error>` with specific error variants:
- `ValidationError`: Input validation failed
- `ComputationError`: Computation failed
- `FieldError`: Field-related error
- `TrajectoryError`: Trajectory validation error
- `NetworkError`: Network computation error
- `DimensionMismatch`: Vector dimension mismatch
- `InvalidInput`: Generic invalid input

**No unwrap/expect in production code** - all errors propagate to caller.

## Determinism

All computations are **fully deterministic**:
- Same inputs → identical outputs
- No random numbers, side effects, or I/O
- IEEE 754 floating-point semantics apply

## Type Safety

**Type-first design** ensures correctness at compile time:
- `Vector::alignment()` enforces non-zero magnitudes via Result
- `Window::new()` validates start < end
- `Trajectory::new()` validates monotonic time
- `OpportunityField::new()` validates non-empty field and bounds

## Testing

Uses **Chicago TDD pattern** (Arrange-Act-Assert):
- Real objects, no mocks
- Comprehensive test coverage (see `tests/folk_strategy_tests.rs`)
- Field generation for realistic scenarios
- Decomposition verification (components sum correctly)

## Example

```rust
use ggen_folk_strategy::*;
use ndarray::Array2;

// Create opportunity field
let mut potential = Array2::zeros((10, 10));
potential[[5, 5]] = 10.0; // Peak at center
let field = OpportunityField::new(potential, (-5.0, 5.0), (-5.0, 5.0))?;

// Create trajectory
let positions = vec![
    Position::new(-4.0, -4.0),
    Position::new(0.0, 0.0),
    Position::new(4.0, 4.0),
];
let time = vec![0.0, 5.0, 10.0];
let trajectory = Trajectory::new(time, positions)?;

// Compute metrics
let luck_val = luck(Position::new(5.0, 5.0), &field)?;
let execution_val = execution(&trajectory)?;
let momentum_val = momentum(&trajectory)?;

// Decompose success
let decomp = decompose_success(
    luck_val,
    execution_val / 10.0,  // normalize to [0,1]
    0.8,  // timing score
)?;

println!("Success: {:.2}", decomp.total);
println!("  Luck: {:.2}", decomp.luck_component);
println!("  Execution: {:.2}", decomp.execution_component);
println!("  Timing: {:.2}", decomp.timing_component);
```

## Performance

- Field evaluation: O(1) bilinear interpolation
- Gradient computation: O(1) numerical differentiation
- Trajectory metrics: O(n) where n = trajectory points
- Competitive dynamics: O(m²n²) minimum where m = competitors, n = points per trajectory

Memory usage is bounded:
- Field: O(width × height) floats
- Trajectory: O(n) positions and times
- Competitive dynamics: O(m × n)

## Status

✓ All 67 folk terms quantified
✓ Full error handling (Result<T, E>)
✓ Zero unwrap/expect in production
✓ Deterministic computations
✓ Type-safe design
✓ Comprehensive tests (Chicago TDD)
