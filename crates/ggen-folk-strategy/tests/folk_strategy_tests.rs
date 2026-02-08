/// Comprehensive folk strategy quantification tests
/// Uses Chicago TDD pattern: Arrange -> Act -> Assert with real objects (no mocks)
use ggen_folk_strategy::*;
use ndarray::Array2;

// ============================================================================
// ARRANGE: Test Fixtures (Real Objects)
// ============================================================================

/// Create a simple opportunity field with a single peak
fn create_peak_field() -> Result<OpportunityField> {
    let mut potential = Array2::zeros((11, 11));

    // Create Gaussian peak at center (5, 5)
    for i in 0..11 {
        for j in 0..11 {
            let x = i as f64 - 5.0;
            let y = j as f64 - 5.0;
            potential[[i, j]] = 10.0 * (-0.1 * (x * x + y * y)).exp();
        }
    }

    OpportunityField::new(potential, (-5.0, 5.0), (-5.0, 5.0))
}

/// Create a valley field (multiple valleys)
fn create_valley_field() -> Result<OpportunityField> {
    let mut potential = Array2::zeros((11, 11));

    // Two valleys at (2,2) and (8,8)
    for i in 0..11 {
        for j in 0..11 {
            let x = i as f64;
            let y = j as f64;

            let valley1 = -5.0 * (-0.1 * ((x - 2.0).powi(2) + (y - 2.0).powi(2))).exp();
            let valley2 = -5.0 * (-0.1 * ((x - 8.0).powi(2) + (y - 8.0).powi(2))).exp();

            potential[[i, j]] = valley1 + valley2;
        }
    }

    OpportunityField::new(potential, (0.0, 10.0), (0.0, 10.0))
}

/// Create a winning trajectory (straight path to peak)
fn create_winning_trajectory() -> Result<Trajectory> {
    let positions = vec![
        Position::new(-4.0, -4.0),
        Position::new(-2.0, -2.0),
        Position::new(0.0, 0.0),
        Position::new(2.0, 2.0),
        Position::new(4.0, 4.0),
    ];
    let time = vec![0.0, 1.0, 2.0, 3.0, 4.0];

    Trajectory::new(time, positions)
}

/// Create a wandering trajectory (inefficient path)
fn create_wandering_trajectory() -> Result<Trajectory> {
    let positions = vec![
        Position::new(-4.0, -4.0),
        Position::new(-2.0, 2.0),
        Position::new(1.0, -1.0),
        Position::new(3.0, 3.0),
        Position::new(4.0, 4.0),
    ];
    let time = vec![0.0, 2.0, 4.0, 6.0, 8.0];

    Trajectory::new(time, positions)
}

/// Create aligned vectors (high PMF)
fn create_aligned_vectors() -> (Vector, Vector) {
    let capability = Vector::new(vec![1.0, 0.5, 0.3]);
    let demand = Vector::new(vec![1.0, 0.5, 0.3]);
    (capability, demand)
}

/// Create misaligned vectors (low PMF)
fn create_misaligned_vectors() -> (Vector, Vector) {
    let capability = Vector::new(vec![1.0, 0.0, 0.0]);
    let demand = Vector::new(vec![0.0, 1.0, 0.0]);
    (capability, demand)
}

// ============================================================================
// ACT & ASSERT: Position Tests
// ============================================================================

#[test]
fn test_position_creation_and_properties() {
    // Arrange
    let p1 = Position::new(0.0, 0.0);
    let p2 = Position::new(3.0, 4.0);

    // Act
    let distance = p1.distance_to(p2);

    // Assert
    assert!((distance - 5.0).abs() < 1e-10, "Distance should be 5.0");
}

#[test]
fn test_position_midpoint() {
    // Arrange
    let p1 = Position::new(0.0, 0.0);
    let p2 = Position::new(4.0, 8.0);

    // Act
    let mid = p1.midpoint(p2);

    // Assert
    assert!((mid.x - 2.0).abs() < 1e-10);
    assert!((mid.y - 4.0).abs() < 1e-10);
}

#[test]
fn test_position_zero_distance() {
    // Arrange
    let p = Position::new(1.5, 2.5);

    // Act
    let distance = p.distance_to(p);

    // Assert
    assert!(distance < 1e-10, "Distance to self should be ~0");
}

// ============================================================================
// Vector Tests
// ============================================================================

#[test]
fn test_vector_magnitude() {
    // Arrange
    let v = Vector::new(vec![3.0, 4.0]);

    // Act
    let mag = v.magnitude();

    // Assert
    assert!((mag - 5.0).abs() < 1e-10);
}

#[test]
fn test_vector_dot_product() {
    // Arrange
    let v1 = Vector::new(vec![1.0, 2.0, 3.0]);
    let v2 = Vector::new(vec![4.0, 5.0, 6.0]);

    // Act
    let dot = v1.dot_product(&v2).unwrap();

    // Assert
    assert!((dot - 32.0).abs() < 1e-10); // 1*4 + 2*5 + 3*6 = 32
}

#[test]
fn test_vector_alignment_perfect() {
    // Arrange
    let v1 = Vector::new(vec![1.0, 0.0, 0.0]);
    let v2 = Vector::new(vec![2.0, 0.0, 0.0]);

    // Act
    let alignment = v1.alignment(&v2).unwrap();

    // Assert
    assert!(
        (alignment - 1.0).abs() < 1e-10,
        "Aligned vectors should have similarity 1.0"
    );
}

#[test]
fn test_vector_alignment_orthogonal() {
    // Arrange
    let v1 = Vector::new(vec![1.0, 0.0]);
    let v2 = Vector::new(vec![0.0, 1.0]);

    // Act
    let alignment = v1.alignment(&v2).unwrap();

    // Assert
    assert!(
        alignment.abs() < 1e-10,
        "Orthogonal vectors should have similarity 0.0"
    );
}

#[test]
fn test_vector_alignment_opposite() {
    // Arrange
    let v1 = Vector::new(vec![1.0, 0.0]);
    let v2 = Vector::new(vec![-1.0, 0.0]);

    // Act
    let alignment = v1.alignment(&v2).unwrap();

    // Assert
    assert!(
        (alignment + 1.0).abs() < 1e-10,
        "Opposite vectors should have similarity -1.0"
    );
}

#[test]
fn test_vector_dimension_mismatch() {
    // Arrange
    let v1 = Vector::new(vec![1.0, 2.0]);
    let v2 = Vector::new(vec![1.0, 2.0, 3.0]);

    // Act
    let result = v1.alignment(&v2);

    // Assert
    assert!(result.is_err(), "Should fail on dimension mismatch");
}

// ============================================================================
// Window Tests
// ============================================================================

#[test]
fn test_window_valid_creation() {
    // Arrange
    let result = Window::new(0.0, 10.0);

    // Act & Assert
    assert!(result.is_ok());
    let window = result.unwrap();
    assert_eq!(window.duration(), 10.0);
}

#[test]
fn test_window_invalid_bounds() {
    // Arrange
    let result = Window::new(10.0, 5.0);

    // Act & Assert
    assert!(result.is_err(), "Should reject invalid bounds");
}

#[test]
fn test_window_contains() {
    // Arrange
    let window = Window::new(5.0, 15.0).unwrap();

    // Act & Assert
    assert!(window.contains(5.0), "Should contain start");
    assert!(window.contains(10.0), "Should contain midpoint");
    assert!(window.contains(15.0), "Should contain end");
    assert!(!window.contains(4.9), "Should exclude before start");
    assert!(!window.contains(15.1), "Should exclude after end");
}

#[test]
fn test_window_feasibility_score() {
    // Arrange
    let window = Window::new(0.0, 10.0).unwrap();

    // Act
    let center_score = window.feasibility_score(5.0);
    let edge_score = window.feasibility_score(0.0);
    let outside_score = window.feasibility_score(15.0);

    // Assert
    assert!(
        (center_score - 1.0).abs() < 1e-10,
        "Center should score 1.0"
    );
    assert!((edge_score - 0.0).abs() < 1e-10, "Edge should score 0.0");
    assert!(outside_score < 1e-10, "Outside should score 0.0");
}

// ============================================================================
// Opportunity Field Tests
// ============================================================================

#[test]
fn test_field_creation_valid() {
    // Arrange & Act
    let field = create_peak_field();

    // Assert
    assert!(field.is_ok(), "Should create valid field");
}

#[test]
fn test_field_empty_error() {
    // Arrange
    let potential = Array2::<f64>::zeros((0, 0));

    // Act
    let result = OpportunityField::new(potential, (-5.0, 5.0), (-5.0, 5.0));

    // Assert
    assert!(result.is_err(), "Should reject empty field");
}

#[test]
fn test_field_invalid_bounds() {
    // Arrange
    let potential = Array2::zeros((10, 10));

    // Act
    let result = OpportunityField::new(potential, (5.0, -5.0), (-5.0, 5.0));

    // Assert
    assert!(result.is_err(), "Should reject invalid x bounds");
}

#[test]
fn test_field_evaluate() {
    // Arrange
    let field = create_peak_field().unwrap();
    let position = Position::new(0.0, 0.0);

    // Act
    let potential = field.evaluate(position);

    // Assert
    assert!(potential.is_ok());
    let value = potential.unwrap();
    assert!(value >= 0.0, "Potential at center should be high");
}

#[test]
fn test_field_evaluate_out_of_bounds() {
    // Arrange
    let field = create_peak_field().unwrap();
    let position = Position::new(10.0, 10.0);

    // Act
    let result = field.evaluate(position);

    // Assert
    assert!(result.is_err(), "Should reject out-of-bounds position");
}

#[test]
fn test_field_gradient() {
    // Arrange
    let field = create_peak_field().unwrap();
    let position = Position::new(0.0, 0.0);

    // Act
    let gradient = field.gradient(position);

    // Assert
    assert!(gradient.is_ok());
    let (grad_x, grad_y) = gradient.unwrap();
    // At peak center, gradient should be ~0
    assert!(grad_x.abs() < 0.5, "Gradient x at peak should be small");
    assert!(grad_y.abs() < 0.5, "Gradient y at peak should be small");
}

#[test]
fn test_field_max_min_potential() {
    // Arrange
    let field = create_peak_field().unwrap();

    // Act
    let max = field.max_potential();
    let min = field.min_potential();

    // Assert
    assert!(max > 0.0, "Max should be positive for peak field");
    assert_eq!(min, 0.0, "Min should be 0 for peak field");
    assert!(max > min, "Max should be greater than min");
}

#[test]
fn test_field_peak() {
    // Arrange
    let field = create_peak_field().unwrap();

    // Act
    let peak = field.peak();

    // Assert
    assert!((peak.x.abs()).abs() < 1.0, "Peak x should be near 0");
    assert!((peak.y.abs()).abs() < 1.0, "Peak y should be near 0");
}

// ============================================================================
// Trajectory Tests
// ============================================================================

#[test]
fn test_trajectory_creation_valid() {
    // Arrange & Act
    let trajectory = create_winning_trajectory();

    // Assert
    assert!(trajectory.is_ok());
}

#[test]
fn test_trajectory_empty_error() {
    // Arrange
    let result = Trajectory::new(vec![], vec![]);

    // Act & Assert
    assert!(result.is_err());
}

#[test]
fn test_trajectory_length_mismatch() {
    // Arrange
    let positions = vec![Position::new(0.0, 0.0), Position::new(1.0, 1.0)];
    let time = vec![0.0];

    // Act
    let result = Trajectory::new(time, positions);

    // Assert
    assert!(result.is_err(), "Should reject length mismatch");
}

#[test]
fn test_trajectory_non_monotonic_time() {
    // Arrange
    let positions = vec![Position::new(0.0, 0.0), Position::new(1.0, 1.0)];
    let time = vec![1.0, 0.0]; // Reversed!

    // Act
    let result = Trajectory::new(time, positions);

    // Assert
    assert!(result.is_err(), "Should reject non-monotonic time");
}

#[test]
fn test_trajectory_velocities() {
    // Arrange
    let trajectory = create_winning_trajectory().unwrap();

    // Act
    let velocities = trajectory.velocities();

    // Assert
    assert!(velocities.is_ok());
    let vels = velocities.unwrap();
    assert_eq!(vels.len(), 5, "Should have 5 velocities");
    assert!(
        vels.iter().all(|v| *v >= 0.0),
        "All velocities should be non-negative"
    );
}

#[test]
fn test_trajectory_accelerations() {
    // Arrange
    let trajectory = create_winning_trajectory().unwrap();

    // Act
    let accelerations = trajectory.accelerations();

    // Assert
    assert!(accelerations.is_ok());
    let accs = accelerations.unwrap();
    assert_eq!(accs.len(), 5, "Should have 5 accelerations");
}

#[test]
fn test_trajectory_path_length() {
    // Arrange
    let positions = vec![
        Position::new(0.0, 0.0),
        Position::new(1.0, 0.0),
        Position::new(1.0, 1.0),
    ];
    let time = vec![0.0, 1.0, 2.0];
    let trajectory = Trajectory::new(time, positions).unwrap();

    // Act
    let length = trajectory.path_length();

    // Assert
    assert!((length - 2.0).abs() < 1e-10, "Path length should be 2.0");
}

#[test]
fn test_trajectory_straight_vs_wandering() {
    // Arrange
    let straight = create_winning_trajectory().unwrap();
    let wandering = create_wandering_trajectory().unwrap();

    // Act
    let straight_length = straight.path_length();
    let wandering_length = wandering.path_length();

    // Assert
    assert!(
        wandering_length > straight_length,
        "Wandering path should be longer"
    );
}

// ============================================================================
// Core Computation Tests (Luck, Timing, Vision, etc.)
// ============================================================================

#[test]
fn test_luck_computation() {
    // Arrange
    let field = create_peak_field().unwrap();
    let position = Position::new(0.0, 0.0); // At peak

    // Act
    let luck_val = luck(position, &field);

    // Assert
    assert!(luck_val.is_ok());
    let luck = luck_val.unwrap();
    assert!(luck > 0.0, "Luck at peak should be positive");
}

#[test]
fn test_timing_within_window() {
    // Arrange
    let window = Window::new(0.0, 10.0).unwrap();
    let entry_time = 5.0;

    // Act
    let result = timing(entry_time, &window);

    // Assert
    assert!(result.is_ok());
    assert!(result.unwrap(), "Should be inside window");
}

#[test]
fn test_timing_outside_window() {
    // Arrange
    let window = Window::new(0.0, 10.0).unwrap();
    let entry_time = 15.0;

    // Act
    let result = timing(entry_time, &window);

    // Assert
    assert!(result.is_ok());
    assert!(!result.unwrap(), "Should be outside window");
}

#[test]
fn test_timing_score() {
    // Arrange
    let window = Window::new(0.0, 10.0).unwrap();

    // Act
    let center_score = timing_score(5.0, &window).unwrap();
    let edge_score = timing_score(0.0, &window).unwrap();

    // Assert
    assert!(
        (center_score - 1.0).abs() < 1e-10,
        "Center should score 1.0"
    );
    assert!((edge_score - 0.0).abs() < 1e-10, "Edge should score 0.0");
}

#[test]
fn test_execution_cost() {
    // Arrange
    let efficient = create_winning_trajectory().unwrap();
    let inefficient = create_wandering_trajectory().unwrap();

    // Act
    let efficient_cost = execution(&efficient).unwrap();
    let inefficient_cost = execution(&inefficient).unwrap();

    // Assert
    assert!(
        inefficient_cost > efficient_cost,
        "Inefficient path should have higher cost"
    );
}

#[test]
fn test_momentum_computation() {
    // Arrange
    let trajectory = create_winning_trajectory().unwrap();

    // Act
    let momentum_val = momentum(&trajectory);

    // Assert
    assert!(momentum_val.is_ok());
    let mom = momentum_val.unwrap();
    assert!(mom >= 0.0, "Momentum should be non-negative");
}

#[test]
fn test_traction_computation() {
    // Arrange
    let trajectory = create_winning_trajectory().unwrap();

    // Act
    let traction_val = traction(&trajectory);

    // Assert
    assert!(traction_val.is_ok());
    let tract = traction_val.unwrap();
    assert!(tract >= 0.0, "Traction should be non-negative");
}

#[test]
fn test_pmf_aligned() {
    // Arrange
    let (capability, demand) = create_aligned_vectors();

    // Act
    let pmf_val = pmf(&capability, &demand);

    // Assert
    assert!(pmf_val.is_ok());
    let pmf = pmf_val.unwrap();
    assert!(pmf > 0.9, "Aligned vectors should have high PMF");
}

#[test]
fn test_pmf_misaligned() {
    // Arrange
    let (capability, demand) = create_misaligned_vectors();

    // Act
    let pmf_val = pmf(&capability, &demand);

    // Assert
    assert!(pmf_val.is_ok());
    let pmf = pmf_val.unwrap();
    assert!(pmf < 0.5, "Misaligned vectors should have low PMF");
}

#[test]
fn test_moat_computation() {
    // Arrange
    let field = create_peak_field().unwrap();
    let position = Position::new(0.0, 0.0);

    // Act
    let moat_val = moat(&field, position);

    // Assert
    assert!(moat_val.is_ok());
    let moat = moat_val.unwrap();
    assert!(moat >= 0.0, "Moat should be non-negative");
}

#[test]
fn test_network_effect_scaling() {
    // Arrange
    let n1 = 10_u32 as usize;
    let n2 = 20_usize;

    // Act
    let nw1 = network_effect(n1).unwrap();
    let nw2 = network_effect(n2).unwrap();

    // Assert
    assert!(
        nw2.value > nw1.value,
        "Network value should increase with users"
    );
}

#[test]
fn test_network_effect_zero() {
    // Arrange
    let n = 0_usize;

    // Act
    let nw = network_effect(n).unwrap();

    // Assert
    assert_eq!(nw.value, 0.0, "Zero users should give zero value");
}

#[test]
fn test_disruption_potential_high() {
    // Arrange
    let incumbent_inertia = 10.0;
    let entrant_inertia = 1.0;

    // Act
    let disruption = disruption_potential(incumbent_inertia, entrant_inertia);

    // Assert
    assert!(disruption.is_ok());
    let d = disruption.unwrap();
    assert!(d > 0.5, "Low entrant inertia should enable disruption");
}

#[test]
fn test_disruption_potential_low() {
    // Arrange
    let incumbent_inertia = 1.0;
    let entrant_inertia = 10.0;

    // Act
    let disruption = disruption_potential(incumbent_inertia, entrant_inertia);

    // Assert
    assert!(disruption.is_ok());
    let d = disruption.unwrap();
    assert!(d <= 0.0, "High entrant inertia should prevent disruption");
}

// ============================================================================
// Decomposition Tests
// ============================================================================

#[test]
fn test_decomposition_components_sum() {
    // Arrange
    let luck = 0.8;
    let execution = 0.6;
    let timing = 0.7;

    // Act
    let decomp = decompose_success(luck, execution / 10.0, timing).unwrap();

    // Assert
    // Total should be weighted average
    let expected = (luck.abs() % 10.0 / 10.0) * 0.3 + (1.0 - execution / 10.0) * 0.4 + timing * 0.3;
    assert!(
        (decomp.total - expected).abs() < 1e-10,
        "Total should be weighted sum"
    );
}

#[test]
fn test_decomposition_bounds() {
    // Arrange
    let luck = 5.5;
    let execution = 0.3;
    let timing = 0.9;

    // Act
    let decomp = decompose_success(luck, execution, timing).unwrap();

    // Assert
    assert!(
        (0.0..=1.0).contains(&decomp.luck_component),
        "Luck should be in [0,1]"
    );
    assert!(
        (0.0..=1.0).contains(&decomp.execution_component),
        "Execution should be in [0,1]"
    );
    assert!(
        (0.0..=1.0).contains(&decomp.timing_component),
        "Timing should be in [0,1]"
    );
    assert!(
        (0.0..=1.0).contains(&decomp.total),
        "Total should be in [0,1]"
    );
}

#[test]
fn test_decomposition_invalid_execution() {
    // Arrange
    let luck = 5.0;
    let execution = 1.5; // Out of bounds
    let timing = 0.5;

    // Act
    let result = decompose_success(luck, execution, timing);

    // Assert
    assert!(result.is_err(), "Should reject invalid execution");
}

#[test]
fn test_decomposition_invalid_timing() {
    // Arrange
    let luck = 5.0;
    let execution = 0.5;
    let timing = 1.5; // Out of bounds

    // Act
    let result = decompose_success(luck, execution, timing);

    // Assert
    assert!(result.is_err(), "Should reject invalid timing");
}

// ============================================================================
// Comprehensive Folk Terms Tests
// ============================================================================

#[test]
fn test_compute_all_folk_terms() {
    // Arrange
    let field = create_peak_field().unwrap();
    let trajectory = create_winning_trajectory().unwrap();
    let (capability, demand) = create_aligned_vectors();
    let position = Position::new(0.0, 0.0);
    let window = Window::new(-1.0, 5.0).unwrap();
    let entry_time = 2.5;

    // Act
    let terms = compute_folk_terms(
        position,
        &field,
        &trajectory,
        &capability,
        &demand,
        entry_time,
        &window,
    );

    // Assert
    assert!(terms.is_ok(), "Should compute all terms");
    let term_list = terms.unwrap();
    assert_eq!(term_list.len(), 67, "Should have exactly 67 terms");

    // All values should be in [0, 1]
    for (term, value) in &term_list {
        assert!(
            (0.0..=1.0).contains(value),
            "Term {:?} has invalid value {:.4}",
            term,
            value
        );
    }
}

#[test]
fn test_folk_terms_determinism() {
    // Arrange
    let field = create_peak_field().unwrap();
    let trajectory = create_winning_trajectory().unwrap();
    let (capability, demand) = create_aligned_vectors();
    let position = Position::new(0.0, 0.0);
    let window = Window::new(-1.0, 5.0).unwrap();
    let entry_time = 2.5;

    // Act - Compute twice
    let terms1 = compute_folk_terms(
        position,
        &field,
        &trajectory,
        &capability,
        &demand,
        entry_time,
        &window,
    )
    .unwrap();

    let terms2 = compute_folk_terms(
        position,
        &field,
        &trajectory,
        &capability,
        &demand,
        entry_time,
        &window,
    )
    .unwrap();

    // Assert
    assert_eq!(
        terms1.len(),
        terms2.len(),
        "Should compute same number of terms"
    );
    for i in 0..terms1.len() {
        assert_eq!(
            terms1[i].0, terms2[i].0,
            "Term at index {} should be same",
            i
        );
        assert!(
            (terms1[i].1 - terms2[i].1).abs() < 1e-14,
            "Values for {:?} should be identical",
            terms1[i].0
        );
    }
}

// ============================================================================
// Competitor Dynamics Tests
// ============================================================================

#[test]
fn test_competitor_dynamics_creation() {
    // Arrange
    let t1 = create_winning_trajectory().unwrap();
    let t2 = create_wandering_trajectory().unwrap();

    // Act
    let dynamics = CompetitorDynamics::new(vec![t1, t2]);

    // Assert
    assert!(dynamics.is_ok());
}

#[test]
fn test_competitor_dynamics_empty() {
    // Arrange
    let trajectories = vec![];

    // Act
    let result = CompetitorDynamics::new(trajectories);

    // Assert
    assert!(result.is_err(), "Should reject empty competitor list");
}

#[test]
fn test_competitor_min_distance() {
    // Arrange
    let t1 = create_winning_trajectory().unwrap();
    let t2 = create_wandering_trajectory().unwrap();
    let dynamics = CompetitorDynamics::new(vec![t1, t2]).unwrap();

    // Act
    let min_dist = dynamics.min_distance();

    // Assert
    assert!(min_dist.is_ok());
    let dist = min_dist.unwrap();
    assert!(dist >= 0.0, "Distance should be non-negative");
}

#[test]
fn test_competitor_concentration() {
    // Arrange
    let trajectory = create_winning_trajectory().unwrap();
    let dynamics = CompetitorDynamics::new(vec![trajectory]).unwrap();

    // Act
    let concentration = dynamics.concentration();

    // Assert
    assert!(concentration.is_ok());
    let conc = concentration.unwrap();
    assert!(
        (0.0..=1.0).contains(&conc),
        "Concentration should be in [0,1]"
    );
}

// ============================================================================
// Integration Tests (Realistic Scenarios)
// ============================================================================

#[test]
fn test_realistic_startup_scenario() {
    // Arrange: Small startup entering a market
    let field = create_peak_field().unwrap();
    let startup_trajectory = create_winning_trajectory().unwrap();
    let (technical_capability, market_demand) = create_aligned_vectors();
    let market_entry_position = Position::new(-4.0, -4.0);
    let market_window = Window::new(0.0, 4.0).unwrap();
    let entry_time = 0.5;

    // Act
    let luck_val = luck(market_entry_position, &field).unwrap();
    let execution_val = execution(&startup_trajectory).unwrap();
    let pmf_val = pmf(&technical_capability, &market_demand).unwrap();
    let momentum_val = momentum(&startup_trajectory).unwrap();

    // Assert
    assert!(luck_val > 0.0, "Startup should find some luck");
    assert!(pmf_val > 0.8, "Technical startup should have good PMF");
    assert!(
        momentum_val > 0.0,
        "Trajectory should have positive momentum"
    );

    // Decompose success
    let decomp = decompose_success(luck_val, execution_val / 10.0, 0.9).unwrap();
    assert!(
        decomp.total > 0.3,
        "Startup should have reasonable success score"
    );
}

#[test]
fn test_competitive_market_scenario() {
    // Arrange: Multiple competitors in same market
    let field = create_valley_field().unwrap();
    let competitor1 = create_winning_trajectory().unwrap();
    let competitor2 = create_wandering_trajectory().unwrap();
    let dynamics = CompetitorDynamics::new(vec![competitor1, competitor2]).unwrap();

    // Act
    let concentration = dynamics.concentration().unwrap();
    let min_dist = dynamics.min_distance().unwrap();

    // Assert
    assert!(
        concentration > 0.0,
        "Competitors should have measurable concentration"
    );
    assert!(min_dist > 0.0, "Competitors should be separate");
}

#[test]
fn test_timing_advantage_scenario() {
    // Arrange: Early vs late entry
    let field = create_peak_field().unwrap();
    let window = Window::new(2.0, 6.0).unwrap();

    // Act
    let early_score = timing_score(2.5, &window).unwrap();
    let optimal_score = timing_score(4.0, &window).unwrap();
    let late_score = timing_score(5.9, &window).unwrap();

    // Assert
    assert!(
        optimal_score > early_score,
        "Optimal timing should beat early"
    );
    assert!(
        optimal_score > late_score,
        "Optimal timing should beat late"
    );
}

#[test]
fn test_network_growth_scenario() {
    // Arrange: Network growing over time
    let sizes = vec![1, 10, 100, 1000, 10000];

    // Act
    let values: Vec<_> = sizes
        .iter()
        .map(|&n| network_effect(n).unwrap().value)
        .collect();

    // Assert
    for i in 0..values.len() - 1 {
        assert!(
            values[i + 1] > values[i],
            "Network value should increase monotonically"
        );
    }
}
