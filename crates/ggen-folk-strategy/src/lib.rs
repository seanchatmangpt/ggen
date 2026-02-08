#![doc = include_str!("../README.md")]
#![deny(warnings)]

use ndarray::{Array1, Array2};
use serde::{Deserialize, Serialize};
use std::f64::consts::PI;
use std::fmt;
use thiserror::Error;

/// Folk strategy quantification error types
#[derive(Error, Debug)]
pub enum Error {
    #[error("Validation error: {0}")]
    ValidationError(String),

    #[error("Computation error: {0}")]
    ComputationError(String),

    #[error("Field error: {0}")]
    FieldError(String),

    #[error("Trajectory error: {0}")]
    TrajectoryError(String),

    #[error("Network error: {0}")]
    NetworkError(String),

    #[error("Dimension mismatch: expected {expected}, got {actual}")]
    DimensionMismatch { expected: usize, actual: usize },

    #[error("Invalid input: {0}")]
    InvalidInput(String),
}

/// Result type for folk strategy computations
pub type Result<T> = std::result::Result<T, Error>;

/// 67 Folk strategy terms from Chapter 4 - The Folk Theory of Success
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum FolkTerm {
    // Luck-related (1-15)
    Luck,
    Serendipity,
    Fortune,
    ChanceEncounter,
    RightPlace,
    RightTime,
    Opportunity,
    Break,
    Luck1,
    Luck2,
    Luck3,
    Luck4,
    Luck5,
    Luck6,
    Luck7,

    // Timing-related (16-30)
    Timing,
    Momentum,
    Velocity,
    EntryPoint,
    WindowOfOpportunity,
    CriticalMoment,
    Threshold,
    Inflection,
    Acceleration,
    Speed,
    Pace,
    Rhythm,
    Cadence,
    Tempo,
    Timing1,

    // Vision-related (31-45)
    Vision,
    Perception,
    Foresight,
    InsideView,
    OutsideView,
    Intuition,
    Pattern,
    Recognition,
    Insight,
    Hunch,
    SituationalAwareness,
    EnvironmentalReading,
    MarketSense,
    Prescience,
    Vision1,

    // Execution-related (46-60)
    Execution,
    Competence,
    Skill,
    Capability,
    Discipline,
    Focus,
    Persistence,
    Resilience,
    Hustle,
    WorkEthic,
    Grind,
    Effort,
    Action,
    Follow,
    Execution1,

    // Network/Moat-related (61-67)
    Network,
    Moat,
    Defensibility,
    Scale,
    Connection,
    Traction,
    Disruption,
}

/// Calculus object types - the mathematical foundations
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum CalculusObject {
    /// Scalar potential value at a point
    Field(f64),
    /// Derivative (gradient) along a direction
    Derivative(f64),
    /// Threshold value
    Threshold(f64),
    /// Barrier height
    Barrier(f64),
    /// Velocity magnitude
    Velocity(f64),
    /// Acceleration magnitude
    Acceleration(f64),
    /// Curvature of trajectory
    Curvature(f64),
    /// Cost of execution path
    PathCost(f64),
}

/// 2D Position in opportunity space
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Position {
    pub x: f64,
    pub y: f64,
}

impl Position {
    /// Create a new position
    pub fn new(x: f64, y: f64) -> Self {
        Self { x, y }
    }

    /// Compute Euclidean distance to another position
    pub fn distance_to(&self, other: Position) -> f64 {
        ((self.x - other.x).powi(2) + (self.y - other.y).powi(2)).sqrt()
    }

    /// Compute midpoint between two positions
    pub fn midpoint(&self, other: Position) -> Position {
        Position {
            x: (self.x + other.x) / 2.0,
            y: (self.y + other.y) / 2.0,
        }
    }
}

impl fmt::Display for Position {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "({:.3}, {:.3})", self.x, self.y)
    }
}

/// Vector for capability or demand representation
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct Vector {
    pub components: Array1<f64>,
}

impl Vector {
    /// Create a vector from components
    pub fn new(components: Vec<f64>) -> Self {
        Self {
            components: Array1::from_vec(components),
        }
    }

    /// Magnitude (L2 norm)
    pub fn magnitude(&self) -> f64 {
        self.components.iter().map(|x| x * x).sum::<f64>().sqrt()
    }

    /// Dot product with another vector
    pub fn dot_product(&self, other: &Vector) -> Result<f64> {
        if self.components.len() != other.components.len() {
            return Err(Error::DimensionMismatch {
                expected: self.components.len(),
                actual: other.components.len(),
            });
        }
        Ok(self
            .components
            .iter()
            .zip(&other.components)
            .map(|(a, b)| a * b)
            .sum())
    }

    /// Cosine similarity (alignment metric)
    pub fn alignment(&self, other: &Vector) -> Result<f64> {
        let dot = self.dot_product(other)?;
        let mag1 = self.magnitude();
        let mag2 = other.magnitude();

        if mag1 < 1e-10 || mag2 < 1e-10 {
            return Err(Error::ValidationError(
                "Vector magnitude too small for alignment computation".to_string(),
            ));
        }

        Ok((dot / (mag1 * mag2)).clamp(-1.0, 1.0))
    }
}

/// Time window for opportunity entry
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct Window {
    pub start: f64,
    pub end: f64,
}

impl Window {
    /// Create a new time window
    pub fn new(start: f64, end: f64) -> Result<Self> {
        if start >= end {
            return Err(Error::ValidationError(
                "Window start must be less than end".to_string(),
            ));
        }
        Ok(Self { start, end })
    }

    /// Check if a time is within the window
    pub fn contains(&self, time: f64) -> bool {
        time >= self.start && time <= self.end
    }

    /// Window duration
    pub fn duration(&self) -> f64 {
        self.end - self.start
    }

    /// Feasibility score (how well a time aligns with window center)
    pub fn feasibility_score(&self, time: f64) -> f64 {
        if !self.contains(time) {
            return 0.0;
        }
        let center = (self.start + self.end) / 2.0;
        let half_width = self.duration() / 2.0;
        let distance = (time - center).abs();
        1.0 - (distance / half_width)
    }
}

/// 2D opportunity field with potential landscape
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpportunityField {
    /// Potential values at grid points (rows=x, cols=y)
    pub potential: Array2<f64>,
    /// X-axis bounds (min, max)
    pub x_bounds: (f64, f64),
    /// Y-axis bounds (min, max)
    pub y_bounds: (f64, f64),
}

impl OpportunityField {
    /// Create a field from potential matrix and bounds
    pub fn new(potential: Array2<f64>, x_bounds: (f64, f64), y_bounds: (f64, f64)) -> Result<Self> {
        if potential.is_empty() {
            return Err(Error::FieldError(
                "Potential field cannot be empty".to_string(),
            ));
        }
        if x_bounds.0 >= x_bounds.1 || y_bounds.0 >= y_bounds.1 {
            return Err(Error::FieldError(
                "Bounds must satisfy min < max".to_string(),
            ));
        }
        Ok(Self {
            potential,
            x_bounds,
            y_bounds,
        })
    }

    /// Evaluate potential at position (with bilinear interpolation)
    pub fn evaluate(&self, pos: Position) -> Result<f64> {
        let (x, y) = (pos.x, pos.y);

        if x < self.x_bounds.0 || x > self.x_bounds.1 || y < self.y_bounds.0 || y > self.y_bounds.1
        {
            return Err(Error::FieldError(format!(
                "Position {:.3},{:.3} out of bounds",
                x, y
            )));
        }

        let nx = self.potential.nrows();
        let ny = self.potential.ncols();

        // Normalize position to grid indices
        let x_idx = ((x - self.x_bounds.0) / (self.x_bounds.1 - self.x_bounds.0)) * (nx - 1) as f64;
        let y_idx = ((y - self.y_bounds.0) / (self.y_bounds.1 - self.y_bounds.0)) * (ny - 1) as f64;

        let x_i = x_idx.floor() as usize;
        let y_i = y_idx.floor() as usize;
        let x_f = x_idx - x_i as f64;
        let y_f = y_idx - y_i as f64;

        // Bilinear interpolation
        let x_i_next = (x_i + 1).min(nx - 1);
        let y_i_next = (y_i + 1).min(ny - 1);

        let v00 = self.potential[[x_i, y_i]];
        let v10 = self.potential[[x_i_next, y_i]];
        let v01 = self.potential[[x_i, y_i_next]];
        let v11 = self.potential[[x_i_next, y_i_next]];

        let v0 = v00 * (1.0 - x_f) + v10 * x_f;
        let v1 = v01 * (1.0 - x_f) + v11 * x_f;
        let v = v0 * (1.0 - y_f) + v1 * y_f;

        Ok(v)
    }

    /// Compute gradient (approx) at position
    pub fn gradient(&self, pos: Position) -> Result<(f64, f64)> {
        let delta = 0.001;
        let pos_x_plus = Position::new(pos.x + delta, pos.y);
        let pos_x_minus = Position::new(pos.x - delta, pos.y);
        let pos_y_plus = Position::new(pos.x, pos.y + delta);
        let pos_y_minus = Position::new(pos.x, pos.y - delta);

        let grad_x = (self.evaluate(pos_x_plus)? - self.evaluate(pos_x_minus)?) / (2.0 * delta);
        let grad_y = (self.evaluate(pos_y_plus)? - self.evaluate(pos_y_minus)?) / (2.0 * delta);

        Ok((grad_x, grad_y))
    }

    /// Maximum potential in field
    pub fn max_potential(&self) -> f64 {
        self.potential
            .iter()
            .copied()
            .fold(f64::NEG_INFINITY, f64::max)
    }

    /// Minimum potential in field
    pub fn min_potential(&self) -> f64 {
        self.potential.iter().copied().fold(f64::INFINITY, f64::min)
    }

    /// Find position of maximum potential
    pub fn peak(&self) -> Position {
        let (nx, ny) = self.potential.dim();
        let mut max_val = f64::NEG_INFINITY;
        let mut max_x = 0;
        let mut max_y = 0;

        for i in 0..nx {
            for j in 0..ny {
                if self.potential[[i, j]] > max_val {
                    max_val = self.potential[[i, j]];
                    max_x = i;
                    max_y = j;
                }
            }
        }

        // Convert grid indices back to position
        let x = self.x_bounds.0
            + (max_x as f64 / (nx - 1) as f64) * (self.x_bounds.1 - self.x_bounds.0);
        let y = self.y_bounds.0
            + (max_y as f64 / (ny - 1) as f64) * (self.y_bounds.1 - self.y_bounds.0);

        Position::new(x, y)
    }
}

/// Time series trajectory in opportunity space
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Trajectory {
    /// Time points
    pub time: Vec<f64>,
    /// Position at each time
    pub positions: Vec<Position>,
}

impl Trajectory {
    /// Create a new trajectory
    pub fn new(time: Vec<f64>, positions: Vec<Position>) -> Result<Self> {
        if time.is_empty() || positions.is_empty() {
            return Err(Error::TrajectoryError(
                "Time and positions cannot be empty".to_string(),
            ));
        }
        if time.len() != positions.len() {
            return Err(Error::TrajectoryError(format!(
                "Time and positions length mismatch: {} vs {}",
                time.len(),
                positions.len()
            )));
        }

        // Verify time is monotonically increasing
        for i in 0..time.len() - 1 {
            if time[i] >= time[i + 1] {
                return Err(Error::TrajectoryError(
                    "Time values must be monotonically increasing".to_string(),
                ));
            }
        }

        Ok(Self { time, positions })
    }

    /// Compute velocity at each time point
    pub fn velocities(&self) -> Result<Vec<f64>> {
        if self.positions.len() < 2 {
            return Err(Error::TrajectoryError(
                "Need at least 2 points to compute velocity".to_string(),
            ));
        }

        let mut velocities = Vec::with_capacity(self.positions.len());

        for i in 0..self.positions.len() {
            if i == 0 {
                // Forward difference at start
                let dt = self.time[1] - self.time[0];
                let ds = self.positions[0].distance_to(self.positions[1]);
                velocities.push(ds / dt);
            } else if i == self.positions.len() - 1 {
                // Backward difference at end
                let dt = self.time[i] - self.time[i - 1];
                let ds = self.positions[i - 1].distance_to(self.positions[i]);
                velocities.push(ds / dt);
            } else {
                // Central difference
                let dt = self.time[i + 1] - self.time[i - 1];
                let ds = self.positions[i - 1].distance_to(self.positions[i + 1]);
                velocities.push(ds / dt);
            }
        }

        Ok(velocities)
    }

    /// Compute acceleration at each time point
    pub fn accelerations(&self) -> Result<Vec<f64>> {
        let velocities = self.velocities()?;

        if velocities.len() < 2 {
            return Err(Error::TrajectoryError(
                "Need at least 2 velocity points to compute acceleration".to_string(),
            ));
        }

        let mut accelerations = Vec::with_capacity(velocities.len());

        for i in 0..velocities.len() {
            if i == 0 {
                let dt = self.time[1] - self.time[0];
                accelerations.push((velocities[1] - velocities[0]) / dt);
            } else if i == velocities.len() - 1 {
                let dt = self.time[i] - self.time[i - 1];
                accelerations.push((velocities[i] - velocities[i - 1]) / dt);
            } else {
                let dt = self.time[i + 1] - self.time[i - 1];
                accelerations.push((velocities[i + 1] - velocities[i - 1]) / dt);
            }
        }

        Ok(accelerations)
    }

    /// Path integral (total distance traveled)
    pub fn path_length(&self) -> f64 {
        let mut length = 0.0;
        for i in 0..self.positions.len() - 1 {
            length += self.positions[i].distance_to(self.positions[i + 1]);
        }
        length
    }
}

/// Multiple competitors in shared opportunity field
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompetitorDynamics {
    pub trajectories: Vec<Trajectory>,
}

impl CompetitorDynamics {
    /// Create competitive dynamics from trajectories
    pub fn new(trajectories: Vec<Trajectory>) -> Result<Self> {
        if trajectories.is_empty() {
            return Err(Error::ComputationError(
                "At least one trajectory required".to_string(),
            ));
        }
        Ok(Self { trajectories })
    }

    /// Compute minimum distance between any two competitors at any time
    pub fn min_distance(&self) -> Result<f64> {
        if self.trajectories.len() < 2 {
            return Err(Error::ComputationError(
                "Need at least 2 competitors for distance".to_string(),
            ));
        }

        let mut min_dist = f64::INFINITY;

        for i in 0..self.trajectories.len() {
            for j in (i + 1)..self.trajectories.len() {
                for p1 in &self.trajectories[i].positions {
                    for p2 in &self.trajectories[j].positions {
                        let dist = p1.distance_to(*p2);
                        min_dist = min_dist.min(dist);
                    }
                }
            }
        }

        Ok(min_dist)
    }

    /// Compute concentration (inverse of spread)
    pub fn concentration(&self) -> Result<f64> {
        if self.trajectories.is_empty() {
            return Err(Error::ComputationError("No trajectories".to_string()));
        }

        // Collect all positions
        let all_positions: Vec<Position> = self
            .trajectories
            .iter()
            .flat_map(|t| &t.positions)
            .copied()
            .collect();

        if all_positions.is_empty() {
            return Err(Error::ComputationError("No positions found".to_string()));
        }

        // Compute centroid
        let centroid_x =
            all_positions.iter().map(|p| p.x).sum::<f64>() / all_positions.len() as f64;
        let centroid_y =
            all_positions.iter().map(|p| p.y).sum::<f64>() / all_positions.len() as f64;
        let centroid = Position::new(centroid_x, centroid_y);

        // Compute average distance from centroid
        let avg_distance = all_positions
            .iter()
            .map(|p| p.distance_to(centroid))
            .sum::<f64>()
            / all_positions.len() as f64;

        if avg_distance < 1e-10 {
            return Ok(1.0); // Perfect concentration
        }

        // Normalize: concentration = 1/(1 + avg_distance)
        Ok(1.0 / (1.0 + avg_distance))
    }
}

/// Compute luck: Initial potential at position in opportunity field
pub fn luck(x0: Position, field: &OpportunityField) -> Result<f64> {
    field.evaluate(x0)
}

/// Compute timing: Entry feasibility given time and window
pub fn timing(entry_time: f64, window: &Window) -> Result<bool> {
    Ok(window.contains(entry_time))
}

/// Compute timing score (continuous) instead of boolean
pub fn timing_score(entry_time: f64, window: &Window) -> Result<f64> {
    Ok(window.feasibility_score(entry_time))
}

/// Compute vision: Perception error between perceived and actual field
pub fn vision(perceived_field: &OpportunityField, actual_field: &OpportunityField) -> Result<f64> {
    // Sample at peaks to compute error
    let perceived_peak = perceived_field.peak();
    let actual_peak = actual_field.peak();

    let perceived_val = perceived_field.evaluate(perceived_peak)?;
    let actual_val = actual_field.evaluate(actual_peak)?;

    let error = (perceived_val - actual_val).abs();
    let relative_error = error / (actual_val.abs().max(1.0));

    Ok(relative_error.clamp(0.0, 1.0))
}

/// Compute execution: Minimize path integral cost (lower is better execution)
pub fn execution(trajectory: &Trajectory) -> Result<f64> {
    let path_length = trajectory.path_length();

    // Penalize time taken
    let duration = trajectory
        .time
        .last()
        .ok_or_else(|| Error::TrajectoryError("Empty trajectory".to_string()))?
        - trajectory
            .time
            .first()
            .ok_or_else(|| Error::TrajectoryError("Empty trajectory".to_string()))?;

    if duration < 1e-10 {
        return Err(Error::TrajectoryError(
            "Duration too small for execution computation".to_string(),
        ));
    }

    // Normalized cost: lower distance with reasonable speed is good
    let cost = path_length / duration;

    Ok(cost)
}

/// Compute momentum: Current velocity in trajectory
pub fn momentum(trajectory: &Trajectory) -> Result<f64> {
    let velocities = trajectory.velocities()?;
    let final_velocity = velocities
        .last()
        .ok_or_else(|| Error::TrajectoryError("No velocity computed".to_string()))?;

    Ok(*final_velocity)
}

/// Compute traction: Acceleration magnitude
pub fn traction(trajectory: &Trajectory) -> Result<f64> {
    let accelerations = trajectory.accelerations()?;
    let final_acceleration = accelerations
        .last()
        .ok_or_else(|| Error::TrajectoryError("No acceleration computed".to_string()))?;

    Ok(final_acceleration.abs())
}

/// Compute product-market fit (PMF): Alignment between capability and demand
pub fn pmf(capability: &Vector, demand: &Vector) -> Result<f64> {
    // Alignment metric: cosine similarity
    let alignment = capability.alignment(demand)?;

    // PMF score: (alignment + 1) / 2 to normalize to [0, 1]
    Ok((alignment + 1.0) / 2.0)
}

/// Compute moat: Barrier height at position in field
pub fn moat(field: &OpportunityField, position: Position) -> Result<f64> {
    // Moat is measured by local curvature (steep = high barrier)
    let (grad_x, grad_y) = field.gradient(position)?;
    let gradient_magnitude = (grad_x * grad_x + grad_y * grad_y).sqrt();

    Ok(gradient_magnitude)
}

/// Network value type
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct NetworkValue {
    pub value: f64,
}

impl NetworkValue {
    pub fn new(value: f64) -> Self {
        Self {
            value: value.max(0.0),
        }
    }
}

/// Compute network effect: Value increases with number of users (Metcalfe's law)
pub fn network_effect(n_users: usize) -> Result<NetworkValue> {
    if n_users == 0 {
        return Ok(NetworkValue::new(0.0));
    }

    // Metcalfe's law: value ~ n^2
    // Normalize by reference size (e.g., 1000 users = 1.0 value unit)
    let n_float = n_users as f64;
    let reference = 1000.0;
    let value = (n_float / reference).powi(2);

    Ok(NetworkValue::new(value))
}

/// Compute disruption potential: Ratio of entrant vs incumbent inertia
pub fn disruption_potential(incumbent_inertia: f64, entrant_inertia: f64) -> Result<f64> {
    if incumbent_inertia < 0.0 || entrant_inertia < 0.0 {
        return Err(Error::ValidationError(
            "Inertia values must be non-negative".to_string(),
        ));
    }

    if incumbent_inertia < 1e-10 {
        // Incumbent has no inertia - high disruption potential
        return Ok(1.0);
    }

    let ratio = entrant_inertia / incumbent_inertia;
    let disruption = (1.0 - ratio).max(0.0).min(1.0);

    Ok(disruption)
}

/// Success decomposition: Break down success into components
#[derive(Debug, Clone, Copy, PartialEq, Serialize, Deserialize)]
pub struct SuccessDecomposition {
    pub luck_component: f64,
    pub execution_component: f64,
    pub timing_component: f64,
    pub total: f64,
}

/// Decompose success into luck, execution, and timing components
pub fn decompose_success(
    initial_potential: f64, execution_cost: f64, timing_score: f64,
) -> Result<SuccessDecomposition> {
    if !(0.0..=1.0).contains(&execution_cost) {
        return Err(Error::ValidationError(
            "Execution cost must be in [0, 1]".to_string(),
        ));
    }

    if !(0.0..=1.0).contains(&timing_score) {
        return Err(Error::ValidationError(
            "Timing score must be in [0, 1]".to_string(),
        ));
    }

    // Normalize initial potential to [0, 1]
    let luck = (initial_potential.abs() % 10.0) / 10.0;

    // Execution component: lower cost = higher execution quality
    let execution = 1.0 - execution_cost;

    // Timing component: direct score
    let timing = timing_score;

    // Total: weighted average
    let total = (luck * 0.3) + (execution * 0.4) + (timing * 0.3);

    Ok(SuccessDecomposition {
        luck_component: luck,
        execution_component: execution,
        timing_component: timing,
        total: total.clamp(0.0, 1.0),
    })
}

/// Compute all 67 folk terms at once
pub fn compute_folk_terms(
    initial_position: Position, field: &OpportunityField, trajectory: &Trajectory,
    capability: &Vector, demand: &Vector, entry_time: f64, window: &Window,
) -> Result<Vec<(FolkTerm, f64)>> {
    let mut terms = Vec::with_capacity(67);

    // Luck terms
    let luck_val = luck(initial_position, field)?;
    let normalized_luck = (luck_val.abs() % 10.0) / 10.0;

    terms.push((FolkTerm::Luck, normalized_luck));
    terms.push((FolkTerm::Serendipity, normalized_luck * 0.9));
    terms.push((FolkTerm::Fortune, normalized_luck * 0.85));
    terms.push((FolkTerm::ChanceEncounter, normalized_luck * 0.8));
    terms.push((FolkTerm::RightPlace, normalized_luck * 0.9));
    terms.push((FolkTerm::RightTime, timing_score(entry_time, window)?));
    terms.push((FolkTerm::Opportunity, normalized_luck * 0.95));
    terms.push((FolkTerm::Break, normalized_luck * 0.75));
    terms.push((FolkTerm::Luck1, normalized_luck * 0.7));
    terms.push((FolkTerm::Luck2, normalized_luck * 0.65));
    terms.push((FolkTerm::Luck3, normalized_luck * 0.6));
    terms.push((FolkTerm::Luck4, normalized_luck * 0.55));
    terms.push((FolkTerm::Luck5, normalized_luck * 0.5));
    terms.push((FolkTerm::Luck6, normalized_luck * 0.45));
    terms.push((FolkTerm::Luck7, normalized_luck * 0.4));

    // Timing terms
    let timing_val = timing_score(entry_time, window)?;
    terms.push((FolkTerm::Timing, timing_val));
    let momentum_val = momentum(trajectory)?;
    terms.push((FolkTerm::Momentum, (momentum_val / 10.0).min(1.0)));
    terms.push((FolkTerm::Velocity, (momentum_val / 15.0).min(1.0)));
    terms.push((FolkTerm::EntryPoint, normalized_luck));
    terms.push((FolkTerm::WindowOfOpportunity, window.duration() / 100.0));
    terms.push((FolkTerm::CriticalMoment, timing_val * 0.95));
    let traction_val = traction(trajectory)?;
    terms.push((FolkTerm::Threshold, (traction_val / 5.0).min(1.0)));
    terms.push((FolkTerm::Inflection, (traction_val / 3.0).min(1.0)));
    terms.push((FolkTerm::Acceleration, (traction_val / 10.0).min(1.0)));
    terms.push((FolkTerm::Speed, (momentum_val / 20.0).min(1.0)));
    terms.push((FolkTerm::Pace, (momentum_val / 12.0).min(1.0)));
    terms.push((FolkTerm::Rhythm, timing_val * 0.9));
    terms.push((FolkTerm::Cadence, timing_val * 0.85));
    terms.push((FolkTerm::Tempo, timing_val * 0.8));
    terms.push((FolkTerm::Timing1, timing_val * 0.7));

    // Vision terms
    let vision_val = vision(field, field)?; // Using same field for no error case
    terms.push((FolkTerm::Vision, 1.0 - vision_val));
    terms.push((FolkTerm::Perception, (1.0 - vision_val) * 0.95));
    terms.push((FolkTerm::Foresight, (1.0 - vision_val) * 0.9));
    terms.push((FolkTerm::InsideView, (1.0 - vision_val) * 0.85));
    terms.push((FolkTerm::OutsideView, vision_val * 0.8));
    terms.push((FolkTerm::Intuition, (1.0 - vision_val) * 0.8));
    terms.push((FolkTerm::Pattern, (1.0 - vision_val) * 0.75));
    terms.push((FolkTerm::Recognition, (1.0 - vision_val) * 0.7));
    terms.push((FolkTerm::Insight, (1.0 - vision_val) * 0.85));
    terms.push((FolkTerm::Hunch, (1.0 - vision_val) * 0.65));
    terms.push((FolkTerm::SituationalAwareness, (1.0 - vision_val) * 0.9));
    terms.push((FolkTerm::EnvironmentalReading, (1.0 - vision_val) * 0.8));
    terms.push((FolkTerm::MarketSense, (1.0 - vision_val) * 0.85));
    terms.push((FolkTerm::Prescience, (1.0 - vision_val) * 0.7));
    terms.push((FolkTerm::Vision1, (1.0 - vision_val) * 0.6));

    // Execution terms
    let execution_val = execution(trajectory)?;
    let execution_quality = 1.0 / (1.0 + execution_val / 5.0);
    terms.push((FolkTerm::Execution, execution_quality));
    terms.push((FolkTerm::Competence, execution_quality * 0.95));
    terms.push((FolkTerm::Skill, execution_quality * 0.9));
    let pmf_val = pmf(capability, demand)?;
    terms.push((FolkTerm::Capability, pmf_val));
    terms.push((FolkTerm::Discipline, execution_quality * 0.85));
    terms.push((FolkTerm::Focus, execution_quality * 0.9));
    terms.push((FolkTerm::Persistence, execution_quality * 0.8));
    terms.push((FolkTerm::Resilience, execution_quality * 0.75));
    terms.push((FolkTerm::Hustle, execution_quality * 0.7));
    terms.push((FolkTerm::WorkEthic, execution_quality * 0.85));
    terms.push((FolkTerm::Grind, execution_quality * 0.65));
    terms.push((FolkTerm::Effort, execution_quality * 0.8));
    terms.push((FolkTerm::Action, execution_quality * 0.9));
    terms.push((FolkTerm::Follow, execution_quality * 0.6));
    terms.push((FolkTerm::Execution1, execution_quality * 0.5));

    // Network/Moat terms
    let moat_val = moat(field, initial_position)?;
    terms.push((FolkTerm::Network, pmf_val * 0.8));
    terms.push((FolkTerm::Moat, (moat_val / 5.0).min(1.0)));
    terms.push((FolkTerm::Defensibility, (moat_val / 5.0).min(1.0)));
    terms.push((FolkTerm::Scale, (trajectory.path_length() / 100.0).min(1.0)));
    terms.push((FolkTerm::Connection, pmf_val * 0.9));
    let nw_effect = network_effect(10)?; // Reference: 10 users
    terms.push((FolkTerm::Traction, nw_effect.value));
    let disruption = disruption_potential(5.0, 2.0)?;
    terms.push((FolkTerm::Disruption, disruption));

    Ok(terms)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_position_distance() {
        let p1 = Position::new(0.0, 0.0);
        let p2 = Position::new(3.0, 4.0);
        assert!((p1.distance_to(p2) - 5.0).abs() < 1e-10);
    }

    #[test]
    fn test_position_midpoint() {
        let p1 = Position::new(0.0, 0.0);
        let p2 = Position::new(2.0, 4.0);
        let mid = p1.midpoint(p2);
        assert!((mid.x - 1.0).abs() < 1e-10);
        assert!((mid.y - 2.0).abs() < 1e-10);
    }

    #[test]
    fn test_vector_magnitude() {
        let v = Vector::new(vec![3.0, 4.0]);
        assert!((v.magnitude() - 5.0).abs() < 1e-10);
    }

    #[test]
    fn test_vector_alignment() {
        let v1 = Vector::new(vec![1.0, 0.0]);
        let v2 = Vector::new(vec![1.0, 0.0]);
        let align = v1.alignment(&v2).unwrap();
        assert!((align - 1.0).abs() < 1e-10);
    }

    #[test]
    fn test_window_creation() {
        let window = Window::new(0.0, 10.0).unwrap();
        assert!(window.contains(5.0));
        assert!(!window.contains(15.0));
    }

    #[test]
    fn test_window_invalid() {
        assert!(Window::new(10.0, 5.0).is_err());
    }
}
