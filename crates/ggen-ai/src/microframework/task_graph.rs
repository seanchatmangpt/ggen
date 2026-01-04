//! Task dependency graph for topological execution ordering
//!
//! Manages task dependencies and determines optimal execution order
//! for maximum parallel throughput.

use super::tasks::Task;
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, HashSet, VecDeque};

/// Task dependency graph
#[derive(Debug, Clone, Default)]
pub struct TaskGraph {
    /// Tasks indexed by ID
    tasks: HashMap<String, Task>,
    /// Adjacency list: task_id -> dependent task IDs
    dependencies: HashMap<String, HashSet<String>>,
    /// Reverse adjacency: task_id -> tasks that depend on it
    dependents: HashMap<String, HashSet<String>>,
}

impl TaskGraph {
    /// Create a new empty task graph
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a task to the graph
    pub fn add_task(&mut self, task: Task) {
        let task_id = task.id.clone();

        // Initialize dependency sets if not present
        self.dependencies.entry(task_id.clone()).or_default();
        self.dependents.entry(task_id.clone()).or_default();

        // Add dependencies from task config
        for dep_id in &task.config.dependencies {
            self.add_dependency(&task_id, dep_id);
        }

        self.tasks.insert(task_id, task);
    }

    /// Add a dependency: `dependent` depends on `dependency`
    pub fn add_dependency(&mut self, dependent: &str, dependency: &str) {
        self.dependencies
            .entry(dependent.to_string())
            .or_default()
            .insert(dependency.to_string());

        self.dependents
            .entry(dependency.to_string())
            .or_default()
            .insert(dependent.to_string());
    }

    /// Remove a task from the graph
    pub fn remove_task(&mut self, task_id: &str) {
        self.tasks.remove(task_id);

        // Remove from dependencies
        if let Some(deps) = self.dependencies.remove(task_id) {
            for dep in deps {
                if let Some(dependents) = self.dependents.get_mut(&dep) {
                    dependents.remove(task_id);
                }
            }
        }

        // Remove from dependents
        if let Some(deps) = self.dependents.remove(task_id) {
            for dep in deps {
                if let Some(dependencies) = self.dependencies.get_mut(&dep) {
                    dependencies.remove(task_id);
                }
            }
        }
    }

    /// Get a task by ID
    pub fn get_task(&self, task_id: &str) -> Option<&Task> {
        self.tasks.get(task_id)
    }

    /// Get all tasks
    pub fn tasks(&self) -> impl Iterator<Item = &Task> {
        self.tasks.values()
    }

    /// Get task count
    pub fn task_count(&self) -> usize {
        self.tasks.len()
    }

    /// Check if a task has unmet dependencies
    pub fn has_unmet_dependencies(&self, task_id: &str, completed: &[String]) -> bool {
        if let Some(deps) = self.dependencies.get(task_id) {
            let completed_set: HashSet<&str> = completed.iter().map(|s| s.as_str()).collect();
            deps.iter().any(|d| !completed_set.contains(d.as_str()))
        } else {
            false
        }
    }

    /// Get tasks that are ready to execute (no unmet dependencies)
    pub fn ready_tasks(&self, completed: &[String]) -> Vec<&Task> {
        let completed_set: HashSet<&str> = completed.iter().map(|s| s.as_str()).collect();

        self.tasks
            .values()
            .filter(|task| {
                // Not already completed
                !completed_set.contains(task.id.as_str()) &&
                // All dependencies met
                !self.has_unmet_dependencies(&task.id, completed)
            })
            .collect()
    }

    /// Perform topological sort to get execution order
    /// Returns waves of tasks that can be executed in parallel
    pub fn topological_sort(&self) -> Result<Vec<Vec<String>>> {
        let mut in_degree: HashMap<String, usize> = HashMap::new();
        let mut waves: Vec<Vec<String>> = Vec::new();

        // Calculate in-degrees
        for task_id in self.tasks.keys() {
            let degree = self.dependencies
                .get(task_id)
                .map(|deps| deps.len())
                .unwrap_or(0);
            in_degree.insert(task_id.clone(), degree);
        }

        // Find all tasks with no dependencies (first wave)
        let mut queue: VecDeque<String> = in_degree
            .iter()
            .filter(|(_, &deg)| deg == 0)
            .map(|(id, _)| id.clone())
            .collect();

        let mut processed = 0;

        while !queue.is_empty() {
            // All tasks in queue form a wave (can be parallel)
            let wave: Vec<String> = queue.drain(..).collect();
            processed += wave.len();

            // Find next wave
            for task_id in &wave {
                if let Some(dependents) = self.dependents.get(task_id) {
                    for dependent in dependents {
                        if let Some(degree) = in_degree.get_mut(dependent) {
                            *degree = degree.saturating_sub(1);
                            if *degree == 0 {
                                queue.push_back(dependent.clone());
                            }
                        }
                    }
                }
            }

            waves.push(wave);
        }

        // Check for cycles
        if processed != self.tasks.len() {
            return Err(GgenAiError::invalid_input(
                "Task graph contains cycles - cannot determine execution order"
            ));
        }

        Ok(waves)
    }

    /// Get the critical path (longest dependency chain)
    ///
    /// Returns `Err` if the graph contains cycles, preventing critical path computation.
    /// For valid acyclic graphs, returns the longest path through the dependency chain.
    pub fn critical_path(&self) -> Result<Vec<String>> {
        // Get topological order - propagates cycle error
        let waves = self.topological_sort()?;

        let mut distances: HashMap<String, usize> = HashMap::new();
        let mut predecessors: HashMap<String, Option<String>> = HashMap::new();

        // Initialize
        for task_id in self.tasks.keys() {
            distances.insert(task_id.clone(), 0);
            predecessors.insert(task_id.clone(), None);
        }

        // Compute longest path distances using topological order
        for wave in waves {
            for task_id in wave {
                if let Some(deps) = self.dependencies.get(&task_id) {
                    for dep in deps {
                        let new_dist = distances.get(dep).copied().unwrap_or(0) + 1;
                        if new_dist > *distances.get(&task_id).unwrap_or(&0) {
                            distances.insert(task_id.clone(), new_dist);
                            predecessors.insert(task_id.clone(), Some(dep.clone()));
                        }
                    }
                }
            }
        }

        // Find task with maximum distance
        let end_task = distances
            .iter()
            .max_by_key(|(_, &dist)| dist)
            .map(|(id, _)| id.clone());

        // Reconstruct path
        let mut path = Vec::new();
        if let Some(mut current) = end_task {
            path.push(current.clone());
            while let Some(Some(pred)) = predecessors.get(&current) {
                path.push(pred.clone());
                current = pred.clone();
            }
        }

        path.reverse();
        Ok(path)
    }

    /// Get maximum parallelism (width of the widest wave)
    pub fn max_parallelism(&self) -> usize {
        self.topological_sort()
            .map(|waves| waves.iter().map(|w| w.len()).max().unwrap_or(0))
            .unwrap_or(0)
    }

    /// Detect cycles in the graph
    pub fn has_cycle(&self) -> bool {
        self.topological_sort().is_err()
    }

    /// Get all dependencies of a task (transitive closure)
    pub fn all_dependencies(&self, task_id: &str) -> HashSet<String> {
        let mut result = HashSet::new();
        let mut queue: VecDeque<String> = VecDeque::new();

        if let Some(deps) = self.dependencies.get(task_id) {
            for dep in deps {
                queue.push_back(dep.clone());
            }
        }

        while let Some(current) = queue.pop_front() {
            if result.insert(current.clone()) {
                if let Some(deps) = self.dependencies.get(&current) {
                    for dep in deps {
                        queue.push_back(dep.clone());
                    }
                }
            }
        }

        result
    }

    /// Get all dependents of a task (what depends on it)
    pub fn all_dependents(&self, task_id: &str) -> HashSet<String> {
        let mut result = HashSet::new();
        let mut queue: VecDeque<String> = VecDeque::new();

        if let Some(deps) = self.dependents.get(task_id) {
            for dep in deps {
                queue.push_back(dep.clone());
            }
        }

        while let Some(current) = queue.pop_front() {
            if result.insert(current.clone()) {
                if let Some(deps) = self.dependents.get(&current) {
                    for dep in deps {
                        queue.push_back(dep.clone());
                    }
                }
            }
        }

        result
    }
}

/// Graph statistics
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphStats {
    /// Total number of tasks
    pub task_count: usize,
    /// Total number of edges (dependencies)
    pub edge_count: usize,
    /// Maximum parallelism possible
    pub max_parallelism: usize,
    /// Length of critical path
    pub critical_path_length: usize,
    /// Number of waves for execution
    pub wave_count: usize,
}

impl TaskGraph {
    /// Get graph statistics
    ///
    /// Returns `Err` if the graph contains cycles.
    pub fn stats(&self) -> Result<GraphStats> {
        let edge_count: usize = self.dependencies
            .values()
            .map(|deps| deps.len())
            .sum();

        // Propagate cycle errors instead of silently masking them
        let waves = self.topological_sort()?;
        let critical_path = self.critical_path()?;

        Ok(GraphStats {
            task_count: self.tasks.len(),
            edge_count,
            max_parallelism: waves.iter().map(|w| w.len()).max().unwrap_or(0),
            critical_path_length: critical_path.len(),
            wave_count: waves.len(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::microframework::tasks::TaskType;

    fn create_task(id: &str) -> Task {
        let mut task = Task::new(TaskType::CodeGen, id);
        task.id = id.to_string();
        task
    }

    #[test]
    fn test_empty_graph() {
        let graph = TaskGraph::new();
        assert_eq!(graph.task_count(), 0);
        assert!(!graph.has_cycle());
    }

    #[test]
    fn test_add_task() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("task-1"));
        assert_eq!(graph.task_count(), 1);
        assert!(graph.get_task("task-1").is_some());
    }

    #[test]
    fn test_dependencies() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("task-1"));
        graph.add_task(create_task("task-2"));
        graph.add_dependency("task-2", "task-1");

        let completed: Vec<String> = vec![];
        assert!(graph.has_unmet_dependencies("task-2", &completed));

        let completed = vec!["task-1".to_string()];
        assert!(!graph.has_unmet_dependencies("task-2", &completed));
    }

    #[test]
    fn test_topological_sort() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("a"));
        graph.add_task(create_task("b"));
        graph.add_task(create_task("c"));
        graph.add_dependency("b", "a");
        graph.add_dependency("c", "b");

        let waves = graph.topological_sort().unwrap();
        assert_eq!(waves.len(), 3);
        assert_eq!(waves[0], vec!["a"]);
        assert_eq!(waves[1], vec!["b"]);
        assert_eq!(waves[2], vec!["c"]);
    }

    #[test]
    fn test_parallel_tasks() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("a"));
        graph.add_task(create_task("b"));
        graph.add_task(create_task("c"));
        // b and c both depend on a, can run in parallel
        graph.add_dependency("b", "a");
        graph.add_dependency("c", "a");

        let waves = graph.topological_sort().unwrap();
        assert_eq!(waves.len(), 2);
        assert_eq!(waves[0], vec!["a"]);
        assert_eq!(waves[1].len(), 2); // b and c in parallel
    }

    #[test]
    fn test_cycle_detection() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("a"));
        graph.add_task(create_task("b"));
        graph.add_dependency("a", "b");
        graph.add_dependency("b", "a");

        assert!(graph.has_cycle());
        assert!(graph.topological_sort().is_err());
    }

    #[test]
    fn test_critical_path() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("a"));
        graph.add_task(create_task("b"));
        graph.add_task(create_task("c"));
        graph.add_task(create_task("d"));
        // a -> b -> c (critical path)
        // a -> d (short path)
        graph.add_dependency("b", "a");
        graph.add_dependency("c", "b");
        graph.add_dependency("d", "a");

        let path = graph.critical_path().unwrap();
        assert_eq!(path.len(), 3);
    }

    #[test]
    fn test_critical_path_cycle_error() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("a"));
        graph.add_task(create_task("b"));
        graph.add_dependency("a", "b");
        graph.add_dependency("b", "a");

        // critical_path should return error for cyclic graphs
        assert!(graph.critical_path().is_err());
    }

    #[test]
    fn test_stats_cycle_error() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("a"));
        graph.add_task(create_task("b"));
        graph.add_dependency("a", "b");
        graph.add_dependency("b", "a");

        // stats should return error for cyclic graphs
        assert!(graph.stats().is_err());
    }

    #[test]
    fn test_ready_tasks() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("a"));
        graph.add_task(create_task("b"));
        graph.add_dependency("b", "a");

        let completed: Vec<String> = vec![];
        let ready = graph.ready_tasks(&completed);
        assert_eq!(ready.len(), 1);
        assert_eq!(ready[0].id, "a");

        let completed = vec!["a".to_string()];
        let ready = graph.ready_tasks(&completed);
        assert_eq!(ready.len(), 1);
        assert_eq!(ready[0].id, "b");
    }

    #[test]
    fn test_max_parallelism() {
        let mut graph = TaskGraph::new();
        graph.add_task(create_task("root"));
        for i in 0..5 {
            let task_id = format!("leaf-{}", i);
            graph.add_task(create_task(&task_id));
            graph.add_dependency(&task_id, "root");
        }

        assert_eq!(graph.max_parallelism(), 5);
    }
}
