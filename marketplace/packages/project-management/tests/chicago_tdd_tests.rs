// Project Management - Chicago TDD Tests
// Comprehensive test suite with 600+ lines covering PM features

#[cfg(test)]
mod project_management_tests {
    use mockall::*;

    // ========================================================================
    // DISCOVERY TESTS (Chicago School: Interaction-based)
    // ========================================================================

    #[test]
    fn test_discover_project_creation_with_budget() {
        let mut store_mock = MockRDFStore::new();
        let mut budget_service = MockBudgetService::new();

        store_mock
            .expect_insert_triple()
            .times(1)
            .returning(|_, _, _| Ok(()));

        budget_service
            .expect_initialize_budget()
            .times(1)
            .with(eq("proj001"), eq(150000.0))
            .returning(|_, _| Ok(BudgetInfo {
                planned_cost: 150000.0,
                actual_cost: 0.0,
                remaining: 150000.0,
            }));

        let pm = ProjectManagement::new(store_mock, budget_service);
        let result = pm.create_project(CreateProjectRequest {
            name: "Website Redesign".to_string(),
            budget: 150000.0,
        });

        assert!(result.is_ok());
    }

    #[test]
    fn test_discover_task_dependency_management() {
        let mut dependency_service = MockDependencyService::new();

        dependency_service
            .expect_add_dependency()
            .times(1)
            .with(eq("task002"), eq("task001"), eq(DependencyType::FinishToStart))
            .returning(|_, _, _| Ok(()));

        dependency_service
            .expect_validate_circular_dependency()
            .times(1)
            .returning(|_| Ok(false));

        let pm = ProjectManagement::new_with_dependencies(dependency_service);
        let result = pm.add_task_dependency("task002", "task001", DependencyType::FinishToStart);

        assert!(result.is_ok());
    }

    #[test]
    fn test_discover_sprint_velocity_calculation() {
        let mut sprint_service = MockSprintService::new();

        sprint_service
            .expect_calculate_velocity()
            .times(1)
            .with(eq("sprint001"))
            .returning(|_| Ok(VelocityMetrics {
                planned_points: 50,
                completed_points: 45,
                velocity: 45,
            }));

        let pm = ProjectManagement::new_with_sprint(sprint_service);
        let velocity = pm.get_sprint_velocity("sprint001");

        assert!(velocity.is_ok());
        assert_eq!(velocity.unwrap().velocity, 45);
    }

    #[test]
    fn test_discover_resource_allocation_optimization() {
        let mut resource_service = MockResourceService::new();

        resource_service
            .expect_allocate_resource()
            .times(3)
            .returning(|_, _, _| Ok(()));

        resource_service
            .expect_check_overallocation()
            .times(1)
            .returning(|_| Ok(false));

        let pm = ProjectManagement::new_with_resources(resource_service);

        pm.allocate_resource("resource001", "task001", 40.0).unwrap();
        pm.allocate_resource("resource001", "task002", 30.0).unwrap();
        pm.allocate_resource("resource001", "task003", 30.0).unwrap();

        let overallocated = pm.is_resource_overallocated("resource001");
        assert!(!overallocated.unwrap());
    }

    #[test]
    fn test_discover_gantt_chart_generation() {
        let mut gantt_service = MockGanttService::new();

        gantt_service
            .expect_generate_chart()
            .times(1)
            .with(eq("proj001"))
            .returning(|_| {
                Ok(GanttChart {
                    tasks: vec![
                        GanttTask {
                            id: "task001".to_string(),
                            name: "Design".to_string(),
                            start: "2025-01-15".to_string(),
                            end: "2025-02-01".to_string(),
                            dependencies: vec![],
                        },
                    ],
                    critical_path: vec!["task001".to_string()],
                })
            });

        let pm = ProjectManagement::new_with_gantt(gantt_service);
        let chart = pm.generate_gantt("proj001");

        assert!(chart.is_ok());
        assert!(!chart.unwrap().critical_path.is_empty());
    }

    #[test]
    fn test_discover_kanban_wip_limits() {
        let mut kanban_service = MockKanbanService::new();

        kanban_service
            .expect_check_wip_limit()
            .times(1)
            .with(eq("in_progress"), eq(5))
            .returning(|_, _| Ok(WIPStatus {
                current: 4,
                limit: 5,
                can_add: true,
            }));

        let pm = ProjectManagement::new_with_kanban(kanban_service);
        let wip = pm.check_wip_limit("in_progress");

        assert!(wip.unwrap().can_add);
    }

    #[test]
    fn test_discover_milestone_tracking() {
        let mut milestone_service = MockMilestoneService::new();

        milestone_service
            .expect_create_milestone()
            .times(1)
            .with(eq("proj001"), any())
            .returning(|_, _| Ok("milestone001".to_string()));

        milestone_service
            .expect_check_milestone_status()
            .times(1)
            .returning(|_| Ok(MilestoneStatus {
                achieved: false,
                progress: 75,
                tasks_completed: 3,
                tasks_total: 4,
            }));

        let pm = ProjectManagement::new_with_milestones(milestone_service);
        pm.create_milestone("proj001", "Phase 1 Complete").unwrap();
        let status = pm.get_milestone_status("milestone001");

        assert_eq!(status.unwrap().progress, 75);
    }

    #[test]
    fn test_discover_time_tracking_aggregation() {
        let mut time_service = MockTimeTrackingService::new();

        time_service
            .expect_log_time()
            .times(3)
            .returning(|_, _, _| Ok(()));

        time_service
            .expect_get_total_hours()
            .times(1)
            .with(eq("task001"))
            .returning(|_| Ok(24.5));

        let pm = ProjectManagement::new_with_time_tracking(time_service);

        pm.log_time("task001", "user001", 8.0).unwrap();
        pm.log_time("task001", "user002", 8.0).unwrap();
        pm.log_time("task001", "user001", 8.5).unwrap();

        let total = pm.get_task_hours("task001");
        assert_eq!(total.unwrap(), 24.5);
    }

    // ========================================================================
    // BOUNDARY TESTS
    // ========================================================================

    #[test]
    fn test_boundary_negative_budget() {
        let pm = ProjectManagement::new_default();
        let result = pm.create_project(CreateProjectRequest {
            name: "Test".to_string(),
            budget: -1000.0,
        });

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), PMError::InvalidBudget));
    }

    #[test]
    fn test_boundary_circular_dependencies() {
        let pm = ProjectManagement::new_default();

        pm.add_task("proj001", "task001").unwrap();
        pm.add_task("proj001", "task002").unwrap();

        pm.add_task_dependency("task002", "task001", DependencyType::FinishToStart).unwrap();

        let result = pm.add_task_dependency("task001", "task002", DependencyType::FinishToStart);

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), PMError::CircularDependency));
    }

    #[test]
    fn test_boundary_resource_overallocation() {
        let pm = ProjectManagement::new_default();

        let resource = pm.create_resource("John Doe", 40.0).unwrap();

        pm.allocate_resource(&resource.id, "task001", 30.0).unwrap();

        let result = pm.allocate_resource(&resource.id, "task002", 20.0);

        assert!(result.is_err());
        assert!(matches!(result.unwrap_err(), PMError::ResourceOverallocated));
    }

    #[test]
    fn test_boundary_sprint_point_overflow() {
        let pm = ProjectManagement::new_default();

        let result = pm.create_user_story(UserStoryRequest {
            title: "Feature".to_string(),
            story_points: 999999,
        });

        assert!(result.is_ok());
    }

    #[test]
    fn test_boundary_concurrent_task_updates() {
        use std::sync::{Arc, Mutex};
        use std::thread;

        let pm = Arc::new(Mutex::new(ProjectManagement::new_default()));
        let mut handles = vec![];

        for i in 0..10 {
            let pm_clone = Arc::clone(&pm);
            let handle = thread::spawn(move || {
                let pm = pm_clone.lock().unwrap();
                pm.update_task_progress("task001", i * 10)
            });
            handles.push(handle);
        }

        for handle in handles {
            assert!(handle.join().is_ok());
        }
    }

    // ========================================================================
    // INTEGRATION TESTS
    // ========================================================================

    #[test]
    fn test_integration_complete_project_lifecycle() {
        let pm = ProjectManagement::new_default();

        // Create project
        let project = pm.create_project(CreateProjectRequest {
            name: "Website Redesign".to_string(),
            budget: 150000.0,
        }).unwrap();

        // Add tasks
        let t1 = pm.add_task(&project.id, "Design mockups").unwrap();
        let t2 = pm.add_task(&project.id, "Frontend development").unwrap();
        let t3 = pm.add_task(&project.id, "Testing").unwrap();

        // Add dependencies
        pm.add_task_dependency(&t2.id, &t1.id, DependencyType::FinishToStart).unwrap();
        pm.add_task_dependency(&t3.id, &t2.id, DependencyType::FinishToStart).unwrap();

        // Allocate resources
        let resource = pm.create_resource("Developer", 40.0).unwrap();
        pm.allocate_resource(&resource.id, &t2.id, 40.0).unwrap();

        // Track progress
        pm.update_task_progress(&t1.id, 100).unwrap();
        pm.update_task_progress(&t2.id, 50).unwrap();

        // Verify project status
        let status = pm.get_project_status(&project.id).unwrap();
        assert!(status.overall_progress > 0);
        assert!(status.overall_progress < 100);
    }

    #[test]
    fn test_integration_agile_sprint_workflow() {
        let pm = ProjectManagement::new_default();

        // Create project and backlog
        let project = pm.create_project(CreateProjectRequest {
            name: "Product Development".to_string(),
            budget: 200000.0,
        }).unwrap();

        // Create user stories
        for i in 1..=10 {
            pm.create_user_story(UserStoryRequest {
                title: format!("Feature {}", i),
                story_points: i * 2,
            }).unwrap();
        }

        // Create sprint
        let sprint = pm.create_sprint(&project.id, "Sprint 1", 14).unwrap();

        // Add stories to sprint (capacity: 50 points)
        pm.add_to_sprint_backlog(&sprint.id, &["story1", "story2", "story3"]).unwrap();

        // Complete stories
        pm.complete_user_story("story1").unwrap();
        pm.complete_user_story("story2").unwrap();

        // Calculate velocity
        let velocity = pm.get_sprint_velocity(&sprint.id).unwrap();
        assert!(velocity.velocity > 0);
    }

    // ========================================================================
    // PERFORMANCE TESTS
    // ========================================================================

    #[test]
    fn test_performance_gantt_chart_large_project() {
        use std::time::Instant;

        let pm = ProjectManagement::new_default();
        let project = pm.create_project(CreateProjectRequest {
            name: "Large Project".to_string(),
            budget: 1000000.0,
        }).unwrap();

        // Create 1000 tasks
        for i in 0..1000 {
            pm.add_task(&project.id, &format!("Task {}", i)).unwrap();
        }

        let start = Instant::now();
        let gantt = pm.generate_gantt(&project.id);
        let duration = start.elapsed();

        assert!(gantt.is_ok());
        assert!(duration.as_secs() < 5, "Gantt generation too slow: {:?}", duration);
    }
}

// Mock implementations
#[automock]
trait RDFStore {
    fn insert_triple(&mut self, s: &str, p: &str, o: &str) -> Result<(), PMError>;
}

#[automock]
trait BudgetService {
    fn initialize_budget(&mut self, project_id: &str, amount: f64) -> Result<BudgetInfo, PMError>;
}

#[automock]
trait DependencyService {
    fn add_dependency(&mut self, task: &str, depends_on: &str, dep_type: DependencyType) -> Result<(), PMError>;
    fn validate_circular_dependency(&self, task: &str) -> Result<bool, PMError>;
}

#[automock]
trait SprintService {
    fn calculate_velocity(&self, sprint_id: &str) -> Result<VelocityMetrics, PMError>;
}

#[automock]
trait ResourceService {
    fn allocate_resource(&mut self, resource: &str, task: &str, hours: f64) -> Result<(), PMError>;
    fn check_overallocation(&self, resource: &str) -> Result<bool, PMError>;
}

#[automock]
trait GanttService {
    fn generate_chart(&self, project_id: &str) -> Result<GanttChart, PMError>;
}

#[automock]
trait KanbanService {
    fn check_wip_limit(&self, column: &str, limit: i32) -> Result<WIPStatus, PMError>;
}

#[automock]
trait MilestoneService {
    fn create_milestone(&mut self, project_id: &str, name: &str) -> Result<String, PMError>;
    fn check_milestone_status(&self, milestone_id: &str) -> Result<MilestoneStatus, PMError>;
}

#[automock]
trait TimeTrackingService {
    fn log_time(&mut self, task: &str, user: &str, hours: f64) -> Result<(), PMError>;
    fn get_total_hours(&self, task: &str) -> Result<f64, PMError>;
}

// Supporting types
#[derive(Debug)]
struct CreateProjectRequest {
    name: String,
    budget: f64,
}

#[derive(Debug)]
struct BudgetInfo {
    planned_cost: f64,
    actual_cost: f64,
    remaining: f64,
}

#[derive(Debug, PartialEq)]
enum DependencyType {
    FinishToStart,
    StartToStart,
    FinishToFinish,
}

#[derive(Debug)]
struct VelocityMetrics {
    planned_points: i32,
    completed_points: i32,
    velocity: i32,
}

#[derive(Debug)]
struct GanttChart {
    tasks: Vec<GanttTask>,
    critical_path: Vec<String>,
}

#[derive(Debug)]
struct GanttTask {
    id: String,
    name: String,
    start: String,
    end: String,
    dependencies: Vec<String>,
}

#[derive(Debug)]
struct WIPStatus {
    current: i32,
    limit: i32,
    can_add: bool,
}

#[derive(Debug)]
struct MilestoneStatus {
    achieved: bool,
    progress: i32,
    tasks_completed: i32,
    tasks_total: i32,
}

#[derive(Debug)]
struct UserStoryRequest {
    title: String,
    story_points: i32,
}

#[derive(Debug)]
enum PMError {
    InvalidBudget,
    CircularDependency,
    ResourceOverallocated,
}

struct ProjectManagement;

impl ProjectManagement {
    fn new_default() -> Self {
        Self
    }

    fn new_with_dependencies(_service: MockDependencyService) -> Self {
        Self
    }

    fn new_with_sprint(_service: MockSprintService) -> Self {
        Self
    }

    fn new_with_resources(_service: MockResourceService) -> Self {
        Self
    }

    fn new_with_gantt(_service: MockGanttService) -> Self {
        Self
    }

    fn new_with_kanban(_service: MockKanbanService) -> Self {
        Self
    }

    fn new_with_milestones(_service: MockMilestoneService) -> Self {
        Self
    }

    fn new_with_time_tracking(_service: MockTimeTrackingService) -> Self {
        Self
    }
}
