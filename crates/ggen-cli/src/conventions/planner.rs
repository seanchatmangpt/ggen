//! Generation planner for creating task execution plans

use ggen_utils::error::{bail, Result};
use std::collections::{HashMap, HashSet};
use std::fs;
use std::path::{Path, PathBuf};

use super::ProjectConventions;

/// Metadata extracted from template comments
#[derive(Debug, Clone, PartialEq)]
pub struct TemplateMetadata {
    pub output: String,
    pub when: Vec<String>,
    pub query: String,
    pub foreach: Option<String>,
}

impl TemplateMetadata {
    /// Parse template metadata from file content
    pub fn parse(content: &str) -> Result<Self> {
        let mut output = None;
        let mut when = Vec::new();
        let mut query = None;
        let mut foreach = None;

        // Parse {# ... #} style comments
        for line in content.lines() {
            let line = line.trim();

            if line.starts_with("{#") && line.ends_with("#}") {
                let inner = line[2..line.len() - 2].trim();

                if let Some(value) = inner.strip_prefix("output:") {
                    output = Some(value.trim().to_string());
                } else if let Some(value) = inner.strip_prefix("when:") {
                    when.push(value.trim().to_string());
                } else if let Some(value) = inner.strip_prefix("query:") {
                    query = Some(value.trim().to_string());
                } else if let Some(value) = inner.strip_prefix("foreach:") {
                    foreach = Some(value.trim().to_string());
                }
            }
        }

        Ok(TemplateMetadata {
            output: output.ok_or_else(|| anyhow!("Missing 'output' directive"))?,
            when,
            query: query.ok_or_else(|| anyhow!("Missing 'query' directive"))?,
            foreach,
        })
    }
}

/// A single generation task
#[derive(Debug, Clone)]
pub struct GenerationTask {
    pub template: String,
    pub output_pattern: String,
    pub trigger_files: Vec<PathBuf>,
    pub query: Option<String>,
    pub foreach: Option<String>,
}

/// Complete generation plan with all tasks
#[derive(Debug)]
pub struct GenerationPlan {
    pub tasks: Vec<GenerationTask>,
}

/// Plans code generation based on conventions and templates
pub struct GenerationPlanner {
    conventions: ProjectConventions,
}

impl GenerationPlanner {
    /// Create a new generation planner
    pub fn new(conventions: ProjectConventions) -> Self {
        Self { conventions }
    }

    /// Create a generation plan by analyzing all templates
    pub fn plan(&self) -> Result<GenerationPlan> {
        let mut tasks = Vec::new();
        let mut task_graph: HashMap<String, Vec<String>> = HashMap::new();

        // Iterate through all discovered templates
        for (template_name, template_path) in &self.conventions.templates {
            let metadata = self.parse_template_metadata(template_path)?;
            let trigger_files = self.resolve_dependencies(&metadata);

            // Track dependencies for circular detection
            let deps: Vec<String> = trigger_files
                .iter()
                .filter_map(|p| {
                    p.file_stem()
                        .and_then(|s| s.to_str())
                        .map(|s| s.to_string())
                })
                .collect();

            task_graph.insert(template_name.clone(), deps);

            tasks.push(GenerationTask {
                template: template_name.clone(),
                output_pattern: metadata.output,
                trigger_files,
                query: Some(metadata.query),
                foreach: metadata.foreach,
            });
        }

        // Check for circular dependencies
        self.check_circular_dependencies(&task_graph)?;

        // Sort tasks by dependencies (topological sort)
        tasks = self.topological_sort(tasks)?;

        Ok(GenerationPlan { tasks })
    }

    /// Parse template metadata from a template file
    fn parse_template_metadata(&self, path: &Path) -> Result<TemplateMetadata> {
        let content = fs::read_to_string(path)?;
        TemplateMetadata::parse(&content)
    }

    /// Resolve file dependencies from template metadata
    fn resolve_dependencies(&self, metadata: &TemplateMetadata) -> Vec<PathBuf> {
        metadata
            .when
            .iter()
            .map(|pattern| {
                // Simple glob pattern resolution
                // In a real implementation, this would use proper glob matching
                PathBuf::from(pattern)
            })
            .collect()
    }

    /// Check for circular dependencies in the task graph
    fn check_circular_dependencies(&self, graph: &HashMap<String, Vec<String>>) -> Result<()> {
        for task in graph.keys() {
            let mut visited = HashSet::new();
            let mut rec_stack = HashSet::new();

            if self.has_cycle(task, graph, &mut visited, &mut rec_stack) {
                return Err(anyhow!(
                    "Circular dependency detected involving task: {}",
                    task
                ));
            }
        }

        Ok(())
    }

    /// DFS-based cycle detection
    #[allow(clippy::only_used_in_recursion)] // Parameter used in recursive calls
    fn has_cycle(
        &self, task: &str, graph: &HashMap<String, Vec<String>>, visited: &mut HashSet<String>,
        rec_stack: &mut HashSet<String>,
    ) -> bool {
        if rec_stack.contains(task) {
            return true;
        }

        if visited.contains(task) {
            return false;
        }

        visited.insert(task.to_string());
        rec_stack.insert(task.to_string());

        if let Some(deps) = graph.get(task) {
            for dep in deps {
                if self.has_cycle(dep, graph, visited, rec_stack) {
                    return true;
                }
            }
        }

        rec_stack.remove(task);
        false
    }

    /// Topologically sort tasks by dependencies
    fn topological_sort(&self, mut tasks: Vec<GenerationTask>) -> Result<Vec<GenerationTask>> {
        // Build dependency map
        let mut dep_count: HashMap<String, usize> = HashMap::new();
        let mut graph: HashMap<String, Vec<String>> = HashMap::new();

        for task in &tasks {
            dep_count.entry(task.template.clone()).or_insert(0);

            for trigger in &task.trigger_files {
                if let Some(dep) = trigger.file_stem().and_then(|s| s.to_str()) {
                    graph
                        .entry(dep.to_string())
                        .or_default()
                        .push(task.template.clone());
                    *dep_count.entry(task.template.clone()).or_insert(0) += 1;
                }
            }
        }

        // Find tasks with no dependencies
        let mut ready: Vec<String> = dep_count
            .iter()
            .filter(|(_, &count)| count == 0)
            .map(|(name, _)| name.clone())
            .collect();

        let mut sorted = Vec::new();

        while let Some(task_name) = ready.pop() {
            // Find and add the task
            if let Some(pos) = tasks.iter().position(|t| t.template == task_name) {
                let task = tasks.remove(pos);
                sorted.push(task);
            }

            // Update dependent tasks
            if let Some(dependents) = graph.get(&task_name) {
                for dependent in dependents {
                    if let Some(count) = dep_count.get_mut(dependent) {
                        *count = count.saturating_sub(1);
                        if *count == 0 {
                            ready.push(dependent.clone());
                        }
                    }
                }
            }
        }

        // If there are remaining tasks, there's a cycle
        if !tasks.is_empty() {
            return Err(anyhow!(
                "Circular dependency detected during topological sort"
            ));
        }

        Ok(sorted)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::fs;
    use tempfile::TempDir;

    fn create_test_template(dir: &Path, name: &str, content: &str) -> PathBuf {
        let path = dir.join(name);
        fs::write(&path, content).unwrap();
        path
    }

    fn create_test_conventions(
        temp_dir: &Path, templates: Vec<(&str, &str)>,
    ) -> ProjectConventions {
        let template_dir = temp_dir.join("templates");
        fs::create_dir_all(&template_dir).unwrap();

        let mut template_map = HashMap::new();
        for (name, content) in templates {
            let full_name = if name.ends_with(".tmpl") {
                name.to_string()
            } else {
                format!("{}.tmpl", name)
            };
            let path = create_test_template(&template_dir, &full_name, content);
            let key = name.strip_suffix(".tmpl").unwrap_or(name).to_string();
            template_map.insert(key, path);
        }

        ProjectConventions {
            rdf_files: vec![],
            rdf_dir: temp_dir.join("domain"),
            templates: template_map,
            templates_dir: template_dir,
            queries: HashMap::new(),
            output_dir: temp_dir.join("generated"),
            preset: "test".to_string(),
        }
    }

    #[test]
    fn test_parse_template_metadata_basic() {
        let content = r#"
{# output: src/{{name}}.rs #}
{# when: src/models/*.rs #}
{# query: SELECT * FROM models #}

template content here
"#;

        let metadata = TemplateMetadata::parse(content).unwrap();
        assert_eq!(metadata.output, "src/{{name}}.rs");
        assert_eq!(metadata.when, vec!["src/models/*.rs"]);
        assert_eq!(metadata.query, "SELECT * FROM models");
        assert_eq!(metadata.foreach, None);
    }

    #[test]
    fn test_parse_template_metadata_with_foreach() {
        let content = r#"
{# output: tests/{{item}}_test.rs #}
{# when: src/{{item}}.rs #}
{# query: SELECT name FROM entities #}
{# foreach: entity #}

test template
"#;

        let metadata = TemplateMetadata::parse(content).unwrap();
        assert_eq!(metadata.output, "tests/{{item}}_test.rs");
        assert_eq!(metadata.foreach, Some("entity".to_string()));
    }

    #[test]
    fn test_parse_template_metadata_multiple_when() {
        let content = r#"
{# output: generated.rs #}
{# when: file1.rs #}
{# when: file2.rs #}
{# query: SELECT * #}
"#;

        let metadata = TemplateMetadata::parse(content).unwrap();
        assert_eq!(metadata.when.len(), 2);
        assert!(metadata.when.contains(&"file1.rs".to_string()));
        assert!(metadata.when.contains(&"file2.rs".to_string()));
    }

    #[test]
    fn test_parse_template_metadata_missing_output() {
        let content = r#"
{# query: SELECT * #}
"#;

        let result = TemplateMetadata::parse(content);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Missing 'output'"));
    }

    #[test]
    fn test_parse_template_metadata_missing_query() {
        let content = r#"
{# output: file.rs #}
"#;

        let result = TemplateMetadata::parse(content);
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Missing 'query'"));
    }

    #[test]
    fn test_generation_planner_empty() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(temp_dir.path(), vec![]);

        let planner = GenerationPlanner::new(conventions);
        let plan = planner.plan().unwrap();

        assert_eq!(plan.tasks.len(), 0);
    }

    #[test]
    fn test_generation_planner_single_task() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(
            temp_dir.path(),
            vec![(
                "test",
                r#"
{# output: generated.rs #}
{# query: SELECT * #}

content
"#,
            )],
        );

        let planner = GenerationPlanner::new(conventions);
        let plan = planner.plan().unwrap();

        assert_eq!(plan.tasks.len(), 1);
        assert_eq!(plan.tasks[0].template, "test");
        assert_eq!(plan.tasks[0].output_pattern, "generated.rs");
    }

    #[test]
    fn test_generation_planner_multiple_tasks() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(
            temp_dir.path(),
            vec![
                (
                    "task1",
                    r#"
{# output: out1.rs #}
{# query: SELECT * FROM table1 #}
"#,
                ),
                (
                    "task2",
                    r#"
{# output: out2.rs #}
{# query: SELECT * FROM table2 #}
"#,
                ),
            ],
        );

        let planner = GenerationPlanner::new(conventions);
        let plan = planner.plan().unwrap();

        assert_eq!(plan.tasks.len(), 2);
    }

    #[test]
    fn test_generation_planner_with_dependencies() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(
            temp_dir.path(),
            vec![
                (
                    "base",
                    r#"
{# output: base.rs #}
{# query: SELECT * FROM base #}
"#,
                ),
                (
                    "derived",
                    r#"
{# output: derived.rs #}
{# when: base.rs #}
{# query: SELECT * FROM derived #}
"#,
                ),
            ],
        );

        let planner = GenerationPlanner::new(conventions);
        let plan = planner.plan().unwrap();

        assert_eq!(plan.tasks.len(), 2);

        // Base task should come before derived
        let base_idx = plan
            .tasks
            .iter()
            .position(|t| t.template == "base")
            .unwrap();
        let derived_idx = plan
            .tasks
            .iter()
            .position(|t| t.template == "derived")
            .unwrap();
        assert!(base_idx < derived_idx);
    }

    #[test]
    fn test_circular_dependency_detection() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(
            temp_dir.path(),
            vec![
                (
                    "task1",
                    r#"
{# output: task1.rs #}
{# when: task2.rs #}
{# query: SELECT * #}
"#,
                ),
                (
                    "task2",
                    r#"
{# output: task2.rs #}
{# when: task1.rs #}
{# query: SELECT * #}
"#,
                ),
            ],
        );

        let planner = GenerationPlanner::new(conventions);
        let result = planner.plan();

        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(
            err_msg.contains("Circular dependency") || err_msg.contains("cycle"),
            "Expected circular dependency error, got: {}",
            err_msg
        );
    }

    #[test]
    fn test_foreach_pattern() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(
            temp_dir.path(),
            vec![(
                "test",
                r#"
{# output: tests/{{entity}}_test.rs #}
{# query: SELECT name FROM entities #}
{# foreach: entity #}
"#,
            )],
        );

        let planner = GenerationPlanner::new(conventions);
        let plan = planner.plan().unwrap();

        assert_eq!(plan.tasks.len(), 1);
        assert_eq!(plan.tasks[0].foreach, Some("entity".to_string()));
        assert_eq!(plan.tasks[0].output_pattern, "tests/{{entity}}_test.rs");
    }

    #[test]
    fn test_once_pattern() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(
            temp_dir.path(),
            vec![(
                "once",
                r#"
{# output: single_file.rs #}
{# query: SELECT COUNT(*) #}
"#,
            )],
        );

        let planner = GenerationPlanner::new(conventions);
        let plan = planner.plan().unwrap();

        assert_eq!(plan.tasks.len(), 1);
        assert_eq!(plan.tasks[0].foreach, None);
        assert_eq!(plan.tasks[0].output_pattern, "single_file.rs");
    }

    #[test]
    fn test_complex_dependency_graph() {
        let temp_dir = TempDir::new().unwrap();
        let conventions = create_test_conventions(
            temp_dir.path(),
            vec![
                (
                    "models",
                    r#"
{# output: models.rs #}
{# query: SELECT * FROM schema #}
"#,
                ),
                (
                    "services",
                    r#"
{# output: services.rs #}
{# when: models.rs #}
{# query: SELECT * FROM services #}
"#,
                ),
                (
                    "controllers",
                    r#"
{# output: controllers.rs #}
{# when: services.rs #}
{# query: SELECT * FROM controllers #}
"#,
                ),
                (
                    "routes",
                    r#"
{# output: routes.rs #}
{# when: controllers.rs #}
{# query: SELECT * FROM routes #}
"#,
                ),
            ],
        );

        let planner = GenerationPlanner::new(conventions);
        let plan = planner.plan().unwrap();

        assert_eq!(plan.tasks.len(), 4);

        // Verify ordering
        let names: Vec<_> = plan.tasks.iter().map(|t| t.template.as_str()).collect();
        let models_idx = names.iter().position(|&n| n == "models").unwrap();
        let services_idx = names.iter().position(|&n| n == "services").unwrap();
        let controllers_idx = names.iter().position(|&n| n == "controllers").unwrap();
        let routes_idx = names.iter().position(|&n| n == "routes").unwrap();

        assert!(models_idx < services_idx);
        assert!(services_idx < controllers_idx);
        assert!(controllers_idx < routes_idx);
    }
}
