use crate::manifest::GenerationRule;
use ggen_utils::error::{Error, Result};
use std::collections::{HashMap, HashSet};
use std::sync::Arc;
use tokio::task::JoinSet;

pub struct ConcurrentRuleExecutor;

impl ConcurrentRuleExecutor {
    pub fn detect_rule_dependencies(rules: &[GenerationRule]) -> HashMap<String, HashSet<String>> {
        let mut dependencies: HashMap<String, HashSet<String>> = HashMap::new();

        for rule in rules {
            let mut deps = HashSet::new();

            // Extract variable references from output_file
            for var in Self::extract_variables(&rule.output_file) {
                // Find which rules produce this variable
                for other_rule in rules {
                    if Self::rule_produces_variable(other_rule, &var) {
                        deps.insert(other_rule.name.clone());
                    }
                }
            }

            dependencies.insert(rule.name.clone(), deps);
        }

        dependencies
    }

    pub fn find_independent_rules(
        rules: &[GenerationRule], dependencies: &HashMap<String, HashSet<String>>,
    ) -> Vec<Vec<String>> {
        let mut batches: Vec<Vec<String>> = vec![];
        let mut processed = HashSet::new();

        while processed.len() < rules.len() {
            let mut batch = vec![];

            for rule in rules {
                if processed.contains(&rule.name) {
                    continue;
                }

                let empty_deps = HashSet::new();
                let rule_deps = dependencies.get(&rule.name).unwrap_or(&empty_deps);
                if rule_deps.iter().all(|dep| processed.contains(dep)) {
                    batch.push(rule.name.clone());
                }
            }

            if batch.is_empty() {
                break;
            }

            for rule_name in &batch {
                processed.insert(rule_name.clone());
            }

            batches.push(batch);
        }

        batches
    }

    pub async fn execute_rules_concurrent<F>(
        rules: &[GenerationRule], max_parallelism: Option<usize>, executor: F,
    ) -> Result<Vec<(String, Result<()>)>>
    where
        F: Fn(GenerationRule) -> futures::future::BoxFuture<'static, Result<()>>
            + Send
            + Sync
            + 'static,
    {
        let dependencies = Self::detect_rule_dependencies(rules);
        let batches = Self::find_independent_rules(rules, &dependencies);

        let executor = Arc::new(executor);
        let mut all_results = vec![];

        for batch in batches {
            let mut join_set = JoinSet::new();
            let parallelism = max_parallelism.unwrap_or(num_cpus::get());

            for (idx, rule_name) in batch.iter().enumerate() {
                if idx > 0 && idx % parallelism == 0 {
                    while let Some(result) = join_set.join_next().await {
                        match result {
                            Ok((name, res)) => {
                                all_results.push((name, res));
                            }
                            Err(e) => {
                                return Err(Error::new(&e.to_string()));
                            }
                        }
                    }
                }

                let rule = rules
                    .iter()
                    .find(|r| r.name == *rule_name)
                    .ok_or_else(|| Error::new(&format!("Rule {} not found", rule_name)))?
                    .clone();

                let executor_clone = Arc::clone(&executor);
                join_set.spawn(async move {
                    let res = executor_clone(rule.clone()).await;
                    (rule.name, res)
                });
            }

            while let Some(result) = join_set.join_next().await {
                match result {
                    Ok((name, res)) => {
                        all_results.push((name, res));
                    }
                    Err(e) => {
                        return Err(Error::new(&e.to_string()));
                    }
                }
            }
        }

        Ok(all_results)
    }

    fn extract_variables(template: &str) -> Vec<String> {
        let mut vars = vec![];
        let mut chars_iter = template.chars().peekable();

        while let Some(ch) = chars_iter.next() {
            // Look for {{ pattern
            if ch == '{' && chars_iter.peek() == Some(&'{') {
                chars_iter.next(); // consume second {

                // Read variable name until }}
                let mut var_name = String::new();
                while let Some(c) = chars_iter.next() {
                    if c == '}' && chars_iter.peek() == Some(&'}') {
                        chars_iter.next(); // consume second }
                        if !var_name.is_empty() {
                            vars.push(var_name.trim().to_string());
                        }
                        break;
                    }
                    var_name.push(c);
                }
            }
        }

        vars
    }

    fn rule_produces_variable(rule: &GenerationRule, var: &str) -> bool {
        // Simple heuristic: rule produces variables if its query selects them
        // In a full implementation, would parse the SPARQL query
        rule.name.contains(&var.to_lowercase())
            || rule.name.replace("-", "_").contains(&var.to_lowercase())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_variables() {
        let template = "src/{{module}}/{{name}}.rs";
        let vars = ConcurrentRuleExecutor::extract_variables(template);
        assert_eq!(vars, vec!["module", "name"]);
    }

    #[test]
    fn test_detect_independent_rules() {
        let rules = vec![
            GenerationRule {
                name: "rule1".to_string(),
                output_file: "file1.rs".to_string(),
                ..Default::default()
            },
            GenerationRule {
                name: "rule2".to_string(),
                output_file: "file2.rs".to_string(),
                ..Default::default()
            },
        ];

        let deps = ConcurrentRuleExecutor::detect_rule_dependencies(&rules);
        let batches = ConcurrentRuleExecutor::find_independent_rules(&rules, &deps);

        assert_eq!(batches.len(), 1);
        assert_eq!(batches[0].len(), 2);
    }
}
