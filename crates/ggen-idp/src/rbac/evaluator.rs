/// CEL (Common Expression Language) based permission evaluator
/// Evaluates complex permission constraints using expression language

use serde_json::Value;
use std::collections::HashMap;

/// CEL Expression Evaluator
pub struct CelEvaluator {
    context: HashMap<String, Value>,
}

impl CelEvaluator {
    pub fn new() -> Self {
        Self {
            context: HashMap::new(),
        }
    }

    pub fn with_context(mut self, context: HashMap<String, Value>) -> Self {
        self.context = context;
        self
    }

    /// Evaluate a CEL expression
    /// Supports basic operations: ==, !=, <, >, <=, >=, &&, ||, !
    pub fn evaluate(&self, expression: &str) -> Result<bool, String> {
        let trimmed = expression.trim();

        // Handle boolean literals
        if trimmed == "true" {
            return Ok(true);
        }
        if trimmed == "false" {
            return Ok(false);
        }

        // Handle negation
        if trimmed.starts_with('!') {
            return Ok(!self.evaluate(&trimmed[1..])?);
        }

        // Handle OR operations (lowest precedence)
        if let Some(pos) = trimmed.find(" || ") {
            let left = self.evaluate(&trimmed[..pos])?;
            let right = self.evaluate(&trimmed[pos + 4..])?;
            return Ok(left || right);
        }

        // Handle AND operations
        if let Some(pos) = trimmed.find(" && ") {
            let left = self.evaluate(&trimmed[..pos])?;
            let right = self.evaluate(&trimmed[pos + 4..])?;
            return Ok(left && right);
        }

        // Handle comparisons
        for (op, f) in &[
            ("==", |l: i64, r: i64| l == r as i64),
            ("!=", |l: i64, r: i64| l != r as i64),
            (">=", |l: i64, r: i64| l >= r as i64),
            ("<=", |l: i64, r: i64| l <= r as i64),
            (">", |l: i64, r: i64| l > r as i64),
            ("<", |l: i64, r: i64| l < r as i64),
        ] {
            if let Some(pos) = trimmed.find(op) {
                let left_str = trimmed[..pos].trim();
                let right_str = trimmed[pos + op.len()..].trim();

                if let (Ok(left), Ok(right)) = (
                    self.resolve_value(left_str),
                    self.resolve_value(right_str),
                ) {
                    let l_num = left
                        .as_i64()
                        .ok_or("Left side must be a number".to_string())?;
                    let r_num = right
                        .as_i64()
                        .ok_or("Right side must be a number".to_string())?;
                    return Ok(f(l_num, r_num));
                }
            }
        }

        // Handle simple variable lookup
        if let Some(value) = self.context.get(trimmed) {
            return Ok(value.as_bool().ok_or("Value must be boolean".to_string())?);
        }

        Err(format!("Unknown expression: {}", expression))
    }

    /// Resolve a value from context or literal
    fn resolve_value(&self, expr: &str) -> Result<Value, String> {
        let trimmed = expr.trim();

        // Number literal
        if let Ok(num) = trimmed.parse::<i64>() {
            return Ok(Value::Number(num.into()));
        }

        // String literal
        if trimmed.starts_with('"') && trimmed.ends_with('"') {
            return Ok(Value::String(trimmed[1..trimmed.len() - 1].to_string()));
        }

        // Boolean literal
        if trimmed == "true" {
            return Ok(Value::Bool(true));
        }
        if trimmed == "false" {
            return Ok(Value::Bool(false));
        }

        // Variable from context
        if let Some(value) = self.context.get(trimmed) {
            return Ok(value.clone());
        }

        Err(format!("Cannot resolve value: {}", expr))
    }
}

impl Default for CelEvaluator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evaluate_boolean_literal() {
        let eval = CelEvaluator::new();
        assert_eq!(eval.evaluate("true").unwrap(), true);
        assert_eq!(eval.evaluate("false").unwrap(), false);
    }

    #[test]
    fn test_evaluate_negation() {
        let eval = CelEvaluator::new();
        assert_eq!(eval.evaluate("!true").unwrap(), false);
        assert_eq!(eval.evaluate("!false").unwrap(), true);
    }

    #[test]
    fn test_evaluate_logical_operators() {
        let eval = CelEvaluator::new();
        assert_eq!(eval.evaluate("true && true").unwrap(), true);
        assert_eq!(eval.evaluate("true && false").unwrap(), false);
        assert_eq!(eval.evaluate("true || false").unwrap(), true);
        assert_eq!(eval.evaluate("false || false").unwrap(), false);
    }

    #[test]
    fn test_evaluate_comparisons() {
        let eval = CelEvaluator::new();
        assert_eq!(eval.evaluate("5 > 3").unwrap(), true);
        assert_eq!(eval.evaluate("5 < 3").unwrap(), false);
        assert_eq!(eval.evaluate("5 == 5").unwrap(), true);
        assert_eq!(eval.evaluate("5 != 3").unwrap(), true);
    }

    #[test]
    fn test_evaluate_context_variables() {
        let mut context = HashMap::new();
        context.insert("is_admin".to_string(), Value::Bool(true));
        context.insert("user_id".to_string(), Value::Number(123.into()));

        let eval = CelEvaluator::new().with_context(context);
        assert_eq!(eval.evaluate("is_admin").unwrap(), true);
        assert_eq!(eval.evaluate("user_id == 123").unwrap(), true);
    }

    #[test]
    fn test_evaluate_complex_expression() {
        let mut context = HashMap::new();
        context.insert("is_admin".to_string(), Value::Bool(true));
        context.insert("is_owner".to_string(), Value::Bool(false));

        let eval = CelEvaluator::new().with_context(context);
        assert_eq!(
            eval.evaluate("(is_admin && !is_owner) || is_owner").unwrap(),
            true
        );
    }
}
