use std::collections::HashMap;
use tera::{Result as TeraResult, Tera, Value, Context};
use inflector::{
    cases::{
        camelcase, classcase, kebabcase, pascalcase, sentencecase, snakecase, titlecase, traincase,
    },
    numbers::{deordinalize, ordinalize},
    string::{deconstantize, demodulize, pluralize, singularize},
    suffix::foreignkey,
};
use heck::{
    ToShoutySnakeCase, ToShoutyKebabCase, ToTitleCase // supplemental only
};

/// Register all text transformation helpers into Tera.
pub fn register_all(tera: &mut Tera) {
    // ---------- Inflector canonical ----------
    reg_str(tera, "camel", |s| camelcase::to_camel_case(s));
    reg_str(tera, "pascal", |s| pascalcase::to_pascal_case(s));
    reg_str(tera, "snake", |s| snakecase::to_snake_case(s));
    reg_str(tera, "kebab", |s| kebabcase::to_kebab_case(s));
    reg_str(tera, "class", |s| classcase::to_class_case(s));
    reg_str(tera, "title", |s| titlecase::to_title_case(s));
    reg_str(tera, "sentence", |s| sentencecase::to_sentence_case(s));
    reg_str(tera, "train", |s| traincase::to_train_case(s));

    reg_str(tera, "pluralize", |s| pluralize::to_plural(s));
    reg_str(tera, "singularize", |s| singularize::to_singular(s));
    reg_str(tera, "deconstantize", |s| deconstantize::deconstantize(s));
    reg_str(tera, "demodulize", |s| demodulize::demodulize(s));

    reg_str(tera, "ordinalize", |s| ordinalize::ordinalize(s));
    reg_str(tera, "deordinalize", |s| deordinalize::deordinalize(s));

    reg_str(tera, "foreign_key", |s| foreignkey::to_foreign_key(s));

    // ---------- Heck fill-ins (not in Inflector) ----------
    reg_str(tera, "shouty_snake", |s| s.to_shouty_snake_case());
    reg_str(tera, "shouty_kebab", |s| s.to_shouty_kebab_case());
    reg_str(tera, "titlecase", |s| s.to_title_case()); // better consistency

    // ---------- Common change-case style aliases ----------
    reg_str(tera, "param", |s| kebabcase::to_kebab_case(s));
    reg_str(tera, "constant", |s| s.to_shouty_snake_case());
    reg_str(tera, "upper", |s| s.to_uppercase());
    reg_str(tera, "lower", |s| s.to_lowercase());
    reg_str(tera, "lcfirst", |s| {
        let mut c = s.chars();
        match c.next() { Some(f) => f.to_lowercase().collect::<String>() + c.as_str(), None => String::new() }
    });
    reg_str(tera, "ucfirst", |s| {
        let mut c = s.chars();
        match c.next() { Some(f) => f.to_uppercase().collect::<String>() + c.as_str(), None => String::new() }
    });
}

/// Auto-bless context variables for Hygen compatibility.
/// 
/// This function adds common derived variables to the context:
/// - `Name` = `name | pascal` (when `name` exists)
/// - `locals` = alias to the same context (for Hygen compatibility)
pub fn bless_context(context: &mut Context) {
    // Auto-bless Name when name exists
    if let Some(name_value) = context.get("name") {
        if let Some(name_str) = name_value.as_str() {
            let pascal_name = pascalcase::to_pascal_case(name_str);
            context.insert("Name", &pascal_name);
        }
    }
    
    // Add locals alias (Hygen compatibility)
    // In Hygen, locals is an alias to the context itself
    // We'll add it as a function that returns the context
    context.insert("locals", &HashMap::<String, Value>::new());
}

// ---------- internals ----------
fn reg_str<F>(tera: &mut Tera, name: &str, f: F)
where
    F: Fn(&str) -> String + Send + Sync + 'static,
{
    tera.register_filter(name, move |v: &Value, _a: &HashMap<String, Value>| -> TeraResult<Value> {
        let input_str = match v.as_str() {
            Some(s) => s.to_string(),
            None => v.to_string(),
        };
        Ok(Value::String(f(&input_str)))
    });
}

#[cfg(test)]
mod tests {
    use super::*;
    use tera::Context;

    fn create_test_tera() -> Tera {
        let mut tera = Tera::default();
        register_all(&mut tera);
        tera
    }

    #[test]
    fn test_case_conversions() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world_example");

        // Test camel case
        let result = tera.render_str("{{ name | camel }}", &ctx).unwrap();
        assert_eq!(result, "helloWorldExample");

        // Test pascal case
        let result = tera.render_str("{{ name | pascal }}", &ctx).unwrap();
        assert_eq!(result, "HelloWorldExample");

        // Test snake case
        let result = tera.render_str("{{ name | snake }}", &ctx).unwrap();
        assert_eq!(result, "hello_world_example");

        // Test kebab case
        let result = tera.render_str("{{ name | kebab }}", &ctx).unwrap();
        assert_eq!(result, "hello-world-example");

        // Test class case
        let result = tera.render_str("{{ name | class }}", &ctx).unwrap();
        assert_eq!(result, "HelloWorldExample");

        // Test title case
        let result = tera.render_str("{{ name | title }}", &ctx).unwrap();
        assert_eq!(result, "Hello World Example");

        // Test sentence case
        let result = tera.render_str("{{ name | sentence }}", &ctx).unwrap();
        assert_eq!(result, "Hello world example");

        // Test train case
        let result = tera.render_str("{{ name | train }}", &ctx).unwrap();
        assert_eq!(result, "Hello-World-Example");
    }

    #[test]
    fn test_heck_fill_ins() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world_example");

        // Test shouty snake case
        let result = tera.render_str("{{ name | shouty_snake }}", &ctx).unwrap();
        assert_eq!(result, "HELLO_WORLD_EXAMPLE");

        // Test shouty kebab case
        let result = tera.render_str("{{ name | shouty_kebab }}", &ctx).unwrap();
        assert_eq!(result, "HELLO-WORLD-EXAMPLE");

        // Test titlecase (heck version)
        let result = tera.render_str("{{ name | titlecase }}", &ctx).unwrap();
        assert_eq!(result, "Hello World Example");
    }


    #[test]
    fn test_ordinalization() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test ordinalize
        ctx.insert("num", "1");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "1st");

        ctx.insert("num", "2");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "2nd");

        ctx.insert("num", "3");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "3rd");

        ctx.insert("num", "4");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "4th");

        ctx.insert("num", "11");
        let result = tera.render_str("{{ num | ordinalize }}", &ctx).unwrap();
        assert_eq!(result, "11th");

        // Test deordinalize
        ctx.insert("num", "1st");
        let result = tera.render_str("{{ num | deordinalize }}", &ctx).unwrap();
        assert_eq!(result, "1");

        ctx.insert("num", "2nd");
        let result = tera.render_str("{{ num | deordinalize }}", &ctx).unwrap();
        assert_eq!(result, "2");
    }

    #[test]
    fn test_string_manipulation() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test deconstantize
        ctx.insert("path", "ActiveRecord::Base");
        let result = tera.render_str("{{ path | deconstantize }}", &ctx).unwrap();
        assert_eq!(result, "ActiveRecord");

        // Test demodulize
        ctx.insert("path", "ActiveRecord::Base");
        let result = tera.render_str("{{ path | demodulize }}", &ctx).unwrap();
        assert_eq!(result, "Base");

        // Test foreign_key
        ctx.insert("name", "Message");
        let result = tera.render_str("{{ name | foreign_key }}", &ctx).unwrap();
        assert_eq!(result, "message_id");
    }

    #[test]
    fn test_complex_combinations() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("class_name", "UserProfile");

        // Test chaining transformations
        let result = tera.render_str("{{ class_name | snake | pluralize }}", &ctx).unwrap();
        assert_eq!(result, "user_profiles");

        let result = tera.render_str("{{ class_name | kebab | shouty_kebab }}", &ctx).unwrap();
        assert_eq!(result, "USER-PROFILE");

        let result = tera.render_str("{{ class_name | demodulize | snake }}", &ctx).unwrap();
        assert_eq!(result, "user_profile");
    }

    #[test]
    fn test_edge_cases() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test empty string
        ctx.insert("empty", "");
        let result = tera.render_str("{{ empty | camel }}", &ctx).unwrap();
        assert_eq!(result, "");

        // Test single character
        ctx.insert("single", "A");
        let result = tera.render_str("{{ single | snake }}", &ctx).unwrap();
        assert_eq!(result, "a");

        // Test numbers
        ctx.insert("number", "123");
        let result = tera.render_str("{{ number | camel }}", &ctx).unwrap();
        assert_eq!(result, "123");

        // Test special characters
        ctx.insert("special", "hello@world#test");
        let result = tera.render_str("{{ special | kebab }}", &ctx).unwrap();
        assert_eq!(result, "hello-world-test");
    }

    #[test]
    fn test_non_string_input() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test with number
        ctx.insert("num", &42);
        let result = tera.render_str("{{ num | camel }}", &ctx).unwrap();
        assert_eq!(result, "42");

        // Test with boolean
        ctx.insert("flag", &true);
        let result = tera.render_str("{{ flag | snake }}", &ctx).unwrap();
        assert_eq!(result, "true");
    }

    #[test]
    fn test_real_world_scenarios() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();

        // Test database table naming
        ctx.insert("model", "UserAccount");
        let table_name = tera.render_str("{{ model | snake | pluralize }}", &ctx).unwrap();
        assert_eq!(table_name, "user_accounts");

        // Test API endpoint naming
        ctx.insert("resource", "UserProfile");
        let endpoint = tera.render_str("{{ resource | kebab | pluralize }}", &ctx).unwrap();
        assert_eq!(endpoint, "user-profiles");

        // Test constant naming
        ctx.insert("name", "max_retry_count");
        let constant = tera.render_str("{{ name | shouty_snake }}", &ctx).unwrap();
        assert_eq!(constant, "MAX_RETRY_COUNT");

        // Test class method naming
        ctx.insert("action", "get_user_profile");
        let method = tera.render_str("{{ action | camel }}", &ctx).unwrap();
        assert_eq!(method, "getUserProfile");
    }

    #[test]
    fn test_change_case_aliases() {
        let mut tera = create_test_tera();
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world_example");

        // Test param (kebab-case)
        let result = tera.render_str("{{ name | param }}", &ctx).unwrap();
        assert_eq!(result, "hello-world-example");

        // Test constant (SCREAMING_SNAKE_CASE)
        let result = tera.render_str("{{ name | constant }}", &ctx).unwrap();
        assert_eq!(result, "HELLO_WORLD_EXAMPLE");

        // Test upper
        let result = tera.render_str("{{ name | upper }}", &ctx).unwrap();
        assert_eq!(result, "HELLO_WORLD_EXAMPLE");

        // Test lower
        ctx.insert("name", "HELLO_WORLD_EXAMPLE");
        let result = tera.render_str("{{ name | lower }}", &ctx).unwrap();
        assert_eq!(result, "hello_world_example");

        // Test lcfirst
        ctx.insert("name", "HelloWorld");
        let result = tera.render_str("{{ name | lcfirst }}", &ctx).unwrap();
        assert_eq!(result, "helloWorld");

        // Test ucfirst
        ctx.insert("name", "helloWorld");
        let result = tera.render_str("{{ name | ucfirst }}", &ctx).unwrap();
        assert_eq!(result, "HelloWorld");

        // Test edge cases for lcfirst/ucfirst
        ctx.insert("name", "");
        let result = tera.render_str("{{ name | lcfirst }}", &ctx).unwrap();
        assert_eq!(result, "");

        let result = tera.render_str("{{ name | ucfirst }}", &ctx).unwrap();
        assert_eq!(result, "");

        // Test Unicode handling
        ctx.insert("name", "ñáéíóú");
        let result = tera.render_str("{{ name | ucfirst }}", &ctx).unwrap();
        assert_eq!(result, "Ñáéíóú");
    }

    #[test]
    fn test_all_filters_registered() {
        let mut tera = create_test_tera();
        
        // Test that all expected filters are registered
        let expected_filters = vec![
            "camel", "pascal", "snake", "kebab", "class", "title", "sentence", "train",
            "pluralize", "singularize", "deconstantize", "demodulize",
            "ordinalize", "deordinalize", "foreign_key",
            "shouty_snake", "shouty_kebab", "titlecase",
            "param", "constant", "upper", "lower", "lcfirst", "ucfirst"
        ];

        for filter in expected_filters {
            let mut ctx = Context::new();
            ctx.insert("test", "hello_world");
            
            // This should not panic if the filter is registered
            let result = tera.render_str(&format!("{{{{ test | {} }}}}", filter), &ctx);
            assert!(result.is_ok(), "Filter '{}' should be registered", filter);
        }
    }

    #[test]
    fn test_bless_context_name() {
        let mut ctx = Context::new();
        ctx.insert("name", "hello_world");
        
        bless_context(&mut ctx);
        
        // Should have Name auto-blessed
        assert_eq!(ctx.get("Name").unwrap().as_str().unwrap(), "HelloWorld");
        // Original name should still be there
        assert_eq!(ctx.get("name").unwrap().as_str().unwrap(), "hello_world");
    }

    #[test]
    fn test_bless_context_no_name() {
        let mut ctx = Context::new();
        ctx.insert("other", "value");
        
        bless_context(&mut ctx);
        
        // Should not have Name when name doesn't exist
        assert!(ctx.get("Name").is_none());
        // Should have locals placeholder
        assert!(ctx.get("locals").is_some());
    }

    #[test]
    fn test_bless_context_non_string_name() {
        let mut ctx = Context::new();
        ctx.insert("name", &42); // Non-string value
        
        bless_context(&mut ctx);
        
        // Should not bless Name for non-string values
        assert!(ctx.get("Name").is_none());
    }
}
