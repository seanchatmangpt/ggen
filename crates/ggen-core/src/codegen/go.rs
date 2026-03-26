//! Go code generation for microservices
//!
//! This module generates Go service structs, HTTP handlers, repository patterns,
//! and main.go templates from specification data.

use std::fmt::Write;

/// Go code generator for microservices
pub struct GoCodeGenerator;

impl GoCodeGenerator {
    /// Generate a Go service struct with dependencies
    ///
    /// Creates a struct with common microservice dependencies:
    /// - Config
    /// - Logger
    /// - Database connection
    ///
    /// # Arguments
    /// * `service_name` - Name of the service (e.g., "UserService")
    /// * `fields` - Additional field names to include (e.g., vec!["cache", "queue"])
    ///
    /// # Returns
    /// Go struct definition with standard dependencies
    pub fn generate_service_struct(service_name: &str, fields: &[&str]) -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "package main\n").map_err(|e| e.to_string())?;
        writeln!(output, "import (").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"fmt\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"log\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"net/http\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"database/sql\"").map_err(|e| e.to_string())?;
        writeln!(output, ")\n").map_err(|e| e.to_string())?;

        writeln!(output, "// {} represents the service with all dependencies", service_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "type {} struct {{", service_name).map_err(|e| e.to_string())?;
        writeln!(output, "\tConfig *Config").map_err(|e| e.to_string())?;
        writeln!(output, "\tLogger *log.Logger").map_err(|e| e.to_string())?;
        writeln!(output, "\tDB *sql.DB").map_err(|e| e.to_string())?;

        // Add custom fields
        for field in fields {
            writeln!(output, "\t{} interface{{}}", field).map_err(|e| e.to_string())?;
        }

        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        // Add Config struct
        writeln!(output, "// Config holds service configuration").map_err(|e| e.to_string())?;
        writeln!(output, "type Config struct {{").map_err(|e| e.to_string())?;
        writeln!(output, "\tPort string").map_err(|e| e.to_string())?;
        writeln!(output, "\tEnv string").map_err(|e| e.to_string())?;
        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate an HTTP handler function signature
    ///
    /// Creates a handler with error wrapping pattern and logging.
    ///
    /// # Arguments
    /// * `method` - HTTP method (GET, POST, etc.)
    /// * `path` - Route path (e.g., "/users/{id}")
    /// * `handler_name` - Function name (e.g., "GetUser")
    ///
    /// # Returns
    /// Handler function with error wrapping
    pub fn generate_handler(
        method: &str,
        path: &str,
        handler_name: &str,
    ) -> Result<String, String> {
        let mut output = String::new();

        writeln!(
            output,
            "// {} handles {} {} requests",
            handler_name, method, path
        )
        .map_err(|e| e.to_string())?;
        writeln!(
            output,
            "func (s *Service) {} (w http.ResponseWriter, r *http.Request) error {{",
            handler_name
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\ts.Logger.Printf(\"[{}] {} %s\\n\", r.URL.Path)",
            method, handler_name
        )
        .map_err(|e| e.to_string())?;

        writeln!(output, "\t// TODO: implement handler logic").map_err(|e| e.to_string())?;
        writeln!(
            output,
            "\tw.Header().Set(\"Content-Type\", \"application/json\")"
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "\tw.WriteHeader(http.StatusOK)").map_err(|e| e.to_string())?;
        writeln!(
            output,
            "\tw.Write([]byte(`{{\"status\":\"ok\"}}`))"
        )
        .map_err(|e| e.to_string())?;

        writeln!(output, "\treturn nil").map_err(|e| e.to_string())?;
        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate a repository pattern (interface + struct)
    ///
    /// Creates a data access layer interface and implementation.
    ///
    /// # Arguments
    /// * `entity_name` - Entity name (e.g., "User")
    ///
    /// # Returns
    /// Interface definition and basic implementation
    pub fn generate_repository(entity_name: &str) -> Result<String, String> {
        let mut output = String::new();
        let repo_name = format!("{}Repository", entity_name);

        writeln!(output, "// {} defines data access operations for {}", repo_name, entity_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "type {} interface {{", repo_name).map_err(|e| e.to_string())?;
        writeln!(output, "\tCreate(entity *{}) error", entity_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tGetByID(id string) (*{}, error)", entity_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tUpdate(entity *{}) error", entity_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tDelete(id string) error").map_err(|e| e.to_string())?;
        writeln!(output, "\tList() ([]*{}, error)", entity_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        // Implementation struct
        writeln!(output, "// {}Impl implements {}", entity_name, repo_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "type {}Impl struct {{", entity_name).map_err(|e| e.to_string())?;
        writeln!(output, "\tdb *sql.DB").map_err(|e| e.to_string())?;
        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        // Create method
        writeln!(
            output,
            "// Create inserts a new {} into the database",
            entity_name
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "func (r *{}Impl) Create(entity *{}) error {{", entity_name, entity_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tif entity == nil {{").map_err(|e| e.to_string())?;
        writeln!(
            output,
            "\t\treturn fmt.Errorf(\"create: %w\", ErrInvalidEntity)"
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;
        writeln!(output, "\t// TODO: implement create logic").map_err(|e| e.to_string())?;
        writeln!(output, "\treturn nil").map_err(|e| e.to_string())?;
        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        // GetByID method
        writeln!(
            output,
            "// GetByID retrieves a {} by its identifier",
            entity_name
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "func (r *{}Impl) GetByID(id string) (*{}, error) {{", entity_name, entity_name)
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tif id == \"\" {{").map_err(|e| e.to_string())?;
        writeln!(
            output,
            "\t\treturn nil, fmt.Errorf(\"get by id: %w\", ErrInvalidID)"
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;
        writeln!(output, "\t// TODO: implement get logic").map_err(|e| e.to_string())?;
        writeln!(output, "\treturn &{}{{}}, nil", entity_name).map_err(|e| e.to_string())?;
        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        // Error definitions
        writeln!(output, "// Error types").map_err(|e| e.to_string())?;
        writeln!(output, "var (").map_err(|e| e.to_string())?;
        writeln!(output, "\tErrInvalidEntity = errors.New(\"entity is nil\")")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tErrInvalidID = errors.New(\"id is empty\")")
            .map_err(|e| e.to_string())?;
        writeln!(output, ")\n").map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate a main.go template
    ///
    /// Creates a complete main.go file with service initialization,
    /// HTTP routing, and graceful shutdown.
    ///
    /// # Returns
    /// Complete main.go implementation
    pub fn generate_main() -> Result<String, String> {
        let mut output = String::new();

        writeln!(output, "package main\n").map_err(|e| e.to_string())?;
        writeln!(output, "import (").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"context\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"database/sql\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"fmt\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"log\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"net/http\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"os\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"os/signal\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"syscall\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t\"time\"").map_err(|e| e.to_string())?;
        writeln!(output, ")\n").map_err(|e| e.to_string())?;

        writeln!(output, "func main() {{").map_err(|e| e.to_string())?;
        writeln!(output, "\tlogger := log.New(os.Stdout, \"[APP] \", log.LstdFlags)")
            .map_err(|e| e.to_string())?;

        writeln!(output, "\n\t// Load configuration").map_err(|e| e.to_string())?;
        writeln!(output, "\tcfg := &Config{{").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tPort: os.Getenv(\"PORT\")").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tEnv: os.Getenv(\"ENV\")").map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;
        writeln!(output, "\tif cfg.Port == \"\" {{").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tcfg.Port = \":8080\"").map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;

        writeln!(output, "\n\t// Connect to database").map_err(|e| e.to_string())?;
        writeln!(output, "\tdb, err := sql.Open(\"postgres\", os.Getenv(\"DATABASE_URL\"))")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tif err != nil {{").map_err(|e| e.to_string())?;
        writeln!(
            output,
            "\t\tlogger.Fatalf(\"failed to connect to database: %v\", err)"
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;
        writeln!(output, "\tdefer db.Close()").map_err(|e| e.to_string())?;

        writeln!(output, "\n\t// Initialize service").map_err(|e| e.to_string())?;
        writeln!(output, "\tsvc := &Service{{").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tConfig: cfg,").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tLogger: logger,").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tDB: db,").map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;

        writeln!(output, "\n\t// Setup HTTP router").map_err(|e| e.to_string())?;
        writeln!(output, "\tmux := http.NewServeMux()").map_err(|e| e.to_string())?;
        writeln!(output, "\tmux.HandleFunc(\"/health\", svc.HealthCheck)")
            .map_err(|e| e.to_string())?;

        writeln!(output, "\n\t// Create HTTP server").map_err(|e| e.to_string())?;
        writeln!(output, "\tserver := &http.Server{{").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tAddr: cfg.Port,").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tHandler: mux,").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tReadTimeout: 10 * time.Second,").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tWriteTimeout: 10 * time.Second,").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tIdleTimeout: 60 * time.Second,").map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;

        writeln!(output, "\n\t// Start server in goroutine").map_err(|e| e.to_string())?;
        writeln!(output, "\tgo func() {{").map_err(|e| e.to_string())?;
        writeln!(output, "\t\tlogger.Printf(\"server starting on %s\\n\", cfg.Port)")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\t\tif err := server.ListenAndServe(); err != nil {{")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\t\t\tlogger.Fatalf(\"server error: %v\", err)")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\t\t}}").map_err(|e| e.to_string())?;
        writeln!(output, "\t}}()").map_err(|e| e.to_string())?;

        writeln!(output, "\n\t// Setup graceful shutdown").map_err(|e| e.to_string())?;
        writeln!(output, "\tsigChan := make(chan os.Signal, 1)")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tsignal.Notify(sigChan, syscall.SIGINT, syscall.SIGTERM)")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\t<-sigChan").map_err(|e| e.to_string())?;

        writeln!(output, "\n\tlogger.Println(\"shutdown signal received\")").map_err(|e| e.to_string())?;
        writeln!(output, "\tctx, cancel := context.WithTimeout(context.Background(), 5*time.Second)")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tdefer cancel()").map_err(|e| e.to_string())?;
        writeln!(output, "\tif err := server.Shutdown(ctx); err != nil {{")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\t\tlogger.Fatalf(\"shutdown error: %v\", err)")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\t}}").map_err(|e| e.to_string())?;
        writeln!(output, "\tlogger.Println(\"server stopped\")").map_err(|e| e.to_string())?;
        writeln!(output, "}}\n").map_err(|e| e.to_string())?;

        writeln!(output, "// HealthCheck returns 200 OK").map_err(|e| e.to_string())?;
        writeln!(output, "func (s *Service) HealthCheck(w http.ResponseWriter, r *http.Request) {{")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tw.Header().Set(\"Content-Type\", \"application/json\")")
            .map_err(|e| e.to_string())?;
        writeln!(output, "\tw.WriteHeader(http.StatusOK)").map_err(|e| e.to_string())?;
        writeln!(
            output,
            "\tw.Write([]byte(`{{\"status\":\"healthy\"}}`))  "
        )
        .map_err(|e| e.to_string())?;
        writeln!(output, "}}").map_err(|e| e.to_string())?;

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_generate_service_struct() {
        let result = GoCodeGenerator::generate_service_struct("UserService", &["cache", "queue"]);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("type UserService struct"));
        assert!(code.contains("Config *Config"));
        assert!(code.contains("Logger *log.Logger"));
        assert!(code.contains("DB *sql.DB"));
        assert!(code.contains("cache interface{}"));
        assert!(code.contains("queue interface{}"));
        assert!(code.contains("type Config struct"));
        assert!(code.contains("Port string"));
    }

    #[test]
    fn test_generate_handler() {
        let result = GoCodeGenerator::generate_handler("GET", "/users/{id}", "GetUser");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("func (s *Service) GetUser (w http.ResponseWriter, r *http.Request) error"));
        assert!(code.contains("[GET]"));
        assert!(code.contains("http.StatusOK"));
        assert!(code.contains("application/json"));
        assert!(code.contains("return nil"));
    }

    #[test]
    fn test_generate_repository() {
        let result = GoCodeGenerator::generate_repository("User");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("type UserRepository interface"));
        assert!(code.contains("Create(entity *User) error"));
        assert!(code.contains("GetByID(id string) (*User, error)"));
        assert!(code.contains("Update(entity *User) error"));
        assert!(code.contains("Delete(id string) error"));
        assert!(code.contains("List() ([]*User, error)"));
        assert!(code.contains("type UserImpl struct"));
        assert!(code.contains("db *sql.DB"));
        assert!(code.contains("ErrInvalidEntity"));
        assert!(code.contains("ErrInvalidID"));
    }

    #[test]
    fn test_generate_main() {
        let result = GoCodeGenerator::generate_main();
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("func main()"));
        assert!(code.contains("sql.Open"));
        assert!(code.contains("http.NewServeMux"));
        assert!(code.contains("ListenAndServe"));
        assert!(code.contains("graceful shutdown"));
        assert!(code.contains("syscall.SIGINT"));
        assert!(code.contains("syscall.SIGTERM"));
        assert!(code.contains("HealthCheck"));
        assert!(code.contains("PORT"));
        assert!(code.contains("DATABASE_URL"));
    }

    #[test]
    fn test_multiple_service_fields() {
        let fields = &["metrics", "tracer", "pubsub"];
        let result = GoCodeGenerator::generate_service_struct("OrderService", fields);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("metrics interface{}"));
        assert!(code.contains("tracer interface{}"));
        assert!(code.contains("pubsub interface{}"));
    }

    #[test]
    fn test_handler_different_methods() {
        let methods = vec!["GET", "POST", "PUT", "DELETE"];
        for method in methods {
            let result = GoCodeGenerator::generate_handler(method, "/items", "HandleItem");
            assert!(result.is_ok());
            let code = result.unwrap();
            assert!(code.contains(method));
            assert!(code.contains("HandleItem"));
        }
    }

    #[test]
    fn test_repository_error_handling() {
        let result = GoCodeGenerator::generate_repository("Product");
        assert!(result.is_ok());
        let code = result.unwrap();
        // Verify error wrapping pattern
        assert!(code.contains("fmt.Errorf"));
        assert!(code.contains("%w"));
        assert!(code.contains("ErrInvalidEntity"));
        assert!(code.contains("ErrInvalidID"));
    }
}
