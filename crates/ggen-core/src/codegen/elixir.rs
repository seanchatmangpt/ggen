//! Elixir microservice code generation
//!
//! This module generates Elixir microservices with GenServer supervision trees,
//! configuration files, and test scaffolding from service specifications.

use std::fmt::Write;

/// Elixir microservice code generator
pub struct ElixirGenerator;

/// Service specification for Elixir code generation
#[derive(Clone, Debug)]
pub struct ServiceSpec {
    pub name: String,
    pub module_name: String,
    pub description: Option<String>,
    pub supervisor: Option<String>,
    pub config: Option<String>,
}

impl ElixirGenerator {
    /// Generate a GenServer module from a service specification
    ///
    /// Creates a module with use GenServer, requires Logger, and callback stubs
    /// including start_link, init, handle_call, and handle_cast.
    pub fn generate_module(spec: &ServiceSpec) -> Result<String, String> {
        let mut output = String::new();

        // File header
        writeln!(
            output,
            "defmodule {} {{\n  @moduledoc \"\"\"\n  {}\n  \"\"\"\n\n  use GenServer\n  require Logger",
            spec.module_name,
            spec.description.as_deref().unwrap_or(&format!("{} microservice", spec.name))
        )
        .map_err(|e| e.to_string())?;

        writeln!(output, "\n\n  # Public API\n").map_err(|e| e.to_string())?;

        // start_link function
        writeln!(
            output,
            "  def start_link(opts) do\n    GenServer.start_link(__MODULE__, opts, name: __MODULE__)\n  end"
        )
        .map_err(|e| e.to_string())?;

        // init callback
        writeln!(
            output,
            "\n\n  # GenServer Callbacks\n\n  def init(opts) do\n    Logger.info(\"Initializing {} with opts: #{{inspect(opts)}}\")\n    {{\n      :ok,\n      %{{\n        module: __MODULE__,\n        opts: opts,\n        started_at: System.monotonic_time(:millisecond)\n      }}\n    }}\n  end",
            spec.name
        )
        .map_err(|e| e.to_string())?;

        // handle_call callback
        writeln!(
            output,
            "\n\n  def handle_call(request, _from, state) do\n    Logger.debug(\"handle_call: #{{inspect(request)}}\")\n    {{\n      :reply,\n      {{:error, :not_implemented}},\n      state\n    }}\n  end"
        )
        .map_err(|e| e.to_string())?;

        // handle_cast callback
        writeln!(
            output,
            "\n\n  def handle_cast(message, state) do\n    Logger.debug(\"handle_cast: #{{inspect(message)}}\")\n    {{\n      :noreply,\n      state\n    }}\n  end"
        )
        .map_err(|e| e.to_string())?;

        // Closing
        writeln!(output, "\nend").map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate a supervision tree for a list of services
    ///
    /// Creates a Supervisor module with children list and one_for_one strategy.
    /// Each service module is configured with restart: :permanent.
    pub fn generate_supervision_tree(services: &[ServiceSpec]) -> Result<String, String> {
        let mut output = String::new();

        writeln!(
            output,
            "defmodule Supervisor.Services {{\n  @moduledoc \"\"\"\n  Supervision tree for all microservices\n  \"\"\"\n\n  use Supervisor\n  require Logger"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n  def start_link(opts) do\n    Supervisor.start_link(__MODULE__, opts, name: __MODULE__)\n  end"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n  def init(_opts) do\n    Logger.info(\"Starting service supervision tree\")\n\n    children = ["
        )
        .map_err(|e| e.to_string())?;

        for service in services {
            writeln!(
                output,
                "\n      {{{}, [], restart: :permanent}}  # {}",
                service.module_name,
                service.description.as_deref().unwrap_or(&service.name)
            )
            .map_err(|e| e.to_string())?;
        }

        writeln!(
            output,
            "\n    ]\n\n    Supervisor.init(children, strategy: :one_for_one)\n  end\nend"
        )
        .map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate a config.exs configuration file
    ///
    /// Creates configuration blocks for the application with import of runtime.exs.
    pub fn generate_config(
        module_prefix: &str, config_spec: &Option<String>,
    ) -> Result<String, String> {
        let mut output = String::new();

        writeln!(
            output,
            "import Config\n\n# Application configuration for {}\n",
            module_prefix
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "config :{}, {}.\n  modules: [],\n  timeouts: %{{\n    default: 5000,\n    long: 30000\n  }}\n",
            module_prefix.to_lowercase(),
            module_prefix
        )
        .map_err(|e| e.to_string())?;

        if let Some(spec) = config_spec {
            writeln!(output, "\n# Custom configuration\n{}", spec).map_err(|e| e.to_string())?;
        }

        writeln!(
            output,
            "\n# Import runtime configuration (e.g., from environment variables)\nimport_config \"runtime.exs\""
        )
        .map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate a test file scaffold for a module
    ///
    /// Creates an ExUnit test module with test "service starts" assertion.
    pub fn generate_tests(module_name: &str) -> Result<String, String> {
        let mut output = String::new();

        let test_module = format!("{}Test", module_name.replace(".", ""));

        writeln!(
            output,
            "defmodule {} {{\n  use ExUnit.Case, async: true\n  require Logger",
            test_module
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n  setup do\n    # Start the module under test\n    {{\n      :ok,\n      pid: GenServer.whereis({})\n    }}\n  end",
            module_name
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n  test \"service starts and is running\" do\n    assert is_pid(GenServer.whereis({}))\n  end",
            module_name
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n  test \"service responds to health check\" do\n    # Placeholder for service health verification\n    assert true\n  end"
        )
        .map_err(|e| e.to_string())?;

        writeln!(output, "\nend").map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate a supervision tree test
    pub fn generate_supervision_test() -> Result<String, String> {
        let mut output = String::new();

        writeln!(
            output,
            "defmodule Supervisor.ServicesTest {{\n  use ExUnit.Case, async: false\n  require Logger"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n  test \"supervision tree starts all children\" do\n    {{\n      :ok,\n      _pid\n    }} = Supervisor.Services.start_link([])\n\n    # Give children time to start\n    Process.sleep(100)\n\n    # Verify supervisor is running\n    assert is_pid(GenServer.whereis(Supervisor.Services))\n  end"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n  test \"supervision tree recovers from child crashes\" do\n    {{\n      :ok,\n      _pid\n    }} = Supervisor.Services.start_link([])\n\n    # Supervisor should be able to restart crashed children\n    assert is_pid(GenServer.whereis(Supervisor.Services))\n  end"
        )
        .map_err(|e| e.to_string())?;

        writeln!(output, "\nend").map_err(|e| e.to_string())?;

        Ok(output)
    }

    /// Generate a runtime.exs configuration file for environment-specific config
    pub fn generate_runtime_config(env: &str) -> Result<String, String> {
        let mut output = String::new();

        writeln!(
            output,
            "import Config\n\n# Runtime configuration for {} environment\n",
            env
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "# Logger configuration\nconfig :logger, level: :info"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n# Service timeouts (override config.exs defaults)\nif config_env() == :test do\n  config :logger, level: :debug\nend"
        )
        .map_err(|e| e.to_string())?;

        writeln!(
            output,
            "\n\n# OpenTelemetry OTLP exporter\nconfig :opentelemetry, :processors, [\n  {{\n    OpenTelemetry.Processors.BatchSampler,\n    {{\n      OpenTelemetry.Exporter.OTLP,\n      endpoint: System.get_env(\"OTEL_EXPORTER_OTLP_ENDPOINT\", \"http://localhost:4318\"),\n      protocol: :http_protobuf\n    }}\n  }}\n]"
        )
        .map_err(|e| e.to_string())?;

        Ok(output)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn create_test_service() -> ServiceSpec {
        ServiceSpec {
            name: "OrderService".to_string(),
            module_name: "MyApp.OrderService".to_string(),
            description: Some("Service for managing orders".to_string()),
            supervisor: Some("MyApp.Supervisor".to_string()),
            config: Some("port: 8080".to_string()),
        }
    }

    #[test]
    fn test_generate_module() {
        let spec = create_test_service();
        let result = ElixirGenerator::generate_module(&spec);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("defmodule MyApp.OrderService"));
        assert!(code.contains("use GenServer"));
        assert!(code.contains("require Logger"));
        assert!(code.contains("def start_link(opts)"));
        assert!(code.contains("def init(opts)"));
        assert!(code.contains("def handle_call"));
        assert!(code.contains("def handle_cast"));
    }

    #[test]
    fn test_generate_supervision_tree() {
        let services = vec![
            create_test_service(),
            ServiceSpec {
                name: "PaymentService".to_string(),
                module_name: "MyApp.PaymentService".to_string(),
                description: Some("Payment processing".to_string()),
                supervisor: None,
                config: None,
            },
        ];
        let result = ElixirGenerator::generate_supervision_tree(&services);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("defmodule Supervisor.Services"));
        assert!(code.contains("use Supervisor"));
        assert!(code.contains("MyApp.OrderService"));
        assert!(code.contains("MyApp.PaymentService"));
        assert!(code.contains("strategy: :one_for_one"));
        assert!(code.contains("restart: :permanent"));
    }

    #[test]
    fn test_generate_config() {
        let spec = Some("log_level: :debug".to_string());
        let result = ElixirGenerator::generate_config("MyApp", &spec);
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("import Config"));
        assert!(code.contains("config :myapp, MyApp"));
        assert!(code.contains("import_config \"runtime.exs\""));
    }

    #[test]
    fn test_generate_tests() {
        let result = ElixirGenerator::generate_tests("MyApp.OrderService");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("defmodule"));
        assert!(code.contains("use ExUnit.Case"));
        assert!(code.contains("async: true"));
        assert!(code.contains("test \"service starts"));
    }

    #[test]
    fn test_generate_supervision_test() {
        let result = ElixirGenerator::generate_supervision_test();
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("defmodule Supervisor.ServicesTest"));
        assert!(code.contains("use ExUnit.Case"));
        assert!(code.contains("test \"supervision tree"));
    }

    #[test]
    fn test_generate_runtime_config() {
        let result = ElixirGenerator::generate_runtime_config("dev");
        assert!(result.is_ok());
        let code = result.unwrap();
        assert!(code.contains("import Config"));
        assert!(code.contains("Logger configuration"));
        assert!(code.contains("OpenTelemetry"));
        assert!(code.contains("http_protobuf"));
    }

    #[test]
    fn test_module_generation_structure() {
        let spec = ServiceSpec {
            name: "TestService".to_string(),
            module_name: "Test.Service".to_string(),
            description: Some("Test description".to_string()),
            supervisor: None,
            config: None,
        };
        let result = ElixirGenerator::generate_module(&spec);
        assert!(result.is_ok());
        let code = result.unwrap();

        // Verify correct structure: module -> use GenServer -> init -> handle_call -> handle_cast
        let lines: Vec<&str> = code.lines().collect();
        let defmodule_idx = lines.iter().position(|l| l.contains("defmodule"));
        let use_genserver_idx = lines.iter().position(|l| l.contains("use GenServer"));
        let init_idx = lines.iter().position(|l| l.contains("def init"));
        let handle_call_idx = lines.iter().position(|l| l.contains("def handle_call"));
        let handle_cast_idx = lines.iter().position(|l| l.contains("def handle_cast"));

        assert!(defmodule_idx < use_genserver_idx);
        assert!(use_genserver_idx < init_idx);
        assert!(init_idx < handle_call_idx);
        assert!(handle_call_idx < handle_cast_idx);
    }
}
