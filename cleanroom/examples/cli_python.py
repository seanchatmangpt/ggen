#!/usr/bin/env python3
"""
Example: Using cleanroom CLI from Python

This example demonstrates how to integrate the cleanroom CLI into Python applications.
It shows proper error handling, JSON parsing, and common usage patterns.
"""

import subprocess
import json
import sys
from typing import Dict, List, Any, Optional


class CleanroomError(Exception):
    """Custom exception for cleanroom CLI errors"""
    pass


class CleanroomCLI:
    """Wrapper for cleanroom CLI commands"""

    def __init__(self, cli_path: str = "cleanroom"):
        self.cli_path = cli_path

    def run(self, args: List[str], output_format: str = "json") -> Optional[Dict[str, Any]]:
        """
        Run cleanroom CLI command and return parsed result

        Args:
            args: Command arguments (e.g., ['environment', 'create', '--name', 'test'])
            output_format: Output format ('json' or 'text')

        Returns:
            Parsed JSON result or None for text output

        Raises:
            CleanroomError: If command fails
        """
        cmd = [self.cli_path] + args
        if output_format == "json":
            cmd += ["--output", "json"]

        try:
            result = subprocess.run(
                cmd,
                capture_output=True,
                text=True,
                check=True
            )

            if output_format == "json" and result.stdout:
                return json.loads(result.stdout)
            return None

        except subprocess.CalledProcessError as e:
            error_msg = e.stderr.strip() if e.stderr else "Unknown error"
            raise CleanroomError(f"Command failed: {error_msg}") from e
        except json.JSONDecodeError as e:
            raise CleanroomError(f"Failed to parse JSON output: {e}") from e


def example_basic_usage():
    """Example: Basic cleanroom operations"""
    print("=== Basic Usage Example ===\n")

    cli = CleanroomCLI()

    try:
        # Create environment
        print("Creating environment...")
        env_result = cli.run(['environment', 'create', '--name', 'test-env'])
        print(f"Created environment: {json.dumps(env_result, indent=2)}\n")

        # Start PostgreSQL container
        print("Starting PostgreSQL container...")
        container = cli.run([
            'container', 'start', 'postgres',
            '--db', 'testdb',
            '--user', 'testuser',
            '--password', 'testpass'
        ])
        print(f"Started container: {json.dumps(container, indent=2)}\n")

        # Get container connection info
        if container and 'port' in container:
            print(f"Connect to: postgresql://testuser:testpass@localhost:{container['port']}/testdb\n")

        # List running containers
        print("Listing containers...")
        containers = cli.run(['container', 'list'])
        print(f"Running containers: {json.dumps(containers, indent=2)}\n")

        # Cleanup
        print("Cleaning up...")
        cli.run(['environment', 'delete', '--name', 'test-env'])
        print("Environment deleted\n")

    except CleanroomError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)


def example_test_runner():
    """Example: Running tests with cleanroom"""
    print("=== Test Runner Example ===\n")

    cli = CleanroomCLI()

    try:
        # Create test environment
        cli.run(['environment', 'create', '--name', 'test-env'])

        # Start required services
        print("Starting services...")
        cli.run(['container', 'start', 'postgres', '--db', 'testdb'])
        cli.run(['container', 'start', 'redis'])

        # Run tests
        print("Running tests...")
        results = cli.run(['test', 'run', '--file', 'integration_tests.rs'])

        if results:
            print(f"Tests passed: {results.get('passed', 0)}")
            print(f"Tests failed: {results.get('failed', 0)}")
            print(f"Duration: {results.get('duration', 0)}s\n")

            if results.get('failed', 0) > 0:
                print("Failed tests:")
                for test in results.get('failures', []):
                    print(f"  - {test}")

        # Get metrics
        print("Collecting metrics...")
        metrics = cli.run(['metrics', 'show'])
        print(f"Metrics: {json.dumps(metrics, indent=2)}\n")

    except CleanroomError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    finally:
        # Cleanup
        print("Cleaning up...")
        try:
            cli.run(['environment', 'delete', '--name', 'test-env'])
        except CleanroomError:
            pass


def example_multi_container():
    """Example: Managing multiple containers"""
    print("=== Multi-Container Example ===\n")

    cli = CleanroomCLI()

    try:
        # Create environment
        cli.run(['environment', 'create', '--name', 'microservices'])

        # Start multiple services
        services = [
            ('postgres', ['--db', 'users', '--user', 'admin']),
            ('redis', []),
            ('rabbitmq', []),
        ]

        print("Starting services...")
        for service, args in services:
            print(f"  Starting {service}...")
            result = cli.run(['container', 'start', service] + args)
            if result and 'port' in result:
                print(f"    {service} available on port {result['port']}")

        print("\nAll services started!\n")

        # Get health status
        print("Checking health...")
        health = cli.run(['health', 'check'])
        print(f"Health status: {json.dumps(health, indent=2)}\n")

        # Wait for user input
        input("Press Enter to stop services...")

    except CleanroomError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    finally:
        # Cleanup
        print("\nStopping services...")
        try:
            cli.run(['environment', 'delete', '--name', 'microservices'])
            print("Services stopped")
        except CleanroomError:
            pass


def example_error_handling():
    """Example: Proper error handling"""
    print("=== Error Handling Example ===\n")

    cli = CleanroomCLI()

    # Try to use non-existent environment
    try:
        cli.run(['environment', 'delete', '--name', 'non-existent'])
    except CleanroomError as e:
        print(f"Expected error caught: {e}\n")

    # Try to start invalid container
    try:
        cli.run(['container', 'start', 'invalid-service'])
    except CleanroomError as e:
        print(f"Expected error caught: {e}\n")

    print("Error handling works correctly!")


def example_metrics_collection():
    """Example: Collecting and analyzing metrics"""
    print("=== Metrics Collection Example ===\n")

    cli = CleanroomCLI()

    try:
        # Create environment
        cli.run(['environment', 'create', '--name', 'metrics-test'])

        # Run some operations
        cli.run(['container', 'start', 'postgres'])
        cli.run(['test', 'run', '--file', 'test.rs'])

        # Collect metrics
        metrics = cli.run(['metrics', 'show'])

        if metrics:
            print("Performance Metrics:")
            print(f"  Container startup time: {metrics.get('startup_time', 'N/A')}")
            print(f"  Tests executed: {metrics.get('tests_executed', 0)}")
            print(f"  Memory usage: {metrics.get('memory_mb', 0)} MB")
            print(f"  CPU usage: {metrics.get('cpu_percent', 0)}%\n")

            # Export metrics to file
            with open('/tmp/cleanroom-metrics.json', 'w') as f:
                json.dump(metrics, f, indent=2)
            print("Metrics exported to /tmp/cleanroom-metrics.json")

    except CleanroomError as e:
        print(f"Error: {e}", file=sys.stderr)
        sys.exit(1)
    finally:
        cli.run(['environment', 'delete', '--name', 'metrics-test'])


def main():
    """Run all examples"""
    examples = [
        ("Basic Usage", example_basic_usage),
        ("Test Runner", example_test_runner),
        ("Multi-Container", example_multi_container),
        ("Error Handling", example_error_handling),
        ("Metrics Collection", example_metrics_collection),
    ]

    print("Cleanroom CLI Python Integration Examples\n")
    print("Available examples:")
    for i, (name, _) in enumerate(examples, 1):
        print(f"  {i}. {name}")

    print("\nRunning all examples...\n")
    print("=" * 60)

    for name, func in examples:
        try:
            func()
        except Exception as e:
            print(f"\nExample '{name}' failed: {e}\n")
        print("=" * 60 + "\n")


if __name__ == "__main__":
    main()
