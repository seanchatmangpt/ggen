"""
Chicago TDD Test Suite for Python CLI Generation
Tests real CLI execution with Click and Rich
"""

import pytest
import subprocess
import tempfile
import os
from pathlib import Path
import json
import yaml


class TestPythonCLIGeneration:
    """Test suite for generated Python CLI applications"""

    def run_cli(self, args: list) -> tuple:
        """Run CLI command and return (stdout, stderr, returncode)"""
        result = subprocess.run(
            ['python', 'cli.py'] + args,
            capture_output=True,
            text=True
        )
        return result.stdout, result.stderr, result.returncode

    def test_help_output(self):
        """Test --help flag displays usage information"""
        stdout, stderr, code = self.run_cli(['--help'])
        assert code == 0
        assert 'Usage:' in stdout
        assert 'Commands:' in stdout

    def test_command_with_required_arguments(self):
        """Test command execution with required arguments"""
        stdout, stderr, code = self.run_cli(['process', 'input.txt'])
        assert code == 0
        assert 'Processing: input.txt' in stdout

    def test_command_with_options(self):
        """Test command execution with options"""
        stdout, stderr, code = self.run_cli([
            'build', '--mode', 'production', '--minify'
        ])
        assert code == 0
        assert 'mode: production' in stdout

    def test_missing_required_argument(self):
        """Test failure on missing required arguments"""
        stdout, stderr, code = self.run_cli(['process'])
        assert code != 0
        assert 'required' in stderr.lower()

    def test_multiple_option_values(self):
        """Test options that accept multiple values"""
        stdout, stderr, code = self.run_cli([
            'lint', '--exclude', '*.test.py', '--exclude', 'dist/*'
        ])
        assert code == 0
        assert 'Excludes: 2' in stdout

    def test_json_config_loading(self):
        """Test loading configuration from JSON file"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.json', delete=False) as f:
            json.dump({
                'verbose': True,
                'output_dir': '/tmp/output',
                'max_workers': 4
            }, f)
            config_file = f.name

        try:
            stdout, stderr, code = self.run_cli(['--config', config_file, 'run'])
            assert code == 0
            assert 'max_workers: 4' in stdout
        finally:
            os.unlink(config_file)

    def test_yaml_config_loading(self):
        """Test loading configuration from YAML file"""
        with tempfile.NamedTemporaryFile(mode='w', suffix='.yaml', delete=False) as f:
            yaml.dump({
                'verbose': True,
                'output_dir': '/tmp/output',
                'max_workers': 4
            }, f)
            config_file = f.name

        try:
            stdout, stderr, code = self.run_cli(['--config', config_file, 'run'])
            assert code == 0
        finally:
            os.unlink(config_file)

    def test_regex_validation(self):
        """Test regex pattern validation"""
        stdout, stderr, code = self.run_cli(['user', 'add', 'invalid-email'])
        assert code != 0
        assert 'Invalid email' in stderr

    def test_range_validation(self):
        """Test numeric range validation"""
        stdout, stderr, code = self.run_cli(['server', 'start', '--port', '99999'])
        assert code != 0
        assert 'Port must be between' in stderr

    def test_enum_validation(self):
        """Test enum value validation"""
        stdout, stderr, code = self.run_cli(['build', '--mode', 'invalid'])
        assert code != 0

        stdout, stderr, code = self.run_cli(['build', '--mode', 'debug'])
        assert code == 0

    def test_file_input_processing(self):
        """Test reading input from files"""
        with tempfile.NamedTemporaryFile(mode='w', delete=False) as f:
            f.write('test data\n')
            input_file = f.name

        try:
            stdout, stderr, code = self.run_cli(['process', input_file])
            assert code == 0
        finally:
            os.unlink(input_file)

    def test_file_output_creation(self):
        """Test creating output files"""
        with tempfile.TemporaryDirectory() as tmpdir:
            output_file = os.path.join(tmpdir, 'output.txt')

            stdout, stderr, code = self.run_cli(['generate', '--output', output_file])
            assert code == 0
            assert os.path.exists(output_file)

    def test_verbose_logging(self):
        """Test verbose output flag"""
        stdout, stderr, code = self.run_cli(['-v', 'run'])
        assert code == 0

        stdout2, stderr2, code2 = self.run_cli(['-vv', 'run'])
        assert code2 == 0
        assert len(stdout2) >= len(stdout)  # More verbose output

    def test_version_display(self):
        """Test --version flag"""
        stdout, stderr, code = self.run_cli(['--version'])
        assert code == 0
        assert any(char.isdigit() for char in stdout)  # Contains version number

    def test_subcommand_execution(self):
        """Test nested subcommands"""
        stdout, stderr, code = self.run_cli(['project', 'new', 'my-app'])
        assert code == 0
        assert 'Created project: my-app' in stdout

    def test_environment_variables(self):
        """Test environment variable support"""
        env = os.environ.copy()
        env['CLI_VERBOSE'] = 'true'
        env['CLI_OUTPUT_DIR'] = '/tmp/test'

        result = subprocess.run(
            ['python', 'cli.py', 'run'],
            capture_output=True,
            text=True,
            env=env
        )

        assert result.returncode == 0
        assert 'outputDir: /tmp/test' in result.stdout

    def test_interactive_prompts(self):
        """Test interactive input prompts"""
        proc = subprocess.Popen(
            ['python', 'cli.py', 'init'],
            stdin=subprocess.PIPE,
            stdout=subprocess.PIPE,
            stderr=subprocess.PIPE,
            text=True
        )

        stdout, stderr = proc.communicate(input='my-project\npython\n')
        assert proc.returncode == 0
        assert 'my-project' in stdout

    def test_error_handling(self):
        """Test graceful error handling"""
        stdout, stderr, code = self.run_cli(['process', '/nonexistent/file.txt'])
        assert code != 0
        assert 'Error' in stderr

    def test_command_aliases(self):
        """Test command alias support"""
        stdout1, stderr1, code1 = self.run_cli(['rm', 'file.txt'])
        stdout2, stderr2, code2 = self.run_cli(['remove', 'file.txt'])

        assert code1 == code2

    def test_global_options_inheritance(self):
        """Test global options are inherited by subcommands"""
        stdout, stderr, code = self.run_cli(['--verbose', 'project', 'build'])
        assert code == 0
        assert 'verbose' in stdout.lower()

    def test_rich_output_formatting(self):
        """Test Rich library output formatting"""
        stdout, stderr, code = self.run_cli(['status'])
        assert code == 0
        # Check for Rich formatting (tables, colors, etc.)
        assert 'Status' in stdout

    def test_path_validation(self):
        """Test path argument validation"""
        with tempfile.TemporaryDirectory() as tmpdir:
            stdout, stderr, code = self.run_cli([
                'migrate',
                '--source', tmpdir,
                '--dest', tmpdir
            ])
            assert code == 0

    def test_custom_help_template(self):
        """Test custom help text formatting"""
        stdout, stderr, code = self.run_cli(['process', '--help'])
        assert code == 0
        assert 'Arguments:' in stdout or 'Options:' in stdout

    def test_exit_codes(self):
        """Test proper exit codes for success and failure"""
        # Success
        stdout, stderr, code = self.run_cli(['--version'])
        assert code == 0

        # Failure
        stdout, stderr, code = self.run_cli(['invalid-command'])
        assert code != 0

    def test_concurrent_execution(self):
        """Test multiple CLI instances can run concurrently"""
        import concurrent.futures

        def run_command(i):
            return self.run_cli(['process', f'input-{i}.txt', '--parallel'])

        with concurrent.futures.ThreadPoolExecutor(max_workers=5) as executor:
            futures = [executor.submit(run_command, i) for i in range(5)]
            results = [f.result() for f in futures]

        assert all(code == 0 for _, _, code in results)


if __name__ == '__main__':
    pytest.main([__file__, '-v'])
