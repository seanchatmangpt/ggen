import { describe, it, expect, beforeEach, afterEach } from '@jest/globals';
import { spawn, ChildProcess } from 'child_process';
import { promises as fs } from 'fs';
import path from 'path';
import os from 'os';

/**
 * Chicago TDD Test Suite for TypeScript CLI Generation
 * Tests real CLI execution with Commander.js and Inquirer
 */

describe('TypeScript CLI Generation', () => {
  let tempDir: string;

  beforeEach(async () => {
    tempDir = await fs.mkdtemp(path.join(os.tmpdir(), 'cli-test-'));
  });

  afterEach(async () => {
    await fs.rm(tempDir, { recursive: true, force: true });
  });

  function runCLI(args: string[]): Promise<{ stdout: string; stderr: string; exitCode: number }> {
    return new Promise((resolve) => {
      const child = spawn('node', ['dist/cli.js', ...args]);
      let stdout = '';
      let stderr = '';

      child.stdout.on('data', (data) => {
        stdout += data.toString();
      });

      child.stderr.on('data', (data) => {
        stderr += data.toString();
      });

      child.on('close', (code) => {
        resolve({ stdout, stderr, exitCode: code || 0 });
      });
    });
  }

  it('should display help text', async () => {
    const result = await runCLI(['--help']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('Usage:');
    expect(result.stdout).toContain('Commands:');
  });

  it('should execute command with required arguments', async () => {
    const result = await runCLI(['process', 'input.txt']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('Processing: input.txt');
  });

  it('should execute command with options', async () => {
    const result = await runCLI(['build', '--mode', 'production', '--minify']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('mode: production');
    expect(result.stdout).toContain('minify: true');
  });

  it('should fail on missing required arguments', async () => {
    const result = await runCLI(['process']);
    expect(result.exitCode).not.toBe(0);
    expect(result.stderr).toContain('required');
  });

  it('should handle multiple option values', async () => {
    const result = await runCLI(['lint', '--exclude', '*.test.js', '--exclude', 'dist/*']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('Excludes: 2');
  });

  it('should load JSON config file', async () => {
    const configFile = path.join(tempDir, 'config.json');
    await fs.writeFile(configFile, JSON.stringify({
      verbose: true,
      outputDir: '/tmp/output',
      maxWorkers: 4
    }));

    const result = await runCLI(['--config', configFile, 'run']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('maxWorkers: 4');
  });

  it('should load YAML config file', async () => {
    const configFile = path.join(tempDir, 'config.yaml');
    await fs.writeFile(configFile, `
verbose: true
outputDir: /tmp/output
maxWorkers: 4
`);

    const result = await runCLI(['--config', configFile, 'run']);
    expect(result.exitCode).toBe(0);
  });

  it('should validate regex patterns', async () => {
    const result = await runCLI(['user', 'add', 'invalid-email']);
    expect(result.exitCode).not.toBe(0);
    expect(result.stderr).toContain('Invalid email');
  });

  it('should validate range constraints', async () => {
    const result = await runCLI(['server', 'start', '--port', '99999']);
    expect(result.exitCode).not.toBe(0);
    expect(result.stderr).toContain('Port must be between');
  });

  it('should validate enum values', async () => {
    const result = await runCLI(['build', '--mode', 'invalid']);
    expect(result.exitCode).not.toBe(0);

    const result2 = await runCLI(['build', '--mode', 'debug']);
    expect(result2.exitCode).toBe(0);
  });

  it('should generate bash completion', async () => {
    const result = await runCLI(['completion', 'bash']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('complete');
  });

  it('should handle file input', async () => {
    const inputFile = path.join(tempDir, 'input.txt');
    await fs.writeFile(inputFile, 'test data\n');

    const result = await runCLI(['process', inputFile]);
    expect(result.exitCode).toBe(0);
  });

  it('should create output files', async () => {
    const outputFile = path.join(tempDir, 'output.txt');

    const result = await runCLI(['generate', '--output', outputFile]);
    expect(result.exitCode).toBe(0);

    const exists = await fs.access(outputFile).then(() => true).catch(() => false);
    expect(exists).toBe(true);
  });

  it('should handle verbose logging', async () => {
    const result = await runCLI(['-v', 'run']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('verbose: true');
  });

  it('should display version', async () => {
    const result = await runCLI(['--version']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toMatch(/\d+\.\d+\.\d+/);
  });

  it('should handle subcommands', async () => {
    const result = await runCLI(['project', 'new', 'my-app']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('Created project: my-app');
  });

  it('should handle environment variables', async () => {
    const child = spawn('node', ['dist/cli.js', 'run'], {
      env: { ...process.env, CLI_VERBOSE: 'true', CLI_OUTPUT_DIR: '/tmp/test' }
    });

    let stdout = '';
    child.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    await new Promise((resolve) => child.on('close', resolve));
    expect(stdout).toContain('outputDir: /tmp/test');
  });

  it('should handle interactive prompts', async () => {
    const child = spawn('node', ['dist/cli.js', 'init']);

    child.stdin.write('my-project\n');
    child.stdin.write('typescript\n');
    child.stdin.end();

    let stdout = '';
    child.stdout.on('data', (data) => {
      stdout += data.toString();
    });

    await new Promise((resolve) => child.on('close', resolve));
    expect(stdout).toContain('my-project');
  });

  it('should handle errors gracefully', async () => {
    const result = await runCLI(['process', '/nonexistent/file.txt']);
    expect(result.exitCode).not.toBe(0);
    expect(result.stderr).toContain('Error');
  });

  it('should support command aliases', async () => {
    const result1 = await runCLI(['rm', 'file.txt']);
    const result2 = await runCLI(['remove', 'file.txt']);

    expect(result1.exitCode).toBe(result2.exitCode);
  });

  it('should inherit global options', async () => {
    const result = await runCLI(['--verbose', 'project', 'build']);
    expect(result.exitCode).toBe(0);
    expect(result.stdout).toContain('verbose');
  });
});
