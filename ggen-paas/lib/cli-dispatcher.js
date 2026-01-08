/**
 * ggen-paas CLI Dispatcher
 *
 * Routes all CLI commands to their respective handlers.
 * Manages command registration, help system, and argument parsing.
 */

import { Logger } from './utils/logger.js';
import { CommandBase } from './command-base.js';

// Simple color utility (no external deps needed for now)
const chalk = {
  bold: (str) => `\x1b[1m${str}\x1b[0m`,
  dim: (str) => `\x1b[2m${str}\x1b[0m`,
  red: (str) => `\x1b[31m${str}\x1b[0m`,
  green: (str) => `\x1b[32m${str}\x1b[0m`,
};

// Import all command handlers
import GenerateCommand from './commands/generate.js';
import ValidateCommand from './commands/validate.js';
import SyncCommand from './commands/sync.js';
import DeployCommand from './commands/deploy.js';
import StatusCommand from './commands/status.js';
import LogsCommand from './commands/logs.js';
import DescribeCommand from './commands/describe.js';
import ExplainCommand from './commands/explain.js';

/**
 * CLI Dispatcher - Routes commands to handlers
 */
export class CLIDispatcher {
  constructor(config = {}) {
    this.config = config;
    this.logger = new Logger(config);
    this.commands = new Map();
    this.commandsByName = new Map();
    this.registerCommands();
  }

  /**
   * Register all commands
   */
  registerCommands() {
    const commandClasses = [
      GenerateCommand,
      ValidateCommand,
      SyncCommand,
      DeployCommand,
      StatusCommand,
      LogsCommand,
      DescribeCommand,
      ExplainCommand,
    ];

    for (const CommandClass of commandClasses) {
      const cmd = new CommandClass();
      this.commands.set(cmd.name, cmd);
      this.commandsByName.set(cmd.name, cmd);

      // Register aliases
      if (cmd.aliases) {
        for (const alias of cmd.aliases) {
          this.commands.set(alias, cmd);
        }
      }
    }
  }

  /**
   * Parse command-line arguments
   */
  parseArgs(argv) {
    if (argv.length === 0) {
      return { command: null, args: {}, options: {} };
    }

    const [command, ...rest] = argv;
    const args = {};
    const options = {};
    let positionalIndex = 0;

    for (let i = 0; i < rest.length; i++) {
      const arg = rest[i];

      if (arg === '--help' || arg === '-h') {
        options.help = true;
      } else if (arg === '--version' || arg === '-v') {
        options.version = true;
      } else if (arg === '--verbose') {
        options.verbose = (options.verbose || 0) + 1;
      } else if (arg === '-vvv') {
        options.verbose = 3;
      } else if (arg === '--quiet' || arg === '-q') {
        options.quiet = true;
      } else if (arg.startsWith('--')) {
        // Long option: --key=value or --key value
        const [key, value] = arg.slice(2).split('=');
        options[this._camelCase(key)] = value || true;

        // If no value and next arg is not an option, treat it as the value
        if (!value && i + 1 < rest.length && !rest[i + 1].startsWith('-')) {
          options[this._camelCase(key)] = rest[++i];
        }
      } else if (arg.startsWith('-') && arg.length === 2) {
        // Short option: -x value
        const shortForm = arg;
        if (i + 1 < rest.length && !rest[i + 1].startsWith('-')) {
          options[shortForm] = rest[++i];
        } else {
          options[shortForm] = true;
        }
      } else {
        // Positional argument
        args[`arg${positionalIndex++}`] = arg;
      }
    }

    return { command, args, options };
  }

  /**
   * Display general help
   */
  showGeneralHelp() {
    console.log('');
    console.log(chalk.bold('ggen paas - Infrastructure generation and management'));
    console.log('');
    console.log('USAGE');
    console.log('  ggen paas <command> [options]');
    console.log('');
    console.log('COMMANDS');

    // Group commands by category
    const commandsByCategory = {};
    for (const [name, cmd] of this.commandsByName) {
      const category = cmd.category || 'Other';
      if (!commandsByCategory[category]) {
        commandsByCategory[category] = [];
      }
      commandsByCategory[category].push({
        name: cmd.name,
        description: cmd.description,
        aliases: cmd.aliases || [],
      });
    }

    for (const [category, cmds] of Object.entries(commandsByCategory)) {
      console.log(`\n${chalk.dim(category)}`);
      for (const cmd of cmds) {
        const aliasStr = cmd.aliases.length > 0 ? ` (${cmd.aliases.join(', ')})` : '';
        console.log(`  ${cmd.name.padEnd(15)} ${cmd.description}${aliasStr}`);
      }
    }

    console.log('');
    console.log('GLOBAL OPTIONS');
    console.log('  --help, -h          Show this help message');
    console.log('  --version, -v       Show version');
    console.log('  --verbose, -vvv     Increase verbosity');
    console.log('  --quiet, -q         Suppress output');
    console.log('');
    console.log('For help on a specific command:');
    console.log('  ggen paas <command> --help');
    console.log('');
  }

  /**
   * Display help for a specific command
   */
  showCommandHelp(commandName) {
    const cmd = this.commands.get(commandName);

    if (!cmd) {
      console.error(chalk.red(`Unknown command: ${commandName}`));
      this.showGeneralHelp();
      process.exit(2);
    }

    const schema = cmd.defineSchema();

    console.log('');
    console.log(chalk.bold(`ggen paas ${cmd.name} - ${cmd.description}`));
    console.log('');
    console.log('USAGE');

    const positionalStr =
      schema.positional.length > 0
        ? ' <' + schema.positional.map(a => a.name).join('> <') + '>'
        : '';
    console.log(`  ggen paas ${cmd.name}${positionalStr} [options]`);
    console.log('');

    if (schema.positional.length > 0) {
      console.log('ARGUMENTS');
      for (const arg of schema.positional) {
        console.log(`  ${arg.name}${arg.required ? '' : ' (optional)'}`.padEnd(20));
        console.log(`                        ${arg.description}`);
        if (arg.choices && arg.choices.length > 0) {
          console.log(`                        Choices: ${arg.choices.join(', ')}`);
        }
      }
      console.log('');
    }

    if (Object.keys(schema.options).length > 0) {
      console.log('OPTIONS');
      for (const [name, opt] of Object.entries(schema.options)) {
        const shortOpt = opt.shortForm ? ` ${opt.shortForm}` : '';
        console.log(`  ${opt.longForm}${shortOpt}`.padEnd(30) + opt.description);
        if (opt.default !== undefined) {
          console.log(`                                    Default: ${opt.default}`);
        }
        if (opt.choices && opt.choices.length > 0) {
          console.log(`                                    Choices: ${opt.choices.join(', ')}`);
        }
      }
      console.log('');
    }

    if (cmd.examples) {
      console.log('EXAMPLES');
      for (const example of cmd.examples.split('\n')) {
        if (example.trim()) {
          console.log(`  ${example.trim()}`);
        }
      }
      console.log('');
    }
  }

  /**
   * Show version
   */
  showVersion() {
    const pkg = this._loadPackageJson();
    console.log(chalk.bold(`ggen-paas v${pkg.version}`));
    console.log('');
  }

  /**
   * Dispatch command
   */
  async dispatch(argv) {
    const { command, args, options } = this.parseArgs(argv);

    // Handle global options
    if (!command || options.help) {
      this.showGeneralHelp();
      process.exit(command ? 0 : 1);
    }

    if (options.version) {
      this.showVersion();
      process.exit(0);
    }

    // Find command handler
    const handler = this.commands.get(command);
    if (!handler) {
      console.error(chalk.red(`Unknown command: ${command}`));
      this.showGeneralHelp();
      process.exit(2);
    }

    // Show command help
    if (options.help) {
      this.showCommandHelp(command);
      process.exit(0);
    }

    // Execute command
    try {
      const result = await handler.execute(args, options);
      process.exit(result.exitCode || 0);
    } catch (error) {
      console.error(chalk.red(`Error: ${error.message}`));
      if (options.verbose) {
        console.error(error.stack);
      }
      process.exit(1);
    }
  }

  /**
   * Convert kebab-case to camelCase
   * @private
   */
  _camelCase(str) {
    return str.replace(/-([a-z])/g, (g) => g[1].toUpperCase());
  }

  /**
   * Load package.json
   * @private
   */
  _loadPackageJson() {
    try {
      const pkg = JSON.parse(require('fs').readFileSync('./package.json', 'utf-8'));
      return pkg;
    } catch {
      return { version: '1.0.0', name: 'ggen-paas' };
    }
  }
}

