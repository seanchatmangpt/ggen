/**
 * Terminal colorization utility
 * Provides colorized output for better CLI UX
 */

/**
 * Colorize text for terminal output
 * Supports basic colors and styles
 *
 * @param {string} text - Text to colorize
 * @param {string} color - Color name (red, green, yellow, blue, cyan, gray, white)
 * @param {string} style - Style (bold, dim, italic, underline)
 * @returns {string} Colorized text with ANSI codes
 */
export function colorize(text, color = 'white', style = 'normal') {
  const colors = {
    reset: '\x1b[0m',
    bold: '\x1b[1m',
    dim: '\x1b[2m',
    italic: '\x1b[3m',
    underline: '\x1b[4m',
    red: '\x1b[31m',
    green: '\x1b[32m',
    yellow: '\x1b[33m',
    blue: '\x1b[34m',
    cyan: '\x1b[36m',
    magenta: '\x1b[35m',
    gray: '\x1b[90m',
    white: '\x1b[97m',
  };

  const colorCode = colors[color] || colors.white;
  const styleCode = colors[style] || '';

  return `${styleCode}${colorCode}${text}${colors.reset}`;
}

/**
 * Print a colored section header
 */
export function header(text) {
  console.log();
  console.log(colorize('═'.repeat(80), 'cyan'));
  console.log(colorize(text, 'cyan', 'bold'));
  console.log(colorize('═'.repeat(80), 'cyan'));
  console.log();
}

/**
 * Print success message
 */
export function success(text) {
  console.log(colorize('✅ ' + text, 'green'));
}

/**
 * Print error message
 */
export function error(text) {
  console.log(colorize('❌ ' + text, 'red'));
}

/**
 * Print info message
 */
export function info(text) {
  console.log(colorize('ℹ️ ' + text, 'blue'));
}

/**
 * Print warning message
 */
export function warning(text) {
  console.log(colorize('⚠️ ' + text, 'yellow'));
}
