/**
 * Converts a hexadecimal color code to an RGB object.
 * @param {string} hex - The hexadecimal color code (e.g., "#RRGGBB" or "RRGGBB").
 * @returns {{r: number, g: number, b: number} | null} An object containing the red, green, and blue components if the input is valid; otherwise, null.
 * @example
 * const rgb = hexToRgb("#ff0000");
 * // rgb === { r: 255, g: 0, b: 0 }
 */
function hexToRgb(hex) {
  const result = /^#?([a-f\d]{2})([a-f\d]{2})([a-f\d]{2})$/i.exec(hex);
  return result
    ? {
        r: parseInt(result[1], 16),
        g: parseInt(result[2], 16),
        b: parseInt(result[3], 16)
      }
    : null;
}

/**
 * Converts an RGB color to a hexadecimal color code.
 * @param {number} r - The red component (0-255).
 * @param {number} g - The green component (0-255).
 * @param {number} b - The blue component (0-255).
 * @returns {string} The hexadecimal color code (e.g., "#RRGGBB").
 * @throws {Error} If any of the components (r, g, b) are not in the range 0-255.
 * @example
 * const hex = rgbToHex(255, 0, 0);
 * // hex === "#ff0000"
 */
function rgbToHex(r, g, b) {
  if (r < 0 || r > 255 || g < 0 || g > 255 || b < 0 || b > 255) {
    throw new Error("RGB values must be in the range 0-255");
  }
  return "#" + ((1 << 24) + (r << 16) + (g << 8) + b).toString(16).slice(1);
}