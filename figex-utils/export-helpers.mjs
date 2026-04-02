/**
 * Converts data to JSON format
 * @param {Object} data - Data to convert
 * @returns {string} JSON string
 * @throws {Error} If data is not an object
 * @example
 * const result = toJSON({ name: "John", age: 30 });
 * console.log(result); // '{"name":"John","age":30}'
 */
export function toJSON(data) {
  if (typeof data !== 'object' || data === null) {
    throw new Error('Input must be an object');
  }
  return JSON.stringify(data, null, 2);
}

/**
 * Converts an array of objects to CSV format
 * @param {Array<Object>} rows - Array of objects to convert
 * @returns {string} CSV string
 * @throws {Error} If rows is not an array or contains non-objects
 * @example
 * const csvData = [{ name: "John", age: 30 }, { name: "Jane", age: 25 }];
 * const csv = toCSV(csvData);
 * console.log(csv); // 'name,age\nJohn,30\nJane,25'
 */
export function toCSV(rows) {
  if (!Array.isArray(rows)) {
    throw new Error('Input must be an array');
  }

  if (rows.length === 0) {
    return '';
  }

  const headers = Object.keys(rows[0]);
  const csv = [headers.join(',')];

  for (const row of rows) {
    const values = headers.map(header => {
      const value = row[header];
      return typeof value === 'string' ? `"${value}"` : value;
    });
    csv.push(values.join(','));
  }

  return csv.join('\n');
}

/**
 * Converts content to HTML format
 * @param {string | Array<string>} content - Content to convert
 * @returns {string} HTML string
 * @throws {Error} If content is not a string or array of strings
 * @example
 * const htmlContent = ['<h1>Hello</h1>', '<p>World</p>'];
 * const html = toHTML(htmlContent);
 * console.log(html); // '<h1>Hello</h1><p>World</p>'
 */
export function toHTML(content) {
  if (typeof content === 'string') {
    return content;
  }

  if (!Array.isArray(content)) {
    throw new Error('Input must be a string or array of strings');
  }

  return content.join('');
}