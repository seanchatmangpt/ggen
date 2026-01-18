function isString(value) { return typeof value === 'string'; }
function isNumber(value) { return typeof value === 'number'; }
function isArray(value) { return Array.isArray(value); }
function isObject(value) { return typeof value === 'object' && value !== null; }
function isNull(value) { return value === null; }