function safeJSONParse(str) {
  try {
    return JSON.parse(str);
  } catch (e) {
    return null;
  }
}

function safeJSONStringify(obj) {
  try {
    return JSON.stringify(obj);
  } catch (e) {
    return 'null';
  }
}

export { safeJSONParse, safeJSONStringify };