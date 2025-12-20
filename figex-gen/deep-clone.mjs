function deepClone(obj) {
  return JSON.parse(JSON.stringify(obj));
}

function deepMerge(obj1, obj2) {
  const result = { ...obj1 };
  for (const key in obj2) {
    if (obj2.hasOwnProperty(key)) {
      result[key] = obj2[key];
    }
  }
  return result;
}