function groupBy(array, key) {
  return array.reduce((result, item) => {
    const value = item[key];
    result[value] = result[value] || [];
    result[value].push(item);
    return result;
  }, {});
}