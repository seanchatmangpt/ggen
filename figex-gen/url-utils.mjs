function parseQuery(url) {
  const parser = document.createElement('a');
  parser.href = url;
  const query = parser.search.substring(1).split('&');
  const result = {};
  for (let i = 0; i < query.length; i++) {
    const pair = query[i].split('=');
    result[decodeURIComponent(pair[0])] = decodeURIComponent(pair[1] || '');
  }
  return result;
}

function buildQuery(params) {
  return Object.keys(params).map(key => {
    return encodeURIComponent(key) + '=' + encodeURIComponent(params[key]);
  }).join('&');
}