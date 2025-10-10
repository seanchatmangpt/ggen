module.exports = {
  timestamp: () => new Date().toISOString(),

  sortBy: (array, path) => {
    return [...array].sort((a, b) => {
      const aVal = path.split('.').reduce((o, k) => o?.[k], a);
      const bVal = path.split('.').reduce((o, k) => o?.[k], b);
      return (aVal ?? 0) - (bVal ?? 0);
    });
  },

  reverse: (array) => [...array].reverse(),

  hasRelationship: (relationships, subjectId, predicate) => {
    return relationships.some(rel =>
      rel.subject === subjectId && rel.predicate === predicate
    );
  },

  getRelated: (relationships, entities, subjectId, predicate) => {
    const relatedIds = relationships
      .filter(rel => rel.subject === subjectId && rel.predicate === predicate)
      .map(rel => rel.object);

    return entities.filter(e => relatedIds.includes(e.id));
  },

  hasProperty: (props, key) => {
    return props && props[key] !== undefined && props[key] !== null;
  },

  getProperty: (props, key, defaultValue = '') => {
    return props && props[key] !== undefined && props[key] !== null
      ? props[key]
      : defaultValue;
  },

  pascalCase: (str) => {
    return str.replace(/(?:^|[-_])(\w)/g, (_, c) => c ? c.toUpperCase() : '');
  },

  camelCase: (str) => {
    const pascal = str.replace(/(?:^|[-_])(\w)/g, (_, c) => c ? c.toUpperCase() : '');
    return pascal.charAt(0).toLowerCase() + pascal.slice(1);
  },

  lowerCase: (str) => str.toLowerCase(),

  add: (a, b) => a + b,
};
