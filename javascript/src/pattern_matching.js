'use strict';

module.exports = match;

function match(parameters, ...equationParts) {
  const equations = pair(equationParts);
  for (let i = 0; i < equations.length; i++) {
    const equation = equations[i];

    const predicateMatches = equation.predicate(parameters);
    if (predicateMatches) {
      return equation.clause(parameters);
    }
  }

  return undefined;

  function pair(equations) {
    if (Array.isArray(equations[0])) {
      const result = [];
      for (let i = 0; i < equations.length; i++) {
        const predicate = equations[i][0];
        const clause = equations[i][1];
        result.push({predicate, clause});
      }
      return result;
    }

    const result = [];
    for (let i = 0; i < equations.length; i+=2) {
      const predicate = equations[i];
      const clause = equations[i+1];
      result.push({predicate, clause});
    }
    return result;
  }
}