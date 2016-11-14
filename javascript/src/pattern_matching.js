'use strict';

module.exports = match;

function match(parameters, ...equationParts) {
  const equations = pair(equationParts);

  check(equations);
  for (let i = 0; i < equations.length; i++) {
    const equation = equations[i];

    const predicateMatches = equation.predicate(parameters);
    if (predicateMatches) {
      return equation.clause(parameters);
    }
  }

  throw Error('Non-exhaustive pattern-matching');

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

  function check(equations) {
    equations.forEach(equation => {
      if (!isFunction(equation.predicate)) {
        throw Error('Predicate '+equations.predicate+' is not a function');
      }

      if (!isFunction(equation.clause)) {
        throw Error('Clause '+equations.clause+' is not a function');
      }

    });

    // Simple typeof comparison
    // (with strict equality)
    function isFunction(object) {
      return typeof(object) === 'function';
    }
  }
}