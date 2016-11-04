'use strict';

module.exports = {infiniteNaturals};

class InfiniteNaturalStream {

  constructor(value) {
    this.value = value;
  }

  next() {
    return new InfiniteNaturalStream(this.value + 1);
  }

  take(n) {
    const take_ = funct((result, n) =>
      match({result, n},
        ({n}) => n === 0, ({result}) =>  result,
        () => true, ({result, n}) => result.concat([this.value]).concat(this.next().take(n-1))
    ));
    return take_([], n);
  }
}

function infiniteNaturals() {
  return new InfiniteNaturalStream(1);
}

/**
 * Binds the parameter with the correct `this`
 * @param lambda
 * @returns {function(this:funct)}
 */
function funct(lambda) {
  return lambda.bind(this);
}

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
    const result = [];
    for (let i = 0; i < equations.length; i+=2) {
      const predicate = equations[i];
      const clause = equations[i+1];
      result.push({predicate, clause});
    }
    return result;
  }
}