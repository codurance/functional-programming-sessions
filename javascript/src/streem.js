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
    const take_ = funct((result, n) => {
      if (n === 0) {
        return result;
      } else {
        return result.concat([this.value]).concat(this.next().take(n-1));
      }
    });
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