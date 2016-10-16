'use strict';

module.exports = {
  // scala equivalent 
  // def partial1(a: A, a_b: (A,B) => C) : B => C
  partial1(a, function_) {
    return function (b) {
      return function_(a,b);
    };
  },
  curry(function_a_b_c) {
    const b_c = function (a) {
      return function (b) {
        return function_a_b_c(a,b);
      };
    };
    return b_c;
  },
  // def CurryAny (f: A => B => C => ... => D): A => (B => C => ... => D)
  // The implementation is different than currying as it not specified how many parameters the original function had, therefore it is necessary to invoke it one more time (in this implementation) to indicate that there are no more parameters
  curryAny(function_) {
    const go = (params, function_) => {
      return function (a) {
        if (a) {
          params.push(a);
          return go(params, function_);
        } else {
          return function_.apply(null, params);
        }
      };
    };
    return go([], function_);
  },
  // def curryAnyWithParameterNumber (n: Int, f: A => B => C => ... => D): A => (B => C => ... => D)
  // The implementation is different than currying as it is specified how many parameters the original function had, but the use of the curried function is the same
  curryAnyWithParameterNumber(numberOfParams, function_) {
    const go = (remainingParams, params, function_) => {
      return function (a) {
        params.push(a);
        remainingParams--;
        if (remainingParams === 0) {
          console.log('finished. params = ', params);
          return function_.apply(null, params);
        }
        return go(remainingParams, params, function_);
      };
    };
    return go(numberOfParams , [], function_);
  }

};
