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
    const self = this;
    const go = (remainingParams, params, function_) => {
      if (remainingParams === 0) {
        return function_.apply(null, params);
      } else {
        return function ( /* arguments */ ) {
          const args = self.toArray(arguments);
          args.forEach(x => params.push(x));
          return go(remainingParams - args.length, params, function_);
        };
      }
    };
    return go(numberOfParams , [], function_);
  },

  // def autocurry ( f: (A => B => ... => D): A => (B => ... => D)
  autocurry(function_) {
    const numberOfParams = function_.length;
    return this.curryAnyWithParameterNumber(numberOfParams, function_);
  },

  toArray(args) {
    return [...args];
  }
};
