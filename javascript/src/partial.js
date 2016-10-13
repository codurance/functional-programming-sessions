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
    const params = [];
    const funct = function_;
    const curryAny2 = function (a) {
      if (a) {
        params.push(a);
        return curryAny2;
      } else {
        return funct.apply(null, params);
      }
    };

    return curryAny2;
  }
};
