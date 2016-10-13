'use strict';

module.exports = {
  // scala equivalent 
  // def partial1(a: A, a_b: (A,B) => C) : B => C
  partial1(a, function_) {
    return function (b) {
      return function_(a,b);
    };
  }
};
