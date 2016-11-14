'use strict';

module.exports = {
  of
};

class Option {

  constructor(value) {
    this.value = value;
  }

  map(function_) {
    if (this.value) {
      return of(function_(this.value));
    }
  }
}

function of(value) {
  return new Option(value);
}