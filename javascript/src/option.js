'use strict';

module.exports = {
  of,
};


class Option {

  constructor(value) {
    this.value = value;
  }

  static empty() {
    return new Option();
  }

  map(function_) {
    if (this.value) {
      return of(function_(this.value));
    } else {
      return this;
    }
  }
}

function of(value) {
  return new Option(value);
}

function empty() {
  return Option.empty();
}

module.exports['empty'] = (Option.empty)
