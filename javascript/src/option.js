'use strict';

class Option {

  constructor(value) {
    this.value = value;
  }

  static empty() {
    return new Option();
  }

  static of(value) {
    return new Option(value);
  }

  map(function_) {
    if (this.isPresent()) {
      return Option.of(function_(this.value));
    } else {
      return Option.empty();
    }
  }

  isPresent() {
    if (this.value) {
      return true;
    } else {
      return false;
    }
  }
}

module.exports['empty'] = Option.empty;
module.exports['of'] = Option.of;
