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
    function take_(result, n) {
      if (n === 0) {
        return result;
      } else {
        result.push(this.value);
        return result.concat(this.next().take(n-1));
      }
    }
    take_ = take_.bind(this);
    return take_([], n);
  }
}

function infiniteNaturals() {
  return new InfiniteNaturalStream(1);
}