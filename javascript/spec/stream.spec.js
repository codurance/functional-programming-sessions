'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

describe('A stream of natural numbers', () => {
  it('returns the first natural number', () => {
    expect(infiniteStreamOfNaturalNumbers().value()).to.equal(1);
  });

  it('returns the second natural numbers', () => {
    expect(infiniteStreamOfNaturalNumbers().next().value()).to.equal(2);
    expect(get(infiniteStreamOfNaturalNumbers(), 1)).to.equal(1);
    expect(get(infiniteStreamOfNaturalNumbers(), 2)).to.equal(2);
    expect(get(infiniteStreamOfNaturalNumbers(), 3)).to.equal(3);
  });

  it('converts a function into a stream', () => {
    // expect(get(streamCreator((n) => n + 1, 1), 1)).to.equal(1);
    // expect(get(streamCreator((n) => n + 1, 1), 2)).to.equal(2);
    //
    // // Binary
    // expect(get(streamCreator((n) => 2 * n, 1), 2)).to.equal(2);
    // expect(get(streamCreator((n) => 2 * n, 1), 4)).to.equal(8);
  });

  describe('finite stream of natural numbers', () => {
    let stream;
    beforeEach(() => {
      stream = finiteStreamOfNaturalNumbers(4);
    });
    it('case', () => {
      expect(stream.next().value()).to.equal(2);
    });
    it('case', () => {
      expect(get(stream, 1)).to.equal(1);
    });
    it('case', () => {
      expect(get(stream, 2)).to.equal(2);
    });
    it('case', () => {
      expect(get(stream, 4)).to.equal(4);
    });
    it('case', () => {
      const lastValueOfTheStream = stream.next().next().next();
      expect(lastValueOfTheStream.next()).to.equal(undefined);
    });
  });
  it('converts a finite stream to a list', () => {
    let aNaturalNumbersStream = finiteStreamOfNaturalNumbers(4);
    let expectedList = [1, 2, 3, 4];
    expect(streamToList(aNaturalNumbersStream)).to.eql(expectedList);

    aNaturalNumbersStream = finiteStreamOfNaturalNumbers(5);
    expectedList = [1, 2, 3, 4, 5];
    expect(streamToList(aNaturalNumbersStream)).to.eql(expectedList);
  });
});

function streamToList(finiteStream) {
  return reduce_( appendTo, [], finiteStream);
  function appendTo(acc, ele) {
    acc.push(ele);
    return acc;
  };
};

function reduce_(function_, accumulator, stream) {
  if (stream === undefined) {
    return accumulator;
  } else {
    return reduce_(function_, function_(accumulator, stream.value()), stream.next());
  }
}

// function streamToList(finiteStream) {
//   function getValue(currentStream, list) {
//     list.push(currentStream.value());
//     if (currentStream.next() === undefined) {
//       return list;
//     }
//     return getValue(currentStream.next(), list);
//   }
//   return getValue(finiteStream, []);
// };

function finiteStreamOfNaturalNumbers(numberOfElements) {
  const stream_ = function (current, numberOfPendingElements) {
    return {
      value: function () {
        return current;
      },
      next: function () {
        if (numberOfPendingElements === 0) {
          return undefined;
        }
        return stream_(current + 1, numberOfPendingElements - 1);
      }
    };
  };

  return stream_(1, numberOfElements - 1);
}

function infiniteStreamOfNaturalNumbers() {
  const stream_ = function (current) {
    return {
      value: function () {
        return current;
      },
      next: function () {
        return stream_(current + 1);
      }
    };
  };

  return stream_(1);
}

// // get :: Stream a -> Int -> a
// function getT(stream, ordinal) {
//   function get_(stream, ordinal) {
//     if (ordinal === 1) {
//       return stream.value();
//     }
//     return get_(stream.next(), ordinal - 1);
//   }
//   return get_(stream, ordinal);
// }

// get :: Stream a -> Int -> a
// function getL(stream, ordinal) {
//   function get_(stream, ordinal) {
//     if(ordinal === 1){
//       return stream;
//     }
//     return get_(stream, ordinal - 1).next()
//   }
//
//   return get_(stream, ordinal).value();
// }

// get :: Stream a -> Int -> a
function get(stream, ordinal) {
  const xx = range(ordinal - 1);
  return xx.reduce(advanceStream, stream).value();

  function advanceStream(stream) {
    return stream.next();
  }

  // range(5) = [0,1,2,3,4]
  function range(n) {
    return [...Array(n).keys()];
  };
}
