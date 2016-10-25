'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

describe('A stream of natural numbers', () => {
  it('returns the first natural number', () => {
    expect(stream().value()).to.equal(1);
  });

  it('returns the second natural numbers', () => {
    expect(stream().next().value()).to.equal(2);
    expect(get(stream(), 1)).to.equal(1);
    expect(get(stream(), 2)).to.equal(2);
    expect(get(stream(), 3)).to.equal(3);
  });

  it('converts a function into a stream', () => {
    // expect(get(streamCreator((n) => n + 1, 1)), 1).to.equal(1);
    // expect(get(streamCreator((n) => n + 1, 1)), 2).to.equal(2);
    //
    // // Binary
    // expect(get(streamCreator((n) => 2 * n, 1)), 2).to.equal(2);
    // expect(get(streamCreator((n) => 2 * n, 1)), 4).to.equal(8);
  });
});

function stream() {
  const stream_= function (current) {
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

function get(stream, ordinal) {
  var xx = range(ordinal - 1);
  return xx.reduce(advanceStream, stream).value();

  function advanceStream (stream) {
    return stream.next();
  }

  // range(5) = [0,1,2,3,4]
  function range(n) {
    return [...Array(n).keys()];
  };
}
