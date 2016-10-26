'use strict';

module.exports = {streamToList,reduce_,finiteStreamOfNaturalNumbers,infiniteStreamOfNaturalNumbers,get,take};

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

function take(stream) {
  return [stream.value()];
}
