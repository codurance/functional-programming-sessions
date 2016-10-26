'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;
const stream = require('../src/stream.js');

describe('A stream of natural numbers', () => {
  it('returns the first natural number', () => {
    expect(stream.infiniteStreamOfNaturalNumbers().value()).to.equal(1);
  });

  it('returns the second natural numbers', () => {
    expect(stream.infiniteStreamOfNaturalNumbers().next().value()).to.equal(2);
    expect(stream.get(stream.infiniteStreamOfNaturalNumbers(), 1)).to.equal(1);
    expect(stream.get(stream.infiniteStreamOfNaturalNumbers(), 2)).to.equal(2);
    expect(stream.get(stream.infiniteStreamOfNaturalNumbers(), 3)).to.equal(3);
  });

  it('converts a function into a stream', () => {
    // expect(stream.get(stream.streamCreator((n) => n + 1, 1), 1)).to.equal(1);
    // expect(stream.get(stream.streamCreator((n) => n + 1, 1), 2)).to.equal(2);
    //
    // // Binary
    // expect(stream.get(stream.streamCreator((n) => 2 * n, 1), 2)).to.equal(2);
    // expect(stream.get(stream.streamCreator((n) => 2 * n, 1), 4)).to.equal(8);
  });

  describe('finite stream of natural numbers', () => {
    let naturals;
    beforeEach(() => {
      naturals = stream.finiteStreamOfNaturalNumbers(4);
    });
    it('case', () => {
      expect(naturals.next().value()).to.equal(2);
    });
    it('case', () => {
      expect(stream.get(naturals, 1)).to.equal(1);
    });
    it('case', () => {
      expect(stream.get(naturals, 2)).to.equal(2);
    });
    it('case', () => {
      expect(stream.get(naturals, 4)).to.equal(4);
    });
    it('case', () => {
      const lastValueOfTheStream = naturals.next().next().next();
      expect(lastValueOfTheStream.next()).to.equal(undefined);
    });
  });
  it('converts a finite stream to a list', () => {
    let aNaturalNumbersStream = stream.finiteStreamOfNaturalNumbers(4);
    let expectedList = [1, 2, 3, 4];
    expect(stream.streamToList(aNaturalNumbersStream)).to.eql(expectedList);

    aNaturalNumbersStream = stream.finiteStreamOfNaturalNumbers(5);
    expectedList = [1, 2, 3, 4, 5];
    expect(stream.streamToList(aNaturalNumbersStream)).to.eql(expectedList);
  });

  describe('Chapter 5 - Exercise 2, take', () => {
    it('takes a single elemnt', () => {
      expect(stream.take(stream.infiniteStreamOfNaturalNumbers(), 1)).to.eql([1]);
    });
    
    it('takes two elements', () => {
      expect(stream.take(stream.infiniteStreamOfNaturalNumbers(), 2)).to.eql([1,2]);
    });

    it('takes only the available elements on the stream', () => {
      expect(stream.take(stream.finiteStreamOfNaturalNumbers(3), 4)).to.eql([1,2,3]);
    });

  });

  describe('Chapter 5 - Exercise 3 - takeWhile', () => {
    it('takes the elements while they match the predicate', () => {
      expect(stream.takeWhile((n) => n < 3, stream.infiniteStreamOfNaturalNumbers())).to.eql([1,2]);
    });
  });
});

