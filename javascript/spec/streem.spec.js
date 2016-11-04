'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

const streem = require('./../src/streem');

describe('Streems (or Streams)', () => {
  describe('Exercise 1 - take: a function to convert a Stream to a List, which will force its evaluation', () => {
    it('on an infinite stream', () => {
      expect(streem.infiniteNaturals().take(10)).to.eql([ 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 ]);
      expect(streem.infiniteNaturals().take(0)).to.eql([]);
    });
  });
});
