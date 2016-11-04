'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

const match = require('./../src/pattern_matching');

describe('Pattern Matching', () => {
  describe('From a set of objects (that are not arrays)', () => {
    it('always matches sample case', () => {
      const result = 0;
      const n = 1;
      const alwaysMatches = match({ result, n },
        () => true, ({ n }) => n);

      expect(alwaysMatches).to.equal(1);
    });
  });

  describe('From a set of arrays', () => {
    it('always matches sample case', () => {
      const result = 0;
      const n = 1;
      const alwaysMatches = match({ result, n },
        [() => true, ({ n }) => n]);

      expect(alwaysMatches).to.equal(1);
    });
  });
});
