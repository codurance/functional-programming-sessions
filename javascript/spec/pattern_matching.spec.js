'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

const match = require('./../src/pattern_matching');
const tautology = () => true;

describe('Pattern Matching', () => {
  describe('From a set of objects (that are not arrays)', () => {
    it('always matches sample case', () => {
      const n = 1;
      const alwaysMatches = match({n},
        tautology, ({ n }) => n);

      expect(alwaysMatches).to.equal(n);
    });
  });

  describe('From a set of arrays', () => {
    it('always matches sample case', () => {
      const n = 1;
      const alwaysMatches = match({n},
        [tautology, ({ n }) => n]);

      expect(alwaysMatches).to.equal(n);
    });
  });
});
