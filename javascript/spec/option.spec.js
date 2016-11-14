'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

const option = require('./../src/option');

describe('Option', () => {
  describe('Exercise 1 Implement the Option itself', () => {
    describe('map', () => {
      it('happy path, when the value is present', () =>{
        expect(option.of(1).map(n => n+1)).to.eql(option.of(2));
      });

      it('happy path, when the value is not present', () =>{
        expect(option.empty().map(n => n+1)).to.eql(option.empty());
      });
    });

    describe('flatMap', () => {
      // The presence of the result is:
      // presence of input AND function returning a present
      it('both values are present', () =>{
        expect(option.of(1).flatMap(n => option.of(n+1))).to.eql(option.of(2));
      });

      it('left is present, right is not', () =>{
        expect(option.of(1).flatMap(() => option.empty())).to.eql(option.empty());
      });

      it('left is missing, right is present', () =>{
        expect(option.empty().flatMap(n => option.of(n+1))).to.eql(option.empty());
      });

      it('both values are missing', () =>{
        expect(option.empty().flatMap(() => option.empty())).to.eql(option.empty());
      });
    });
  });
});
