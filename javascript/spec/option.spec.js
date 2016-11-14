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

    describe('getOrElse', () => {
      it('value is present, using the value present', () =>{
        expect(option.of(1).getOrElse(0)).to.eql(1);
      });

      it('value is missing, then using the provided value', () =>{
        expect(option.empty().getOrElse(1)).to.eql(1);
      });
    });

    describe('OrElse', () => {
      it('value is present, using the value present', () =>{
        expect(option.of(1).orElse(option.of(0))).to.eql(option.of(1));
      });

      it('value is missing, then using the provided value', () =>{
        expect(option.empty().orElse(option.of(1))).to.eql(option.of(1));
      });

      it('value is missing - also supports empty option', () =>{
        expect(option.empty().orElse(option.empty())).to.eql(option.empty());
      });
    });
  });
});
