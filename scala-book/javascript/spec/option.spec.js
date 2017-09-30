'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

const option = require('./../src/option');

function variance(values) {

  const mean = meanOf(values);

  return option.of(meanOf(values
    .map(element => element - mean)
    .map(element => Math.pow(element, 2))));
  function meanOf(values) {
    return values.reduce((accumulator, element) => accumulator + element) / values.length;
  }
}
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

    describe('filter', () => {
      it('value is present, with a tautology', () =>{
        expect(option.of(1).filter(() => true)).to.eql(option.of(1));
      });

      it('value is present, with a contradiction', () =>{
        expect(option.of(1).filter(() => false)).to.eql(option.empty());
      });

      it('value is present, matching the value in the option', () =>{
        expect(option.of(1).filter((n) => n===1)).to.eql(option.of(1));
      });

      it('value is present, not matching the value in the option', () =>{
        expect(option.of(1).filter((n) => n===2)).to.eql(option.empty());
      });

      it('value is missing, with a tautology', () =>{
        expect(option.empty().filter(() => true)).to.eql(option.empty());
      });

      it('value is missing, with a contradiction', () =>{
        expect(option.empty().filter(() => false)).to.eql(option.empty());
      });

      it('value is missing, but you can filter by a predicate using it', () =>{
        expect(option.empty().filter((n) => n===2)).to.eql(option.empty());
      });
    });
  });

  describe('Exercise 2 - Variance', () => {
    it('it is 0', ()=>{
      expect(variance([1,1,1])).to.eql(option.of(0.0));
    });
  });
});
