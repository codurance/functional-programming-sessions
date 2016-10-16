'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

const modul = require('./../src/partial');

describe ('Exercise 3', () => {
  it('partially applies a function', () => {
    expect(modul.partial1(1, (a,b) => (a+b).toString())(2)).to.equal('3');
    expect(modul.partial1('a', (a,b) => a+b)('2')).to.equal('a2');
  });
});

describe ('Exercise 4', () => {
  it('to curry a function with two parameters', () => {
    const two = (a,b) => a+b;
    expect(two(1,2)).to.equal(3);

    const curriedTwo = modul.curry(two);
    expect(typeof(curriedTwo)).to.equal('function');
    expect(typeof(curriedTwo(1))).to.equal('function');
    expect(curriedTwo(1)(2)).to.equal(3);
  });

  describe('to curry a function with any number of parameters', () => {
    const three = (a,b,c) => a+b+c;
    let curriedThree = null;
    describe('finishing the application with an invocation without parameters', () => {
      beforeEach(()=>{
        curriedThree = modul.curryAny(three);
      });

      it('always a function before invoking for the last time', ()=> {
        expect(typeof(curriedThree)).to.equal('function');
        expect(typeof(curriedThree(1))).to.equal('function');
        expect(typeof(curriedThree(1)(2))).to.equal('function');
        expect(typeof(curriedThree(1)(2)(3))).to.equal('function');
      });

      it('applies the function when invoked without parameters', ()=>{
        expect(curriedThree(1)(2)(3)()).to.equal(6);
      });
    });

    describe('specifying the number of parameters in the currying function', () =>{
      beforeEach(()=>{
        curriedThree = modul.curryAnyWithParameterNumber(3, three);
      });

      it('always a function before invoking for the last time', ()=> {
        expect(typeof(curriedThree)).to.equal('function');
        expect(typeof(curriedThree(1))).to.equal('function');
      });

      it('always a function before invoking for the last time', ()=> {
        expect(typeof(curriedThree(1)(2))).to.equal('function');
      });

      it('applies the function when invoked with the specified parameters', ()=>{
        expect(curriedThree(1)(2)(3)).to.equal(6);
      });
    });

    describe('autocurry: no need to specify the number of parameters in the currying function', () =>{
      beforeEach(()=>{
        curriedThree = modul.autocurry(three);
      });

      it('always a function before invoking for the last time', ()=> {
        expect(typeof(curriedThree)).to.equal('function');
        expect(typeof(curriedThree(1))).to.equal('function');
      });

      it('always a function before invoking for the last time', ()=> {
        expect(typeof(curriedThree(1)(2))).to.equal('function');
      });

      it('applies the function when invoked with the specified parameters', ()=>{
        expect(curriedThree(1)(2)(3)).to.equal(6);
      });
    });
  });
});

