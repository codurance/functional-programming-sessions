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
  it('to curry a function', () => {
    const two = (a,b) => a+b;
    expect(two(1,2)).to.equal(3);

    const curriedTwo = modul.curry(two);
    expect(typeof(curriedTwo)).to.equal('function');
    expect(typeof(curriedTwo(1))).to.equal('function');
    expect(curriedTwo(1)(2)).to.equal(3);
  });
});

