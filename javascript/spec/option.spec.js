'use strict';

const chai = require('chai');
const expect = chai.expect;
chai.config.includeStack = true;

const option = require('./../src/option');

describe('Option', () => {
  describe('Exercise 1 Implement the Option itself', () => {
    describe('map', () => {
      it('happy path', () =>{
        expect(option.of(1).map(n => n+1)).to.eql(option.of(2));
      });
    });
  });
});
