'use strict';

function* argumentsGenerator() {
  for (let i = 0; i < arguments.length; i += 1) {
    yield arguments[i];
  }
}

var argumentsIterator = argumentsGenerator('a', 'b', 'c');

console.log(
    argumentsIterator.next().value,
    argumentsIterator.next().value,
    argumentsIterator.next().value
    );
