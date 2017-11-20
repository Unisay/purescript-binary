"use strict";

exports.imul = function(a) {
  return function(b) {
    return Math.imul(a, b);
  }
}
