"use strict";

exports.eqEventTarget = function(a) {
    return function(b) {
        return function() {
            return a === b;
        };
    };
};

