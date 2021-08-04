"use strict";

exports.stringify = function(object) {
    return require("json-bigint").stringify(object);
};