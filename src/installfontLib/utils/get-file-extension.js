'use strict';

module.exports = function getFileExtension(file) {
    var fileSplit = file.split('.');
    var ext = fileSplit[fileSplit.length - 1];
    return ext;
};
