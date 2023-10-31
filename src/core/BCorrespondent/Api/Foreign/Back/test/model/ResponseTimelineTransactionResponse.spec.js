/**
 * BCorrespondent. Tag (v1.0.0). Commit (9e7bfa7)
 * BCorrespondent server api
 *
 * The version of the OpenAPI document: 1.0.0
 * Contact: fclaw007@gmail.com
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 *
 */

(function(root, factory) {
    if (typeof define === 'function' && define.amd) {
        // AMD.
        define(['expect.js', process.cwd() + '/src/index'], factory);
    } else if (typeof module === 'object' && module.exports) {
        // CommonJS-like environments that support module.exports, like Node.
        factory(require('expect.js'), require(process.cwd() + '/src/index'));
    } else {
        // Browser globals (root is window)
        factory(root.expect, root.BCorrespondentTagV100Commit9e7bfa7);
    }
}(this, function(expect, BCorrespondentTagV100Commit9e7bfa7) {
    'use strict';

    var instance;

    beforeEach(function() {
        instance = new BCorrespondentTagV100Commit9e7bfa7.ResponseTimelineTransactionResponse();
    });

    var getProperty = function(object, getter, property) {
        // Use getter method if present; otherwise, get the property directly.
        if (typeof object[getter] === 'function')
            return object[getter]();
        else
            return object[property];
    }

    var setProperty = function(object, setter, property, value) {
        // Use setter method if present; otherwise, set the property directly.
        if (typeof object[setter] === 'function')
            object[setter](value);
        else
            object[property] = value;
    }

    describe('ResponseTimelineTransactionResponse', function() {
        it('should create an instance of ResponseTimelineTransactionResponse', function() {
            // uncomment below and update the code to test ResponseTimelineTransactionResponse
            //var instance = new BCorrespondentTagV100Commit9e7bfa7.ResponseTimelineTransactionResponse();
            //expect(instance).to.be.a(BCorrespondentTagV100Commit9e7bfa7.ResponseTimelineTransactionResponse);
        });

        it('should have the property errors (base name: "errors")', function() {
            // uncomment below and update the code to test the property errors
            //var instance = new BCorrespondentTagV100Commit9e7bfa7.ResponseTimelineTransactionResponse();
            //expect(instance).to.be();
        });

        it('should have the property success (base name: "success")', function() {
            // uncomment below and update the code to test the property success
            //var instance = new BCorrespondentTagV100Commit9e7bfa7.ResponseTimelineTransactionResponse();
            //expect(instance).to.be();
        });

        it('should have the property warnings (base name: "warnings")', function() {
            // uncomment below and update the code to test the property warnings
            //var instance = new BCorrespondentTagV100Commit9e7bfa7.ResponseTimelineTransactionResponse();
            //expect(instance).to.be();
        });

    });

}));