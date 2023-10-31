/**
 * BCorrespondent. Tag (v1.1.0). Commit (83a5bf4)
 * BCorrespondent server api
 *
 * The version of the OpenAPI document: 1.1.0
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
        factory(root.expect, root.BCorrespondentTagV110Commit83a5bf4);
    }
}(this, function(expect, BCorrespondentTagV110Commit83a5bf4) {
    'use strict';

    var instance;

    beforeEach(function() {
        instance = new BCorrespondentTagV110Commit83a5bf4.Notifications();
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

    describe('Notifications', function() {
        it('should create an instance of Notifications', function() {
            // uncomment below and update the code to test Notifications
            //var instance = new BCorrespondentTagV110Commit83a5bf4.Notifications();
            //expect(instance).to.be.a(BCorrespondentTagV110Commit83a5bf4.Notifications);
        });

        it('should have the property count (base name: "count")', function() {
            // uncomment below and update the code to test the property count
            //var instance = new BCorrespondentTagV110Commit83a5bf4.Notifications();
            //expect(instance).to.be();
        });

        it('should have the property items (base name: "items")', function() {
            // uncomment below and update the code to test the property items
            //var instance = new BCorrespondentTagV110Commit83a5bf4.Notifications();
            //expect(instance).to.be();
        });

    });

}));