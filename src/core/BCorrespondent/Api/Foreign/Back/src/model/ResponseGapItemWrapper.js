/**
 * BCorrespondent. Tag (-). Commit (bb65df7)
 * BCorrespondent server api
 *
 * The version of the OpenAPI document: 1.2.1
 * Contact: fclaw007@gmail.com
 *
 * NOTE: This class is auto generated by OpenAPI Generator (https://openapi-generator.tech).
 * https://openapi-generator.tech
 * Do not edit the class manually.
 *
 */

import ApiClient from '../ApiClient';
import Error from './Error';
import GapItemWrapper from './GapItemWrapper';
import WithFieldCodeError from './WithFieldCodeError';

/**
 * The ResponseGapItemWrapper model module.
 * @module model/ResponseGapItemWrapper
 * @version 1.2.1
 */
class ResponseGapItemWrapper {
    /**
     * Constructs a new <code>ResponseGapItemWrapper</code>.
     * @alias module:model/ResponseGapItemWrapper
     */
    constructor() {

        ResponseGapItemWrapper.initialize(this);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj) {}

    /**
     * Constructs a <code>ResponseGapItemWrapper</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/ResponseGapItemWrapper} obj Optional instance to populate.
     * @return {module:model/ResponseGapItemWrapper} The populated <code>ResponseGapItemWrapper</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new ResponseGapItemWrapper();

            if (data.hasOwnProperty('errors')) {
                obj['errors'] = ApiClient.convertToType(data['errors'], [WithFieldCodeError]);
            }
            if (data.hasOwnProperty('success')) {
                obj['success'] = GapItemWrapper.constructFromObject(data['success']);
            }
            if (data.hasOwnProperty('warnings')) {
                obj['warnings'] = ApiClient.convertToType(data['warnings'], [Error]);
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>ResponseGapItemWrapper</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>ResponseGapItemWrapper</code>.
     */
    static validateJSON(data) {
        if (data['errors']) { // data not null
            // ensure the json data is an array
            if (!Array.isArray(data['errors'])) {
                throw new Error("Expected the field `errors` to be an array in the JSON data but got " + data['errors']);
            }
            // validate the optional field `errors` (array)
            for (const item of data['errors']) {
                WithFieldCodeError.validateJSON(item);
            };
        }
        // validate the optional field `success`
        if (data['success']) { // data not null
            GapItemWrapper.validateJSON(data['success']);
        }
        if (data['warnings']) { // data not null
            // ensure the json data is an array
            if (!Array.isArray(data['warnings'])) {
                throw new Error("Expected the field `warnings` to be an array in the JSON data but got " + data['warnings']);
            }
            // validate the optional field `warnings` (array)
            for (const item of data['warnings']) {
                Error.validateJSON(item);
            };
        }

        return true;
    }

    /**
     * @return {Array.<module:model/WithFieldCodeError>}
     */
    getErrors() {
        return this.errors;
    }

    /**
     * @param {Array.<module:model/WithFieldCodeError>} errors
     */
    setErrors(errors) {
        this['errors'] = errors;
    }
    /**
     * @return {module:model/GapItemWrapper}
     */
    getSuccess() {
        return this.success;
    }

    /**
     * @param {module:model/GapItemWrapper} success
     */
    setSuccess(success) {
        this['success'] = success;
    }
    /**
     * @return {Array.<module:model/Error>}
     */
    getWarnings() {
        return this.warnings;
    }

    /**
     * @param {Array.<module:model/Error>} warnings
     */
    setWarnings(warnings) {
        this['warnings'] = warnings;
    }

}



/**
 * @member {Array.<module:model/WithFieldCodeError>} errors
 */
ResponseGapItemWrapper.prototype['errors'] = undefined;

/**
 * @member {module:model/GapItemWrapper} success
 */
ResponseGapItemWrapper.prototype['success'] = undefined;

/**
 * @member {Array.<module:model/Error>} warnings
 */
ResponseGapItemWrapper.prototype['warnings'] = undefined;






export default ResponseGapItemWrapper;