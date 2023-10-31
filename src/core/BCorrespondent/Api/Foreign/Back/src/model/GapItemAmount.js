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
import Currency from './Currency';

/**
 * The GapItemAmount model module.
 * @module model/GapItemAmount
 * @version 1.2.1
 */
class GapItemAmount {
    /**
     * Constructs a new <code>GapItemAmount</code>.
     * @alias module:model/GapItemAmount
     * @param currency {module:model/Currency} 
     * @param value {Number} 
     */
    constructor(currency, value) {

        GapItemAmount.initialize(this, currency, value);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj, currency, value) {
        obj['currency'] = currency;
        obj['value'] = value;
    }

    /**
     * Constructs a <code>GapItemAmount</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/GapItemAmount} obj Optional instance to populate.
     * @return {module:model/GapItemAmount} The populated <code>GapItemAmount</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new GapItemAmount();

            if (data.hasOwnProperty('currency')) {
                obj['currency'] = Currency.constructFromObject(data['currency']);
            }
            if (data.hasOwnProperty('value')) {
                obj['value'] = ApiClient.convertToType(data['value'], 'Number');
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>GapItemAmount</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>GapItemAmount</code>.
     */
    static validateJSON(data) {
        // check to make sure all required properties are present in the JSON string
        for (const property of GapItemAmount.RequiredProperties) {
            if (!data[property]) {
                throw new Error("The required field `" + property + "` is not found in the JSON data: " + JSON.stringify(data));
            }
        }

        return true;
    }

    /**
     * @return {module:model/Currency}
     */
    getCurrency() {
        return this.currency;
    }

    /**
     * @param {module:model/Currency} currency
     */
    setCurrency(currency) {
        this['currency'] = currency;
    }
    /**
     * @return {Number}
     */
    getValue() {
        return this.value;
    }

    /**
     * @param {Number} value
     */
    setValue(value) {
        this['value'] = value;
    }

}

GapItemAmount.RequiredProperties = ["currency", "value"];

/**
 * @member {module:model/Currency} currency
 */
GapItemAmount.prototype['currency'] = undefined;

/**
 * @member {Number} value
 */
GapItemAmount.prototype['value'] = undefined;






export default GapItemAmount;