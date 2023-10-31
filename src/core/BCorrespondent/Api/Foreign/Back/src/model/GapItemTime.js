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

/**
 * The GapItemTime model module.
 * @module model/GapItemTime
 * @version 1.2.1
 */
class GapItemTime {
    /**
     * Constructs a new <code>GapItemTime</code>.
     * @alias module:model/GapItemTime
     * @param hour {Number} 
     * @param min {Number} 
     */
    constructor(hour, min) {

        GapItemTime.initialize(this, hour, min);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj, hour, min) {
        obj['hour'] = hour;
        obj['min'] = min;
    }

    /**
     * Constructs a <code>GapItemTime</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/GapItemTime} obj Optional instance to populate.
     * @return {module:model/GapItemTime} The populated <code>GapItemTime</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new GapItemTime();

            if (data.hasOwnProperty('hour')) {
                obj['hour'] = ApiClient.convertToType(data['hour'], 'Number');
            }
            if (data.hasOwnProperty('min')) {
                obj['min'] = ApiClient.convertToType(data['min'], 'Number');
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>GapItemTime</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>GapItemTime</code>.
     */
    static validateJSON(data) {
        // check to make sure all required properties are present in the JSON string
        for (const property of GapItemTime.RequiredProperties) {
            if (!data[property]) {
                throw new Error("The required field `" + property + "` is not found in the JSON data: " + JSON.stringify(data));
            }
        }

        return true;
    }

    /**
     * minimum: -9223372036854775808
     * maximum: 9223372036854775807
     * @return {Number}
     */
    getHour() {
        return this.hour;
    }

    /**
     * @param {Number} hour
     */
    setHour(hour) {
        this['hour'] = hour;
    }
    /**
     * minimum: -9223372036854775808
     * maximum: 9223372036854775807
     * @return {Number}
     */
    getMin() {
        return this.min;
    }

    /**
     * @param {Number} min
     */
    setMin(min) {
        this['min'] = min;
    }

}

GapItemTime.RequiredProperties = ["hour", "min"];

/**
 * @member {Number} hour
 */
GapItemTime.prototype['hour'] = undefined;

/**
 * @member {Number} min
 */
GapItemTime.prototype['min'] = undefined;






export default GapItemTime;