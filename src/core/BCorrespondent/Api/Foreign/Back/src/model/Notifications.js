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
import Notification from './Notification';

/**
 * The Notifications model module.
 * @module model/Notifications
 * @version 1.2.1
 */
class Notifications {
    /**
     * Constructs a new <code>Notifications</code>.
     * @alias module:model/Notifications
     * @param count {Number} 
     * @param items {Array.<module:model/Notification>} 
     */
    constructor(count, items) {

        Notifications.initialize(this, count, items);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj, count, items) {
        obj['count'] = count;
        obj['items'] = items;
    }

    /**
     * Constructs a <code>Notifications</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/Notifications} obj Optional instance to populate.
     * @return {module:model/Notifications} The populated <code>Notifications</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new Notifications();

            if (data.hasOwnProperty('count')) {
                obj['count'] = ApiClient.convertToType(data['count'], 'Number');
            }
            if (data.hasOwnProperty('items')) {
                obj['items'] = ApiClient.convertToType(data['items'], [Notification]);
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>Notifications</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>Notifications</code>.
     */
    static validateJSON(data) {
        // check to make sure all required properties are present in the JSON string
        for (const property of Notifications.RequiredProperties) {
            if (!data[property]) {
                throw new Error("The required field `" + property + "` is not found in the JSON data: " + JSON.stringify(data));
            }
        }
        if (data['items']) { // data not null
            // ensure the json data is an array
            if (!Array.isArray(data['items'])) {
                throw new Error("Expected the field `items` to be an array in the JSON data but got " + data['items']);
            }
            // validate the optional field `items` (array)
            for (const item of data['items']) {
                Notification.validateJSON(item);
            };
        }

        return true;
    }

    /**
     * minimum: -9223372036854775808
     * maximum: 9223372036854775807
     * @return {Number}
     */
    getCount() {
        return this.count;
    }

    /**
     * @param {Number} count
     */
    setCount(count) {
        this['count'] = count;
    }
    /**
     * @return {Array.<module:model/Notification>}
     */
    getItems() {
        return this.items;
    }

    /**
     * @param {Array.<module:model/Notification>} items
     */
    setItems(items) {
        this['items'] = items;
    }

}

Notifications.RequiredProperties = ["count", "items"];

/**
 * @member {Number} count
 */
Notifications.prototype['count'] = undefined;

/**
 * @member {Array.<module:model/Notification>} items
 */
Notifications.prototype['items'] = undefined;






export default Notifications;