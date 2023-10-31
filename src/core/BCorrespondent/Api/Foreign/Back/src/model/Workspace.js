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
 * The Workspace model module.
 * @module model/Workspace
 * @version 1.2.1
 */
class Workspace {
    /**
     * Constructs a new <code>Workspace</code>.
     * @alias module:model/Workspace
     * @param unreadNotification {Number} 
     */
    constructor(unreadNotification) {

        Workspace.initialize(this, unreadNotification);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj, unreadNotification) {
        obj['unreadNotification'] = unreadNotification;
    }

    /**
     * Constructs a <code>Workspace</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/Workspace} obj Optional instance to populate.
     * @return {module:model/Workspace} The populated <code>Workspace</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new Workspace();

            if (data.hasOwnProperty('unreadNotification')) {
                obj['unreadNotification'] = ApiClient.convertToType(data['unreadNotification'], 'Number');
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>Workspace</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>Workspace</code>.
     */
    static validateJSON(data) {
        // check to make sure all required properties are present in the JSON string
        for (const property of Workspace.RequiredProperties) {
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
    getUnreadNotification() {
        return this.unreadNotification;
    }

    /**
     * @param {Number} unreadNotification
     */
    setUnreadNotification(unreadNotification) {
        this['unreadNotification'] = unreadNotification;
    }

}

Workspace.RequiredProperties = ["unreadNotification"];

/**
 * @member {Number} unreadNotification
 */
Workspace.prototype['unreadNotification'] = undefined;






export default Workspace;