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
 * The Notification model module.
 * @module model/Notification
 * @version 1.2.1
 */
class Notification {
    /**
     * Constructs a new <code>Notification</code>.
     * @alias module:model/Notification
     * @param created {String} 
     * @param ident {Number} 
     * @param text {String} 
     */
    constructor(created, ident, text) {

        Notification.initialize(this, created, ident, text);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj, created, ident, text) {
        obj['created'] = created;
        obj['ident'] = ident;
        obj['text'] = text;
    }

    /**
     * Constructs a <code>Notification</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/Notification} obj Optional instance to populate.
     * @return {module:model/Notification} The populated <code>Notification</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new Notification();

            if (data.hasOwnProperty('created')) {
                obj['created'] = ApiClient.convertToType(data['created'], 'String');
            }
            if (data.hasOwnProperty('ident')) {
                obj['ident'] = ApiClient.convertToType(data['ident'], 'Number');
            }
            if (data.hasOwnProperty('text')) {
                obj['text'] = ApiClient.convertToType(data['text'], 'String');
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>Notification</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>Notification</code>.
     */
    static validateJSON(data) {
        // check to make sure all required properties are present in the JSON string
        for (const property of Notification.RequiredProperties) {
            if (!data[property]) {
                throw new Error("The required field `" + property + "` is not found in the JSON data: " + JSON.stringify(data));
            }
        }
        // ensure the json data is a string
        if (data['created'] && !(typeof data['created'] === 'string' || data['created'] instanceof String)) {
            throw new Error("Expected the field `created` to be a primitive type in the JSON string but got " + data['created']);
        }
        // ensure the json data is a string
        if (data['text'] && !(typeof data['text'] === 'string' || data['text'] instanceof String)) {
            throw new Error("Expected the field `text` to be a primitive type in the JSON string but got " + data['text']);
        }

        return true;
    }

    /**
     * @return {String}
     */
    getCreated() {
        return this.created;
    }

    /**
     * @param {String} created
     */
    setCreated(created) {
        this['created'] = created;
    }
    /**
     * minimum: -9223372036854775808
     * maximum: 9223372036854775807
     * @return {Number}
     */
    getIdent() {
        return this.ident;
    }

    /**
     * @param {Number} ident
     */
    setIdent(ident) {
        this['ident'] = ident;
    }
    /**
     * @return {String}
     */
    getText() {
        return this.text;
    }

    /**
     * @param {String} text
     */
    setText(text) {
        this['text'] = text;
    }

}

Notification.RequiredProperties = ["created", "ident", "text"];

/**
 * @member {String} created
 */
Notification.prototype['created'] = undefined;

/**
 * @member {Number} ident
 */
Notification.prototype['ident'] = undefined;

/**
 * @member {String} text
 */
Notification.prototype['text'] = undefined;






export default Notification;