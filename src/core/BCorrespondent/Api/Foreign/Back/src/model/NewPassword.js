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
 * The NewPassword model module.
 * @module model/NewPassword
 * @version 1.2.1
 */
class NewPassword {
    /**
     * Constructs a new <code>NewPassword</code>.
     * @alias module:model/NewPassword
     * @param key {String} 
     * @param password {String} 
     */
    constructor(key, password) {

        NewPassword.initialize(this, key, password);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj, key, password) {
        obj['key'] = key;
        obj['password'] = password;
    }

    /**
     * Constructs a <code>NewPassword</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/NewPassword} obj Optional instance to populate.
     * @return {module:model/NewPassword} The populated <code>NewPassword</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new NewPassword();

            if (data.hasOwnProperty('key')) {
                obj['key'] = ApiClient.convertToType(data['key'], 'String');
            }
            if (data.hasOwnProperty('password')) {
                obj['password'] = ApiClient.convertToType(data['password'], 'String');
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>NewPassword</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>NewPassword</code>.
     */
    static validateJSON(data) {
        // check to make sure all required properties are present in the JSON string
        for (const property of NewPassword.RequiredProperties) {
            if (!data[property]) {
                throw new Error("The required field `" + property + "` is not found in the JSON data: " + JSON.stringify(data));
            }
        }
        // ensure the json data is a string
        if (data['key'] && !(typeof data['key'] === 'string' || data['key'] instanceof String)) {
            throw new Error("Expected the field `key` to be a primitive type in the JSON string but got " + data['key']);
        }
        // ensure the json data is a string
        if (data['password'] && !(typeof data['password'] === 'string' || data['password'] instanceof String)) {
            throw new Error("Expected the field `password` to be a primitive type in the JSON string but got " + data['password']);
        }

        return true;
    }

    /**
     * @return {String}
     */
    getKey() {
        return this.key;
    }

    /**
     * @param {String} key
     */
    setKey(key) {
        this['key'] = key;
    }
    /**
     * @return {String}
     */
    getPassword() {
        return this.password;
    }

    /**
     * @param {String} password
     */
    setPassword(password) {
        this['password'] = password;
    }

}

NewPassword.RequiredProperties = ["key", "password"];

/**
 * @member {String} key
 */
NewPassword.prototype['key'] = undefined;

/**
 * @member {String} password
 */
NewPassword.prototype['password'] = undefined;






export default NewPassword;