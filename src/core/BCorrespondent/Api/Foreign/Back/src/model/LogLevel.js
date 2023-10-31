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
 * Enum class LogLevel.
 * @enum {}
 * @readonly
 */
export default class LogLevel {

    /**
     * value: "prod"
     * @const
     */
    "prod" = "prod";


    /**
     * value: "dev"
     * @const
     */
    "dev" = "dev";



    /**
     * Returns a <code>LogLevel</code> enum value from a Javascript object name.
     * @param {Object} data The plain JavaScript object containing the name of the enum value.
     * @return {module:model/LogLevel} The enum <code>LogLevel</code> value.
     */
    static constructFromObject(object) {
        return object;
    }
}