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
import WalletType from './WalletType';

/**
 * The BalancedBookWallet model module.
 * @module model/BalancedBookWallet
 * @version 1.2.1
 */
class BalancedBookWallet {
    /**
     * Constructs a new <code>BalancedBookWallet</code>.
     * @alias module:model/BalancedBookWallet
     * @param amount {Number} 
     * @param currency {module:model/Currency} 
     * @param ident {Number} 
     * @param walletType {module:model/WalletType} 
     */
    constructor(amount, currency, ident, walletType) {

        BalancedBookWallet.initialize(this, amount, currency, ident, walletType);
    }

    /**
     * Initializes the fields of this object.
     * This method is used by the constructors of any subclasses, in order to implement multiple inheritance (mix-ins).
     * Only for internal use.
     */
    static initialize(obj, amount, currency, ident, walletType) {
        obj['amount'] = amount;
        obj['currency'] = currency;
        obj['ident'] = ident;
        obj['walletType'] = walletType;
    }

    /**
     * Constructs a <code>BalancedBookWallet</code> from a plain JavaScript object, optionally creating a new instance.
     * Copies all relevant properties from <code>data</code> to <code>obj</code> if supplied or a new instance if not.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @param {module:model/BalancedBookWallet} obj Optional instance to populate.
     * @return {module:model/BalancedBookWallet} The populated <code>BalancedBookWallet</code> instance.
     */
    static constructFromObject(data, obj) {
        if (data) {
            obj = obj || new BalancedBookWallet();

            if (data.hasOwnProperty('amount')) {
                obj['amount'] = ApiClient.convertToType(data['amount'], 'Number');
            }
            if (data.hasOwnProperty('currency')) {
                obj['currency'] = Currency.constructFromObject(data['currency']);
            }
            if (data.hasOwnProperty('ident')) {
                obj['ident'] = ApiClient.convertToType(data['ident'], 'Number');
            }
            if (data.hasOwnProperty('walletType')) {
                obj['walletType'] = WalletType.constructFromObject(data['walletType']);
            }
        }
        return obj;
    }

    /**
     * Validates the JSON data with respect to <code>BalancedBookWallet</code>.
     * @param {Object} data The plain JavaScript object bearing properties of interest.
     * @return {boolean} to indicate whether the JSON data is valid with respect to <code>BalancedBookWallet</code>.
     */
    static validateJSON(data) {
        // check to make sure all required properties are present in the JSON string
        for (const property of BalancedBookWallet.RequiredProperties) {
            if (!data[property]) {
                throw new Error("The required field `" + property + "` is not found in the JSON data: " + JSON.stringify(data));
            }
        }

        return true;
    }

    /**
     * @return {Number}
     */
    getAmount() {
        return this.amount;
    }

    /**
     * @param {Number} amount
     */
    setAmount(amount) {
        this['amount'] = amount;
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
     * @return {module:model/WalletType}
     */
    getWalletType() {
        return this.walletType;
    }

    /**
     * @param {module:model/WalletType} walletType
     */
    setWalletType(walletType) {
        this['walletType'] = walletType;
    }

}

BalancedBookWallet.RequiredProperties = ["amount", "currency", "ident", "walletType"];

/**
 * @member {Number} amount
 */
BalancedBookWallet.prototype['amount'] = undefined;

/**
 * @member {module:model/Currency} currency
 */
BalancedBookWallet.prototype['currency'] = undefined;

/**
 * @member {Number} ident
 */
BalancedBookWallet.prototype['ident'] = undefined;

/**
 * @member {module:model/WalletType} walletType
 */
BalancedBookWallet.prototype['walletType'] = undefined;






export default BalancedBookWallet;