import sha256 from 'crypto-js/sha256';

export const _createHash = obj => tm => {
    return () => {
        let str = JSON.stringify(obj) + tm;
        return sha256(str);
    };
}