import sha256 from 'crypto-js/sha256';
import sha512 from 'crypto-js/sha512';

export const _createHash = algo => obj => tm => {
    return () => {
        let str = JSON.stringify(obj) + tm;
        if (algo == "SHA256") {
            return sha256(str);
        } else if (algo == "SHA512") {
            return sha512(str);
        } else throw "algo not found";
    };
}