import FingerprintJS from '@fingerprintjs/fingerprintjs';

export const _getBrowserIdentifier = function() {
   return function(onError, onOk) {
    const promise = FingerprintJS.load();
    promise.then(fp => fp.get()).then(r => onOk(r.visitorId)).catch(onError);
   };
}