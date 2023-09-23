import * as e from '../BCorrespondent.Api.Foreign.Back/Back/src/index';

export const mkFrontApi = function(api) {
    return () => {
        return new e.FrontendApi(api);
    }
}

export const _init =
    function(withError, token, api) {
        return function(onError, onOk) {
            api.frontendInitGet(token).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }