import * as e from '../BCorrespondent.Api.Foreign.Back/Back/src/index';

export const mkAuthApi = function(api) {
    return () => {
        return new e.AuthApi(api);
    }
}

export const _login =
    function(withError, authType, cred, api) {
        return function(onError, onOk) {
            api.authLoginAuthTypePost(authType, cred).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }