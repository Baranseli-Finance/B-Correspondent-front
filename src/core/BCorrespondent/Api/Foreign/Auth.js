import * as e from '../BCorrespondent.Api.Foreign.Back/Back/src/index';

export const mkAuthApi = function(api) {
    return () => {
        return new e.AuthApi(api);
    }
}

export const _sendCode =
    function(withError, cred, api) {
        return function(onError, onOk) {
            api.authCodePut(cred).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _login =
    function(withError, authType, code, api) {
        return function(onError, onOk) {
            api.authLoginAuthTypePost(authType, code).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _resendCode =
    function(withError, resendCode, api) {
        return function(onError, onOk) {
            api.authCodeResendPut(resendCode).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _logout =
    function(withError, api) {
        return function(onError, onOk) {
            api.authLogoutPost().then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }