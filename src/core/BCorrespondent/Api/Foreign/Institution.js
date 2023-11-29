import * as e from '../BCorrespondent.Api.Foreign.Back/Back/src/index';

export const mkInstitutionApi = function(api) {
    return () => {
        return new e.InstitutionApi(api);
    }
}

export const _initWithdrawal =
    function(withError, api) {
        return function(onError, onOk) {
            api.institutionFiatWithdrawInitGet().then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _registerWithdrawal =
    function(withError, withdrawal, api) {
        return function(onError, onOk) {
            api.institutionFiatWithdrawRegisterPostWithHttpInfo(withdrawal).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _confirmWithdrawal =
    function(withError, code, api) {
        return function(onError, onOk) {
            api.institutionFiatWithdrawConfirmPut(code).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _fetchWithdrawHistoryPage =
    function(withError, page, api) {
        return function(onError, onOk) {
            api.institutionFiatWithdrawHistoryPageGet(page).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }