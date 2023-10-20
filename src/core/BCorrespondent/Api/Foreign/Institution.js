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

export const _withdraw =
    function(withError, body, api) {
        return function(onError, onOk) {
            api.institutionFiatWithdrawPut(body).then(onOk).catch(resp => {
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