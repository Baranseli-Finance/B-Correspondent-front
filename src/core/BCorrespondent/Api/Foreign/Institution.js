import * as e from '../BCorrespondent.Api.Foreign.Back/Back/src/index';

export const mkInstitutionApi = function(api) {
    return () => {
        return new e.InstitutionApi(api);
    }
}

export const _fetchBalances =
    function(withError, api) {
        return function(onError, onOk) {
            api.institutionFiatBalancesGet().then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _withdraw =
    function(withError, body, api) {
        return function(onError, onOk) {
            api.institutionFiatWithdrawPost(body).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }
