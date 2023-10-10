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

export const _initUserDashboardDailyBalanceSheet =
function(withError, api) {
    return function(onError, onOk) {
        api.frontendUserDashboardDailyBalanceSheetInitGet().then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _loadNextGap =
function(withError, from, to, api) {
    return function(onError, onOk) {
        api.frontendUserDashboardDailyBalanceSheetGapGet(from, to).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}