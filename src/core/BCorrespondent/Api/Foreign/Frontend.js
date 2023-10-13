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

export const _initDashboard =
    function(withError, api) {
        return function(onError, onOk) {
            api.frontendUserDashboardInitGet().then(onOk).catch(resp => {
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

export const _fetchTimelineForParticularHour =
    function(withError, direction, point, api) {
        return function(onError, onOk) {
            api.frontendUserDashboardDailyBalanceSheetTimelineDirectionGet(direction, point).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _fetchTransaction =
    function(withError, ident, api) {
        return function(onError, onOk) {
            api.frontendUserDashboardTimelineTransactionIdentGet(ident).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }