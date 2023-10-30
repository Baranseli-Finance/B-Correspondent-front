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
            api.frontendUserDashboardTimelineGapGet(from, to).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _fetchTimelineForParticularHour =
    function(withError, direction, point, api) {
        return function(onError, onOk) {
            api.frontendUserDashboardTimelineDirectionGet(direction, point).then(onOk).catch(resp => {
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

export const _initHistoryTimeline =
    function(withError, date, api) {
        return function(onError, onOk) {
            api.frontendUserHistoryTimelineGet(date).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _fetchShiftHistoryTimeline =
    function(withError, params, api) {
        return function(onError, onOk) {
            let y = params['year']
            let m = params['month']
            let d = params['day']
            let direction = params['direction']
            let hour = params['hour']
            let inst = params['institution']
            api.frontendUserHistoryTimelineYearMonthDayDirectionInstitutionGetWithHttpInfo(y, m, d, direction, inst, hour).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _fetchNotifications =
    function(withError, api) {
        return function(onError, onOk) {
            api.frontendUserNotificationsGet().then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _submitIssue =
    function(withError, issue, api) {
        return function(onError, onOk) {
            api.frontendUserIssuePut(issue).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _markNotificationRead =
    function(withError, ident, api) {
        return function(onError, onOk) {
            api.frontendUserNotificationPost(ident).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _initBalancedBook =
    function(withError, api) {
        return function(onError, onOk) {
            api.frontendUserBalancedBookGet().then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _fetchBalancedBook =
    function(withError, y, m, d, dir, api) {
        return function(onError, onOk) {
            api.frontendUserBalancedBookYearMonthDayDirectionGet(y, m, d, dir).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const _loadUnreadNotification =
    function(withError, api) {
        return function(onError, onOk) {
            api.frontendUserWorkspaceGet().then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }