import * as e from '../Buzgibi.Api.Foreign.BuzgibiBack/BuzgibiBack/src/index';

export const _mkApiClient = function(jwt, host) {
    return () => {
        let cl = new e.ApiClient(host);
        if (jwt !== undefined) {
            cl.defaultHeaders = {
                Authorization: 'Token ' + jwt
            };
        } else {
            cl.defaultHeaders = [];
        }
        return cl;
    }
}

export const _getDataFromObj = left => right => resp => {
    let success = resp.getSuccess();

    let errMsg = (xs) => {
        tmp = '';
        xs.forEach(e => {
            tmp += e.getMessage();
        });
        return tmp;
    }

    let warnMsg = (xs) => {
        tmp = [];
        xs.forEach(e => {
            tmp.push(e.getMessage());
        });
        return tmp;
    }

    let err = resp.getErrors() !== undefined ? errMsg(resp.getErrors()) : 'malformed resp: ' + JSON.stringify(resp);
    let warns = resp.getWarnings() !== undefined ? warnMsg(resp.getWarnings()) : [];
    const obj = new Object();
    obj.success = success;
    obj.warnings = warns;
    return () => {
        return success !== undefined ? right(obj) : left(err);
    };
}

export const _getDataFromObjWS = left => right => resp => {
    let success = resp['success'];

    let errMsg = (xs) => {
        tmp = '';
        xs.forEach(e => {
            tmp += e['message'];
        });
        return tmp;
    }

    let warnMsg = (xs) => {
        tmp = [];
        xs.forEach(e => {
            tmp.push(e['message']);
        });
        return tmp;
    }

    let err = resp['errors'] !== undefined ? errMsg(resp['errors']) : 'malformed resp: ' + JSON.stringify(resp);
    let warns = resp['warnings'] !== undefined ? warnMsg(resp['warnings']) : [];
    const obj = new Object();
    obj.success = success;
    obj.warnings = warns;
    return () => {
        return success !== undefined ? right(obj) : left(err);
    };
}

export const _printError = (err) => {
    return err.getMessage();
}

export const withError = function(resp, onError) {
    let e = new Error();
    let mkMsg = '';
    if (resp['type'] !== undefined && resp['type'] == 'error') {
        mkMsg += JSON.stringify(resp['currentTarge']);
    } else if (resp['body']['combinator'] !== undefined) {
        mkMsg += "combinator " + resp['body']['combinator'] + " has failed with error " + resp['body']['error'];
    } else if (resp['body'] != undefined) {
        mkMsg += "server responded with " + resp['body'];
    } else {
        mkMsg += 'server responded with an unknown error';
    }
    e.message = "error: " + mkMsg;
    onError(e);
}

export const _fetchWS = function(withError, ws) {
    return function(onError, onOk) {
        ws.onmessage = function(event) {
            onOk(JSON.parse(event.data));
        }
        ws.onerror = function(err) {
            return withError(err, onError)
        }
    };
}