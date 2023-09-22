import * as e from '../Buzgibi.Api.Foreign.BuzgibiBack/BuzgibiBack/src/index';

export const mkFrontApi = function(api) {
    return () => {
        return new e.FrontApi(api);
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

export const _showInit =
    function(obj) {
        return JSON.stringify(obj);
    }

export const _showTranslation =
    function(obj) {
        return JSON.stringify(obj);
    }

export const getShaCommit = (obj) => {
    return obj.getSha();
}

export const _getLogLevel = obj => {
    return obj.getEnv().getLogLevel();
}

export const getShaCSSCommit = (obj) => {
    return obj.getShaCss();
}

export const _getJwtStatus = obj => {
    return obj.getIsJwtValid();
}

export const _getIsTest = nothing => just => obj => {
    let env = obj.getEnv();
    return env !== undefined ? just(env.getIsTest()) : nothing;
}

export const _getIsCaptcha = nothing => just => obj => {
    let env = obj.getEnv();
    return env !== undefined ? just(env.getIsCaptcha()) : nothing;
}

export const _getToTelegram = nothing => just => obj => {
    let env = obj.getEnv();
    return env !== undefined ? just(env.getToTelegram()) : nothing;
}

export const _loadTranslation =
    function(withError, lang, api) {
        return function(onError, onOk) {
            api.frontendTranslateLangGet(lang).then(onOk).catch(resp => {
                return withError(resp, onError)
            })
        };
    }

export const mkLogReq = function(build, payload) {
    let req = new e.FrontendLogRequest();
    req.setBuild(build);
    req.setPayload(payload);
    return () => {
        return req;
    };
}

export const _sendLog = function(withError, req, api) {
    return function(onError, onOk) {
        api.frontendLogPut(req).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _showCookie = cookie => {
    return cookie.getName()
}

export const _getCookies = function(withError, api) {
    return function(onError, onOk) {
        api.frontendCookiesGet().then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const _getMeta = function(withError, page, api) {
    return function(onError, onOk) {
        api.frontendMetaGet(page).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}

export const getMetaDescription = meta => {
    return meta.getDescription();
}

export const _getKeyText = obj => {
    return obj.getKey();
}
export const _getValText = obj => {
    return obj.getValue();
}

export const getCookiesInit = (obj) => {
    return obj.getCookies();
}

export const _getTranslationMenu = obj => {
    return obj.getMenu();
}

export const _getTranslationPage = obj => {
    return obj.getPage();
}

export const _getTranslationEndpoints = obj => {
    return obj.getEndpoints();
}

export const _getTranslationPageItem = (obj) => {
    let item = {
        key: obj.getKey(),
        value: obj.getValue()
    }
    return item;
}

export const _showMapMenuText = menu => {
    return "{ key: " + menu.getKey() + ", value: " + menu.getValue() + " }";
}

export const getTranslationCopyright = (obj) => {
    return obj.getCopyright();
}