import * as e from './BuzgibiBack/src/index';

export const mkForeignApi = function(api) {
    return () => {
        return new e.ForeignApi(api);
    }
}

export const mkSendGridSendMailRequest =
    function(body) {
        return () => {
            let req = new e.SendGridSendMailRequest();
            return e.SendGridSendMailRequest.constructFromObject(body, req)
        };
    }

export const _sendEmail =
    function(withError, mail, api) {
        return function(onError, onOk) {
            api.foreignSendgridSendPost(mail).then(onOk).catch(resp => {
                return withError(resp, onError);
            })
        };
    }

export const mkReCaptchaApi = function(api) {
    return () => {
        return new e.ReCaptchaApi(api);
    }
}

export const _goReCaptcha = function(withError, key, api) {
    return function(onError, onOk) {
        grecaptcha.ready(function() {
            grecaptcha.execute(key, {
                action: 'submit'
            }).then(function(token) {
                api.captchaVerifyPost('\"' + token + '\"').then(onOk).catch(resp => {
                    return withError(resp, onError)
                });
            });
        });
    }
}

export const getSuccessReCaptcha = captcha => {
    return captcha.getSuccess();
}