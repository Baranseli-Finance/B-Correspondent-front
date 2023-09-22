export const set = function(cookie) {
    return () => {
        let value =
            cookie.getName() + "=" +
            cookie.getValue() + ";expires=" +
            cookie.getExpires() + ";path=" +
            cookie.getPath();
        document.cookie = value;
    }
}

export const getIml = function(nothing, just, cName) {
    return () => {
        const name = cName + "=";
        const cDecoded = decodeURIComponent(document.cookie); //to be careful
        const cArr = cDecoded.split(';');
        let res;
        cArr.forEach(val => {
            if (val.indexOf(name) === 0)
                res = val.substring(name.length);
        })
        return res !== undefined ? just(res) : nothing;
    }
}