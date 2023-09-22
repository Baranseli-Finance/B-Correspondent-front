export const _create = function create(url, protocols) {
    return function() {
        let ws = new WebSocket(url, protocols);
        ws.binaryType = "blob";
        return ws;
    }
};

export const _readState = function(ws) {
    return function() {
        return ws.readyState;
    };
}

export const _send = function(ws, o) {
    return function() {
        return ws.send(o);
    };
}

export const _unsafeStringify = JSON.stringify;

export const _close = function(ws) {
    return function() {
        ws.close();
    };
}