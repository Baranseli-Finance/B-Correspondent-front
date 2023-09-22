import Bowser, * as e from './Bowser/src/bowser.js';

export const getPlatform = function(ua) {
    return function() {
        return Bowser.getParser(ua).getPlatformType();
    };
};