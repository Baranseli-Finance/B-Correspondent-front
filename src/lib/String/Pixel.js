import getWidth from 'string-pixel-width';

export const lengthToPixels = s => size => {
    return getWidth(s, {
        size: size
    });
}