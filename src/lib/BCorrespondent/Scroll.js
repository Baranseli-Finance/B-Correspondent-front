export const _onDetectVisible = container => element => () => {
    var elementTop = calculateSpace(container, element);
    var elementHeight = element.offsetHeight;
    var scrollTop = container.scrollY || container.scrollTop || 0;
    var containerHeight = container.innerHeight || container.clientHeight || 0;
    return elementTop - containerHeight < scrollTop && scrollTop < elementTop + elementHeight;
};

function calculateOffset(element) {
    var result = 0;
    while (element) {
        var value = element.offsetTop;
        if (value) {
            result += value;
        }
        // jump to next relative, absolute or fixed parent element
        element = element.offsetParent;
    }
    return result;
};

// Calculates space between elements.
//
function calculateSpace(firstElement, secondElement) {
    var firstOffset = calculateOffset(firstElement);
    var secondOffset = calculateOffset(secondElement);
    return secondOffset - firstOffset;
};