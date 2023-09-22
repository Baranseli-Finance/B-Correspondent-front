export const removeValue = (el) => () => {
    if (el.value) {
        try {
            el.value = ''; //for IE11, latest Chrome/Firefox/Opera...
        } catch (err) {}
        if (el.value) { //for IE5 ~ IE10
            var form = document.createElement('form'),
                parentNode = el.parentNode,
                ref = el.nextSibling;
            form.appendChild(el);
            form.reset();
            parentNode.insertBefore(el, ref);
        }
    }
}