export const isNumber = s => {
    return /^\[0-9]{1,}$/.test(s);
}

export const isValidNote = s => {
    return /^[0-9]{1,14}[.]{1}[0-9]{2}$/.test(s);
}