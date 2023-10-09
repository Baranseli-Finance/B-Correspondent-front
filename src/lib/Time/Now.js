export const now = () => { return Date.now(); }

export const _addMinutes = minutes => tm => () => { return  new Date(Date.now(tm) + minutes*60000); }