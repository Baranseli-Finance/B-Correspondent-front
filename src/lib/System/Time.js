export const getTimestamp = () => {
    return Math.floor(Date.now() / 1000);
}


export const timestampToDate = tm => () => {
    // Create a new JavaScript Date object based on the timestamp
    // multiplied by 1000 so that the argument is in milliseconds, not seconds.
    var date = new Date(tm * 1000);
    // Will display time in 10:30:23 format
    return date.getHours() + ":" + date.getMinutes() + ":" + date.getSeconds() + ", " + date.toDateString();
}