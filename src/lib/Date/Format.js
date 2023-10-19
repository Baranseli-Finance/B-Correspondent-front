export const format = function(s) {
    return () => {
        let d = new Date(s);
        let sec = d.getSeconds() < 10 ? '0' + d.getSeconds() : d.getSeconds();
        let min = d.getMinutes() < 10 ? '0' + d.getMinutes() : d.getMinutes();
        let hour = d.getHours() < 10 ? '0' + d.getHours() : d.getHours();
        return d.getDate() + "-" + (d.getMonth() + 1) + "-" + d.getFullYear() + " " + hour + ":" + min + ":" + sec;
    };
}