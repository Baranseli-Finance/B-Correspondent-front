export const _getLocation = function() {
    return function(onError, onOk) {
        navigator.geolocation.getCurrentPosition(
            (loc) => {
                let coordinates = {
                    latitude: loc.coords.latitude,
                    longitude: loc.coords.longitude
                }
                onOk(coordinates);
            }, onError);
    };
}