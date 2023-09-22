import * as e from '../Buzgibi.Api.Foreign.BuzgibiBack/BuzgibiBack/src/index';

export const mkFileApi = function(api) {
    return () => {
        return new e.FileApi(api);
    }
}

export const _upload = function(withError, bucket, file, api) {
    return function(onError, onOk) {
        api.fileBucketPut(bucket, file).then(onOk).catch(resp => {
            return withError(resp, onError)
        })
    };
}