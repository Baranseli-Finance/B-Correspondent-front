# BCorrespondentTagCommitBb65df7.FileApi

All URIs are relative to *http://api.b-correspondent.app*

Method | HTTP request | Description
------------- | ------------- | -------------
[**fileUploadBucketPut**](FileApi.md#fileUploadBucketPut) | **PUT** /file/upload/{bucket} | 



## fileUploadBucketPut

> ResponseIdFile fileUploadBucketPut(bucket, payloadFiles)



BCorrespondent apiupload to server

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';
let defaultClient = BCorrespondentTagCommitBb65df7.ApiClient.instance;
// Configure API key authorization: JwtSecurity
let JwtSecurity = defaultClient.authentications['JwtSecurity'];
JwtSecurity.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//JwtSecurity.apiKeyPrefix = 'Token';

let apiInstance = new BCorrespondentTagCommitBb65df7.FileApi();
let bucket = "bucket_example"; // String | 
let payloadFiles = "/path/to/file"; // Number | 
apiInstance.fileUploadBucketPut(bucket, payloadFiles).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **bucket** | **String**|  | 
 **payloadFiles** | **Number**|  | 

### Return type

[**ResponseIdFile**](ResponseIdFile.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: multipart/form-data
- **Accept**: application/json;charset=utf-8

