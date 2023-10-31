# BCorrespondentTagCommitBb65df7.AdminApi

All URIs are relative to *http://api.b-correspondent.app*

Method | HTTP request | Description
------------- | ------------- | -------------
[**adminUserPut**](AdminApi.md#adminUserPut) | **PUT** /admin/user | 



## adminUserPut

> Response adminUserPut(body)



BCorrespondent apicreate new user

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';
let defaultClient = BCorrespondentTagCommitBb65df7.ApiClient.instance;
// Configure API key authorization: JwtSecurity
let JwtSecurity = defaultClient.authentications['JwtSecurity'];
JwtSecurity.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//JwtSecurity.apiKeyPrefix = 'Token';

let apiInstance = new BCorrespondentTagCommitBb65df7.AdminApi();
let body = new BCorrespondentTagCommitBb65df7.NewUser(); // NewUser | 
apiInstance.adminUserPut(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**NewUser**](NewUser.md)|  | 

### Return type

[**Response**](Response.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8

