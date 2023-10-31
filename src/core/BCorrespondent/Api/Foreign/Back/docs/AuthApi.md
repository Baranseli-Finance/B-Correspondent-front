# BCorrespondentTagCommitBb65df7.AuthApi

All URIs are relative to *http://api.b-correspondent.app*

Method | HTTP request | Description
------------- | ------------- | -------------
[**authCodePut**](AuthApi.md#authCodePut) | **PUT** /auth/code | 
[**authCodeResendPut**](AuthApi.md#authCodeResendPut) | **PUT** /auth/code/resend | 
[**authLoginAuthTypePost**](AuthApi.md#authLoginAuthTypePost) | **POST** /auth/login/{auth_type} | 
[**authLogoutPost**](AuthApi.md#authLogoutPost) | **POST** /auth/logout | 
[**authPasswordResetLinkPut**](AuthApi.md#authPasswordResetLinkPut) | **PUT** /auth/password/reset/link | 
[**authPasswordResetPost**](AuthApi.md#authPasswordResetPost) | **POST** /auth/password/reset | 
[**authTokenGenerateKeyPost**](AuthApi.md#authTokenGenerateKeyPost) | **POST** /auth/token/generate/{key} | 



## authCodePut

> ResponseAuthCodeHash authCodePut(body)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.AuthApi();
let body = new BCorrespondentTagCommitBb65df7.Credentials(); // Credentials | 
apiInstance.authCodePut(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Credentials**](Credentials.md)|  | 

### Return type

[**ResponseAuthCodeHash**](ResponseAuthCodeHash.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## authCodeResendPut

> ResponseAuthCodeHash authCodeResendPut(body)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.AuthApi();
let body = new BCorrespondentTagCommitBb65df7.ResendCode(); // ResendCode | 
apiInstance.authCodeResendPut(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ResendCode**](ResendCode.md)|  | 

### Return type

[**ResponseAuthCodeHash**](ResponseAuthCodeHash.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## authLoginAuthTypePost

> ResponseAuthToken authLoginAuthTypePost(authType, body)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.AuthApi();
let authType = "authType_example"; // String | 
let body = new BCorrespondentTagCommitBb65df7.AuthCode(); // AuthCode | 
apiInstance.authLoginAuthTypePost(authType, body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **authType** | **String**|  | 
 **body** | [**AuthCode**](AuthCode.md)|  | 

### Return type

[**ResponseAuthToken**](ResponseAuthToken.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## authLogoutPost

> Response authLogoutPost()



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';
let defaultClient = BCorrespondentTagCommitBb65df7.ApiClient.instance;
// Configure API key authorization: JwtSecurity
let JwtSecurity = defaultClient.authentications['JwtSecurity'];
JwtSecurity.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//JwtSecurity.apiKeyPrefix = 'Token';

let apiInstance = new BCorrespondentTagCommitBb65df7.AuthApi();
apiInstance.authLogoutPost().then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters

This endpoint does not need any parameter.

### Return type

[**Response**](Response.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## authPasswordResetLinkPut

> ResponseMaybeInt64 authPasswordResetLinkPut()



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';
let defaultClient = BCorrespondentTagCommitBb65df7.ApiClient.instance;
// Configure API key authorization: JwtSecurity
let JwtSecurity = defaultClient.authentications['JwtSecurity'];
JwtSecurity.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//JwtSecurity.apiKeyPrefix = 'Token';

let apiInstance = new BCorrespondentTagCommitBb65df7.AuthApi();
apiInstance.authPasswordResetLinkPut().then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters

This endpoint does not need any parameter.

### Return type

[**ResponseMaybeInt64**](ResponseMaybeInt64.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## authPasswordResetPost

> ResponseBool authPasswordResetPost(body)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';
let defaultClient = BCorrespondentTagCommitBb65df7.ApiClient.instance;
// Configure API key authorization: JwtSecurity
let JwtSecurity = defaultClient.authentications['JwtSecurity'];
JwtSecurity.apiKey = 'YOUR API KEY';
// Uncomment the following line to set a prefix for the API key, e.g. "Token" (defaults to null)
//JwtSecurity.apiKeyPrefix = 'Token';

let apiInstance = new BCorrespondentTagCommitBb65df7.AuthApi();
let body = new BCorrespondentTagCommitBb65df7.NewPassword(); // NewPassword | 
apiInstance.authPasswordResetPost(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**NewPassword**](NewPassword.md)|  | 

### Return type

[**ResponseBool**](ResponseBool.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## authTokenGenerateKeyPost

> ResponseAuthToken authTokenGenerateKeyPost(key)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.AuthApi();
let key = "key_example"; // String | 
apiInstance.authTokenGenerateKeyPost(key).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **key** | **String**|  | 

### Return type

[**ResponseAuthToken**](ResponseAuthToken.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8

