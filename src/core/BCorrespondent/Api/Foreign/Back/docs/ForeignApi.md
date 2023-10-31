# BCorrespondentTagCommitBb65df7.ForeignApi

All URIs are relative to *http://api.b-correspondent.app*

Method | HTTP request | Description
------------- | ------------- | -------------
[**foreignSendgridSendPost**](ForeignApi.md#foreignSendgridSendPost) | **POST** /foreign/sendgrid/send | 
[**foreignWebhookGithubPost**](ForeignApi.md#foreignWebhookGithubPost) | **POST** /foreign/webhook/github | 
[**foreignWebhookProviderPost**](ForeignApi.md#foreignWebhookProviderPost) | **POST** /foreign/webhook/{provider} | 



## foreignSendgridSendPost

> Response foreignSendgridSendPost(body)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.ForeignApi();
let body = new BCorrespondentTagCommitBb65df7.SendGridSendMailRequest(); // SendGridSendMailRequest | 
apiInstance.foreignSendgridSendPost(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**SendGridSendMailRequest**](SendGridSendMailRequest.md)|  | 

### Return type

[**Response**](Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## foreignWebhookGithubPost

> Response foreignWebhookGithubPost(body)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.ForeignApi();
let body = {key: null}; // Object | 
apiInstance.foreignWebhookGithubPost(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | **Object**|  | 

### Return type

[**Response**](Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## foreignWebhookProviderPost

> Response foreignWebhookProviderPost(provider, body)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.ForeignApi();
let provider = "provider_example"; // String | 
let body = {key: null}; // Object | 
apiInstance.foreignWebhookProviderPost(provider, body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **provider** | **String**|  | 
 **body** | **Object**|  | 

### Return type

[**Response**](Response.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8

