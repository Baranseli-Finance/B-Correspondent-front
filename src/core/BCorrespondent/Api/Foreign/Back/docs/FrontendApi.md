# BCorrespondentTagCommitBb65df7.FrontendApi

All URIs are relative to *http://api.b-correspondent.app*

Method | HTTP request | Description
------------- | ------------- | -------------
[**frontendInitGet**](FrontendApi.md#frontendInitGet) | **GET** /frontend/init | 
[**frontendUserBalancedBookGet**](FrontendApi.md#frontendUserBalancedBookGet) | **GET** /frontend/user/balanced-book | 
[**frontendUserBalancedBookYearMonthDayDirectionGet**](FrontendApi.md#frontendUserBalancedBookYearMonthDayDirectionGet) | **GET** /frontend/user/balanced-book/{year}/{month}/{day}/{direction} | 
[**frontendUserDashboardInitGet**](FrontendApi.md#frontendUserDashboardInitGet) | **GET** /frontend/user/dashboard/init | 
[**frontendUserDashboardTimelineDirectionGet**](FrontendApi.md#frontendUserDashboardTimelineDirectionGet) | **GET** /frontend/user/dashboard/timeline/{direction} | 
[**frontendUserDashboardTimelineGapGet**](FrontendApi.md#frontendUserDashboardTimelineGapGet) | **GET** /frontend/user/dashboard/timeline/gap | 
[**frontendUserDashboardTimelineTransactionIdentGet**](FrontendApi.md#frontendUserDashboardTimelineTransactionIdentGet) | **GET** /frontend/user/dashboard/timeline/transaction/{ident} | 
[**frontendUserHistoryTimelineGet**](FrontendApi.md#frontendUserHistoryTimelineGet) | **GET** /frontend/user/history/timeline | 
[**frontendUserHistoryTimelineYearMonthDayDirectionInstitutionGet**](FrontendApi.md#frontendUserHistoryTimelineYearMonthDayDirectionInstitutionGet) | **GET** /frontend/user/history/timeline/{year}/{month}/{day}/{direction}/{institution} | 
[**frontendUserIssuePut**](FrontendApi.md#frontendUserIssuePut) | **PUT** /frontend/user/issue | 
[**frontendUserNotificationPost**](FrontendApi.md#frontendUserNotificationPost) | **POST** /frontend/user/notification | 
[**frontendUserNotificationsGet**](FrontendApi.md#frontendUserNotificationsGet) | **GET** /frontend/user/notifications | 
[**frontendUserProcuratoryPut**](FrontendApi.md#frontendUserProcuratoryPut) | **PUT** /frontend/user/procuratory | 
[**frontendUserWorkspaceGet**](FrontendApi.md#frontendUserWorkspaceGet) | **GET** /frontend/user/workspace | 



## frontendInitGet

> ResponseInit frontendInitGet(opts)



BCorrespondent api

### Example

```javascript
import BCorrespondentTagCommitBb65df7 from 'b_correspondent__tag_____commit__bb65df7';

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let opts = {
  'token': "token_example" // String | 
};
apiInstance.frontendInitGet(opts).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **token** | **String**|  | [optional] 

### Return type

[**ResponseInit**](ResponseInit.md)

### Authorization

No authorization required

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserBalancedBookGet

> ResponseBalancedBook frontendUserBalancedBookGet()



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
apiInstance.frontendUserBalancedBookGet().then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters

This endpoint does not need any parameter.

### Return type

[**ResponseBalancedBook**](ResponseBalancedBook.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserBalancedBookYearMonthDayDirectionGet

> ResponseBalancedBook frontendUserBalancedBookYearMonthDayDirectionGet(year, month, day, direction)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let year = 789; // Number | 
let month = 789; // Number | 
let day = 789; // Number | 
let direction = "direction_example"; // String | 
apiInstance.frontendUserBalancedBookYearMonthDayDirectionGet(year, month, day, direction).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **year** | **Number**|  | 
 **month** | **Number**|  | 
 **day** | **Number**|  | 
 **direction** | **String**|  | 

### Return type

[**ResponseBalancedBook**](ResponseBalancedBook.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserDashboardInitGet

> ResponseInitDashboard frontendUserDashboardInitGet()



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
apiInstance.frontendUserDashboardInitGet().then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters

This endpoint does not need any parameter.

### Return type

[**ResponseInitDashboard**](ResponseInitDashboard.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserDashboardTimelineDirectionGet

> ResponseGapItemWrapper frontendUserDashboardTimelineDirectionGet(direction, point)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let direction = "direction_example"; // String | 
let point = "point_example"; // String | 
apiInstance.frontendUserDashboardTimelineDirectionGet(direction, point).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **direction** | **String**|  | 
 **point** | **String**|  | 

### Return type

[**ResponseGapItemWrapper**](ResponseGapItemWrapper.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserDashboardTimelineGapGet

> ResponseFetchGap frontendUserDashboardTimelineGapGet(from, to)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let from = "from_example"; // String | 
let to = "to_example"; // String | 
apiInstance.frontendUserDashboardTimelineGapGet(from, to).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **from** | **String**|  | 
 **to** | **String**|  | 

### Return type

[**ResponseFetchGap**](ResponseFetchGap.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserDashboardTimelineTransactionIdentGet

> ResponseTimelineTransactionResponse frontendUserDashboardTimelineTransactionIdentGet(ident)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let ident = 789; // Number | 
apiInstance.frontendUserDashboardTimelineTransactionIdentGet(ident).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **ident** | **Number**|  | 

### Return type

[**ResponseTimelineTransactionResponse**](ResponseTimelineTransactionResponse.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserHistoryTimelineGet

> ResponseHistoryTimeline frontendUserHistoryTimelineGet(date)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let date = "date_example"; // String | 
apiInstance.frontendUserHistoryTimelineGet(date).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **date** | **String**|  | 

### Return type

[**ResponseHistoryTimeline**](ResponseHistoryTimeline.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserHistoryTimelineYearMonthDayDirectionInstitutionGet

> ResponseGapItemWrapper frontendUserHistoryTimelineYearMonthDayDirectionInstitutionGet(year, month, day, direction, institution, hour)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let year = 56; // Number | 
let month = 56; // Number | 
let day = 56; // Number | 
let direction = "direction_example"; // String | 
let institution = 56; // Number | 
let hour = 56; // Number | 
apiInstance.frontendUserHistoryTimelineYearMonthDayDirectionInstitutionGet(year, month, day, direction, institution, hour).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **year** | **Number**|  | 
 **month** | **Number**|  | 
 **day** | **Number**|  | 
 **direction** | **String**|  | 
 **institution** | **Number**|  | 
 **hour** | **Number**|  | 

### Return type

[**ResponseGapItemWrapper**](ResponseGapItemWrapper.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserIssuePut

> Response frontendUserIssuePut(body)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let body = new BCorrespondentTagCommitBb65df7.Issue(); // Issue | 
apiInstance.frontendUserIssuePut(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Issue**](Issue.md)|  | 

### Return type

[**Response**](Response.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## frontendUserNotificationPost

> Response frontendUserNotificationPost(body)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let body = [null]; // [Number] | 
apiInstance.frontendUserNotificationPost(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[Number]**](Number.md)|  | 

### Return type

[**Response**](Response.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## frontendUserNotificationsGet

> ResponseNotifications frontendUserNotificationsGet()



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
apiInstance.frontendUserNotificationsGet().then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters

This endpoint does not need any parameter.

### Return type

[**ResponseNotifications**](ResponseNotifications.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## frontendUserProcuratoryPut

> Response frontendUserProcuratoryPut(body)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
let body = new BCorrespondentTagCommitBb65df7.ProcuratoryRequest(); // ProcuratoryRequest | 
apiInstance.frontendUserProcuratoryPut(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**ProcuratoryRequest**](ProcuratoryRequest.md)|  | 

### Return type

[**Response**](Response.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## frontendUserWorkspaceGet

> ResponseWorkspace frontendUserWorkspaceGet()



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

let apiInstance = new BCorrespondentTagCommitBb65df7.FrontendApi();
apiInstance.frontendUserWorkspaceGet().then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters

This endpoint does not need any parameter.

### Return type

[**ResponseWorkspace**](ResponseWorkspace.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8

