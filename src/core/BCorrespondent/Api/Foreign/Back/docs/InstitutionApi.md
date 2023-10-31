# BCorrespondentTagCommitBb65df7.InstitutionApi

All URIs are relative to *http://api.b-correspondent.app*

Method | HTTP request | Description
------------- | ------------- | -------------
[**institutionFiatTransactionOrderPost**](InstitutionApi.md#institutionFiatTransactionOrderPost) | **POST** /institution/fiat/transaction/order | 
[**institutionFiatWithdrawHistoryPageGet**](InstitutionApi.md#institutionFiatWithdrawHistoryPageGet) | **GET** /institution/fiat/withdraw/history/page | 
[**institutionFiatWithdrawInitGet**](InstitutionApi.md#institutionFiatWithdrawInitGet) | **GET** /institution/fiat/withdraw/init | 
[**institutionFiatWithdrawPut**](InstitutionApi.md#institutionFiatWithdrawPut) | **PUT** /institution/fiat/withdraw | 
[**institutionInvoiceRegisterPut**](InstitutionApi.md#institutionInvoiceRegisterPut) | **PUT** /institution/invoice/register | 



## institutionFiatTransactionOrderPost

> Response institutionFiatTransactionOrderPost()



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

let apiInstance = new BCorrespondentTagCommitBb65df7.InstitutionApi();
apiInstance.institutionFiatTransactionOrderPost().then((data) => {
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


## institutionFiatWithdrawHistoryPageGet

> ResponseMaybeWithdrawalHistory institutionFiatWithdrawHistoryPageGet(page)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.InstitutionApi();
let page = 56; // Number | 
apiInstance.institutionFiatWithdrawHistoryPageGet(page).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **page** | **Number**|  | 

### Return type

[**ResponseMaybeWithdrawalHistory**](ResponseMaybeWithdrawalHistory.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## institutionFiatWithdrawInitGet

> ResponseInitWithdrawal institutionFiatWithdrawInitGet()



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

let apiInstance = new BCorrespondentTagCommitBb65df7.InstitutionApi();
apiInstance.institutionFiatWithdrawInitGet().then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters

This endpoint does not need any parameter.

### Return type

[**ResponseInitWithdrawal**](ResponseInitWithdrawal.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: Not defined
- **Accept**: application/json;charset=utf-8


## institutionFiatWithdrawPut

> ResponseWithdrawResult institutionFiatWithdrawPut(body)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.InstitutionApi();
let body = new BCorrespondentTagCommitBb65df7.Withdraw(); // Withdraw | 
apiInstance.institutionFiatWithdrawPut(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**Withdraw**](Withdraw.md)|  | 

### Return type

[**ResponseWithdrawResult**](ResponseWithdrawResult.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8


## institutionInvoiceRegisterPut

> ResponseInvoiceRegisterResponse institutionInvoiceRegisterPut(body)



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

let apiInstance = new BCorrespondentTagCommitBb65df7.InstitutionApi();
let body = [new BCorrespondentTagCommitBb65df7.InvoiceRegisterRequest()]; // [InvoiceRegisterRequest] | 
apiInstance.institutionInvoiceRegisterPut(body).then((data) => {
  console.log('API called successfully. Returned data: ' + data);
}, (error) => {
  console.error(error);
});

```

### Parameters


Name | Type | Description  | Notes
------------- | ------------- | ------------- | -------------
 **body** | [**[InvoiceRegisterRequest]**](InvoiceRegisterRequest.md)|  | 

### Return type

[**ResponseInvoiceRegisterResponse**](ResponseInvoiceRegisterResponse.md)

### Authorization

[JwtSecurity](../README.md#JwtSecurity)

### HTTP request headers

- **Content-Type**: application/json;charset=utf-8
- **Accept**: application/json;charset=utf-8

