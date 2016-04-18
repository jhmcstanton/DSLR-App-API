dslr.service('backendService', function($http) {
  return ({
postApiUserNew: function(body)
{
  return $http(
    { url: '/api/user/new'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , method: 'POST'
    });
}
,
getApiAll: function(headertokenauth)
{
  return $http(
    { url: '/api/all'
    , headers: { "token-auth": headertokenauth }
    , method: 'GET'
    });
}
,
getApiSingleByFrameListID: function(frameListID, headertokenauth)
{
  return $http(
    { url: '/api/single/' + encodeURIComponent(frameListID) + ''
    , headers: { "token-auth": headertokenauth }
    , method: 'GET'
    });
}
,
postApiNew: function(body, headertokenauth)
{
  return $http(
    { url: '/api/new'
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , headers: { "token-auth": headertokenauth }
    , method: 'POST'
    });
}
});
});
