dslr.service('backendService', function($http) {
  return ({
getApiAllByUserId: function(userId)
{
  return $http(
    { url: '/api/all/' + encodeURIComponent(userId) + ''
    , method: 'GET'
    });
}
,
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
getApiSingleByUserIdByFrameListID: function(userId, frameListID)
{
  return $http(
    { url: '/api/single/' + encodeURIComponent(userId) + '/' + encodeURIComponent(frameListID) + ''
    , method: 'GET'
    });
}
,
postApiNewByUserId: function(userId, body)
{
  return $http(
    { url: '/api/new/' + encodeURIComponent(userId) + ''
    , data: JSON.stringify(body)
    , contentType: 'application/json'
    , method: 'POST'
    });
}
});
});
