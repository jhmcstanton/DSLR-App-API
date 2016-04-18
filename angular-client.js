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
});
});
