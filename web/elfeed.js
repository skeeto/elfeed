var INITIAL_QUERY = '@2days';

function favicon(url) {
    return URI(url).path('favicon.ico').search('')
        .toString().replace(/\?$/, '');
}

function SearchCtrl($scope, $http) {
    $scope.query = INITIAL_QUERY;
    $scope.update = function() {
        $http.get(URI('/elfeed/search').search({
            q: $scope.query
        }).toString()).success(function(data) {
            data.map(function(entry) {
                entry.favicon = favicon(entry.link);
                var date = new Date(entry.date);
                entry.dateString = [
                    1900 + date.getYear(),
                    1 + date.getMonth(),
                    date.getDate()
                ].join('-');
            });
            $scope.entries = data;
        });
    };
    $scope.update();
}
