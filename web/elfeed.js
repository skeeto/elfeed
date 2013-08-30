var INITIAL_QUERY = '@2days';

function favicon(url) {
    return URI(url).path('favicon.ico').search('')
        .toString().replace(/\?$/, '');
}

function entryFill(entry) {
    entry.favicon = favicon(entry.link);
    var date = new Date(entry.date);
    entry.dateString = [
        1900 + date.getYear(),
        1 + date.getMonth(),
        date.getDate()
    ].join('-');
    entry.classes = entry.tags.map(function(tag) {
        return 'tag-' + tag;
    }).join(' ');
}

function SearchCtrl($scope, $http) {
    $scope.query = INITIAL_QUERY;
    $scope.busy = false;
    $scope.dirty = true;

    $scope.update = function() {
        if (!$scope.busy) {
            $scope.busy = true;
            $scope.dirty = false;
            $http.get(URI('/elfeed/search').search({
                q: $scope.query
            }).toString()).success(function(data) {
                data.forEach(entryFill);
                $scope.entries = data;
                $scope.busy = false;
                if ($scope.dirty) $scope.update();
            });
        } else {
            $scope.dirty = true;
        }
    };
    $scope.update();
}
