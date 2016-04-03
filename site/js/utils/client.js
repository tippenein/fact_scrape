'use strict'

var get = function(url, success) {
  $.ajax({
    type: "GET",
    url: url,
    dataType: 'json',
    success: success
  })
}

var statementSource = "http://localhost:8081/persons/truthiness?person_name=";
var TruthClient = {
  load: function(person, success, failure) {
    var uri = statementSource + person;
    console.log("querying " + uri)
    get(uri, function (result) {
      success(result);
    })
  },
};

module.exports = TruthClient;
