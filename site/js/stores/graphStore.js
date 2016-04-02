var Fluxxor = require('fluxxor');
var actions = require('../actions/graphActions.js');
var constants = require('../constants/graphConstants.js')

var GraphStore = Fluxxor.createStore({
  initialize: function() {
    this.loading = false;
    this.error = null;
    this.graphData = {};

    this.bindActions(
      constants.LOAD_GRAPH, this.onLoadGraph,
      constants.LOAD_GRAPH_SUCCESS, this.onLoadGraphSuccess,
      constants.LOAD_GRAPH_FAIL, this.onLoadGraphFail
    );
  },

  onLoadGraph: function() {
    this.loading = true;
    this.emit("change");
  },

  onLoadGraphSuccess: function(payload) {
    console.log(payload)
    var person = payload[0]._person
    var options = {
      legend: 'top'
    };
    var columns = [
      { 'type': 'string',
        'label' : 'truthiness'
      },
      { 'type' : 'number',
        'label' : 'Amount'
      }
      ];
    var rows = payload.map(function(stat) {
      return [stat.truthValue, stat.total]
    })

    this.loading = false;
    this.error = null;
    this.graphData = {
      rows    : rows,
      columns : columns,
      options : options,
    }
    this.emit("change");
  },

  onLoadGraphFail: function(payload) {
    this.loading = false;
    this.error = payload.error;
    this.emit("change");
  },
});
module.exports = new GraphStore();
