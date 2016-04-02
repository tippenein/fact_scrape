var constants = require('../constants/graphConstants.js');
var TruthClient = require('../utils/client.js');

module.exports = {
  loadGraph: function(person) {
    this.dispatch(constants.LOAD_GRAPH, person);

    TruthClient.load(person, function(payload) {
      this.dispatch(constants.LOAD_GRAPH_SUCCESS, payload);
    }.bind(this), function(error) {
      this.dispatch(constants.LOAD_GRAPH_FAIL, {error: error});
    }.bind(this));
  },
};
