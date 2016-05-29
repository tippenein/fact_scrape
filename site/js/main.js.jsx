'use strict';
var React = require('react');
var Fluxxor = require('fluxxor');
var ReactDOM = require('react-dom');
var _ = require('underscore');

var PersonTable = require('./components/personTable.js.jsx');
var TruthGraph = require('./components/truthGraph.js.jsx');
var graphStore = require('./stores/graphStore.js');
var graphActions = require('./actions/graphActions.js');

window.React = React;

var stores = {
  GraphStore: graphStore
};

var flux = new Fluxxor.Flux(stores, graphActions);

window.flux = flux;

flux.on("dispatch", function(type, payload) {
  if (console && console.log) {
    console.log("[Dispatch]", type, payload);
  }
});

var FluxMixin = Fluxxor.FluxMixin(React),
    StoreWatchMixin = Fluxxor.StoreWatchMixin;

var Application = React.createClass({
  mixins: [FluxMixin, StoreWatchMixin("GraphStore")],

  getInitialState: function() {
    return {selected: "", persons: []};
  },
  componentDidMount: function() {
    var uri = this.props.personSource
    this.serverRequest = $.get(uri, function (result) {
      var ps = result.map(function(obj) {return obj.name;});
      this.setState({
        persons: ps,
        selected: ps[0]
      });
      this.getFlux().actions.loadGraph(ps[0]);
    }.bind(this))
  },

  handlePersonTableClick: function(event) {
    var person = event.target.innerHTML
    console.log("you clicked " + person)
    this.setState({selected: person})
    this.getFlux().actions.loadGraph(person);
  },
  componentWillUnmount: function() {
    this.serverRequest.abort();
  },
  render: function() {
    return (
      <div>
        {this.state.error ? "Error loading data" : null}
        <PersonTable
          persons={this.state.persons}
          selected={this.state.selected}
          onClick={this.handlePersonTableClick}
        />
        <TruthGraph
          loading={this.state.loading}
          selected={this.state.selected}
          graphData={this.state.graphData}
          graphName="lie-graph"
        />
      </div>
    );
  },
  getStateFromFlux: function() {
    var store = this.getFlux().store("GraphStore");
    return {
      loading: store.loading,
      error: store.error,
      graphData: store.graphData
    };
  }
})

ReactDOM.render(
  <Application
    flux={flux}
    personSource="/truth/api/persons"
  />,
  document.getElementById('main')
);

