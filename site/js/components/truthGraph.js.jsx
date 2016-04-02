'use strict';
var React = require('react');
var ReactDOM = require('react-dom');
var Chart = require('react-google-charts').Chart;

var TruthGraph = React.createClass({
  render: function(){
    return (
      <div className="col-sm-6">
        <h2>Truth Breakdown for {this.props.selected}</h2>
        {this.props.loading ? <b>Loading...</b> : null}
        <Chart
          key={this.props.selected} chartType="PieChart"
          width={"100%"} height={"600px"}
          rows={this.props.graphData.rows} columns={this.props.graphData.columns}
          options={this.props.graphData.options} graph_id={this.props.graphName} legend_toggle={true}
        />
      </div>
    )
  }
})

module.exports = TruthGraph;
