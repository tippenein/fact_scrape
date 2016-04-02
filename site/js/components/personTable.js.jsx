'use strict';
var React = require('react');
var ReactDOM = require('react-dom');

var PersonTable = React.createClass({
  render: function() {
    return (
      <div className="col-sm-6 person-table">
        {this.props.persons.map(function(person, i) {
          return (
            <div className={person === this.props.selected ? 'person active' : 'person'}
              onClick={this.props.onClick}
              key={i}>{person}</div>
          );
        }, this)}
      </div>
    );
  }
});

module.exports = PersonTable;
