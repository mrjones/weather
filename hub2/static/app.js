google.load('visualization', '1.0', {'packages':['corechart']});

function init() {
  renderTimeseries('es.mrjon.temperatureFMillis', 'temps_div');
  renderTimeseries('es.mrjon.relativeHumidityMillis', 'humid_div');
}

function renderTimeseries(seriesName, targetDivName) {
  var params = window.location.search.replace("\?", "&");

  $.getJSON( "/query?tsname=" + seriesName + params, function( data ) {
      var options;

      var table = new google.visualization.DataTable();

      var ridToColumn = {};
      var allRids = [];

      for (var i = 0; i < data.length; i++) {
          alert(data[i]);
          var rid = data[i].rid;
          if (!ridToColumn[rid]) {
              ridToColumn[rid] = 1 + allRids.length;
              allRids.push(rid)
          }
      }

      table.addRows(data.length);
      table.addColumn('datetime', 'Time');
      for (var i = 0; i < allRids.length; i++) {
          table.addColumn('number', 'Reporter ' + allRids[i]);
      }

      for (var i = 0; i < data.length; i++) {
          table.setCell(i, 0, new Date(1000 * data[i].ts));
          table.setCell(
          i, ridToColumn[data[i].rid], (1.0 * data[i].val) / 1000);
      }

      var chart = new google.visualization.LineChart(
          document.getElementById(targetDivName));
      chart.draw(table, options);
  });
}
