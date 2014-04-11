google.load('visualization', '1.0', {'packages':['corechart']});

function init() {
  renderTimeseries('es.mrjon.temperatureFMillis', 'temps_div');
  renderTimeseries('es.mrjon.relativeHumidityMillis', 'humid_div');
}

function renderTimeseries(seriesName, targetDivName) {
  $.getJSON( "/v2/query?tsname=" + seriesName, function( data ) {
    var options;

    var table = new google.visualization.DataTable();

    var ridToColumn = {};
    var allRids = [];

    for (var i = 0; i < data.points.length; i++) {
      for (rid in data.points[i].vals) {
        if (!ridToColumn[rid]) {
          ridToColumn[rid] = 1 + allRids.length;
          allRids.push(rid)
        }
      }
    }

    table.addRows(data.points.length);
    table.addColumn('datetime', 'Time');
    for (var i = 0; i < allRids.length; i++) {
      table.addColumn('number', 'Reporter ' + allRids[i]);
    }

    for (var i = 0; i < data.points.length; i++) {
      table.setCell(i, 0, new Date(1000 * data.points[i].ts));
      for (var rid in data.points[i].vals) {
        table.setCell(
          i, ridToColumn[rid], (1.0 * data.points[i].vals[rid]) / 1000);
      }
    }

    var chart = new google.visualization.LineChart(
      document.getElementById(targetDivName));
    chart.draw(table, options);
  });
}
