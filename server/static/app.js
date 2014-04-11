google.load('visualization', '1.0', {'packages':['corechart']});


function renderTimeseries() {
  $.getJSON( "/v2/query", function( data ) {
    var options;

    var table = new google.visualization.DataTable();
    table.addColumn('datetime', 'Time');
    table.addColumn('number', 'Value');

    for (var i = 0; i < data.points.length; i++) {
      table.addRow( [ new Date(data.points[i].ts * 1000), data.points[i].v ] );
    }

    var chart = new google.visualization.LineChart(
      document.getElementById('temps_div'));
    chart.draw(table, options);
  });
}
