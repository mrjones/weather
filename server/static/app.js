
function init() {
    google.load('visualization', '1.0', {'packages':['corechart']});

    renderTimeseries('es.mrjon.temperatureFMillis', 'temps_div');
    renderTimeseries('es.mrjon.relativeHumidityMillis', 'humid_div');
}

function renderTimeseries(seriesName, targetDivName) {
  var params = window.location.search.replace("\?", "&");

  $.getJSON( "/query?tsname=" + seriesName + params, function( data ) {
      var jsStart = Date.now();
      var options;

      var table = new google.visualization.DataTable();

      var ridToColumn = {};
      var allRids = [];

      for (var i = 0; i < data.points.length; i++) {
          var rid = data.points[i].rid;
          if (!ridToColumn[rid]) {
              ridToColumn[rid] = 1 + allRids.length;
              allRids.push(rid)
          }
      }

      table.addRows(data.points.length);
      table.addColumn('datetime', 'Time');
      for (var i = 0; i < allRids.length; i++) {
          table.addColumn('number', 'Reporter ' + allRids[i]);
      }

      for (var i = 0; i < data.points.length; i++) {
          table.setCell(i, 0, new Date(1000 * data.points[i].ts));
          var rid = data.points[i].rid;
          var val = data.points[i].val;
          table.setCell(i, ridToColumn[rid], (1.0 * val) / 1000);
      }

      var renderStart = Date.now();
      var chart = new google.visualization.LineChart(
          document.getElementById(targetDivName));
      chart.draw(table, options);
      var renderEnd = Date.now();

      $("#debug").append("<div>DBConnect: " + data.connTimeUsec +
                         "us - DBQuery: " + data.queryTimeUsec +
                         "us - Render: " + 1000 * (renderEnd - renderStart) +
                         "us - All JS: " + 1000 * (renderEnd - jsStart) +
                         "us</div>");

  });
}


//------

function init_v3() {
  renderTimeseriesV3('es.mrjon.temperatureFMillis', 'temps_div', 'Temperature (F)');
  renderTimeseriesV3('es.mrjon.relativeHumidityMillis', 'humid_div', 'Relative Humidity (%)');
}

function renderTimeseriesV3(seriesName, targetDivName, yAxisLabel) {
  var params = window.location.search.replace("\?", "&");

  $.getJSON( "/query?tsname=" + seriesName + params, function( data ) {
      var jsStart = Date.now();
      var ridToColumn = {};
      var allRids = [];

      var chartData = [];

      for (var i = 0; i < data.points.length; i++) {
          var rid = data.points[i].rid;
          if (!ridToColumn[rid]) {
              ridToColumn[rid] = 1 + allRids.length;
              allRids.push(rid)
          }
      }

      for (var i = 0; i < data.points.length; i++) {
          var row = new Array(allRids.length + 1);
          var rid = data.points[i].rid;
          var val = data.points[i].val;
          var ts = data.points[i].ts;

          row[0] = new Date(1000 * ts);
          for (var c = 0; c < allRids.length; c++) {
              row[c+1] = null;   
          }

          row[ridToColumn[rid]] = (1.0 * val) / 1000;
          chartData.push(row);
      }

      var labels = ["Time"];
      for (var i = 0; i < allRids.length; i++) {
          labels.push("Reporter_" + allRids[i]);
      }

      var renderHour = function(h) {
          modH = h % 12;
          if (modH == 0) {
              return 12;
          }
          return modH;
      }

      var renderDate = function(d, gran) {
          return renderHour(d.getHours())
              + ":"
              + Dygraph.zeropad(d.getMinutes())
              + (d.getHours() >= 12 ? "PM" : "AM");
      };

      var renderStart = Date.now();
      new Dygraph(document.getElementById(targetDivName),
                  chartData,
                  {
                      labels: labels,
                      width: 800,
                      height: 200,
                      drawPoints: false,
                      strokeWidth: 2,
                      legend: 'always',
                      connectSeparatedPoints: true,
//                      pointSize: 1.5,
                      colors: [ '#F15854', '#5DA5DA' ],
                      ylabel: yAxisLabel,
                      axisLabelWidth: 60,
                      axisLineColor: '#666666',
                      axisLabelColor: '#666666',
                      axisLineWidth: 1.5,
                      interactionModel: {},
                      gridLineColor: '#CCCCCC',
                      axes: {
                          x: {
                              axisLabelFormatter: renderDate,
                          },
                      }
                  });
      var renderEnd = Date.now();

      $("#debug").append("<div>DBConnect: " + data.connTimeUsec +
                         "us - DBQuery: " + data.queryTimeUsec +
                         "us - Render: " + 1000 * (renderEnd - renderStart) +
                         "us - All JS: " + 1000 * (renderEnd - jsStart) +
                         "us</div>");

  });
}
