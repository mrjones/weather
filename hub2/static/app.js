function init() {
  renderTimeseries('es.mrjon.temperatureFMillis', 'temps_summary', 'temps_chart', 'Temperature (F)');
  renderTimeseries('es.mrjon.relativeHumidityMillis', 'humid_summary', 'humid_chart', 'Relative Humidity (%)');
//  renderTimeseries('es.mrjon.pressureMilliPascals', 'pressure_summary', 'pressure_chart', 'Pressure (Pa)');
}

function millisToFloat(v) {
    return 1.0 * v / 1000;
}



function colorize(reporterNames) {
    // https://www.google.com/design/spec/style/color.html#color-color-palette
    var anonymousPalette = [ '#4CAF50', '#FFC107', '#009688', '#9C27B0' ];

    var sortedNames = reporterNames.slice().sort();
    var paletteMap = {};

    var anonymousIdx = 0;
    for (var i = 0; i < sortedNames.length; i++) {
        if (sortedNames[i] == 'Bedroom') {
            paletteMap['Bedroom'] = '#F44336';
        } else if (sortedNames[i] == 'Living Room') {
            paletteMap['Living Room'] = '#2196F3';
        } else {
            paletteMap[sortedNames[i]] = anonymousPalette[anonymousIdx++];
        }
    }

    return paletteMap;
}

function format(value, seriesName) {
    if (seriesName == "es.mrjon.temperatureFMillis") {
        return value.toFixed(1) + "&deg;"
    } else if (seriesName == "es.mrjon.relativeHumidityMillis") {
        return value.toFixed(0) + "%";
    } else {
        return value.toFixed(1);
    }
}

function renderTimeseries(seriesName, summaryDivName, chartDivName, yAxisLabel) {
  var jsStart = Date.now();
  var params = window.location.search.replace("\?", "&");

  $.getJSON( "/query?tsname=" + seriesName + params, function( data ) {
      var gotResponse = Date.now();
      var ridToColumn = {};
      var allRids = [];

      var chartData = [];

      var allReporterNames = [];
      var ridToName = {};
      for (var i = 0; i < data.points.length; i++) {
          var rid = data.points[i].rid;
          if (!ridToColumn[rid]) {
              ridToColumn[rid] = 1 + allRids.length;
              allRids.push(rid)

              var reporterName = "Reporter_" + rid;
              if (data.reporterNames.hasOwnProperty(rid)) {
                  reporterName = data.reporterNames[rid];
              }
              ridToName[rid] = reporterName;
              allReporterNames.push(reporterName);
          }
      }
      var colorsByReporterName = colorize(allReporterNames);

      var latestByRid = {};

      for (var i = 0; i < data.points.length; i++) {
          var row = new Array(allRids.length + 1);
          var rid = data.points[i].rid;
          var val = data.points[i].val;
          var ts = data.points[i].ts;

          row[0] = new Date(1000 * ts);
          for (var c = 0; c < allRids.length; c++) {
              row[c+1] = null;   
          }

          row[ridToColumn[rid]] = millisToFloat(val);
          chartData.push(row);

          latestByRid[rid] = data.points[i];
      }

      var summaryDiv = $('#' + summaryDivName);
      var labels = ["Time"];
      summaryDiv.append("<div class='metric_name'>" + yAxisLabel + "</div>");
      for (var i = 0; i < allRids.length; i++) {
          var rid = allRids[i];
          var reporterName = ridToName[rid];
          labels.push(reporterName);

          var latestData = latestByRid[rid];
          summaryDiv.append(
              "<div class='stat' style='color: " +
                  colorsByReporterName[reporterName] + "'><div class='label'>" +
                  reporterName + "</div><div class='value'>" +
                  format(millisToFloat(latestData.val), seriesName) +
                  "</div></div>");
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

      var chartConfig = {
          labels: labels,
          width: 800,
          height: 200,
          drawPoints: false,
          strokeWidth: 2,
          legend: 'always',
          connectSeparatedPoints: true,
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
          },
          series: { },
      };

      for (var i = 0; i < allReporterNames.length; i++) {
          var name = allReporterNames[i];
          var color = colorsByReporterName[name];
          chartConfig.series[name] = { color: color };
      }

      new Dygraph(document.getElementById(chartDivName),
                  chartData, chartConfig);

      var renderEnd = Date.now();

      $("#debug").append("<div>" + seriesName +
                         " -- <b>Server</b> " +
                         "DBConnect: " + data.connTimeUsec +
                         "us, DBQuery: " + data.queryTimeUsec +
                         "us, Total: " + data.totalTimeUsec +
                         "us -- <b>Client</b> " +
                         "Wait: " + 1000 * (gotResponse - jsStart) +
                         "us, Render: " + 1000 * (renderEnd - renderStart) +
                         "us -- <b>Total</b>: " + 1000 * (renderEnd - jsStart) +
                         "us</div>");

  });
}
