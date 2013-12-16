package weatherserver

import (
	"errors"
	"fmt"
	"log"
	"net/http"
	"strconv"
	"time"

	"appengine"
	"appengine/datastore"
)

func init() {
	http.HandleFunc("/upload", handleUpload)
	http.HandleFunc("/latest", handleLatest)
	http.HandleFunc("/statusz", handleStatus)
	http.HandleFunc("/", handleDashboard)
}

func handleDashboard(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)
	q := datastore.NewQuery("reading").Order("Timestamp");
	result := q.Run(ctx)

	table := ""

	for {
		var reading Reading
		_, err := result.Next(&reading)

		if err == datastore.Done {
			break
		}

		if err != nil {
			ctx.Errorf("Error fetching readings")
			break
		}

		table = fmt.Sprintf("%s table.addRow([new Date(%d), %f]);", table, reading.Timestamp.Unix() * 1000, reading.TemperatureF)
	}

	fmt.Fprintf(resp,
		"<html>" +
		" <head>" +
		"  <script type='text/javascript' src='https://www.google.com/jsapi'></script>" +
		"  <script type='text/javascript'>" +
		"   google.load('visualization', '1.0', {'packages':['corechart']});" +
		"   google.setOnLoadCallback(drawChart);" +
		"   function drawChart() {" +
		"    var table = new google.visualization.DataTable();" +
		"    table.addColumn('datetime', 'Time');" +
		"    table.addColumn('number', 'Temperature'); " +
		table +
		"    var options;" +
		"    var chart = new google.visualization.LineChart(" +
		"      document.getElementById('chart_div'));"+
		"    chart.draw(table, options);" +
		"   }" +
		"  </script>" +
		" <head>" +
		" <body>" +
		"  <div id='chart_div'></div>" +
		" </body>" +
		"</html>");
}

func handleStatus(resp http.ResponseWriter, req *http.Request) {
	for k,vs := range(req.Header) {
		log.Printf("%s=%vs\n", k, vs)
	}
	fmt.Fprintf(resp, "weatherserver ok")
}

type Reading struct {
	TemperatureF float64
	RelativeHumidity float64
	Timestamp time.Time
}

func handleLatest(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)

	q := datastore.NewQuery("reading").Order("-Timestamp").Limit(1)
	var readings []Reading
	if _, err := q.GetAll(ctx, &readings); err != nil {
		http.Error(resp, err.Error(), http.StatusInternalServerError)
		return
	}
	

	fmt.Fprintf(resp, "latest t_f=%f r_h=%f @%s", readings[0].TemperatureF, readings[0].RelativeHumidity, readings[0].Timestamp);
}

func parseTemperature(t_f_str string) (float64, error) {
	if t_f_str == "" {
		return -1.0, errors.New("Missing parameter t_f")
	}

	return strconv.ParseFloat(t_f_str, 64)
}

func parseHumidity(r_h_str string) (float64, error) {
	if r_h_str == "" {
		return -1.0, errors.New("Missing parameter r_h")
	}

	return strconv.ParseFloat(r_h_str, 64)
}

func handleUpload(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)

	t_f_str := req.FormValue("t_f")
	r_h_str := req.FormValue("r_h")

	t_f, err := parseTemperature(t_f_str)
	if err != nil {
		fmt.Fprintf(resp, "Could not parse temperature: %s", err)
		return
	}

	r_h, err := parseHumidity(r_h_str)
	if err != nil {
		fmt.Fprintf(resp, "Could not parse humidity: %s.", err)
		return
	}

	timestamp := time.Now()
	reading := Reading{TemperatureF: t_f, RelativeHumidity: r_h, Timestamp: timestamp}

	_, err = datastore.Put(
		ctx, 
		datastore.NewKey(ctx, "reading", "", timestamp.UnixNano(), nil),
		&reading)

	if err != nil {
		fmt.Fprintf(resp, "Error persisting reading: %s.", err)
		return
	}

	fmt.Fprintf(resp, "saved t_f=%f r_h=%f @%s", reading.TemperatureF, reading.RelativeHumidity, reading.Timestamp);
}
