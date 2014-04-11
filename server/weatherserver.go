package weatherserver

import (
	"encoding/json"
	"errors"
	"fmt"
	"hash/crc64"
	"html/template"
	"io"
	"log"
	"net/http"
	"strconv"
	"time"

	"appengine"
	"appengine/datastore"
)


var crcTable = crc64.MakeTable(crc64.ECMA)
//var tsidCache map[string]int64

func init() {
//	tsidCache = make(map[string]int64)

	http.HandleFunc("/statusz", handleStatus)

	// V1 (deprecated) handlers
	http.HandleFunc("/upload", handleUpload)
	http.HandleFunc("/latest", handleLatest)
	http.HandleFunc("/", handleDashboard)

	// V2 (in progress) handlers
	http.HandleFunc("/v2/simplereport", handleSimpleReport)
	http.HandleFunc("/v2/latest", handleLatestV2)
	http.HandleFunc("/v2/dashboard", handleDashboardV2)
	http.HandleFunc("/v2/query", handleQuery)
}

type DataPoint struct {
	TimeseriesId int64
	Timestamp time.Time
	Value int64
	ReporterId int64
}



// For JSON
type JsonDataSeries struct {
	Points []JsonDataPoint `json:"points"`
}

type JsonDataPoint struct {
	Timestamp int64 `json:"ts"`
	Value int64 `json:"v"`
	ReporterId int64 `json:"rid"`
}

func (d *DataPoint) DebugString() string {
	return fmt.Sprintf(
		"DataPoint <\n" +
			"  TimeseriesId: %d\n" +
			"  Timestamp: %d\n" +
			"  Value: %d\n" +
			"  ReporterId: %d\n" +
			">",
		d.TimeseriesId,
		d.Timestamp.UnixNano(),
		d.Value,
		d.ReporterId);
}

func onError(context string, err error, resp http.ResponseWriter) {
	log.Printf("%s: %s\n", context, err.Error())
	resp.WriteHeader(http.StatusInternalServerError)
	resp.Write([]byte(
		fmt.Sprintf("%s: %s\n", context, err.Error())))
}

func tsid(name string) int64 {
	crc := crc64.New(crcTable)
	io.WriteString(crc, name)
	return int64(crc.Sum64())
}

// /simplereport?t_sec=1234567890&v=42&tsname=es.mrjon.metric&rid=2222
func handleSimpleReport(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)

	point := &DataPoint{}

	timeseriesName := req.FormValue("tsname")
	if timeseriesName == "" {
		onError("tsname", fmt.Errorf("missing tsname"), resp)
		return
	}

	timeseriesId := tsid(timeseriesName)

	timestampSec, err := strconv.ParseInt(req.FormValue("t_sec"), 10, 64)
	if err != nil {
		onError("t_sec", err, resp)
		return
	}

	reporterId, err := strconv.ParseInt(req.FormValue("rid"), 10, 64)
	if err != nil {
		onError("rid", err, resp)
		return
	}

	value, err := strconv.ParseInt(req.FormValue("v"), 10, 64)
	if err != nil {
		onError("v", err, resp)
		return
	}

	point = &DataPoint{
		TimeseriesId: timeseriesId,
		Timestamp: time.Unix(timestampSec, 0),
		ReporterId: reporterId,
		Value: value,
	}

	key := fmt.Sprintf("%lld-%lld-%s",
		point.TimeseriesId,
		point.ReporterId,
		point.Timestamp.UnixNano())

	_, err = datastore.Put(
		ctx,
		datastore.NewKey(ctx, "datapoint", key, 0, nil),
		point)

	log.Println(point.DebugString())

	resp.Write([]byte("ok"))
}

func handleLatestV2(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)

	q := datastore.NewQuery("datapoint").Order("-Timestamp").Limit(1)
	var datapoints []DataPoint
	if _, err := q.GetAll(ctx, &datapoints); err != nil {
		onError("fetch", err, resp)
		return
	}
	log.Println(datapoints[0].DebugString())
	resp.Write([]byte(datapoints[0].DebugString()))
}


func handleDashboardV2(resp http.ResponseWriter, req *http.Request) {
//	ctx := appengine.NewContext(req)

	t, err := template.ParseFiles("templates/dashboard.html")
	if err != nil {
		onError("parsing template", err, resp)
	}

	t.Execute(resp, nil)
}

func handleQuery(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)

	q := datastore.NewQuery("datapoint").Order("Timestamp").Filter("Timestamp >", time.Now().Add(-24 * time.Hour))

	series := &JsonDataSeries{
		Points: make([]JsonDataPoint, 0),
	}

	timeseriesName := req.FormValue("tsname")
	if timeseriesName != "" {
		timeseriesId := tsid(timeseriesName)
		q = q.Filter("TimeseriesId =", timeseriesId)
	}


	result := q.Run(ctx)

	for {
		var dp DataPoint
		_, err := result.Next(&dp)

		if err == datastore.Done {
			break
		}

		if err != nil {
			ctx.Errorf("Error fetching readings")
			break
		}

		jdp := JsonDataPoint{
			Timestamp: dp.Timestamp.Unix(),
			Value: dp.Value,
			ReporterId: dp.ReporterId,
		}

		series.Points = append(series.Points, jdp);

//		resp.Write([]byte(dp.DebugString()))
//		resp.Write([]byte("\n"))
	} 

	b, err := json.Marshal(series)
	if err != nil {
		onError("making json", err, resp)
	}

	resp.Write(b);
}

func handleDashboard(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)
	q := datastore.NewQuery("reading").Order("Timestamp").Filter("Timestamp >", time.Now().Add(-24 * time.Hour))
	result := q.Run(ctx)

	temps := ""
	humid := ""

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
 
		temps = fmt.Sprintf("%s temps.addRow([new Date(%d), %f]);", temps, reading.Timestamp.Unix() * 1000, reading.TemperatureF)
		humid = fmt.Sprintf("%s humid.addRow([new Date(%d), %f]);", humid, reading.Timestamp.Unix() * 1000, reading.RelativeHumidity)
	}

	fmt.Fprintf(resp,
		"<html>" +
		" <head>" +
		"  <script type='text/javascript' src='https://www.google.com/jsapi'></script>" +
		"  <script type='text/javascript'>" +
//		"   setTimeout('location.reload(true);', 5 * 60 * 1000);"+
		"   google.load('visualization', '1.0', {'packages':['corechart']});" +
		"   google.setOnLoadCallback(drawChart);" +
		"   function drawChart() {" +
		"    var temps = new google.visualization.DataTable();" +
		"    temps.addColumn('datetime', 'Time');" +
		"    temps.addColumn('number', 'Temperature'); " +
		temps +
		"    var humid = new google.visualization.DataTable();" +
		"    humid.addColumn('datetime', 'Time');" +
		"    humid.addColumn('number', 'Humidity'); " +
		humid +
		"    var options;" +
		"    var temps_chart = new google.visualization.LineChart(" +
		"      document.getElementById('temps_div'));"+
		"    var humid_chart = new google.visualization.LineChart(" +
		"      document.getElementById('humid_div'));"+
		"    temps_chart.draw(temps, options);" +
		"    humid_chart.draw(humid, options);" +
		"   }" +
		"  </script>" +
		" <head>" +
		" <body>" +
		"  <div id='temps_div'></div>" +
		"  <div id='humid_div'></div>" +
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
