package weatherserver

import (
	"encoding/json"
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
	Values map[string]int64 `json:"vals"`
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

	windowSize := 24 * time.Hour
	windowSizeSecsStr := req.FormValue("secs")
	if windowSizeSecsStr != "" {
		windowSizeSecs, err := strconv.Atoi(windowSizeSecsStr)
		if err != nil {
			ctx.Errorf("Error parsing '%s': %v", windowSizeSecsStr, err)
		} else {
			windowSize = time.Duration(windowSizeSecs) * time.Second
		}
	}

	q := datastore.NewQuery("datapoint").Order("Timestamp").Filter("Timestamp >", time.Now().Add(-1 * windowSize))


	timeseriesName := req.FormValue("tsname")
	if timeseriesName != "" {
		timeseriesId := tsid(timeseriesName)
		q = q.Filter("TimeseriesId =", timeseriesId)
	}

	series := JsonDataSeries{
		Points: make([]JsonDataPoint, 0),
	}

	result := q.Run(ctx)

	initted := false
	jdp := JsonDataPoint{}
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

		if !initted || jdp.Timestamp != dp.Timestamp.Unix() {
			initted = true
			jdp = JsonDataPoint{
				Timestamp: dp.Timestamp.Unix(),
				Values: make(map[string]int64),
			}
			series.Points = append(series.Points, jdp)
		}

		jdp.Values[strconv.FormatInt(dp.ReporterId, 10)] = dp.Value
	} 


	b, err := json.Marshal(series)
	if err != nil {
		onError("making json", err, resp)
	}

	resp.Write(b);
}

func handleStatus(resp http.ResponseWriter, req *http.Request) {
	for k,vs := range(req.Header) {
		log.Printf("%s=%vs\n", k, vs)
	}
	fmt.Fprintf(resp, "weatherserver ok")
}
