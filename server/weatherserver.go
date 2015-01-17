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
)


var crcTable = crc64.MakeTable(crc64.ECMA)
//var tsidCache map[string]int64

type Server struct {
	storageFactory *StorageFactory
}

var server *Server

func init() {
//	tsidCache = make(map[string]int64)
	server = &Server{
		storageFactory: &StorageFactory{},
	}

	http.HandleFunc("/statusz", server.handleStatus)

	// V2 (in progress) handlers
	http.HandleFunc("/report", server.handleSimpleReport)
	http.HandleFunc("/", server.handleDashboardV2)
	http.HandleFunc("/query", server.handleQuery)
	http.HandleFunc("/v3", server.handleDashboardV3)
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
	DbConnectUsec int64 `json:"connTimeUsec"`
	DbQueryUsec int64 `json:"queryTimeUsec"`
}

type JsonDataPoint struct {
	Timestamp int64 `json:"ts"`
	Value int64 `json:"val"`
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
func (s *Server) handleSimpleReport(resp http.ResponseWriter, req *http.Request) {
	storage, err := s.storageFactory.NewStorage(req)

	if err != nil {
		onError("storage", err, resp)
		return
	}

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

	log.Println("Storing: " + point.DebugString())
	err = storage.Put(point)

	if err != nil {
		log.Println("Error storing point: %v", err)
		resp.Write([]byte(err.Error()))
	} else {
		resp.Write([]byte("ok"))
	}
}

func (s *Server) handleDashboardV3(resp http.ResponseWriter, req *http.Request) {
	t, err := template.ParseFiles("templates/dashboard_v3.html")
	if err != nil {
		onError("parsing template", err, resp)
	}

	t.Execute(resp, nil)
}

func (s *Server) handleDashboardV2(resp http.ResponseWriter, req *http.Request) {
	t, err := template.ParseFiles("templates/dashboard.html")
	if err != nil {
		onError("parsing template", err, resp)
	}

	t.Execute(resp, nil)
}

func (s *Server) handleQuery(resp http.ResponseWriter, req *http.Request) {
	ctx := appengine.NewContext(req)
	storage, err := s.storageFactory.NewStorage(req)
	if err != nil {
		ctx.Errorf("Error loading storage: %v", err)
		return
	}

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
	
	timeseriesName := req.FormValue("tsname")
	scanStart := time.Now()
	points, errors := storage.Scan(
		time.Now().Add(-1 * windowSize),
		time.Now(),
		timeseriesName)
	scanEnd := time.Now()

	series := JsonDataSeries{
		Points: make([]JsonDataPoint, 0),
	}

	done := false
	for !done {
		select {
		case dp, ok := <-points:
			if (!ok) {
				// channel is closed
				done = true
				break
			}

			jdp := JsonDataPoint{
				Timestamp: dp.Timestamp.Unix(),
				Value: dp.Value,
				ReporterId: dp.ReporterId,
			}
			series.Points = append(series.Points, jdp)
		case err, ok := <- errors:
			if !ok {
				done = true
				break
			}

			ctx.Errorf("Error fetching readings: %v", err)
			return
		}
	} 

	processingEnd := time.Now()

	series.DbConnectUsec = scanEnd.Sub(scanStart).Nanoseconds() / 1000
	series.DbQueryUsec = processingEnd.Sub(scanEnd).Nanoseconds() / 1000
	// TODO(mrjones): time json marshalling

	b, err := json.Marshal(series)
	if err != nil {
		onError("making json", err, resp)
	}

	resp.Write(b);
}

func (s *Server) handleStatus(resp http.ResponseWriter, req *http.Request) {
	for k,vs := range(req.Header) {
		log.Printf("%s=%vs\n", k, vs)
	}
	fmt.Fprintf(resp, "weatherserver ok")
}
