package weatherserver

import (
	"fmt"
	"net/http"
	"time"
	"database/sql"
	_ "github.com/ziutek/mymysql/godrv"

	"appengine"
	"appengine/datastore"
)

type Storage interface {
	Put(d *DataPoint) error

	Scan(start, end time.Time, seriesName string) (<-chan *DataPoint, <-chan error)
}

type StorageFactory struct {
	
}

func (f *StorageFactory) NewStorage(req *http.Request) (Storage, error) {
	return NewAppEngineStorage(req)
//	return NewMySqlStorage("test", "test")
}

// ===================================================================
// MySqlStorage
// ===================================================================
//
// mysql> CREATE TABLE history (value BIGINT NOT NULL, series_id BIGINT NOT NULL, timestamp DATETIME NOT NULL, reporter_id INT NOT NULL, PRIMARY KEY(series_id, timestamp, reporter_id));

type MySqlStorage struct {
	user string
	password string
}

func NewMySqlStorage(user, password string) (*MySqlStorage, error) {
	return &MySqlStorage{
		user: user,
		password: password,
	}, nil
}



func (s *MySqlStorage) Put(p *DataPoint) error {
//	conn, err := sql.Open("mymysql", database+"/"+user+"/"+password)
//	conn, err := sql.Open("mymysql",
//		fmt.Sprintf("cloudsql:sql-fortress:fortress-one*weather/%s/%s",
//			s.user, s.password))
	conn, err := sql.Open("mymysql", "tcp:127.0.0.1*weather/weather/weather")
	defer conn.Close()

	if err != nil {
		return err
	}

	_, err = conn.Exec("INSERT INTO history (value, series_id, timestamp, reporter_id) VALUES (?, ?, ?, ?)",
		p.Value, p.TimeseriesId, p.Timestamp, p.ReporterId)

	return err
}


func (s *MySqlStorage) Scan(start, end time.Time, seriesName string) (<-chan *DataPoint, <-chan error) {
	points := make(chan *DataPoint)
	errors := make(chan error)

	go s.doScan(start, end, seriesName, points, errors)
	return points, errors
}

func (s *MySqlStorage) doScan(start, end time.Time, seriesName string, points chan<- *DataPoint, errors chan<- error) {
//	conn, err := sql.Open("mymysql",
//		fmt.Sprintf("cloudsql:sql-fortress:fortress-one*weather/%s/%s",
//			s.user, s.password))
//	defer conn.Close()
	defer close(errors)
	defer close(points)
//
//	if err != nil {
//		errors <- err
//		return
//	}
}

// ===================================================================
// AppEngineStorage
// ===================================================================

type AppEngineStorage struct {
	context appengine.Context
}

func NewAppEngineStorage(req *http.Request) (*AppEngineStorage, error) {
	return &AppEngineStorage{
		context: appengine.NewContext(req),
	}, nil
}

func (s *AppEngineStorage) Put(p *DataPoint) error {
	key := fmt.Sprintf("%lld-%lld-%s",
		p.TimeseriesId,
		p.ReporterId,
		p.Timestamp.UnixNano())

	_, err := datastore.Put(
		s.context,
		datastore.NewKey(s.context, "datapoint", key, 0, nil),
		p)

	return err
}

// TODO(mrjones): pay attention to the "end" parameter
func (s *AppEngineStorage) Scan(start, end time.Time, seriesName string) (<-chan *DataPoint, <-chan error) {
	q := datastore.NewQuery("datapoint").Order("Timestamp").Filter("Timestamp >", start)

	if seriesName != "" {
		timeseriesId := tsid(seriesName)
		q = q.Filter("TimeseriesId =", timeseriesId)
	}

	points := make(chan *DataPoint)
	errors := make(chan error)

	/*
	result := q.Run(s.context)	
	go drain(points, errors, result)
*/
	go s.drainAll(points, errors, q)

	return points, errors
}

func (s *AppEngineStorage) drainAll(
	outPoints chan<- *DataPoint,
	outErrors chan<- error,
	query *datastore.Query) {

	buf := []*DataPoint{}

	_, err := query.GetAll(s.context, &buf)

	if err != nil {
		outErrors <- err
	} else {
		for _, p := range(buf) {
			outPoints <- p
		}
	}

	close(outPoints)
	close(outErrors)
}

func drain(
	outPoints chan<- *DataPoint,
	outErrors chan<- error,
	fromDb *datastore.Iterator) {

	for {
		var p DataPoint
		_, err := fromDb.Next(&p)
		if err == nil {
			outPoints <- &p
		} else if err == datastore.Done {
			break
		} else {
			outErrors <- err
		}
	}

	close(outPoints)
	close(outErrors)
}
