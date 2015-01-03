package weatherserver

import (
	"fmt"
	"net/http"
	"time"

	"appengine"
	"appengine/datastore"
)

type Storage interface {
     Put(d *DataPoint) error

     Scan(start, end time.Time) (<-chan *DataPoint, error)
}


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

	result := q.Run(s.context)
	
	points := make(chan *DataPoint)
	errors := make(chan error)

	go drain(points, errors, result)
	return points, errors
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
