// ftp://ftp1.digi.com/support/documentation/90000982_A.pdf
// stty -F /dev/ttyAMA0 9600 crtscts
package main

import (
	"flag"
	"fmt"
	"io/ioutil"
	"log"
	"net/http"
	"time"
)

const (
	CURRENT_API_VERSION = 0x01

	REPORT_METRICS_RPC_ID = 3
)

type ReportMetricsArg struct {
	reporterId uint64
	metrics    map[string]int64 // map from name to value
}

func (m *ReportMetricsArg) DebugString() string {
	vals := ""
	sep := ""
	for k, v := range m.metrics {
		vals += fmt.Sprintf("%s{%s=%d}", sep, k, v)
		sep = ", "
	}
	return fmt.Sprintf(
		"ReporterID:  0x%x\n"+
			"Metrics: %s\n",
		m.reporterId, vals)
}

func (m *ReportMetricsArg) Serialize() ([]byte, error) {
	i := uint64(0)
	buf := make([]byte, 1024)
	var err error

	err, i = encodeVarUint(&buf, i, m.reporterId)
	if err != nil {
		return []byte{}, err
	}

	err, i = encodeVarUint(&buf, i, uint64(len(m.metrics)))
	if err != nil {
		return []byte{}, err
	}

	for id, val := range m.metrics {
		err, i = encodeString(&buf, i, id)
		if err != nil {
			return []byte{}, err
		}

		err, i = encodeVarUint(&buf, i, uint64(val))
		if err != nil {
			return []byte{}, err
		}
	}

	return buf[:i], nil
}

func ParseReportMetricsArg(data []byte, offset uint64) (*ReportMetricsArg, error) {
	report := &ReportMetricsArg{}
	numMetrics := uint64(0)

	err, i, reporterId := decodeVarUint(data, offset)
	if err != nil {
		return nil, err
	}
	report.reporterId = reporterId
	log.Printf("reporterId: %d\n", report.reporterId)

	err, i, numMetrics = decodeVarUint(data, i)
	if err != nil {
		return nil, err
	}
	log.Printf("num metrics: %d\n", numMetrics)
	report.metrics = make(map[string]int64)

	for m := uint64(0); m < numMetrics; m++ {
		name := ""
		val := uint64(0)
		err, i, name = decodeString(data, i)
		if err != nil {
			return nil, err
		}

		err, i, val = decodeVarUint(data, i)
		if err != nil {
			return nil, err
		}

		report.metrics[name] = int64(val)
		log.Printf("metric[%s]: %d\n", name, report.metrics[name])
	}

	return report, nil
}

func arrayAsHex(a []byte) string {
	return arrayAsHexWithLen(a, len(a))
}

func arrayAsHexWithLen(a []byte, length int) string {
	s := "[ "
	for i := 0; i < length; i++ {
		s += fmt.Sprintf("0x%x ", a[i])
	}
	s += "]"
	return s
}

func encodeString(data *[]byte, offset uint64, s string) (e error, pos uint64) {
	length := uint64(len(s))
	err, offset := encodeVarUint(data, offset, length)
	if err != nil {
		return err, offset
	}

	if offset+length > uint64(len(*data)) {
		return fmt.Errorf("Buffer overrun (encoding string %s) %d vs. %d.", s, offset, len(*data)), offset

	}

	for i := uint64(0); i < length; i++ {
		(*data)[offset] = s[i]
		offset++
	}

	return nil, offset
}

func encodeVarUint(data *[]byte, offset uint64, n uint64) (e error, pos uint64) {
	width := 0
	n2 := n
	for n2 > 0 {
		width++
		n2 = n2 >> 8
	}

	if width > 0xFF {
		return fmt.Errorf("Number too wide: %d", n), offset
	}
	(*data)[offset] = byte(width)
	offset++

	for n > 0 {
		if offset >= uint64(len(*data)) {
			return fmt.Errorf("Buffer overrun (encoding varint %d) %d vs. %d.", n, offset, len(*data)), offset
		}
		(*data)[offset] = byte(n & 0xFF)
		n = n >> 8
		offset++
	}
	return nil, offset
}

func decodeString(data []byte, offset uint64) (e error, pos uint64, s string) {
	if offset >= uint64(len(data)) {
		return fmt.Errorf("Index out of bounds %d vs %d.", offset, len(data)), offset, ""
	}

	var err error
	length := uint64(0)

	err, offset, length = decodeVarUint(data, offset)
	if err != nil {
		return err, offset, ""
	}

	if uint64(offset)+length >= uint64(len(data)) {
		return fmt.Errorf("Can't parse string of length %d startting at %d. Length is only %d.", length, offset, len(data)), offset, ""
	}

	chars := make([]byte, length)
	for i := uint64(0); i < length; i++ {
		chars[i] = data[offset]
		offset++
	}

	return nil, offset, string(chars)
}

func decodeVarUint(data []byte, offset uint64) (e error, pos uint64, val uint64) {
	if offset >= uint64(len(data)) {
		return fmt.Errorf("Index out of bounds %d vs %d.", offset, len(data)), 0, 0
	}

	width := uint64(data[offset])

	if offset+1+width > uint64(len(data)) {
		return fmt.Errorf("Can't parse value of width %d startting at %d. Length is only %d.", width, offset+1, len(data)), 0, 0
	}

	val = 0
	for i := uint64(0); i < width; i++ {
		val += uint64(data[offset+i+1]) << (8 * i)
	}

	return nil, offset + 1 + width, val
}

func (r *Relay) processPacket(packet *RxPacket) {
	data := packet.payload
	sender := packet.sender

	log.Printf("Payload: %s\n", arrayAsHex(data))
	log.Printf("Sender:  0x%x\n", sender)
	i := uint64(0)

	err, i, protocolVersion := decodeVarUint(data, i)
	if err != nil {
		fmt.Println(err)
		return
	}
	if protocolVersion != 1 {
		fmt.Println(fmt.Errorf("Unsupported protocol version %d.", protocolVersion))
		return
	}

	log.Printf("protocol version: %d\n", protocolVersion)

	err, i, method := decodeVarUint(data, i)
	if err != nil {
		fmt.Println(err)
		return
	}
	log.Printf("method: %d\n", method)

	if method == REPORT_METRICS_RPC_ID {
		report, err := ParseReportMetricsArg(data, i)
		if err != nil {
			log.Println(err)
		} else {
			r.reports <- report
		}
	} else {
		fmt.Println(fmt.Errorf("Unknown method %d", method))
	}
}

type Relay struct {
	packets   *PacketPair
	metricIds map[string]uint
	nextId    uint
	reports   chan<- *ReportMetricsArg
	shutdown  bool
}

func MakeRelay(serial *SerialPair, reports chan *ReportMetricsArg) (*Relay, error) {
	framesFromDevice := make(chan *XbeeFrame)
	framesToDevice := make(chan *XbeeFrame)
	_ = NewRawXbeeDevice(serial, framesFromDevice, framesToDevice)
	xbee := NewXbeeConnection(framesFromDevice, framesToDevice)

	return NewRelay(xbee.IO(), reports)
}

func NewRelay(packets *PacketPair, reports chan<- *ReportMetricsArg) (*Relay, error) {
	r := &Relay{
		packets:   packets,
		metricIds: make(map[string]uint),
		nextId:    0,
		reports:   reports,
	}
	go r.loop()
	return r, nil
}

func (r *Relay) Start() {}

func (r *Relay) Shutdown() {
	if !r.shutdown {
		close(r.reports) // need a mutex here?
		r.shutdown = true
	}
}

func (r *Relay) loop() {
	for {
		packet, ok := <-r.packets.FromDevice
		if !ok {
			log.Println("Relay shutting down")
			r.Shutdown()
			return
		}
		r.processPacket(packet)
	}
}

func drainReports(reports <-chan *ReportMetricsArg) {
	for {
		report := <-reports
		log.Println(report.DebugString())
		for id, value := range(report.metrics) {
			url := fmt.Sprintf(
				"http://fortressweather.appspot.com/v2/simplereport?t_sec=%d&v=%d&tsname=%s&rid=%d",
				time.Now().Unix(), value, id, report.reporterId)
			fmt.Printf("URL: %s\n", url)

			resp, err := http.Get(url)
			if err != nil {
				log.Println(err);
				continue;
			}

			defer resp.Body.Close()
			body, err := ioutil.ReadAll(resp.Body)
			if err != nil {
				log.Println(err);
				continue;
			}

			fmt.Printf("Reported metric: %s\n", body)
		}
	}
}

type StatusServer struct {
	port int
	startTime time.Time
}

func (s *StatusServer) healthHandler(resp http.ResponseWriter, req *http.Request) {
	resp.Write([]byte("OK"))
}

func (s *StatusServer) statusHandler(resp http.ResponseWriter, req *http.Request) {
	body := fmt.Sprintf(
		"<h1>Weather Relay</h1>"+
			"Started: %s (Up: %s)<br/>",
		s.startTime.Format(time.RFC850),
		time.Now().Sub(s.startTime).String())
	resp.Write([]byte(body))
}

func (s *StatusServer) ServeForever() {
	http.HandleFunc("/healthz", s.healthHandler)
	http.HandleFunc("/statusz", s.statusHandler)
	http.HandleFunc("/", s.statusHandler)

	s.startTime = time.Now()
	http.ListenAndServe(fmt.Sprintf(":%d", s.port), nil)
	fmt.Printf("Status port: %d\n", s.port)
}

func NewStatusServer(port int) *StatusServer {
	return &StatusServer{port: port}
}


func main() {
	var statusPort = flag.Int(
		"status_port",
		-1,
		"Port to run a status server on (no server if <= 0).")

	flag.Parse()

	serial, err := NewSerialChannel("/dev/ttyAMA0")
	if err != nil {
		log.Fatal(err)
	}

	reports := make(chan *ReportMetricsArg)
	go drainReports(reports)
	relay, err := MakeRelay(serial.Pair(), reports)
	if err != nil {
		log.Fatal(err)
	}

	/*

		f := NewXbeeFrame([]byte{AT_COMMAND, 0x52, 'M', 'Y'})
		log.Printf("Framer %s\n", arrayAsHex(f.Serialize()))

		wn, err := file.Write(f.Serialize())
		if err != nil {
			log.Println(err)
		} else {
			log.Printf("Write %d bytes\n", wn)
		}
	*/

	if (*statusPort > 0) {
		statusServer := NewStatusServer(*statusPort)
		go statusServer.ServeForever()
	} else {
		fmt.Printf("No status server (port = %d).\n", *statusPort)
	}

	shutdown := make(chan bool)
	<-shutdown

	relay.Shutdown()
	//	rawDevice.Shutdown()
}
