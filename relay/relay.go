// ftp://ftp1.digi.com/support/documentation/90000982_A.pdf
// stty -F /dev/ttyAMA0 9600 crtscts
package main

import (
	"fmt"
	"log"
)

const (
	REPORT_METRICS_WITH_NAMES_MESSAGE_TYPE = 3
)


type ReportMetricsByNameRequest struct {
	sender  uint16
	metrics map[string]int64 // map from name to value
}

func (m *ReportMetricsByNameRequest) DebugString() string {
	vals := ""
	sep := ""
	for k, v := range m.metrics {
		vals += fmt.Sprintf("%s{%s=%d}", sep, k, v)
		sep = ", "
	}
	return fmt.Sprintf(
		"Sender:  0x%x\n"+
			"Metrics: %s\n",
		m.sender, vals)
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

func encodeString(data *[]byte, offset uint, s string) (e error, pos uint) {
	length := uint(len(s))
	err, offset := encodeVarUint(data, offset, length)
	if err != nil {
		return err, offset
	}

	if offset + length > uint(len(*data)) {
		return fmt.Errorf("Buffer overrun (encoding string %s) %d vs. %d.", s, offset, len(*data)), offset
		
	}

	for i := uint(0); i < length; i++ {
		(*data)[offset] = s[i]
		offset++
	}

	return nil, offset
}

func encodeVarUint(data *[]byte, offset uint, n uint) (e error, pos uint) {
	for n > 0 {
		if offset >= uint(len(*data)) {
			return fmt.Errorf("Buffer overrun (encoding varint %d) %d vs. %d.", n, offset, len(*data)), offset
		}
		(*data)[offset] = byte(n & 0xFF)
		n = n >> 8
		offset++
	}
	return nil, offset
}

func decodeString(data []byte, offset uint) (e error, pos uint, s string) {
	if offset >= uint(len(data)) {
		return fmt.Errorf("Index out of bounds %d vs %d.", offset, len(data)), offset, ""
	}
	
	var err error
	length := uint64(0)

	err, offset, length = decodeVarUint(data, offset)
	if err != nil {
		return err, offset, ""
	}

	if uint64(offset) + length >= uint64(len(data)) {
		return fmt.Errorf("Can't parse string of length %d startting at %d. Length is only %d.", length, offset, len(data)), offset, ""
	}

	chars := make([]byte, length)
	for i := uint64(0) ; i < length; i++ {
		chars[i] = data[offset]
		offset++
	}

	return nil, offset, string(chars)
}

func decodeVarUint(data []byte, offset uint) (e error, pos uint, val uint64) {
	if offset >= uint(len(data)) {
		return fmt.Errorf("Index out of bounds %d vs %d.", offset, len(data)), 0, 0
	}

	width := uint(data[offset])

	if offset+1+width > uint(len(data)) {
		return fmt.Errorf("Can't parse value of width %d startting at %d. Length is only %d.", width, offset+1, len(data)), 0, 0
	}

	val = 0
	for i := uint(0); i < width; i++ {
		val += uint64(data[offset+i+1]) << (8 * i)
	}

	return nil, offset + 1 + width, val
}

func (r *Relay) processPacket(packet *RxPacket) {
	data := packet.payload
	sender := packet.sender

	log.Printf("Payload: %s\n", arrayAsHex(data))
	log.Printf("Sender:  0x%x\n", sender)
	i := uint(0)

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

	if method == REPORT_METRICS_WITH_NAMES_MESSAGE_TYPE {
		report := &ReportMetricsByNameRequest{sender: sender}
		err, i, numMetrics := decodeVarUint(data, i)
		if err != nil {
			fmt.Println(err)
			return
		}
		log.Printf("num metrics: %d\n", numMetrics)
		report.metrics = make(map[string]int64)

		for m := uint64(0); m < numMetrics; m++ {
			name := ""
			val := uint64(0)
			err, i, name = decodeString(data, i)
			if err != nil {
				fmt.Println(err)
				return
			}

			err, i, val = decodeVarUint(data, i)
			if err != nil {
				fmt.Println(err)
				continue
			}

			report.metrics[name] = int64(val)
			log.Printf("metric[%s]: %d\n", name, report.metrics[name])
		}
		r.reports <- report
		
	} else {
		fmt.Println(fmt.Errorf("Unknown method %d", method))
	}
}

type PacketPair struct {
	ToDevice chan *TxPacket
	FromDevice chan *RxPacket
}

func NewPacketPair(capacity int) *PacketPair {
	return &PacketPair{
		ToDevice: make(chan *TxPacket, capacity),
		FromDevice: make(chan *RxPacket, capacity),
	}
}

type Relay struct {
	packets *PacketPair
	metricIds map[string]uint
	nextId uint
	reports chan<- *ReportMetricsByNameRequest
	shutdown bool
}

func NewRelay(packets *PacketPair, reports chan<- *ReportMetricsByNameRequest) (*Relay, error) {
	r := &Relay{
		packets: packets,
		metricIds: make(map[string]uint),
		nextId: 0,
		reports: reports,
	}
	go r.loop()
	return r, nil
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

func (r *Relay) Start() {
	
}

func (r *Relay) Shutdown() {
	if !r.shutdown {
		close(r.reports) // need a mutex here?
		r.shutdown = true
	}
}

type SerialPair struct {
	FromDevice chan []byte
	ToDevice chan []byte
}

func NewSerialPair(n int) *SerialPair {
	return &SerialPair{
		FromDevice: make(chan []byte, n),
		ToDevice: make(chan []byte, n),
	}
}

func MakeRelay(serial *SerialPair, reports chan *ReportMetricsByNameRequest) (*Relay, error) {
	framesFromDevice := make(chan *XbeeFrame)
	framesToDevice := make(chan *XbeeFrame)
	_ = NewRawXbeeDevice(serial.FromDevice, serial.ToDevice, framesFromDevice, framesToDevice)
	xbee := NewXbeeConnection(framesFromDevice, framesToDevice);

	return NewRelay(xbee.IO(), reports)
}

func drainReports(reports <-chan *ReportMetricsByNameRequest) {
	for {
		report := <- reports
		log.Println(report.DebugString())
	}
}

func main() {
	serial, err := NewSerialChannel("/dev/ttyAMA0")
	if err != nil {
		log.Fatal(err)
	}

	reports := make(chan *ReportMetricsByNameRequest)
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

	shutdown := make(chan bool)
	<-shutdown

	relay.Shutdown()
//	rawDevice.Shutdown()
}
