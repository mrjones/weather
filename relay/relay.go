// ftp://ftp1.digi.com/support/documentation/90000982_A.pdf
// stty -F /dev/ttyAMA0 9600 crtscts
package main

import (
	"fmt"
	"log"
)

const (
	REPORT_METRICS_MESSAGE_TYPE = 1
	REGISTER_METRICS_MESSAGE_TYPE = 2
)



type RegisterMetricsRequest struct {
	metricNames []string
}

func (m *RegisterMetricsRequest) DebugString() string {
	s := "["
	sep := ""

	for _, r := range m.metricNames {
		s += r + sep
		sep = ","
	}
	return s + "]"
}

type ReportMetricsMessage struct {
	sender  uint16
	metrics map[uint64]int64 // map from id to value
}

func (m *ReportMetricsMessage) DebugString() string {
	vals := ""
	sep := ""
	for k, v := range m.metrics {
		vals += fmt.Sprintf("%s{%d=%d}", sep, k, v)
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

func (r *Relay) reportMetrics(report *ReportMetricsMessage) {
	log.Printf("Reported metrics: %s", report.DebugString())
}

func (r *Relay) registerMetrics(registration *RegisterMetricsRequest) {
	data := make([]byte, 1024)
	offset := uint(0)
	var err error
	log.Printf("Registered metrics: %s", registration.DebugString())
	for _, name := range registration.metricNames {
		id, ok := r.metricIds[name]
		if !ok {
			id = r.nextId
			r.nextId++
			r.metricIds[name] = id
		}

		err, offset = encodeVarUint(&data, offset, id)
		if err != nil {
			log.Print(err)
			return
		}

		err, offset = encodeString(&data, offset, name)
		if err != nil {
			log.Print(err)
			return
		}
	}

	log.Printf("Response: %s\n", arrayAsHex(data[0:offset]))
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

	if method == REPORT_METRICS_MESSAGE_TYPE {
		report := &ReportMetricsMessage{sender: sender}
		err, i, numMetrics := decodeVarUint(data, i)
		if err != nil {
			fmt.Println(err)
			return
		}
		log.Printf("num metrics: %d\n", numMetrics)
		report.metrics = make(map[uint64]int64)

		for m := uint64(0); m < numMetrics; m++ {
			mid := uint64(0)
			val := uint64(0)
			err, i, mid = decodeVarUint(data, i)
			if err != nil {
				fmt.Println(err)
				return
			}

			err, i, val = decodeVarUint(data, i)
			if err != nil {
				fmt.Println(err)
				continue
			}

			report.metrics[mid] = int64(val)
			log.Printf("metric[%d]: %d\n", mid, report.metrics[mid])
		}
		r.reportMetrics(report)
	} else if method == REGISTER_METRICS_MESSAGE_TYPE {
		registration := &RegisterMetricsRequest{}
		numMetrics := uint64(0)
		err, i, numMetrics = decodeVarUint(data, i)
		if err != nil {
			fmt.Println(err)
			return
		}
		log.Printf("num metrics: %d\n", numMetrics)
		registration.metricNames = make([]string, numMetrics)
			
		for m := uint64(0); m < numMetrics; m++ {
			chars := uint64(0)
			err, i, chars = decodeVarUint(data, i)
			if err != nil {
				fmt.Println(err)
				continue
			}

			registration.metricNames[m] = ""
			for c := uint64(0); c < chars; c++ {
				if i >= uint(len(data)) {
					fmt.Println("Write a better error message")
					continue
				}
				registration.metricNames[m] += string(data[i])
				i++
			}
		}

		r.registerMetrics(registration)
	} else {
		fmt.Println(fmt.Errorf("Unknown method %d", method))
	}
}

type Relay struct {
	xbee *XbeeConnection
	metricIds map[string]uint
	nextId uint
}

func NewRelay(xbee *XbeeConnection) (*Relay, error) {
	r := &Relay{
		xbee: xbee,
		metricIds: make(map[string]uint),
		nextId: 0,
	}
	go r.loop()
	return r, nil
}

func (r *Relay) loop() {
	for {
		packet, ok := <-r.xbee.RxData()
		if !ok {
			log.Println("Relay shutting down")
			return
		}
		r.processPacket(packet)
	}
}

func (r *Relay) Shutdown() {
	
}

func main() {
//	serialPort, err := NewSerialConnection("/dev/ttyAMA0")
	serial, err := NewSerialChannel("/dev/ttyAMA0")
	if err != nil {
		log.Fatal(err)
	}

	framesFromDevice := make(chan *XbeeFrame)
	framesToDevice := make(chan *XbeeFrame)
	rawDevice := NewRawXbeeDevice(serial.ReadChannel(), serial.WriteChannel(), framesFromDevice, framesToDevice)
	xbee := NewXbeeConnection(framesFromDevice, framesToDevice);

	relay, err := NewRelay(xbee)
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
	rawDevice.Shutdown()
}
