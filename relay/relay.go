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

type RegisterMetricsMessage struct {
	sender uint16
	metricNames []string
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

func HandleReportedMetrics(metricReports <-chan *ReportMetricsMessage) {
	for {
		report, ok := <-metricReports
		if !ok {
			log.Println("HandleReportedMetrics shutting down")
			return
		}

		log.Printf("Reported metrics: %s", report.DebugString())
	}
}

func handleMetricReport(report *ReportMetricsMessage) {

}

func HandleReceivedPackets(rxPackets <-chan *RxPacket, metricReports chan<- *ReportMetricsMessage, metricRegistrations chan<- *RegisterMetricsMessage) {
	for {
		packet, ok := <-rxPackets
		if !ok {
			log.Println("HandleReceivedPackets shutting down")
			close(metricReports)
			return
		}

		data := packet.payload
		sender := packet.sender

		log.Printf("Payload: %s\n", arrayAsHex(data))
		log.Printf("Sender:  0x%x\n", sender)
		i := uint(0)

		err, i, protocolVersion := decodeVarUint(data, i)
		if err != nil {
			fmt.Println(err)
			continue
		}
		if protocolVersion != 1 {
			fmt.Println(fmt.Errorf("Unsupported protocol version %d.", protocolVersion))
			continue
		}

		log.Printf("protocol version: %d\n", protocolVersion)

		err, i, method := decodeVarUint(data, i)
		if err != nil {
			fmt.Println(err)
			continue
		}
		log.Printf("method: %d\n", method)

		if method == REPORT_METRICS_MESSAGE_TYPE {
			report := &ReportMetricsMessage{sender: sender}
			err, i, numMetrics := decodeVarUint(data, i)
			if err != nil {
				fmt.Println(err)
				continue
			}
			log.Printf("num metrics: %d\n", numMetrics)
			report.metrics = make(map[uint64]int64)

			for m := uint64(0); m < numMetrics; m++ {
				mid := uint64(0)
				val := uint64(0)
				err, i, mid = decodeVarUint(data, i)
				if err != nil {
					fmt.Println(err)
					continue
				}

				err, i, val = decodeVarUint(data, i)
				if err != nil {
					fmt.Println(err)
					continue
				}

				report.metrics[mid] = int64(val)
				log.Printf("metric[%d]: %d\n", mid, report.metrics[mid])
			}
			metricReports <- report
		} else if method == REGISTER_METRICS_MESSAGE_TYPE {
			registration := &RegisterMetricsMessage{sender: sender}
			numMetrics := uint64(0)
			err, i, numMetrics = decodeVarUint(data, i)
			if err != nil {
				fmt.Println(err)
				continue
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

			metricRegistrations <- registration
		} else {
			fmt.Println(fmt.Errorf("Unknown method %d", method))
		}
	}
}

func main() {
	serialPort, err := NewSerialConnection("/dev/ttyAMA0")
	if err != nil {
		log.Fatal(err)
	}

	xbee := NewXbeeConnection(serialPort);

	metricReports := make(chan *ReportMetricsMessage)
	metricRegistrations := make(chan *RegisterMetricsMessage)

	go HandleReceivedPackets(
		xbee.RxData(), metricReports, metricRegistrations)
	go HandleReportedMetrics(metricReports)

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
}
