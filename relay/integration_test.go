package main

import (
	"testing"
)

func makeFrame(payload []byte) *XbeeFrame {
	sum := uint8(0)
	for _, b := range(payload) {
		sum += b
	}

	return &XbeeFrame{
		length: uint16(len(payload)),
		payload: payload,
		checksum: 0xFF - sum,
	}
}

func TestSimple(t *testing.T) {
	fakeSerial := NewSerialPair(10)
	reports := make(chan *ReportMetricsArg)

	relay, err := MakeRelay(fakeSerial, reports)
	AssertNoError(err, t)
	relay.Start() // Necessary?

	payload := []byte {
		0x81, // API Identifier
		0x22, 0x22, // Sender
		0x38, // RSSI
		0x00, // Options
		// -- PAYLOAD --
		0x01, 0x01, // API Version
		0x01, 0x03, // RPC Method ID
		0x01, 0x02, // Num Metrics
		0x01, 0x03, 'F', 'O', 'O', // metric[0] name
		0x01, 0xFF, // metric[0] value
		0x01, 0x03, 'b', 'a', 'r', // metric[1] name
		0x02, 0x00, 0x01 }  // metric[0] value
	fakeSerial.FromDevice <- makeFrame(payload).Serialize()

	report := <- reports

	ReportsEq(
		&ReportMetricsArg{
			sender: 0x2222,
			metrics: map[string]int64{"FOO": 255, "bar": 256},
		}, report, t);

	relay.Shutdown()
}
