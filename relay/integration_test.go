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

func TestReceiveOneMessage(t *testing.T) {
	fakeSerial := NewSerialPair(10)
	reports := make(chan *ReportMetricsArg)

	relay, err := MakeRelay(fakeSerial, reports)
	AssertNoError(err, t)
	relay.Start() // Necessary?

	rxPacket := &RxPacket{
		sender: 0x2222,
		rssi: 0x38,
		options: 0x0,
		payload: []byte {
			0x01, 0x01, // API Version
			0x01, 0x03, // RPC Method ID
			0x01, 0x02, // Num Metrics
			0x01, 0x03, // len(metric[0].name)
			'F', 'O', 'O', // metric[0].name
			0x01, 0xFF, // metric[0].value
			0x01, 0x03, // len(metric[1].name)
			'b', 'a', 'r', // metric[1] name
			0x02, 0x00, 0x01,  // metric[1].value
		},
	}

	fakeSerial.FromDevice <- makeFrame(rxPacket.Serialize()).Serialize()

	report := <- reports

	ReportsEq(
		&ReportMetricsArg{
			sender: 0x2222,
			metrics: map[string]int64{"FOO": 255, "bar": 256},
		}, report, t);

	relay.Shutdown()
}
