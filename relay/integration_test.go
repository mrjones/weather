package main

import (
	"bytes"
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

	reportArg := ReportMetricsArg{
		metrics: map[string]int64 {
			"FOO":255,
			"bar":256,
		},
	}
	reportArgBytes, err := reportArg.Serialize()
	AssertNoError(err, t)

	appPayload := &bytes.Buffer{}
	appPayload.Write([]byte {
		0x01, 0x01, // API Version
		0x01, 0x03, // RPC Method ID
	})
	appPayload.Write(reportArgBytes)

	rxPacket := &RxPacket{
		sender: 0x2222,
		rssi: 0x38,
		options: 0x0,
		payload: appPayload.Bytes(),
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
