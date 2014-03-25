package main

import (
	"bytes"
	"testing"
)

func makeFrame(payload []byte) *XbeeFrame {
	sum := uint8(0)
	for _, b := range payload {
		sum += b
	}

	return &XbeeFrame{
		length:   uint16(len(payload)),
		payload:  payload,
		checksum: 0xFF - sum,
	}
}

func encode(arg ReportMetricsArg) ([]byte, error) {
	buf, err := arg.Serialize()
	if err != nil {
		return []byte{}, err
	}

	appPayload := &bytes.Buffer{}
	appPayload.Write([]byte{
		0x01, 0x01, // API Version
		0x01, 0x03, // RPC Method ID
	})
	appPayload.Write(buf)

	rxPacket := &RxPacket{
		sender:  0x2222,
		rssi:    0x38,
		options: 0x0,
		payload: appPayload.Bytes(),
	}

	return makeFrame(rxPacket.Serialize()).Serialize(), nil
}

func TestReceiveOneMessage(t *testing.T) {
	fakeSerial := NewSerialPair(10)
	reports := make(chan *ReportMetricsArg)

	relay, err := MakeRelay(fakeSerial, reports)
	AssertNoError(err, t)
	relay.Start() // Necessary?

	buf, err := encode(ReportMetricsArg{
		reporterId: 123456,
		metrics: map[string]int64{
			"FOO": 255,
			"bar": 256,
		},
	})
	AssertNoError(err, t)
	fakeSerial.FromDevice <- buf

	report := <-reports

	ReportsEq(
		&ReportMetricsArg{
			reporterId: 123456,
			metrics:    map[string]int64{"FOO": 255, "bar": 256},
		}, report, t)

	relay.Shutdown()
}
