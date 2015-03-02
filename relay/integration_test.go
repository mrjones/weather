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

func packageRpc(appPayload []byte, t *testing.T) []byte {
	rxPacket := &RxPacket{
		sender:  0x2222,
		rssi:    0x38,
		options: 0x0,
		payload: appPayload,
	}

	return makeFrame(rxPacket.Serialize()).Serialize()

}

func errorAsRxPacketBytes(arg ReportErrorArg, t *testing.T) []byte {
	appPayload := &bytes.Buffer{}

	// RPC Header
	appPayload.Write([]byte{
		0x01, CURRENT_API_VERSION,
		0x01, REPORT_ERROR_RPC_ID,
	})

	// RPC Payload
	buf, err := arg.Serialize()
	AssertNoError(err, t)
	appPayload.Write(buf)

	return packageRpc(appPayload.Bytes(), t)
}

func reportAsRxPacketBytes(arg ReportMetricsArg, t *testing.T) []byte {
	appPayload := &bytes.Buffer{}

	// RPC Header
	appPayload.Write([]byte{
		0x01, CURRENT_API_VERSION,
		0x01, REPORT_METRICS_RPC_ID,
	})

	// RPC Payload
	buf, err := arg.Serialize()
	AssertNoError(err, t)
	appPayload.Write(buf)

	return packageRpc(appPayload.Bytes(), t)
}

func StandardSetUp(t *testing.T) (*SerialPair, chan *ReportMetricsArg, chan *ReportErrorArg, *Relay) {
	fakeSerial := NewSerialPair(10)
	reports := make(chan *ReportMetricsArg)
	errors := make(chan *ReportErrorArg)

	relay, err := MakeRelay(fakeSerial, reports, errors)
	AssertNoError(err, t)

	return fakeSerial, reports, errors, relay
}

func TestReceiveOneMessage(t *testing.T) {
	fakeSerial, reports, _, relay := StandardSetUp(t)
	relay.Start() // Necessary?

	original := ReportMetricsArg{
		reporterId: 123456,
		metrics:    map[string]int64{"FOO": 255, "bar": 256},
	}

	fakeSerial.FromDevice <- reportAsRxPacketBytes(original, t)

	ReportsEq(&original, <-reports, t)

	relay.Shutdown()
}

func TestGarbageBeforeMessage(t *testing.T) {
	fakeSerial, reports, _, relay := StandardSetUp(t)
	relay.Start() // Necessary?

	original := ReportMetricsArg{
		reporterId: 123456,
		metrics:    map[string]int64{"FOO": 255, "bar": 256},
	}

	// TODO(mrjones): Need to be able to handle a stray 0x7E here
	fakeSerial.FromDevice <- []byte{0x01, 0x02, 0x03, 0x04, 0x05}
	fakeSerial.FromDevice <- reportAsRxPacketBytes(original, t)

	ReportsEq(&original, <-reports, t)

	relay.Shutdown()
}

func TestReportError(t *testing.T) {
	fakeSerial, _, errors, relay := StandardSetUp(t)
	relay.Start() // Necessary?

	original := ReportErrorArg{
		errorMessage: "There was a problem!",
	}

	fakeSerial.FromDevice <- errorAsRxPacketBytes(original, t)

	ErrorReportsEq(&original, <-errors, t)

	relay.Shutdown()
}
