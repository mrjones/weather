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

func asRxPacketBytes(arg ReportMetricsArg) ([]byte, error) {
	buf, err := arg.Serialize()
	if err != nil {
		return []byte{}, err
	}

	appPayload := &bytes.Buffer{}

	// RPC Header
	appPayload.Write([]byte{
		0x01, CURRENT_API_VERSION,
		0x01, REPORT_METRICS_RPC_ID,
	})

	// RPC Payload
	appPayload.Write(buf)

	rxPacket := &RxPacket{
		sender:  0x2222,
		rssi:    0x38,
		options: 0x0,
		payload: appPayload.Bytes(),
	}

	return makeFrame(rxPacket.Serialize()).Serialize(), nil
}

func StandardSetUp(t *testing.T) (*SerialPair, chan *ReportMetricsArg, *Relay) {
	fakeSerial := NewSerialPair(10)
	reports := make(chan *ReportMetricsArg)

	relay, err := MakeRelay(fakeSerial, reports)
	AssertNoError(err, t)

	return fakeSerial, reports, relay
}

func TestReceiveOneMessage(t *testing.T) {
	fakeSerial, reports, relay := StandardSetUp(t)
	relay.Start() // Necessary?

	original := ReportMetricsArg{
		reporterId: 123456,
		metrics: map[string]int64{"FOO": 255,"bar": 256},
	}

	buf, err := asRxPacketBytes(original)
	AssertNoError(err, t)
	fakeSerial.FromDevice <- buf
	actual := <-reports

	ReportsEq(&original, actual, t)

	relay.Shutdown()
}

func TestGarbageBeforeMessage(t *testing.T) {
	fakeSerial, reports, relay := StandardSetUp(t)
	relay.Start() // Necessary?

	original := ReportMetricsArg{
		reporterId: 123456,
		metrics: map[string]int64{"FOO": 255,"bar": 256},
	}

	buf, err := asRxPacketBytes(original)
	AssertNoError(err, t)

	// TODO(mrjones): Need to be able to handle a stray 0x7E here
	fakeSerial.FromDevice <- []byte{0x01, 0x02, 0x03, 0x04, 0x05}
	fakeSerial.FromDevice <- buf
	actual := <-reports

	ReportsEq(&original, actual, t)

	relay.Shutdown()
}
