package main

import (
	"testing"
)

func TestSimple(t *testing.T) {
	fakeSerial := NewSerialPair(0)
	reports := make(chan *ReportMetricsArg)
	
	relay, err := MakeRelay(fakeSerial, reports)
	AssertNoError(err, t)
	relay.Start() // Necessary?

	fakeSerial.FromDevice <- []byte {
		0x7E, 0x00, 0x13, 0x81, 0x22, 0x22, 0x38, 0x0 }
	fakeSerial.FromDevice <- []byte {
		0x01, 0x01, 0x01, 0x01, 0x02, 0x02, 0x01, 0x01 }
	fakeSerial.FromDevice <- []byte {
		0x01, 0x0B, 0x01, 0x02, 0x01, 0x43, 0xA6 }

	

	relay.Shutdown()
}
