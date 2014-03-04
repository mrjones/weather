package main

import (
	"testing"
)

type FakeHub struct { 
}

func (h *FakeHub) RegisterMetrics(req *RegisterMetricsRequest) (*RegisterMetricsReply) {
	return nil
}

func (h *FakeHub) ReportMetricsById(req *ReportMetricsRequest) {
}

func (h *FakeHub) ReportMetricsByName(req *ReportMetricsByNameRequest) {
}

func TestSimple(t *testing.T) {
	fakeSerial := NewSerialPair(0)
	fakeHub := &FakeHub{}
	
	relay, err := MakeRelay(fakeSerial, fakeHub)
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
