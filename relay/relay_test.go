package main

import (
	"bytes"
	"testing"
)

func FramesEq(expected, actual *XbeeFrame, t *testing.T) {
	if expected.length != actual.length {
		t.Errorf("XbeeFrames don't match in 'length' param.\nExpected: '%d'.\nActual: '%d'.", expected.length, actual.length)
	}

	if bytes.Compare(expected.payload, actual.payload) != 0 {
		t.Errorf("XbeeFrames don't match in 'payload' param.\nExpected: '%s'.\nActual: '%s'.", arrayAsHex(expected.payload), arrayAsHex(actual.payload))
	}

	if expected.checksum != actual.checksum {
		t.Errorf("XbeeFrames don't match in 'checksum' param.\nExpected: '0x%x'.\nActual: '0x%x'.", expected.checksum, actual.checksum)
	}
}

func AssertNoError(err error, t *testing.T) {
	if err != nil {
		t.Fatalf("Unexpected error: '%s'.", err.Error())
	}
}

func TestConsumeMessageAllAtOnce(t *testing.T) {
	output := make(chan *XbeeFrame, 1)
	accum := NewAccum(output)

	AssertNoError(accum.Consume(
		[]byte{0x7e, 0x00, 0x03, 0x12, 0x34, 0x56, 0x63}, 0, 7), t)

	actual := <-output

	FramesEq(
		&XbeeFrame{
			length:   3,
			payload:  []byte{0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual, t)
}

func TestConsumeMessageByteByByte(t *testing.T) {
	output := make(chan *XbeeFrame, 1)
	accum := NewAccum(output)

	AssertNoError(accum.Consume([]byte{0x7e}, 0, 1), t)
	AssertNoError(accum.Consume([]byte{0x00}, 0, 1), t)
	AssertNoError(accum.Consume([]byte{0x03}, 0, 1), t)
	AssertNoError(accum.Consume([]byte{0x12}, 0, 1), t)
	AssertNoError(accum.Consume([]byte{0x34}, 0, 1), t)
	AssertNoError(accum.Consume([]byte{0x56}, 0, 1), t)
	AssertNoError(accum.Consume([]byte{0x63}, 0, 1), t)

	actual := <-output

	FramesEq(
		&XbeeFrame{
			length:   3,
			payload:  []byte{0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual, t)
}

func TestConsumeTwoMessages(t *testing.T) {
	output := make(chan *XbeeFrame, 2)
	accum := NewAccum(output)

	AssertNoError(accum.Consume(
		[]byte{0x7e, 0x00, 0x03, 0x12, 0x34, 0x56, 0x63}, 0, 7), t)
	AssertNoError(accum.Consume(
		[]byte{0x7e, 0x00, 0x04, 0x12, 0x34, 0x56, 0x78, 0xEB}, 0, 8), t)

	actual1 := <-output

	FramesEq(
		&XbeeFrame{
			length:   3,
			payload:  []byte{0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual1, t)

	actual2 := <-output

	FramesEq(
		&XbeeFrame{
			length:   4,
			payload:  []byte{0x12, 0x34, 0x56, 0x78},
			checksum: 0xEB,
		}, actual2, t)
}

func TestBoundsChecking(t *testing.T) {
	output := make(chan *XbeeFrame, 0)
	accum := NewAccum(output)

	err := accum.Consume([]byte{0x7e}, 1, 1)
	if err == nil {
		t.Errorf("Didn't detect starting past end of array")
	}

	err = accum.Consume([]byte{0x7e}, 0, 2)
	if err == nil {
		t.Errorf("Didn't detect going past end of array")
	}
}

func TestVerifiesChecksum(t *testing.T) {
	output := make(chan *XbeeFrame, 0)
	accum := NewAccum(output)

	err := accum.Consume([]byte{0x7e, 0x00, 0x01, 0x12, 0x00}, 0, 5)
	if err == nil {
		t.Errorf("Didn't detect invalid checksum.")
	}
}

// ===============

func PacketsEq(expected, actual *RxPacket, t *testing.T) {
	if expected.sender != actual.sender {
		t.Errorf("RxPackets don't match in 'sender' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.sender, actual.sender)
	}

	if bytes.Compare(expected.payload, actual.payload) != 0 {
		t.Errorf("RxPackets don't match in 'payload' param.\nExpected: '%s'.\nActual: '%s'.", arrayAsHex(expected.payload), arrayAsHex(actual.payload))
	}

	if expected.rssi != actual.rssi {
		t.Errorf("RxPackets don't match in 'rssi' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.rssi, actual.rssi)
	}

	if expected.options != actual.options {
		t.Errorf("RxPackets don't match in 'options' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.options, actual.options)
	}
}

func TestConsumeRxFrame(t *testing.T) {
	frames := make(chan *XbeeFrame, 1)
	rxPackets := make(chan *RxPacket, 1)

	go ConsumeXbeeFrames(frames, rxPackets)

	frames <- &XbeeFrame{
		length:   8,
		payload:  []byte{0x81, 0x22, 0x22, 0x28, 0x01, 0x12, 0x34, 0x56},
		checksum: 0x00}

	actual := <-rxPackets

	PacketsEq(
		&RxPacket{
			sender:  0x2222,
			payload: []byte{0x12, 0x34, 0x56},
			rssi:    0x28,
			options: 0x01,
		}, actual, t)
}

func TestMalformedRxFrame_TooShort(t *testing.T) {
	frames := make(chan *XbeeFrame, 1)
	rxPackets := make(chan *RxPacket, 1)

	go ConsumeXbeeFrames(frames, rxPackets)

	frames <- &XbeeFrame{
		length:   4,
		payload:  []byte{0x81, 0x12, 0x34, 0x56},
		checksum: 0x00}

	close(frames)

	packet, ok := <-rxPackets

	if packet != nil {
		t.Errorf("Got an unexpected packet while processing a malformed frame")
	}

	if ok {
		t.Errorf("Got an unexpected packet while processing a malformed frame")
	}
}

func TestIgnoresUnknownFrames(t *testing.T) {
	frames := make(chan *XbeeFrame, 1)
	rxPackets := make(chan *RxPacket, 1)

	go ConsumeXbeeFrames(frames, rxPackets)

	frames <- &XbeeFrame{
		length:   4,
		payload:  []byte{0xEE, 0x12, 0x34, 0x56},
		checksum: 0x00}

	close(frames)

	packet, ok := <-rxPackets

	if packet != nil {
		t.Errorf("Got an unexpected packet after reading a garbage frame")
	}

	if ok {
		t.Errorf("Got an unexpected packet after reading a garbage frame")
	}
}

// ===============

func ReportsEq(expected, actual *ReportMetricsMessage, t *testing.T) {
	if expected.sender != actual.sender {
		t.Errorf("ReportMetricsMessages don't match in 'sender' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.sender, actual.sender)
	}

	if len(expected.metrics) != len(actual.metrics) {
		t.Errorf("ReportMetricsMessages don't match in 'len(metrics)' param.\nExpected: %d.\nActual: %d.", len(expected.metrics), len(actual.metrics))
	}

	for ek, ev := range expected.metrics {
		av := actual.metrics[ek]
		if ev != av {
			t.Errorf("ReportMetricsMessages don't match at metric with id '%d'.\nExpected %d\nActual: %d.", ek, ev, av)
		}
	}
}

func RegistrationsEq(expected, actual *RegisterMetricsMessage, t *testing.T) {
	if expected.sender != actual.sender {
		t.Errorf("RegisterMetricsMessages don't match in 'sender' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.sender, actual.sender)
	}

	if len(expected.metricNames) != len(actual.metricNames) {
		t.Errorf("RegisterMetricsMessages don't match in 'len(metricNames)' param.\nExpected: %d.\nActual: %d.", len(expected.metricNames), len(actual.metricNames))
	}

	for i := 0 ; i < len(expected.metricNames) && i < len(actual.metricNames); i++ {
		if expected.metricNames[i] != actual.metricNames[i] {
			t.Errorf("RegisterMetricsMessages don't match at metric[%d].\nExpected: %s.\nActual: %s.", i, expected.metricNames[i], actual.metricNames[i])
		}
	}
}

func TestMetricReport(t *testing.T) {
	packets := make(chan *RxPacket, 1)
	reports := make(chan *ReportMetricsMessage, 1)
	registrations := make(chan *RegisterMetricsMessage, 1)

	go HandleReceivedPackets(packets, reports, registrations)

	packets <- &RxPacket{
		payload: []byte{0x1, 0x1, 0x1, 0x1, 0x1, 0x2, 0x1, 0x1, 0x1, 0x3, 0x1, 0x2, 0x1, 0x4},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	actual := <-reports

	ReportsEq(
		&ReportMetricsMessage{
			sender:  0x2222,
			metrics: map[uint64]int64{1: 3, 2: 4},
		}, actual, t)
}

func TestMalformedMetricReport(t *testing.T) {
	packets := make(chan *RxPacket, 1)
	reports := make(chan *ReportMetricsMessage, 1)
	registrations := make(chan *RegisterMetricsMessage, 1)

	go HandleReceivedPackets(packets, reports, registrations)

	packets <- &RxPacket{
		payload: []byte{0x1},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	close(packets)

	report, ok := <-reports

	if report != nil || ok {
		t.Errorf("Got an unexpected report after processing garbage.")
	}
}

func TestUnsupportedProtocolVersion(t *testing.T) {
	packets := make(chan *RxPacket, 1)
	reports := make(chan *ReportMetricsMessage, 1)
	registrations := make(chan *RegisterMetricsMessage, 1)

	go HandleReceivedPackets(packets, reports, registrations)

	packets <- &RxPacket{
		payload: []byte{0x1, 0x2, 0x1, 0x1},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	close(packets)

	report, ok := <-reports

	if report != nil || ok {
		t.Errorf("Got an unexpected report when protocol version was too new.")
	}
}

func TestUnknownMethod(t *testing.T) {
	packets := make(chan *RxPacket, 1)
	reports := make(chan *ReportMetricsMessage, 1)
	registrations := make(chan *RegisterMetricsMessage, 1)

	go HandleReceivedPackets(packets, reports, registrations)

	packets <- &RxPacket{
		payload: []byte{0x1, 0x1, 0x1, 0xFF},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	close(packets)

	report, ok := <-reports

	if report != nil || ok {
		t.Errorf("Got an unexpected report when the method id was bogus.")
	}
}

func TestReadMessageAfterError(t *testing.T) {
	packets := make(chan *RxPacket, 1)
	reports := make(chan *ReportMetricsMessage, 1)
	registrations := make(chan *RegisterMetricsMessage, 1)

	go HandleReceivedPackets(packets, reports, registrations)

	packets <- &RxPacket{
		payload: []byte{0x1, 0x1, 0x1, 0xFF},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	packets <- &RxPacket{
		payload: []byte{0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0x1, 0xA, 0x1, 0xB},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	actual := <-reports

	ReportsEq(
		&ReportMetricsMessage{
			sender:  0x2222,
			metrics: map[uint64]int64{10: 11},
		}, actual, t)
}

func TestRegisterMetrics(t *testing.T) {
	packets := make(chan *RxPacket, 1)
	reports := make(chan *ReportMetricsMessage, 1)
	registrations := make(chan *RegisterMetricsMessage, 1)

	go HandleReceivedPackets(packets, reports, registrations)

	packets <- &RxPacket{
		payload: []byte{0x1, 0x1, 0x1, 0x2, 0x1, 0x1, 0x1, 0x3, 'F', 'O', 'O'},
		sender: 0x2222,
		rssi: 0x12,
		options: 0x00,
	}

	actual := <- registrations

	RegistrationsEq(
		&RegisterMetricsMessage{
			sender: 0x2222,
			metricNames: []string { "FOO" },
		}, actual, t)
}
