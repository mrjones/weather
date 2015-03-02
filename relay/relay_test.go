package main

import (
	"fmt"
	"testing"
)

func ReportsEq(expected, actual *ReportMetricsArg, t *testing.T) {
	if expected.reporterId != actual.reporterId {
		t.Errorf("ReportMetricsByNameMessages don't match in 'reporterId' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.reporterId, actual.reporterId)
	}

	if len(expected.metrics) != len(actual.metrics) {
		t.Errorf("ReportMetricsMessages don't match in 'len(metrics)' param.\nExpected: %d.\nActual: %d.", len(expected.metrics), len(actual.metrics))
	}

	for ek, ev := range expected.metrics {
		av := actual.metrics[ek]
		if ev != av {
			t.Errorf("ReportMetricsMessages don't match at metric with name '%s'.\nExpected %d\nActual: %d.", ek, ev, av)
		}
	}
}

func ErrorReportsEq(expected, actual *ReportErrorArg, t *testing.T) {
	if expected.errorMessage != actual.errorMessage {
		t.Errorf("ReportErrorArgs don't match in 'errorMessage' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.errorMessage, actual.errorMessage)
	}
}

func TestMetricReport(t *testing.T) {
	packets := NewPacketPair(0)
	reports := make(chan *ReportMetricsArg)
	errors := make(chan *ReportErrorArg)
	relay, err := NewRelay(packets, reports, errors)
	AssertNoError(err, t)

	packets.FromDevice <- &RxPacket{
		payload: []byte{
			0x1, 0x1, // Protocol Version
			0x1, 0x3, // Method ID
			0x2, 0x22, 0x22, // ReporterID
			0x1, 0x2, // Num metrics
			0x1, 0x3, // metric[0] name length
			'F', 'O', 'O', // metric[0] name
			0x1, 0x1, // metric[0] value
			0x1, 0x3, // metric[0] name length
			'b', 'a', 'r', // metric[0] name
			0x1, 0x2, // metric[1] value
		},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	report := <-reports

	ReportsEq(
		&ReportMetricsArg{
			reporterId: 0x2222,
			metrics:    map[string]int64{"FOO": 1, "bar": 2},
		}, report, t)

	relay.Shutdown()
}

func TestMalformedMetricReport(t *testing.T) {
	packets := NewPacketPair(0)
	reports := make(chan *ReportMetricsArg)
	errors := make(chan *ReportErrorArg)
	relay, err := NewRelay(packets, reports, errors)
	AssertNoError(err, t)

	packets.FromDevice <- &RxPacket{
		payload: []byte{0x1},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	close(packets.FromDevice)

	report, ok := <-reports

	if report != nil || ok {
		t.Errorf("Got an unexpected report after processing garbage.")
	}

	relay.Shutdown()
}

func TestUnsupportedProtocolVersion(t *testing.T) {
	packets := NewPacketPair(0)
	reports := make(chan *ReportMetricsArg)
	errors := make(chan *ReportErrorArg)
	relay, err := NewRelay(packets, reports, errors)
	AssertNoError(err, t)

	packets.FromDevice <- &RxPacket{
		payload: []byte{0x1, 0x2, 0x1, 0x1},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	close(packets.FromDevice)

	report, ok := <-reports

	if report != nil || ok {
		t.Errorf("Got an unexpected report when protocol version was too new.")
	}

	relay.Shutdown()
}

func TestUnknownMethod(t *testing.T) {
	packets := NewPacketPair(0)
	reports := make(chan *ReportMetricsArg)
	errors := make(chan *ReportErrorArg)
	relay, err := NewRelay(packets, reports, errors)
	AssertNoError(err, t)

	packets.FromDevice <- &RxPacket{
		payload: []byte{0x1, 0x1, 0x1, 0xFF},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	close(packets.FromDevice)

	report, ok := <-reports

	if report != nil || ok {
		t.Errorf("Got an unexpected report when the method id was bogus.")
	}

	relay.Shutdown()
}

func TestReadMessageAfterError(t *testing.T) {
	packets := NewPacketPair(0)
	reports := make(chan *ReportMetricsArg)
	errors := make(chan *ReportErrorArg)
	relay, err := NewRelay(packets, reports, errors)
	AssertNoError(err, t)

	packets.FromDevice <- &RxPacket{
		payload: []byte{0x1, 0x1, 0x1, 0xFF},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	packets.FromDevice <- &RxPacket{
		payload: []byte{
			0x1, 0x1, // Protocol Version
			0x1, 0x3, // Method ID
			0x2, 0x22, 0x22, // ReporterID
			0x1, 0x1, // Num metrics
			0x1, 0x3, // metric[0] name length
			'F', 'O', 'O', // metric[0] name
			0x1, 0xB, // metric[0] value
		},
		sender:  0x2222,
		rssi:    0x12,
		options: 0x00,
	}

	actual := <-reports

	ReportsEq(
		&ReportMetricsArg{
			reporterId: 0x2222,
			metrics:    map[string]int64{"FOO": 11},
		}, actual, t)

	relay.Shutdown()
}

// ===============

func TestParseAndSerializeReportMetricsArgs(t *testing.T) {
	original := &ReportMetricsArg{
		reporterId: 0xFFFF,
		metrics: map[string]int64{
			"foo": 255,
			"bar": 256,
		},
	}

	encoded, err := original.Serialize()
	AssertNoError(err, t)

	fmt.Printf("ENCODED: %s\n", arrayAsHex(encoded))

	result, err := ParseReportMetricsArg(encoded, 0)
	AssertNoError(err, t)
	ReportsEq(original, result, t)
}

func TestParseAndSerializeReportErrorArgs(t *testing.T) {
	original := &ReportErrorArg{
		reporterId:   0x12345,
		errorMessage: "Something went wrong!",
	}

	encoded, err := original.Serialize()
	AssertNoError(err, t)

	fmt.Printf("ENCODED: %s\n", arrayAsHex(encoded))

	result, err := ParseReportErrorArg(encoded, 0)
	AssertNoError(err, t)
	ErrorReportsEq(original, result, t)
}
