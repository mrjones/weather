package main

import (
	"bytes"
	"testing"
)

type FramePair struct {
	FromDevice chan *XbeeFrame
	ToDevice chan *XbeeFrame
}

func NewFramePair(n int) *FramePair {
	return &FramePair{
		FromDevice: make(chan *XbeeFrame, n),
		ToDevice: make(chan *XbeeFrame, n),
	}
}

func ArraysEq(expected, actual []byte, t *testing.T) {
	if bytes.Compare(expected, actual) != 0 {
		t.Errorf("Arrays don't match.\nExpected: '%s'.\nActual: '%s'.", arrayAsHex(expected), arrayAsHex(actual))
	}

}

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
	serial := NewSerialPair(0)
	frames := NewFramePair(0)
	device := NewRawXbeeDevice(serial.FromDevice, serial.ToDevice, frames.FromDevice, frames.ToDevice)

	serial.FromDevice <-[]byte{0x7e, 0x00, 0x03, 0x12, 0x34, 0x56, 0x63}

	actual := <-frames.FromDevice
	device.Shutdown()

	FramesEq(
		&XbeeFrame{
			length:   3,
			payload:  []byte{0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual, t)
}

func TestConsumeMessageByteByByte(t *testing.T) {
	serial := NewSerialPair(0)
	frames := NewFramePair(0)
	device := NewRawXbeeDevice(serial.FromDevice, serial.ToDevice, frames.FromDevice, frames.ToDevice)
	serial.FromDevice <- []byte{0x7e}
	serial.FromDevice <- []byte{0x00}
	serial.FromDevice <- []byte{0x03}
	serial.FromDevice <- []byte{0x12}
	serial.FromDevice <- []byte{0x34}
	serial.FromDevice <- []byte{0x56}
	serial.FromDevice <- []byte{0x63}

	actual := <-frames.FromDevice
	device.Shutdown()

	FramesEq(
		&XbeeFrame{
			length:   3,
			payload:  []byte{0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual, t)
}

func TestConsumeTwoMessages(t *testing.T) {
	serial := NewSerialPair(1)
	frames := NewFramePair(0)
	device := NewRawXbeeDevice(serial.FromDevice, serial.ToDevice, frames.FromDevice, frames.ToDevice)

	serial.FromDevice <- []byte{0x7e, 0x00, 0x03, 0x12, 0x34, 0x56, 0x63}
	serial.FromDevice <- []byte{0x7e, 0x00, 0x04, 0x12, 0x34, 0x56, 0x78, 0xEB}

	actual1 := <-frames.FromDevice

	FramesEq(
		&XbeeFrame{
			length:   3,
			payload:  []byte{0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual1, t)

	actual2 := <-frames.FromDevice
	device.Shutdown()

	FramesEq(
		&XbeeFrame{
			length:   4,
			payload:  []byte{0x12, 0x34, 0x56, 0x78},
			checksum: 0xEB,
		}, actual2, t)
}

func TestDropsBadChecksumMessageAndKeepsGoing(t *testing.T) {
	serial := NewSerialPair(0)
	frames := NewFramePair(0)
	device := NewRawXbeeDevice(serial.FromDevice, serial.ToDevice, frames.FromDevice, frames.ToDevice)

	serial.FromDevice <- []byte{0x7e, 0x00, 0x01, 0x99, 0x00}
	serial.FromDevice <- []byte{0x7e, 0x00, 0x03, 0x12, 0x34, 0x56, 0x63}

	actual := <-frames.FromDevice
	device.Shutdown()

	FramesEq(
		&XbeeFrame{
			length:   3,
			payload:  []byte{0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual, t)
}

func TestToDevicesFrames(t *testing.T) {
	serial := NewSerialPair(0)
	frames := NewFramePair(0)
	device := NewRawXbeeDevice(serial.FromDevice, serial.ToDevice, frames.FromDevice, frames.ToDevice)

	frames.ToDevice <- &XbeeFrame{
		length: 3,
		payload: []byte{0x12, 0x34, 0x56},
		checksum: 0x63,
	}

	actual := <- serial.ToDevice
	device.Shutdown()

	ArraysEq([]byte{0x7E, 0x00, 0x03, 0x12, 0x34, 0x56, 0x63}, actual, t)
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
	frames := NewFramePair(0)
	conn := NewXbeeConnection(frames.FromDevice, frames.ToDevice)

	frames.FromDevice <- &XbeeFrame{
		length:   8,
		payload:  []byte{0x81, 0x22, 0x22, 0x28, 0x01, 0x12, 0x34, 0x56},
		checksum: 0x00}

	actual := <-conn.RxData()

	PacketsEq(
		&RxPacket{
			sender:  0x2222,
			payload: []byte{0x12, 0x34, 0x56},
			rssi:    0x28,
			options: 0x01,
		}, actual, t)
}

func TestMalformedRxFrame_TooShort(t *testing.T) {
	frames := NewFramePair(0)
	conn := NewXbeeConnection(frames.FromDevice, frames.ToDevice)

	frames.FromDevice <- &XbeeFrame{
		length:   4,
		payload:  []byte{0x81, 0x12, 0x34, 0x56},
		checksum: 0x00}

	close(frames.FromDevice)

	packet, ok := <-conn.RxData()

	if packet != nil {
		t.Errorf("Got an unexpected packet while processing a malformed frame")
	}

	if ok {
		t.Errorf("Got an unexpected packet while processing a malformed frame")
	}
}

func TestIgnoresUnknownFrames(t *testing.T) {
	frames := NewFramePair(0)
	conn := NewXbeeConnection(frames.FromDevice, frames.ToDevice)

	frames.FromDevice <- &XbeeFrame{
		length:   4,
		payload:  []byte{0xEE, 0x12, 0x34, 0x56},
		checksum: 0x00}

	close(frames.FromDevice)

	packet, ok := <-conn.RxData()

	if packet != nil {
		t.Errorf("Got an unexpected packet after reading a garbage frame")
	}

	if ok {
		t.Errorf("Got an unexpected packet after reading a garbage frame")
	}
}

func TestTransmitPacket(t *testing.T) {
	frames := NewFramePair(0)
	conn := NewXbeeConnection(frames.FromDevice, frames.ToDevice)

	packet := &TxPacket {
		payload: []byte{0x12, 0x34, 0x56},
		destination: 0x2222,
		options: 0x01,  // Disable ACK
	}

	conn.TxData() <- packet

	actual := <- frames.ToDevice

	FramesEq(
		&XbeeFrame{
			length: 8,
			payload: []byte{0x01, 0x00, 0x22, 0x22, 0x01, 0x12, 0x34, 0x56},
			checksum: 0xF7,
		}, actual, t)
}


// ===============

func ReportsEq(expected, actual *ReportMetricsByNameRequest, t *testing.T) {
	if expected.sender != actual.sender {
		t.Errorf("ReportMetricsByNameMessages don't match in 'sender' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.sender, actual.sender)
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

func TestMetricReport(t *testing.T) {
	packets := NewPacketPair(0)
	reports := make(chan *ReportMetricsByNameRequest)
	relay, err := NewRelay(packets, reports)
	AssertNoError(err, t)

	packets.FromDevice <- &RxPacket{
		payload: []byte{
			0x1, 0x1, // Protocol Version
			0x1, 0x3, // Method ID
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
		&ReportMetricsByNameRequest{
			sender:  0x2222,
			metrics: map[string]int64{"FOO": 1, "bar": 2},
		}, report, t)

	relay.Shutdown()
}

func TestMalformedMetricReport(t *testing.T) {
	packets := NewPacketPair(0)
	reports := make(chan *ReportMetricsByNameRequest)
	relay, err := NewRelay(packets, reports)
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
	reports := make(chan *ReportMetricsByNameRequest)
	relay, err := NewRelay(packets, reports)
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
	reports := make(chan *ReportMetricsByNameRequest)
	relay, err := NewRelay(packets, reports)
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
	reports := make(chan *ReportMetricsByNameRequest)
	relay, err := NewRelay(packets, reports)
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
		&ReportMetricsByNameRequest{
			sender:  0x2222,
			metrics: map[string]int64{"FOO": 11},
		}, actual, t)

	relay.Shutdown()
}
