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
			length: 3,
			payload: []byte {0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual, t);
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
			length: 3,
			payload: []byte {0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual, t);
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
			length: 3,
			payload: []byte {0x12, 0x34, 0x56},
			checksum: 0x63,
		}, actual1, t);

	actual2 := <-output

	FramesEq(
		&XbeeFrame{
			length: 4,
			payload: []byte {0x12, 0x34, 0x56, 0x78},
			checksum: 0xEB,
		}, actual2, t);
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

func PacketsEq(expected, actual *DataPacket, t *testing.T) {
	if expected.sender != actual.sender {
		t.Errorf("DataPackets don't match in 'sender' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.sender, actual.sender)
	}
	
	if bytes.Compare(expected.payload, actual.payload) != 0 {
		t.Errorf("DataPackets don't match in 'payload' param.\nExpected: '%s'.\nActual: '%s'.", arrayAsHex(expected.payload), arrayAsHex(actual.payload))
	}

	if expected.rssi != actual.rssi {
		t.Errorf("DataPackets don't match in 'rssi' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.rssi, actual.rssi)
	}

	if expected.options != actual.options {
		t.Errorf("DataPackets don't match in 'options' param.\nExpected: 0x%x.\nActual: 0x%x.", expected.options, actual.options)
	}
}

func TestConsumeRxFrame(t *testing.T) {
	frames := make(chan *XbeeFrame, 1)
	rxPackets := make(chan *DataPacket, 1)

	go ConsumeXbeeFrames(frames, rxPackets)

	frames <- &XbeeFrame{
		length: 8,
		payload: []byte{0x81, 0x22, 0x22, 0x28, 0x01, 0x12, 0x34, 0x56},
		checksum: 0x00}

	actual := <- rxPackets

	PacketsEq(
		&DataPacket{
			sender: 0x2222,
			payload: []byte{0x12, 0x34, 0x56},
			rssi: 0x28,
			options: 0x01,
		}, actual, t)
}

func TestMalformedRxFrame_TooShort(t *testing.T) {
	frames := make(chan *XbeeFrame, 1)
	rxPackets := make(chan *DataPacket, 1)

	go ConsumeXbeeFrames(frames, rxPackets)

	frames <- &XbeeFrame{
		length: 4,
		payload: []byte{0x81, 0x12, 0x34, 0x56},
		checksum: 0x00}

	close(frames)

	packet, ok := <- rxPackets

	if packet != nil {
		t.Errorf("Got an unexpected packet while processing a malformed frame")
	}

	if ok {
		t.Errorf("Got an unexpected packet while processing a malformed frame")
	}
}

func TestIgnoresUnknownFrames(t *testing.T) {
	frames := make(chan *XbeeFrame, 1)
	rxPackets := make(chan *DataPacket, 1)

	go ConsumeXbeeFrames(frames, rxPackets)

	frames <- &XbeeFrame{
		length: 4,
		payload: []byte{0xEE, 0x12, 0x34, 0x56},
		checksum: 0x00}

	close(frames)

	packet, ok := <- rxPackets

	if packet != nil {
		t.Errorf("Got an unexpected packet after reading a garbage frame")
	}

	if ok {
		t.Errorf("Got an unexpected packet after reading a garbage frame")
	}
}
