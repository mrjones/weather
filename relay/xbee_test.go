package main

import (
	"testing"
)

func TestEncodeDecordRxPacket(t *testing.T) {
	original := &RxPacket{
		payload: []byte{0x01, 0x02, 0x03, 0x04},
		sender:  0x2222,
		rssi:    0x33,
		options: 0x44,
	}

	result, err := ParseRxPacket(original.Serialize())
	AssertNoError(err, t)

	PacketsEq(original, result, t)
}
