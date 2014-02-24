package main

import (
	"fmt"
)

const (
	FRAME_DELIMITER = 0x7E
	ESCAPE          = 0x7D

	LENGTH_BYTES = 2

	CHECKSUM_LENGTH = 1
	RX_PACKET_16BIT = 0x81

	AT_COMMAND = 0x08
)

const (
	STATE_OUT_OF_SYNC           = iota
	STATE_FOUND_FRAME_DELIMITER = iota
	STATE_PARSED_LENGTH         = iota
	STATE_CONSUMED_PAYLOAD      = iota
	STATE_MESSAGE_DONE          = iota
)

type RxPacket struct {
	payload []byte
	sender  uint16
	rssi    uint8
	options byte
}

func (d *RxPacket) DebugString() string {
	return fmt.Sprintf(
		"RSSI:    -%d dBm\n"+
			"Sender:  0x%x\n"+
			"Options: 0x%x\n"+
			"Payload: %s\n",
		d.rssi, d.sender, d.options, arrayAsHex(d.payload))
}

type XbeeFrame struct {
	length   uint16
	payload  []byte
	checksum uint8
}

func NewXbeeFrame(payload []byte) *XbeeFrame {
	length := len(payload)
	sum := uint8(0)
	for i := 0; i < length; i++ {
		sum = (sum + uint8(payload[i])) & 0xFF
	}
	checksum := 0xFF - sum

	return &XbeeFrame{
		length:   uint16(length),
		payload:  payload,
		checksum: checksum,
	}
}

func (f *XbeeFrame) Serialize() []byte {
	data := make([]byte, f.length+4)
	data[0] = 0x7E
	data[1] = byte(f.length >> 8)
	data[2] = byte(f.length & 0xFF)
	for i := uint16(0); i < f.length; i++ {
		data[3+i] = f.payload[i]
	}
	data[f.length+3] = f.checksum
	return data
}

type Accum struct {
	state                int
	bytesConsumedInState uint16

	currentFrame *XbeeFrame
	frameSink    chan<- *XbeeFrame
}

func NewAccum(frameSink chan<- *XbeeFrame) *Accum {
	a := &Accum{frameSink: frameSink}
	a.reset()
	return a
}

func (a *Accum) Consume(data []byte, offset int, length int) error {
	if offset+length > len(data) {
		return fmt.Errorf("Can't consume bytes [%d,%d). Array length is %d.",
			offset, offset+length, len(data))
	}

	fmt.Println("CONSUME: " + arrayAsHexWithLen(data, length))

	for {
		var err error
		n := 0
		switch a.state {
		case STATE_OUT_OF_SYNC:
			n, err = a.getSync(data, offset, length)
		case STATE_FOUND_FRAME_DELIMITER:
			n, err = a.parseLength(data, offset, length)
		case STATE_PARSED_LENGTH:
			n, err = a.copyPayload(data, offset, length)
		case STATE_CONSUMED_PAYLOAD:
			n, err = a.verifyChecksum(data, offset, length)
		default:
			return fmt.Errorf("Internal Error. Unknown state: %d", a.state)
		}

		if err != nil {
			return err
		}

		offset += n

		if offset == length {
			return nil
		} else if offset > length {
			return fmt.Errorf("Internal Error. Buffer overrun (%d > %d)",
				offset, length)
		}
	}
}

func (a *Accum) transition(state int) {
	fmt.Printf("Transitioning to %d\n", state)
	a.state = state
	a.bytesConsumedInState = 0
}

func (a *Accum) reset() {
	a.currentFrame = &XbeeFrame{length: 0, checksum: 0}
	a.transition(STATE_OUT_OF_SYNC)
}

func (a *Accum) verifyChecksum(data []byte, offset int, len int) (int, error) {
	a.currentFrame.checksum = uint8(data[offset])

	v := 0
	for i := uint16(0); i < a.currentFrame.length; i++ {
		v = (v + int(a.currentFrame.payload[i])) & 0xFF
	}

	v = (v + int(a.currentFrame.checksum)) & 0xFF

	if v == 0xFF {
		fmt.Println("Valid checksum! Sending frame")
		a.frameSink <- a.currentFrame
		a.reset()
	} else {
		return 1, fmt.Errorf("Invalid checksum: 0x%x", v)
	}

	return 1, nil
}

func (a *Accum) copyPayload(data []byte, offset int, len int) (int, error) {
	if a.bytesConsumedInState == 0 {
		a.currentFrame.payload = make([]byte, a.currentFrame.length)
	}

	consumed := 0
	for offset < len && a.bytesConsumedInState < a.currentFrame.length {
		a.currentFrame.payload[a.bytesConsumedInState] = data[offset]
		offset++
		a.bytesConsumedInState++
		consumed++
	}

	if a.bytesConsumedInState == a.currentFrame.length {
		a.transition(STATE_CONSUMED_PAYLOAD)
	}

	return consumed, nil
}

func (a *Accum) parseLength(data []byte, offset int, len int) (int, error) {
	a.currentFrame.length = (a.currentFrame.length << 8) + uint16(data[offset])
	a.bytesConsumedInState++

	if a.bytesConsumedInState == LENGTH_BYTES {
		a.transition(STATE_PARSED_LENGTH)
	}

	return 1, nil
}

func (a *Accum) getSync(data []byte, offset int, len int) (int, error) {
	if data[offset] == FRAME_DELIMITER {
		a.transition(STATE_FOUND_FRAME_DELIMITER)
	}

	return 1, nil
}
