package main

import (
	"fmt"
	"log"
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

type XbeeConnection struct {
	rxData chan *RxPacket
	xbeeFrames chan *XbeeFrame
}

func NewXbeeConnection(xbeeFrames chan *XbeeFrame) *XbeeConnection {
	connection := &XbeeConnection{
		rxData: make(chan *RxPacket),
		xbeeFrames: xbeeFrames,
	}

	go connection.processIncomingFrames()

	return connection
}

func (x *XbeeConnection) processIncomingFrames() {
	for {
		frame, ok := <-x.xbeeFrames
		if !ok {
			log.Printf("processIncomingFrames shutting down.")
			close(x.rxData)
			return
		}

		data := frame.payload
		if data[0] == RX_PACKET_16BIT {
			if len(data) < 5 {
				log.Printf("Malformed packet (too short, length = %d).", len(data))
				continue
			}
			senderAddr := (uint16(data[1]) << 8) + uint16(data[2])
			payloadLength := len(data) - 5
			var payload = make([]byte, payloadLength)
			for i := 0; i < payloadLength; i++ {
				payload[i] = data[i+5]
			}

			packet := &RxPacket{
				payload: payload,
				sender:  senderAddr,
				rssi:    uint8(data[3]),
				options: data[4],
			}
			log.Printf("%s", packet.DebugString())
			x.rxData <- packet
		} else {
			fmt.Printf("Unknown message type 0x%x: %s\n", data[0], arrayAsHex(data))
		}
	}
}

func (x *XbeeConnection) RxData() <-chan *RxPacket {
	return x.rxData
}

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

type RawXbeeDevice struct {
	state                int
	bytesConsumedInState uint16

	serial       chan []byte
	currentFrame *XbeeFrame
	frameSink    chan<- *XbeeFrame
}

func NewRawXbeeDevice(serial chan []byte, frameSink chan<- *XbeeFrame) *RawXbeeDevice {
	a := &RawXbeeDevice{frameSink: frameSink, serial: serial}
	a.reset()
	go a.readChannelInLoop()
	return a
}

func (x *RawXbeeDevice) Shutdown() {
	// TODO: implement me
}

func (a *RawXbeeDevice) readChannelInLoop() {
	for {
		buf := <- a.serial
		a.Consume(buf, 0, len(buf))
	}
}

/*
func (a *RawXbeeDevice) serialIoLoop() {
	buf := make([]byte, 128)

	for {
		n, err := a.serial.Read(buf)
		log.Printf("Read %d bytes\n", n)
		if err != nil {
			log.Fatal(err)
		}

		err = a.Consume(buf, 0, n)
		if err != nil {
			log.Fatal(err)
		}
	}
}
*/

func (a *RawXbeeDevice) Consume(data []byte, offset int, length int) error {
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

func (a *RawXbeeDevice) transition(state int) {
	fmt.Printf("Transitioning to %d\n", state)
	a.state = state
	a.bytesConsumedInState = 0
}

func (a *RawXbeeDevice) reset() {
	a.currentFrame = &XbeeFrame{length: 0, checksum: 0}
	a.transition(STATE_OUT_OF_SYNC)
}

func (a *RawXbeeDevice) verifyChecksum(data []byte, offset int, len int) (int, error) {
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

func (a *RawXbeeDevice) copyPayload(data []byte, offset int, len int) (int, error) {
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

func (a *RawXbeeDevice) parseLength(data []byte, offset int, len int) (int, error) {
	a.currentFrame.length = (a.currentFrame.length << 8) + uint16(data[offset])
	a.bytesConsumedInState++

	if a.bytesConsumedInState == LENGTH_BYTES {
		a.transition(STATE_PARSED_LENGTH)
	}

	return 1, nil
}

func (a *RawXbeeDevice) getSync(data []byte, offset int, len int) (int, error) {
	if data[offset] == FRAME_DELIMITER {
		a.transition(STATE_FOUND_FRAME_DELIMITER)
	}

	return 1, nil
}
