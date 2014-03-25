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

type RxPacket struct {
	payload []byte
	sender  uint16
	rssi    uint8
	options byte
}

type TxPacket struct {
	payload     []byte
	destination uint16
	options     uint8
}

type PacketPair struct {
	ToDevice   chan *TxPacket
	FromDevice chan *RxPacket
}

func NewPacketPair(capacity int) *PacketPair {
	return &PacketPair{
		ToDevice:   make(chan *TxPacket, capacity),
		FromDevice: make(chan *RxPacket, capacity),
	}
}

type XbeeConnection struct {
	rxData      chan *RxPacket
	txData      chan *TxPacket
	readFrames  <-chan *XbeeFrame
	writeFrames chan<- *XbeeFrame
}

func NewXbeeConnection(readFrames <-chan *XbeeFrame, writeFrames chan<- *XbeeFrame) *XbeeConnection {
	connection := &XbeeConnection{
		rxData:      make(chan *RxPacket),
		txData:      make(chan *TxPacket),
		readFrames:  readFrames,
		writeFrames: writeFrames,
	}

	go connection.processIncomingFrames()

	return connection
}

func (x *XbeeConnection) IO() *PacketPair {
	return &PacketPair{
		FromDevice: x.rxData,
		ToDevice:   x.txData,
	}
}

func (d *RxPacket) DebugString() string {
	return fmt.Sprintf(
		"RSSI:    -%d dBm\n"+
			"Sender:  0x%x\n"+
			"Options: 0x%x\n"+
			"Payload: %s\n",
		d.rssi, d.sender, d.options, arrayAsHex(d.payload))
}

func (p *RxPacket) Serialize() []byte {
	plen := len(p.payload)

	data := make([]byte, plen+5)
	data[0] = RX_PACKET_16BIT
	data[1] = byte((p.sender >> 8) & 0xFF)
	data[2] = byte(p.sender & 0xFF)
	data[3] = byte(p.rssi)
	data[4] = p.options
	for i := 0; i < plen; i++ {
		data[5+i] = p.payload[i]
	}

	return data
}

func ParseRxPacket(data []byte) (*RxPacket, error) {
	if len(data) < 5 {
		return nil, fmt.Errorf("Malformed packet (too short, length = %d).", len(data))
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
	return packet, nil
}

func (x *XbeeConnection) processIncomingFrames() {
	for {
		select {
		case frame, ok := <-x.readFrames:
			if !ok {
				log.Printf("processIncomingFrames shutting down.")
				close(x.rxData)
				return
			}
			x.processIncomingFrame(frame)
		case txPacket, ok := <-x.txData:
			if !ok {
				log.Printf("processIncomingFrames shutting down.")
				close(x.txData)
				return
			}
			x.processOutgoingPacket(txPacket)
		}
	}
}

func (x *XbeeConnection) processOutgoingPacket(packet *TxPacket) {
	payload := make([]byte, len(packet.payload)+5)
	payload[0] = 0x01
	payload[1] = 0x00
	payload[2] = byte((packet.destination >> 8) & 0xFF)
	payload[3] = byte(packet.destination & 0xFF)
	payload[4] = packet.options
	for i := 0; i < len(packet.payload); i++ {
		payload[5+i] = packet.payload[i]
	}

	sum := uint8(0)
	for i := 0; i < len(payload); i++ {
		sum += uint8(payload[0])
	}

	frame := &XbeeFrame{
		length:   uint16(len(payload)), // TODO: check overflow
		payload:  payload,
		checksum: 0xFF - sum,
	}

	x.writeFrames <- frame
}

func (x *XbeeConnection) processIncomingFrame(frame *XbeeFrame) {
	data := frame.payload
	if data[0] == RX_PACKET_16BIT {
		packet, err := ParseRxPacket(data)
		if err != nil {
			fmt.Println(err)
		} else {
			x.rxData <- packet
		}
	} else {
		fmt.Printf("Unknown message type 0x%x: %s\n", data[0], arrayAsHex(data))
	}
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

	serial           *SerialPair
	currentFrame     *XbeeFrame
	framesFromDevice chan<- *XbeeFrame
	framesToDevice   <-chan *XbeeFrame
}

func NewRawXbeeDevice(serialPair *SerialPair, framesFromDevice chan<- *XbeeFrame, framesToDevice <-chan *XbeeFrame) *RawXbeeDevice {
	a := &RawXbeeDevice{
		serial:           serialPair,
		framesFromDevice: framesFromDevice,
		framesToDevice:   framesToDevice,
	}
	a.reset()
	go a.serialIoLoop()
	return a
}

func (x *RawXbeeDevice) Shutdown() {
	// TODO: implement me
}

func (a *RawXbeeDevice) serialIoLoop() {
	for {
		select {
		case buf := <-a.serial.FromDevice:
			a.consume(buf, 0, len(buf))
		case frame := <-a.framesToDevice:
			if len(frame.payload) != int(frame.length) {
				fmt.Printf("Malformed length field")
				continue
			}
			payload := make([]byte, frame.length+4)
			payload[0] = 0x7E
			payload[1] = byte((frame.length >> 8) & 0xFF)
			payload[2] = byte(frame.length & 0xFF)
			for i := uint16(0); i < frame.length; i++ {
				payload[3+i] = frame.payload[i]
			}
			payload[len(payload)-1] = frame.checksum
			a.serial.ToDevice <- payload
		}
	}
}

func (a *RawXbeeDevice) consume(data []byte, offset int, length int) error {
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
		a.framesFromDevice <- a.currentFrame
		a.reset()
	} else {
		fmt.Printf("Invalid checksum (0x%x)! Dropping frame.\n", v)
		a.reset()
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
