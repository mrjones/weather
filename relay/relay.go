// ftp://ftp1.digi.com/support/documentation/90000982_A.pdf
// stty -F /dev/ttyAMA0 9600 crtscts
package main

import (
	"container/list"
	"fmt"
	"log"
	"os"
	"syscall"
  "unsafe"
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

type Accum struct {
	state                int
	bytesConsumedInState int
	payloadLength        int
	payload              []byte
	checksum             int

	messages *list.List
}

func NewAccum() *Accum {
	a := &Accum{messages: list.New()}
	a.reset()
	return a
}

func (a *Accum) MessagesAvailable() int {
	return a.messages.Len()
}

func (a *Accum) Pop() []byte {
	e := a.messages.Front()
	a.messages.Remove(e)
	return e.Value.([]byte)
}

func (a *Accum) Consume(data []byte, offset int, len int) error {
	if offset >= len {
		return nil
	}

	for {
		var err error
		n := 0
		switch a.state {
		case STATE_OUT_OF_SYNC:
			n, err = a.getSync(data, offset, len)
		case STATE_FOUND_FRAME_DELIMITER:
			n, err = a.parseLength(data, offset, len)
		case STATE_PARSED_LENGTH:
			n, err = a.copyPayload(data, offset, len)
		case STATE_CONSUMED_PAYLOAD:
			n, err = a.verifyChecksum(data, offset, len)
		default:
			return fmt.Errorf("Internal Error. Unknown state: %d", a.state)
		}

		if err != nil {
			return err
		}

		offset += n

		if offset == len {
			return nil
		} else if offset > len {
			return fmt.Errorf("Internal Error. Buffer overrun (%d > %d)",
				offset, len)
		}
	}
}

func (a *Accum) transition(state int) {
	a.state = state
	a.bytesConsumedInState = 0
}

func (a *Accum) reset() {
	a.payloadLength = 0
	a.checksum = 0
	a.transition(STATE_OUT_OF_SYNC)
}

func arrayAsHex(a []byte) string {
	s := "[ "
	for i := 0; i < len(a); i++ {
		s += fmt.Sprintf("0x%x ", a[i])
	}
	s += "]"
	return s
}

func (a *Accum) verifyChecksum(data []byte, offset int, len int) (int, error) {
	a.checksum = int(data[offset])

	v := 0
	for i := 0; i < a.payloadLength; i++ {
		v = (v + int(a.payload[i])) & 0xFF
	}

	v = (v + a.checksum) & 0xFF

	if v == 0xFF {
		a.messages.PushBack(a.payload)
		a.reset()
	} else {
		return 1, fmt.Errorf("Invalid checksum: 0x%x", v)
	}

	return 1, nil
}

func (a *Accum) copyPayload(data []byte, offset int, len int) (int, error) {
	if a.bytesConsumedInState == 0 {
		a.payload = make([]byte, a.payloadLength)
	}

	consumed := 0
	for offset < len && a.bytesConsumedInState < a.payloadLength {
		a.payload[a.bytesConsumedInState] = data[offset]
		offset++
		a.bytesConsumedInState++
		consumed++
	}

	if a.bytesConsumedInState == a.payloadLength {
		a.transition(STATE_CONSUMED_PAYLOAD)
	}

	return consumed, nil
}

func (a *Accum) parseLength(data []byte, offset int, len int) (int, error) {
	a.payloadLength = (a.payloadLength << 8) + int(data[offset])
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

type Frame struct {
	length   uint16
	payload  []byte
	checksum uint8
}

func NewFrame(payload []byte) *Frame {
	length := len(payload)
	sum := uint8(0)
	for i := 0; i < length; i++ {
		sum = (sum + uint8(payload[i])) & 0xFF
	}
	checksum := 0xFF - sum

	return &Frame{
		length:   uint16(length),
		payload:  payload,
		checksum: checksum,
	}
}

func (f *Frame) Serialize() []byte {
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

func main() {
	serialPort := "/dev/ttyAMA0"

	file, err := os.OpenFile(serialPort, syscall.O_RDWR|syscall.O_NOCTTY, 0666)
	if err != nil {
		log.Fatal(err)
	}

	log.Printf("Opened '%s'\n", serialPort)

	fd := file.Fd()
	t := syscall.Termios{
		Iflag:  0,
		Cflag:  syscall.CS8 | syscall.CREAD | syscall.CLOCAL | syscall.B9600,
		Cc:     [32]uint8{syscall.VMIN: 1},
		Ispeed: syscall.B9600,
		Ospeed: syscall.B9600,
	}

	if _, _, errno := syscall.Syscall6(
		syscall.SYS_IOCTL,
		uintptr(fd),
		uintptr(syscall.TCSETS),
		uintptr(unsafe.Pointer(&t)),
		0,
		0,
		0,
	); errno != 0 {
		log.Fatalf("Errno configuring serial port: %d\n", errno)
	}

	buf := make([]byte, 128)
	accum := NewAccum()

	// ND doesn't work
	/*
		f := NewFrame([]byte{AT_COMMAND, 0x52, 'V', 'R'})
		log.Printf("Framer %s\n", arrayAsHex(f.Serialize()))

		wn, err := file.Write(f.Serialize())
		if err != nil {
			log.Println(err)
		} else {
			log.Printf("Write %d bytes\n", wn)
		}
	*/
	for {
		n, err := file.Read(buf)
		log.Printf("Read %d bytes\n", n)
		if err != nil {
			log.Fatal(err)
		}

		err = accum.Consume(buf, 0, n)
		if err != nil {
			log.Fatal(err)
		}

		for accum.MessagesAvailable() > 0 {
			data := accum.Pop()

			if data[0] == RX_PACKET_16BIT {
				senderAddr := (int(data[1]) << 8) + int(data[2])
				strength := int(data[3])
				log.Printf("RSSI:    -%d dBm\n", strength)
				log.Printf("Sender:  0x%x\n", senderAddr)
				payloadLength := len(data) - 4
				var payload = make([]byte, payloadLength)
				for i := 0; i < payloadLength; i++ {
					payload[i] = data[i+4]
				}

				log.Printf("Payload: %s\n", arrayAsHex(payload))
			} else {
				fmt.Printf("Unknown message type 0x%x: %s\n", data[0], arrayAsHex(data))
			}
		}
	}
}
