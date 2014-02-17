// stty -F /dev/ttyAMA0 9600 crtscts
package main

import (
	"fmt"
  "log"
	"os"
)

const (
	FRAME_DELIMITER = 0x7E
	ESCAPE = 0x7D
	
	LENGTH_BYTES = 2
	
	CHECKSUM_LENGTH = 1
	RX_PACKET_16BIT = 0x81
)

const (
	STATE_OUT_OF_SYNC = iota
	STATE_FOUND_FRAME_DELIMITER = iota
	STATE_PARSED_LENGTH = iota
	STATE_CONSUMED_PAYLOAD = iota
	STATE_MESSAGE_DONE = iota
)

type Accum struct {
	state int
	bytesConsumedInState int
	payloadLength int
	payload []byte
	checksum int
}

func NewAccum() *Accum {
	a := &Accum{}
	a.reset()
	return a
}

func (a* Accum) Consume(data []byte, offset int, len int) error {
	if offset >= len {
		return nil
	}

	switch a.state {
	case STATE_OUT_OF_SYNC:
		return a.GetSync(data, offset, len)
	case STATE_FOUND_FRAME_DELIMITER:
		return a.ParseLength(data, offset, len)
	case STATE_PARSED_LENGTH:
		return a.ConsumePayload(data, offset, len)
	case STATE_CONSUMED_PAYLOAD:
		return a.VerifyChecksum(data, offset, len)
	default:
		return fmt.Errorf("Internal Error. Unknown state: %d", a.state)
	}
}

func (a* Accum) transition(state int) {
	a.state = state
	fmt.Printf("IN STATE %d\n", a.state)
	a.bytesConsumedInState = 0
}

func (a* Accum) reset() {
	a.payloadLength = 0
	a.checksum = 0
	a.transition(STATE_OUT_OF_SYNC)
}

func printHexArray(a []byte) {
	fmt.Printf("[ ")
	for i := 0; i < len(a); i++ {
		fmt.Printf("0x%x ", a[i])
	}
	fmt.Printf("]")
}

func (a* Accum) VerifyChecksum(data []byte, offset int, len int) error {
	a.checksum = int(data[offset])

	v := 0;
	for i := 0; i < a.payloadLength; i++ {
		v = (v + int(a.payload[i])) & 0xFF;
	}

	v = (v + a.checksum) & 0xFF

	if v == 0xFF {
		fmt.Printf("Consumed message: ")
		printHexArray(a.payload)
		fmt.Println("")
		a.reset()
	} else {
		return fmt.Errorf("Invalid checksum: 0x%x", v)
	}

	return nil
}

func (a* Accum) ConsumePayload(data []byte, offset int, len int) error {
	if a.bytesConsumedInState == 0 {
		a.payload = make([]byte, a.payloadLength)
	}

	for ; offset < len && a.bytesConsumedInState < a.payloadLength; {
		a.payload[a.bytesConsumedInState] = data[offset]
		offset++
		a.bytesConsumedInState++
	}

	if a.bytesConsumedInState == a.payloadLength {
		a.transition(STATE_CONSUMED_PAYLOAD)
	}

	return a.Consume(data, offset, len)
}

func (a* Accum) ParseLength(data []byte, offset int, len int) error {
	a.payloadLength = (a.payloadLength << 8) + int(data[offset])
	a.bytesConsumedInState++

	if a.bytesConsumedInState == LENGTH_BYTES {
		a.transition(STATE_PARSED_LENGTH)
	}
	return a.Consume(data, offset + 1, len)
}

func (a* Accum) GetSync(data []byte, offset int, len int) error {
	if data[offset] == FRAME_DELIMITER {
		a.transition(STATE_FOUND_FRAME_DELIMITER)
	}
	return a.Consume(data, offset + 1, len)
}



func main() {
	file, err := os.Open("/dev/ttyAMA0")
	if err != nil {
		log.Fatal(err);
	}

	inSync := false
	buf := make([]byte, 128)
	bytesLeft := -1
	data := make([]byte, 65535)
	dataPtr := 0
	bufPtr := 0
	localChecksum := 0
	length := 0

	accum := NewAccum();

	for {
		n, err := file.Read(buf)
		if err != nil {
			log.Fatal(err)
		}
		err = accum.Consume(buf, 0, n)
		if err != nil {
			log.Fatal(err)
		}
		log.Printf("read %d bytes: %q\n", n, buf[:n])

		code := buf[0];
		if inSync {
			bufPtr = 0
		} else {
			if code == FRAME_DELIMITER && n >= 3 {
				inSync = true
				length = (int(buf[1]) << 8) + int(buf[2]);
				log.Printf("Code: 0x%x %d\n", buf[0], code);
				log.Printf("Length: 0x%x%x %d\n", buf[1], buf[2], length);
				bytesLeft = length + CHECKSUM_LENGTH
				dataPtr = 0
				bufPtr = 3
				localChecksum = 0
			}
		}

		if inSync {
			for ; bufPtr < n && bytesLeft > CHECKSUM_LENGTH; bufPtr++ {
				log.Printf("Copying: 0x%x %d of %d (%d)\n", buf[bufPtr], bufPtr, n, bytesLeft)
				data[dataPtr] = buf[bufPtr]
				bytesLeft--
				dataPtr++
				localChecksum += int(buf[bufPtr])
			}
			if bytesLeft == CHECKSUM_LENGTH {
				log.Printf("Checksum: 0x%x %d of %d\n", buf[bufPtr], bufPtr, n)
				localChecksum += int(buf[bufPtr])
				inSync = false
				log.Printf("Checksum verified: 0x%x\n", (localChecksum & 0xFF))

				log.Printf("Received message of type: 0x%x\n", data[0])
				if (data[0] == RX_PACKET_16BIT) {
					senderAddr := (int(data[1]) << 8) + int(data[2])
					strength := int(data[3])
						log.Printf("RSSI: -%d dBm\n", strength)
					log.Printf("Received message from: 0x%x%x %d\n", data[1], data[2], senderAddr)
					payloadLength := length - 4
					fmt.Printf("Payload: 0x")
					for i := 0; i < payloadLength; i++ {
						fmt.Printf("%d ", data[i + 4])
					}
					fmt.Println("")
				}
			}
		} else {
			log.Printf("ignoring message. not in sync")
		}
	}
}

