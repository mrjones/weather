// stty -F /dev/ttyAMA0 9600 crtscts
package main

import (
	"container/list"
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

	messages *list.List
}

func NewAccum() *Accum {
	a := &Accum{messages: list.New()}
	a.reset()
	return a
}

func (a *Accum) MessagesAvailable() (int) {
	return a.messages.Len()
}

func (a *Accum) Pop() ([]byte) {
	e := a.messages.Front();
	a.messages.Remove(e)
	return e.Value.([]byte)
}

func (a* Accum) Consume(data []byte, offset int, len int) error {
	if offset >= len {
		return nil
	}

	for {
		var err error
		n := 0
		switch a.state {
		case STATE_OUT_OF_SYNC:
			n, err = a.GetSync(data, offset, len)
		case STATE_FOUND_FRAME_DELIMITER:
			n, err = a.ParseLength(data, offset, len)
		case STATE_PARSED_LENGTH:
			n, err = a.ConsumePayload(data, offset, len)
		case STATE_CONSUMED_PAYLOAD:
			n, err = a.VerifyChecksum(data, offset, len)
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

func (a* Accum) VerifyChecksum(data []byte, offset int, len int) (int, error) {
	a.checksum = int(data[offset])

	v := 0;
	for i := 0; i < a.payloadLength; i++ {
		v = (v + int(a.payload[i])) & 0xFF;
	}

	v = (v + a.checksum) & 0xFF

	if v == 0xFF {
		fmt.Printf("Consumed message: ")
		a.messages.PushBack(a.payload)
		a.reset()
	} else {
		return 1, fmt.Errorf("Invalid checksum: 0x%x", v)
	}

	return 1, nil
}

func (a* Accum) ConsumePayload(data []byte, offset int, len int) (int, error) {
	if a.bytesConsumedInState == 0 {
		a.payload = make([]byte, a.payloadLength)
	}

	consumed := 0
	for ; offset < len && a.bytesConsumedInState < a.payloadLength; {
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

func (a* Accum) ParseLength(data []byte, offset int, len int) (int, error) {
	a.payloadLength = (a.payloadLength << 8) + int(data[offset])
	a.bytesConsumedInState++

	if a.bytesConsumedInState == LENGTH_BYTES {
		a.transition(STATE_PARSED_LENGTH)
	}

	return 1, nil
}

func (a* Accum) GetSync(data []byte, offset int, len int) (int, error) {
	if data[offset] == FRAME_DELIMITER {
		a.transition(STATE_FOUND_FRAME_DELIMITER)
	}

	return 1, nil
}



func main() {
	file, err := os.Open("/dev/ttyAMA0")
	if err != nil {
		log.Fatal(err);
	}


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

		for accum.MessagesAvailable() > 0 {
			data := accum.Pop()
			fmt.Printf("Popped: ")
			printHexArray(data)
			fmt.Println("")

			if (data[0] == RX_PACKET_16BIT) {
				senderAddr := (int(data[1]) << 8) + int(data[2])
				strength := int(data[3])
				log.Printf("RSSI: -%d dBm\n", strength)
				log.Printf("Received message from: 0x%x%x %d\n", data[1], data[2], senderAddr)
				payloadLength := len(data) - 4
				fmt.Printf("Payload: 0x")
				for i := 0; i < payloadLength; i++ {
					fmt.Printf("%d ", data[i + 4])
				}
				fmt.Println("")
			}
		}
	}
}

