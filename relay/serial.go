package main

import (
	"log"
	"os"
	"syscall"
	"unsafe"
)

type SerialPair struct {
	FromDevice chan []byte
	ToDevice   chan []byte
}

func NewSerialPair(n int) *SerialPair {
	return &SerialPair{
		FromDevice: make(chan []byte, n),
		ToDevice:   make(chan []byte, n),
	}
}

type SerialChannel struct {
	underlyingFile *os.File
	readChannel    chan []byte
	writeChannel   chan []byte
}

func NewSerialChannel(filename string) (*SerialChannel, error) {
	file, err := os.OpenFile(filename, syscall.O_RDWR|syscall.O_NOCTTY, 0666)
	if err != nil {
		return nil, err
	}

	configureSerial(file)
	sc := &SerialChannel{
		underlyingFile: file,
		readChannel:    make(chan []byte),
		writeChannel:   make(chan []byte),
	}
	go sc.readLoop()
	return sc, nil
}

func (sc *SerialChannel) Pair() *SerialPair {
	return &SerialPair{
		FromDevice: sc.readChannel,
		ToDevice:   sc.writeChannel,
	}
}

func (sc *SerialChannel) ReadChannel() <-chan []byte {
	return sc.readChannel
}

func (sc *SerialChannel) WriteChannel() chan<- []byte {
	return sc.writeChannel
}

func (sc *SerialChannel) readLoop() {
	buf := make([]byte, 1024)
	for {
		n, err := sc.underlyingFile.Read(buf)
		if err != nil {
			log.Println(err)
			continue
		}
		if n > 0 {
			sc.readChannel <- buf[:n]
		}
	}
}

func (sc *SerialChannel) writeLoop() {
	// TODO: implement me
}

// ----

type SerialConnection struct {
	file *os.File
}

func (s *SerialConnection) Read(p []byte) (n int, err error) {
	return s.file.Read(p)
}

func (s *SerialConnection) Write(p []byte) (n int, err error) {
	return s.file.Write(p)
}

func NewSerialConnection(name string) (*SerialConnection, error) {
	file, err := os.OpenFile(
		name, syscall.O_RDWR|syscall.O_NOCTTY, 0666)

	if err != nil {
		return nil, err
	}
	configureSerial(file)

	log.Printf("Opened '%s'\n", name)

	return &SerialConnection{file: file}, nil
}

func configureSerial(file *os.File) {
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
}
