package main

import (
	"log"
	"os"
	"syscall"
	"unsafe"
)

type SerialConnection struct {
	file *os.File
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
