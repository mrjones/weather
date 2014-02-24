package main

import (
	"log"
	"os"
	"syscall"
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
