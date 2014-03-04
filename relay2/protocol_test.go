package main

import (
	"fmt"
	"testing"
)

func arrayAsHex(a []byte) string {
	return arrayAsHexWithLen(a, len(a))
}

func arrayAsHexWithLen(a []byte, length int) string {
	s := "[ "
	for i := 0; i < length; i++ {
		s += fmt.Sprintf("0x%x ", a[i])
	}
	s += "]"
	return s
}

func TestReportRequest(t *testing.T) {
	r1 := &ReportRequest{
		Metrics: map[uint64]uint64{ 1:100, 2:200 },
	}

	buf := make([]byte, 1024)
	offset, err := r1.SerializeTo(&buf, 0)
	if err != nil {
		t.Fatal(err)
	}

	fmt.Printf("Encoded to: %s", arrayAsHexWithLen(buf, int(offset) + 5))

	r2, err := ParseReportRequestFrom(buf, 0, offset)
	if err != nil {
		t.Fatal(err)
	}

	if !r1.Equals(r2) {
		t.Fatalf("Messages don't match.\nExpected: %s\nActual: %s",
			r1.DebugString(), r2.DebugString())
	}
}
