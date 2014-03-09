package main

import (
	"fmt"
)

//
// Request / reply structures
//


type RegisterRequest struct {
	MetricNames []string
}

type RegisterReply struct {

}

type ReportRequest struct {
	Metrics map[uint64]uint64 // map from id to value
}

type ReportReply struct {
}

//
// DebugString() pretty printers
//

func (m *RegisterRequest) DebugString() string {
	s := "["
	sep := ""

	for _, r := range m.MetricNames {
		s += r + sep
		sep = ","
	}
	return s + "]"
}

func (m *ReportRequest) DebugString() string {
	vals := ""
	sep := ""
	for k, v := range m.Metrics {
		vals += fmt.Sprintf("%s{%d=%d}", sep, k, v)
		sep = ", "
	}
	return fmt.Sprintf("Metrics: %s\n",	vals)
}

//
// Wire format
//

const (
	MAXLEN = 1024
)

func ParseReportRequestFrom(buf []byte, offset, length uint) (*ReportRequest, error) {
	var err error
	numMetrics := uint64(0)
	r := &ReportRequest{Metrics: make(map[uint64]uint64)}

	offset, numMetrics, err = decodeVarUint(buf, offset, length)
	if err != nil {
		return nil, err
	}

	for i := uint64(0); i < numMetrics; i++ {
		id := uint64(0)
		val := uint64(0)

		offset, id, err = decodeVarUint(buf, offset, length)
		if err != nil {
			return nil, err
		}

		offset, val, err = decodeVarUint(buf, offset, length)
		if err != nil {
			return nil, err
		}

		r.Metrics[id] = val
	}

	return r, nil
}

func (r *ReportRequest) SerializeTo(buf *[]byte, offset uint) (uint, error) {
	var err error
	
	offset, err = encodeVarUint(buf, offset, uint64(len(r.Metrics)))
	if err != nil {
		return offset, err
	}

	for id, val := range r.Metrics {
		offset, err = encodeVarUint(buf, offset, id)
		if err != nil {
			return offset, err
		}

		offset, err = encodeVarUint(buf, offset, val)
		if err != nil {
			return offset, err
		}
	}

	return offset, nil
}

func (this *ReportRequest) Equals(that *ReportRequest) (bool) {
	return true
}

//
// Type encoders / decoders
//

func encodeString(data *[]byte, offset uint, s string) (uint, error) {
	length := uint(len(s))
	offset, err := encodeVarUint(data, offset, uint64(length))
	if err != nil {
		return offset, err
	}

	if offset + length > uint(len(*data)) {
		return offset, fmt.Errorf("Buffer overrun (encoding string %s) %d vs. %d.", s, offset, len(*data))

	}

	for i := uint(0); i < length; i++ {
		(*data)[offset] = s[i]
		offset++
	}

	return offset, nil
}

func encodeVarUint(data *[]byte, offset uint, n uint64) (uint, error) {
	widthIdx := offset
	offset++
	width := 0

	for n > 0 {
		if offset >= uint(len(*data)) {
			return offset, fmt.Errorf("Buffer overrun (encoding varint %d) %d vs. %d.", n, offset, len(*data))
		}
		(*data)[offset] = byte(n & 0xFF)
		n = n >> 8
		offset++
		width++
	}
	(*data)[widthIdx] = byte(width)
	return offset, nil
}

func decodeVarUint(data []byte, offset, max uint) (pos uint, val uint64, e error) {
	if offset >= uint(len(data)) || offset >= max {
		return 0, 0, fmt.Errorf("Index out of bounds %d vs length:%d and max:%d.", offset, len(data), max)
	}

	width := uint(data[offset])

	if offset+1+width > uint(len(data)) {
		return 0, 0,fmt.Errorf("Can't parse value of width %d starting at %d. Length is only %d.", width, offset+1, len(data))
	}

	val = 0
	for i := uint(0); i < width; i++ {
		val += uint64(data[offset+i+1]) << (8 * i)
	}

	return offset + 1 + width, val, nil
}
