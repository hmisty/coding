package main

import (
	"fmt"
	"net"
	"gopkg.in/mgo.v2/bson"
)

func panicIfError(err error) {
	if err != nil {
		panic(err)
	}
}

func requestAndResponse(conn *net.TCPConn) *bson.M {
	data, err := bson.Marshal(bson.M{"_id": 1, "fn":"__stats__", "args":[]string{}})
	panicIfError(err)

	fmt.Printf("%q\n", data)
	_, err = conn.Write(data)
	panicIfError(err)

	b := make([]byte, 4) //first 4 bytes is length of the message
	_, err = conn.Read(b)
	panicIfError(err)

	msglen := int32((uint32(b[0]) << 0) |
	(uint32(b[1]) << 8) |
	(uint32(b[2]) << 16) |
	(uint32(b[3]) << 24))

	rest := make([]byte, msglen - 4)
	_, err = conn.Read(rest)
	panicIfError(err)

	b = append(b, rest...)
	out := bson.M{}
	bson.Unmarshal(b, out)
	return &out
}

func main() {
	service := "127.0.0.1:8181"
	tcpAddr, err := net.ResolveTCPAddr("tcp4", service)
	conn, err := net.DialTCP("tcp", nil, tcpAddr)
	if err != nil {
		panic(err)
	}

	out := requestAndResponse(conn)
	fmt.Printf("%v\n", out)
}

