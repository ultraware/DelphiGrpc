package main

import (
	"fmt"
	"log"
	"net"

	"../testservice"
	"google.golang.org/grpc"
)

func newServer() *testServer {
	s := new(testServer)
	return s
}

func main() {
	tcp, err := net.Listen("tcp", fmt.Sprintf("127.0.0.1:%d", 1000))
	if err != nil {
		log.Fatalf("failed to listen: %v", err)
	}

	var opts []grpc.ServerOption
	grpcServer := grpc.NewServer(opts...)
	testservice.RegisterTestServiceServer(grpcServer, newServer())

	grpcServer.Serve(tcp)
}
