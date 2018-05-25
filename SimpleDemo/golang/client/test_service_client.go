package main

import (
	"context"
	"io"
	"log"
	"time"

	"../testservice"
	"google.golang.org/grpc"
)

type testServiceClient struct {
	client testservice.TestServiceClient
}

func newTestServiceClient(cc *grpc.ClientConn) *testServiceClient {
	c := new(testServiceClient)
	c.client = testservice.NewTestServiceClient(cc)
	return c
}

func (c *testServiceClient) test() {
	log.Println(`Testing...`)
	c.sleep(3)
	c.countDown(5)
	c.calcSum(5)
	log.Println(`Test DONE`)
}

func (c *testServiceClient) sleep(sec int) {
	log.Println(`Sleeping:`)
	t, err := c.client.Sleep(context.Background(), &testservice.Time{Sec: int32(sec)})
	if err != nil {
		log.Fatalf("Sleep() = %v", err)
		return
	}
	log.Println(t)
}

func (c *testServiceClient) countDown(sec int) {
	log.Println(`countDown:`)
	strm, err := c.client.CountDown(context.Background(), &testservice.Time{Sec: int32(sec)})
	if err != nil {
		log.Fatalf("countDown() = %v", err)
		return
	}

	for {
		t, err := strm.Recv()
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatalf("countDown() = %v", err)
		}
		log.Println(t)
	}
}

func (c *testServiceClient) calcSum(sec int) {
	log.Println(`calcSum:`)
	strm, err := c.client.CalcSum(context.Background())
	if err != nil {
		log.Fatalf("calcSum() = %v", err)
		return
	}
	for i := 0; i < 5; i++ {
		strm.Send(&testservice.Time{Sec: int32(i)})
		time.Sleep(1000)
	}
	t, err := strm.CloseAndRecv()
	if err != nil {
		log.Fatalf("calcSum.CloseAndRecv = %v", err)
		return
	}
	log.Println(t)
}
