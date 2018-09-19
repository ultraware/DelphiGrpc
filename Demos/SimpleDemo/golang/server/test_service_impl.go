package main

import (
	"io"
	"log"
	"time"

	context "golang.org/x/net/context"

	"../testservice"
)

type testServer struct {
}

func (s *testServer) Sleep(ctx context.Context, t *testservice.Time) (*testservice.Time, error) {
	time.Sleep(time.Duration(t.Sec) * time.Second)
	return &testservice.Time{Sec: t.Sec, Msg: `slept`}, nil
}

func (s *testServer) CountDown(t *testservice.Time, strm testservice.TestService_CountDownServer) error {
	for i := t.Sec; i >= 0; i-- {
		if i > 0 {
			strm.Send(&testservice.Time{Sec: int32(i), Msg: `counting down...`})
		} else {
			strm.Send(&testservice.Time{Sec: int32(i), Msg: `counting DONE`})
		}
		time.Sleep(1 * time.Second)
	}
	//strm.Close()
	return nil
}

func (s *testServer) CalcSum(strm testservice.TestService_CalcSumServer) error {
	total := testservice.Time{}
	for {
		t, err := strm.Recv()
		if err == io.EOF {
			break
		} else if err != nil {
			log.Fatalf("CalcSum() = %v", err)
		}
		total.Sec += t.Sec
	}
	total.Msg = `Total`
	strm.SendAndClose(&total)
	return nil
}

func (s *testServer) UpdateableCountDown(strm testservice.TestService_UpdateableCountDownServer) error {

	//wait for client time's in separate goroutine (Recv blocks)
	twait := make(chan testservice.Time)
	go func() {
		for {
			t, err := strm.Recv()
			if err != nil { //io.EOF {
				// read done.
				close(twait)
				return
			} else if t.Sec > 0 {
				twait <- *t //put in queue
			}
		}
	}()

	tloop := <-twait //read first item from queue
	for {
		for i := tloop.Sec; i >= 0; i-- {
			if i > 0 {
				strm.Send(&testservice.Time{Sec: int32(i), Msg: `counting down...`})
			}

			//peek if we have new time received?
			select {
			case t, closed := <-twait:
				//update countdown and start again
				tloop = t
				i = t.Sec + 1
				if closed {
					break
				}
				break
			case <-time.After(1 * time.Second):
				continue
			}
		}
		//done
		strm.Send(&testservice.Time{Sec: 0, Msg: `counting DONE`})
		return nil
	}
}
