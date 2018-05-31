package testservice

/*
1. Download protoc-win32.zip from https://developers.google.com/protocol-buffers/docs/downloads
2. Unzip and add location of the protoc.exe to your PATH environment variable
3. Run `protoc --version` from command prompt to verify
4. Verify the your GOPATH environment variable is set
5. Run `go get -u github.com/golang/protobuf/protoc-gen-go` from command prompt. This should install the binary to %GOPATH%/bin
6. Add `%GOPATH%/bin` to your PATH environment variable
Open a new command prompt, navigate to your .proto file, run `protoc --go_out=. *.proto`
*/
//go:generate protoc --go_out=plugins=grpc:. ../../proto/test_service.proto -I ../../proto
