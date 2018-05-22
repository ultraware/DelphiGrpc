# Scalable HTTP sockets for the cloud, Part 2

In this article we will expand on our TgoHttpClient class by adding some core new features including non-blocking http responses, unifying HTTP 1.1, HTTP/S and HTTP/2 support into a common class, incremental data transfers, various fixes and performance improvements.

[In a previous article](https://blog.grijjy.com/2017/01/09/scalable-https-and-tcp-client-sockets-for-the-cloud/) we discussed the scalable client socket problem and how it creates a bottleneck for services that need to communicate over TCP as a client socket.  This model is common in backend services when you interact with third-party providers such as databases, remote push notification services (Apple or Google) or just about any JSON/REST based HTTP API.

If you use a typical HTTP client or component in your Windows or Linux service, you are almost always using [Berkeley based sockets](https://en.wikipedia.org/wiki/Berkeley_sockets).  There are numerous scale issues presented by the Winsock stack under Windows including port limitations, connection timeout retry and delay limitations, page pool issues and much more.  As discussed previously, most of these limitations are worked around in Windows by using Windows IO Completion Ports (IOCP).  IOCP has been available for quite some time but it is notoriously difficult to use and has been almost exclusively used for creating server applications for TCP sockets.  

IOCP is well designed solution to scalable client sockets but it is rarely used as a foundation model that allows you to build client sockets for other protocols.  In our previous article on [Scalable HTTP/S and TCP client sockets for the cloud](https://blog.grijjy.com/2017/01/09/scalable-https-and-tcp-client-sockets-for-the-cloud/), we demonstrated how to build a foundation class that uses IOCP to create client socket protocols with a brief examination of the HTTP protocol.

> This foundation model could easily be expanded and future client protocols and the apps that rely upon them could operate without modification on other operating systems such as Linux.  Internally we have done this on Linux directly using FPC and Lazarus and hope to soon demonstrate this running for Linux on the latest Delphi compilers.   

IOCP is at the heart of how Windows IIS operates in conjunction with the HTTP.SYS kernel driver and scales up efficiently.  On Linux we have similar technologies such as EPOLL and KQueue and they are widely utilized in server based sockets.  These technologies are also a perfect match for client based socket issues.

The TgoHttpClient class demonstrates how you can leverage IOCP and our foundation class TgoSocketConnection to create client sockets for the purpose of HTTP and take advantage of the many benefits of IOCP at the same time.  

## Non-blocking HTTP

IOCP is designed to be non-blocking and handle numerous activities simultaneously.  This is due to the fact that IOCP operations occur within a thread pool. 

In our previous example we showed a simple blocking HTTP client.  The example waited in a loop for the entire http response to arrive or a timeout to happen.   Under normal circumstances this is the behavior we want for http requests.  You send a request and you wait for a response.  Your code is blocked while this transaction takes place.

Because IOCP operates in a thread pool your app does not necessarily have to be blocked, and activity and progress can occur during the request and response cycle.  There are times when you either do not want to wait for a response or you do not care about the response.  This is when non-blocking could be helpful.

To create a non-blocking http client we first construct `TgoHttpClient` with the `Blocking` parameter set to `False`.  
```delphi
HTTP := TgoHttpClient.Create(False, False);
```

Because it is non-blocking, the response will arrive as a TMessage and at a later time.  Therefore we cannot simply `Free` the `TgoHttpClient` immediately.  Instead we use the helper class `TgoHttpClientManager` to handle the destruction of the class by calling `TgoHttpClientManager.Release`.  This method does not immediately destruct the object but instead waits until the response is fully received and the TMessage is called, an error occurs or a timeout elapses.

```delphi  
  HTTP := TgoHttpClient.Create(True, False);
  try
    HTTP.Get('https://nghttp2.org');
  finally
    HttpClientManager.Release(HTTP);
  end;
```
In the above example we set the `HTTP2` parameter to `True` because we know that the nghttp2.org web server supports the HTTP/2 protocol and we set the parameter `Blocking` to `False`.  After performing the request to `Get` we immediately call `HTTPClientManager.Release` which queues the object for destruction.

> It is interesting to note that under a typical Winsock model, each construction and destruction of an object that directly related to socket resources would deplete vital system resources or trigger other unwanted behavior such as TIMEWAIT on sockets, however in our model the `TgoHttpClient` is only a simple Delphi object with straightforward properties that do not directly depend on system resources.  

To handle non-blocking responses we declare a listener for the response TMessage.
```delphi
procedure TFormMain.HttpResponseMessageListener(const Sender: TObject;
  const M: TMessage);
var
  HttpResponseMessage: TgoHttpResponseMessage;
begin
  HttpResponseMessage := M as TgoHttpResponseMessage;
  Writeln('ResponseStatusCode = ' + HttpResponseMessage.ResponseStatusCode.ToString);
  Writeln('Response = ' + HttpResponseMessage.Response);
end;
```

After you make a request using one of the various methods, for example:
```delphi
HTTP.Get('https://nghttp2.org');
```
The TMessage will be created and your `HttpResponseMessageListener` will be called. The TMessage has the following details:

```delphi
  { Http response message }
  TgoHttpResponseMessage = class(TMessage)
  public
    property HttpClient: TgoHttpClient read FHttpClient;
    property ResponseHeaders: TgoHttpHeaders read FResponseHeaders;
    property ResponseStatusCode: Integer read FResponseStatusCode;
    property ResponseContentType: String read FResponseContentType;
    property ResponseContentCharset: String read FResponseContentCharset;
    property Response: TBytes read FResponse;
  end;
```
This includes the actual `Response`, the `ResponseStatusCode` and various other details.

## Incremental data transfers
One of the many benefits of IOCP and scalable sockets is that you have easy and direct access to the data as it is being sent or received over the wire.

This latest version of the `TgoHttpClient` adds the ability to receive data in chunks as it arrives over the wire.  This methodology works the same for HTTP 1.1, HTTP/S and HTTP/2 requests as well as `Content-Length` and `Chunked` transfer encoding.

To use this new feature assign an event and define the event procedure.
```delphi
  HTTP := TgoHTTPClient.Create;
  HTTP.OnRecv := OnRecv;
```

```delphi
procedure TFormMain.OnRecv(Sender: TObject; const ABuffer: Pointer; const ASize: Integer; var ACreateResponse: Boolean);
begin
  { do not buffer the response }
  ACreateResponse := False;
end;  
```
In the above example the latest received bytes are indicated as a the pointer `ABuffer` with a length of `ASize`.  

If we are receiving a large response and we do not want a complete response built in memory we can set `ACreateResponse` to `False`.  This would be useful if we wanted to download a large file but we did not want it cached into memory for efficiency reasons. 

## HTTP/2
HTTP/2 is now becoming more widely used on web servers and in various interfaces such as Apple's latest Push Notification service for iOS devices.  This latest revision to TgoHttpClient seamlessly blends the ability to perform HTTP 1.1, HTTP/S and HTTP/2 into a single class.

In order to make HTTP/2 requests you simply need to construct the `TgoHttpClient` with the `HTTP2` parameter set to `True` as follows:

```delphi
FHTTP := TgoHttpClient.Create(True);
``` 

Once enabled `TgoHttpClient` will only make requests using HTTP/2.  Not all web servers support HTTP/2 so your mileage may vary.

To perform HTTP/2 requests we rely on the nghttp2 library. You will need the nghttp2.dll in your distribution in order to utilize HTTP/2.

### nghttp2

[Nghttp2 is a fairly established library](http://nghttp2.org) for implementing the HTTP/2 protocol.  It handles all the compression and bitstreaming for implementing HTTP/2.

The [HTTP/2 protocol](https://http2.github.io/) and its header compression are rather complex and evolving so it is probably best not to try and implement this ourselves.

One of the great things about nghttp2 is you can utilize it entirely with memory buffers so it is a good match for our own [scalable client socket classes](https://github.com/grijjy/DelphiScalableClientSockets) for Windows and Linux.  This way we get the benefit of HTTP/2 but we don't have to rely on another implementation of sockets for scaling up our service. 

### Building nghttp2

If you want to build the nghttp2 library for Windows to use in your Delphi application you will need to download the [latest source](https://github.com/nghttp2/nghttp2).  To build you can use Visual Studio or just [download the Visual Studio Build Tools](http://landinghub.visualstudio.com/visual-cpp-build-tools).  You will also need to download and install [CMAKE](https://cmake.org/).

  1.  Download the latest nghttp2 source from https://github.com/nghttp2/nghttp2
  2.  Install CMAKE and the Build Tools for Visual Studio.
  3.  Run CMAKE followed by a period
	  `cmake .`
  4.  Run CMAKE to build the release version 
  	  `cmake --build . --config RELEASE`

### Delphi header translation
Once completed you will have a nghttp2.dll.  We will need our [conversion for the header file for Delphi](https://github.com/grijjy/DelphiScalableClientSockets/blob/master/Nghttp2.pas) so we can use the nghttp2 methods directly.

> If you do not want HTTP/2 support and nghttp2.dll you can simple comment out the reference at the top of the Grijjy.Http unit called `{$DEFINE HTTP2}`.

## Example application

The example [DelphiScalableHttp application](https://github.com/grijjy/DelphiScalableClientSockets) demonstrates how to actually make blocking and non-blocking http calls using the `TgoHttpClient` class. 

![](http://i.imgur.com/Fj3N78d.png)

The full implementation of the base http class for Windows is contained in the repository [https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.Http.pas](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.Http.pas)

## Conclusion
We hope you enjoyed this continued discussion about scalable client sockets and find all this information useful and informative.

For more information about us, our support and services visit the [Grijjy homepage](http://www.grijjy.com) or the [Grijjy developers blog](http://blog.grijjy.com).

The base classes described herein are part of our [Grijjy Foundation library](https://github.com/grijjy/GrijjyFoundation).  The example program is hosted on GitHub at [https://github.com/grijjy/DelphiScalableClientSockets](https://github.com/grijjy/DelphiScalableClientSockets).