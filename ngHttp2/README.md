# Scalable HTTP/S and TCP client sockets for the cloud

Now that the cloud has become a core part of every developer's life, we are faced with designing scalable, distributed services that reside on these platforms ([Amazon's AWS](https://aws.amazon.com/), [Google Compute](https://cloud.google.com/compute/) and [Microsoft Azure](https://azure.microsoft.com/en-us/)) and are able to interact with other third-party facing services and middle-ware such as databases, messaging brokers, remote push messaging services and more.

This is part 1 of a three part series.  For more detailed information on HTTP 1.1, HTTP/S and HTTP/2, [see Part 2](HTTP.md).  For building on Delphi for Linux, [see Part 3](LINUX.md).

## The scalable client socket problem
   
One goal we have from our services is to squeeze as much resources out of the node to handle user load and volume without adding more nodes or servers to our service.  Typical third-party services either take an approach where they offer an HTTP enabled API (such as REST/JSON) or a TCP binary protocol interface (such as remote push messaging).  Some third-party services offer their own libraries to interact with their protocol (common with databases, messaging brokers).

If your service runs in the cloud and your are connecting to other third-party products or services, then you are creating a client socket originating from your process and connecting to a server-socket at the third-party service.

With all of these solutions you are faced with a common bottleneck, client sockets.  Because you are originating a socket connection from your service to another third-party service, you are limited by the operating system's implementation of sockets.  Most operating systems offer a scalable socket option (such as IOCP on Windows or [EPOLL on Linux](http://man7.org/linux/man-pages/man7/epoll.7.html)) but all too often the client-side HTTP and TCP support libraries only implement basic [Berkeley sockets](https://en.wikipedia.org/wiki/Berkeley_sockets).  We also see this problem in many third-party drivers interfaces when they provide their own library for the service.  Whether you are using HTTP, TCP or a library driver, these limitations can become an issue for your service if you intend to scale.  

Some forward thinking libraries have embedded scalable sockets into their drivers to work around these limitations, but most of the time this is focused on server sockets.  

## The lack of a foundation 

The team here at Grijjy was faced with the same reality.  We wanted scalable sockets for our distributed cloud services but the embedded libraries often only leverage Berkeley sockets.  While there are numerous libraries that fully exploit scalable server sockets, there are relatively few examples of scalable client sockets.  In addition, very few communication libraries or examples have built a reusable base foundation for scalable client sockets that provides a cross-platform (Windows, Linux) model where other protocols could be layered and built upon.

A good model would involve a base class model for client sockets that is scalable on Windows and Linux with a common class syntax.  The base class would support SSL for TCP and HTTP.  The base class would be inherited to provide not just TCP clients, but HTTP, HTTPS and HTTP2/S clients on Windows and Linux with a common class syntax.  

Then upon this foundation we could be build scalable client drivers, whether those drivers are HTTP/REST oriented or they are binary protocols, to provide a better solution to interacting with third-party services.

Over the coming months we will be demonstrating a variety of protocols and drivers that implement over these base classes including RabbitMQ, MongoDB, Google APIs, iOS and Android remote push message sending, etc.

## Windows IO Completion Ports (IOCP)

On Windows we have IO Completion Ports for scalable client sockets.  IOCP has been around for quite some time, but it is quite tricky to utilize and operations are difficult to manage so developers typically have only used it for server sockets.  This means very little is documented about using IOCP for strictly client sockets even though it is an ideal solution to scalable client sockets on Windows.

A full primer on using IOCP for client scalable sockets is beyond the scope of this article, but we will are glad to provide guidance for your efforts if you need more information.

## Memory pooling

In order for scalable client sockets to remain efficient, we use very small memory buffers.  Because these memory buffers are used for a single operation and must be maintained until the operation is completed, it requires us to create our own memory pooling class.  Without a memory pooling class we would be constantly allocating and releasing small blocks of memory and this operation alone could become the primary bottleneck for performance of the scalable client socket class.

The memory pool doesn't release the memory block but instead preserves it and provides it again for future scalable client socket operations.

## Linux (EPOLL)

On Linux we utilize EPOLL for scalable client sockets.  On Linux there are several accepted mechanisms for scalable sockets but most documentation relates to scalable server sockets, not client sockets.  Our class is a mirror of the Windows class structure and works in the same manner.

A full primer on using EPOLL for client scalable sockets is also beyond the scope of this article.

## First steps

Our first step is to create a base class designed specifically for scalable client sockets that works on both Windows and Linux.  To accomplish this objective we created the TgoSocketConnection base class.  This class abstracts the internals of managing a scalable client socket connection for the platform.  It takes care of connecting, disconnecting, sending and receiving.   It handles SSL with basic certificate support using OpenSSL internally so that other higher level protocols can use SSL, such as HTTPS.

```Delphi
  { Socket connection instance }
  TgoSocketConnection = class(TObject)
  public
    constructor Create(const AOwner: TgoClientSocketManager; const AHostname: String; const APort: Word);
    destructor Destroy; override;
  public
    { Connects the socket }
    function Connect(const AUseNagle: Boolean = True): Boolean;

    { Sends the bytes to the socket }
    function Send(const ABytes: TBytes): Boolean;
  public
    { Socket handle }
    property Socket: TgoSocket read FSocket write FSocket;

    { Current state of the socket connection }
    property State: TgoConnectionState read FState write FState;

    { Number of pending operations on the socket }
    property Pending: Integer read GetPending;

    { Socket is shutdown }
    property Shutdown: Boolean read GetShutdown;

    { Connection is closed }
    property Closed: Boolean read GetClosed write SetClosed;

    { OpenSSL interface }
    property OpenSSL: TgoOpenSSL read GetOpenSSL;
  public
    { Using SSL }
    property SSL: Boolean read FSSL write FSSL;

    { Using ALPN }
    property ALPN: Boolean read FALPN write FALPN;

    { Certificate in PEM format }
    property Certificate: TBytes read GetCertificate write SetCertificate;

    { Private key in PEM format }
    property PrivateKey: TBytes read GetPrivateKey write SetPrivateKey;

    { Password for private key }
    property Password: String read GetPassword write SetPassword;
  public
    { Fired when the socket is connected and ready to be written }
    property OnConnected: TgoSocketNotifyEvent read FOnConnected write FOnConnected;

    { Fired when the socket is disconnected, either gracefully if the state
      is Disconnecting or abrupt if the state is Connected }
    property OnDisconnected: TgoSocketNotifyEvent read FOnDisconnected write FOnDisconnected;

    { Fired when the data has been received by the socket }
    property OnRecv: TgoSocketDataEvent read FOnRecv write FOnRecv;

    { Fired when the data has been sent by the socket }
    property OnSent: TgoSocketDataEvent read FOnSent write FOnSent;
  end;
```

TgoClientSocketManager was created so that we could pool, reuse and cleanup client sockets.  This lessens the burden of operating system resources.  Use of the socket manager is abstracted and transparent to the parent protocol class that uses it.

Our TgoClientSocketManager helps manage the issues of releasing resources, cleaning up and allocating objects in a manner that is compatible with the given platform.  In the case of IOCP, we are making sure that all IO activity on a given completion port has completed and we are safe to remove and destroy objects.

```Delphi
  { Client socket manager }
  TgoClientSocketManager = class(TThread)
  protected
    procedure Execute; override;
  public
    constructor Create(const AOptimization: TgoSocketOptimization = TgoSocketOptimization.Scale;
      const ABehavior: TgoSocketPoolBehavior = TgoSocketPoolBehavior.CreateAndDestroy; const AWorkers: Integer = 0);
    destructor Destroy; override;
  public
    { Releases the connection back to the socket pool }
    procedure Release(const AConnection: TgoSocketConnection);

    { Requests a connection from the socket pool }
    function Request(const AHostname: String; const APort: Word): TgoSocketConnection;
  public
    { Completion handle for IOCP }
    property Handle: THandle read FHandle;

    { Optimization mode }
    property Optimization: TgoSocketOptimization read FOptimization;
  end;
```

The full implementation of the base socket classes for Windows is contained in the repository [https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.SocketPool.Win.pas](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.SocketPool.Win.pas)

## HTTP/S Protocol 

The first protocol we demonstrate using scalable client sockets is HTTP/S.

For more detailed information on HTTP 1.1, HTTP/S and HTTP/2, [see Part 2](HTTP.md).

For building on Delphi for Linux, [see Part 3](LINUX.md).

## Next steps...

In coming articles we will demonstrate how to use these classes to build drivers that work with a variety of services such as iOS and Android remote push messaging (sending messages from your service), Google APIs (interacting from your service), RabbitMQ, MongoDB and much more.

For more information about us, our support and services visit the [Grijjy homepage](http://www.grijjy.com) or the [Grijjy developers blog](http://blog.grijjy.com).

The base classes described herein are part of our [Grijjy Foundation library](https://github.com/grijjy/GrijjyFoundation).  The example program is hosted on GitHub at [https://github.com/grijjy/DelphiScalableClientSockets](https://github.com/grijjy/DelphiScalableClientSockets).

Note: Linux examples are currently waiting approval and we are working to make them available as soon as possible.