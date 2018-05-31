# Scalable Linux HTTP sockets for the cloud, Part 3

In this article we will show how to use Delphi's new Linux compiler to expand our [HTTP](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.Http.pas) and [TCP](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.SocketPool.Linux.pas) client base classes to also run on Linux.  On [Windows we used IOCP](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.SocketPool.Win.pas) to create a highly scalable client, and on Linux we will use EPOLL to also create a scalable client. 

We will extend the libc related headers in Delphi by adding [support for the EPoll API](https://github.com/grijjy/GrijjyFoundation/blob/master/Linuxapi.Epoll.pas) and demonstrate how to use it.  Additionally we added HTTP/2 support to the base class for Linux as well.

If you are currently using the TgoHttpClient on Windows, your code should operate unchanged on Linux with the exact the same syntax.  The syntax used in [Part 1](https://blog.grijjy.com/2017/01/09/scalable-https-and-tcp-client-sockets-for-the-cloud/) and [Part 2](https://blog.grijjy.com/2017/03/15/scalable-http-sockets-for-the-cloud-part-2/) of this article remains the same under Linux, so you should refer to those articles for a discussion of how to use the class for blocking, non-blocking, incremental http requests and more.

In this article we will focus on the additions and changes related to the Linux compiler and operating system.

## Delphi for Linux
Here at Grijjy we are excited about the introduction of a Delphi Linux compiler.  Building server-side modules and cloud based service processes is a key part of our focus.  In many ways Linux is preferable to Windows in the cloud and being able to build Linux modules is a great addition to the engineering arsenal.

We have already done quite a bit of work on Linux here at Grijjy with Delphi and I can safely say the new compiler seems to be quite solid with a high degree of compatibility with existing source code.  We easily ported existing Windows code, but also Free Pascal (FPC) code that was originally authored for FPC Linux.

## EPoll APIs
On Linux the EPoll APIs are very useful and can be applied to solving a variety of issues.  In addition to being useful for scalable client sockets, they are widely used for server sockets, eventing and signaling, timer queues and much more.  They are also an evolving mess at the Linux kernel level with numerous bugs in older Linux kernels that create strange issues and new capabilities in updated versions to address performance issues.

Don't let that scare you though, because IOCP on Windows is its own unique mess as well and we can work around these issues to create a highly scalable client socket pool.

We use the EPoll APIs in conjunction with a thread pool (much like IOCP) to handle numerous client sockets simultaneously with a limited number of threads.  This allows us to scale up our client connections and let the worker threads handle the flow of traffic more efficiently.

Delphi's headers do not currently have a translation for the EPoll APIs so we are including a conversion as shown below.

```Delphi
unit Linuxapi.Epoll;
{ Linux API for epoll }

{$I Grijjy.inc}

interface

uses
  Posix.Base,
  Posix.Signal;

const
  EPOLLIN = $01;
  EPOLLPRI = $02;
  EPOLLOUT = $04;
  EPOLLERR = $08;
  EPOLLHUP = $10;
  EPOLLRDNORM = $40;
  EPOLLRDBAND = $80;
  EPOLLWRNORM = $100;
  EPOLLWRBAND = $200;
  EPOLLMSG = $400;
  EPOLLRDHUP = $2000;
  EPOLLWAKEUP = 1 shl 29;
  EPOLLONESHOT = 1 shl 30;
  EPOLLET  = UInt32(1 shl 31);

  { opcodes epoll_ctl }
  EPOLL_CTL_ADD = 1;
  EPOLL_CTL_DEL = 2;
  EPOLL_CTL_MOD = 3;

type
  epoll_data = record
    case Integer of
      0: (ptr: Pointer);
      1: (fd: Integer);
      2: (u32: UInt32);
      3: (u64: UInt64);
  end;

  epoll_event = packed record
    events: UInt32;
    data : epoll_data;
  end;
  pepoll_event = ^epoll_event;

  ptsigset = ^sigset_t;

// create an epoll instance
function epoll_create(size: Integer): Integer; cdecl; external libc name _PU + 'epoll_create';
function epoll_create1(flags: Integer): Integer; cdecl; external libc name _PU + 'epoll_create1';

// apply an operation to an epoll instance
function epoll_ctl(epfd: Integer; op: Integer; fd: Integer; event: pepoll_event): Integer; cdecl; external libc name _PU + 'epoll_ctl';

// wait for events on an epoll instance
function epoll_wait(epfd: Integer; events: pepoll_event; maxevents, timeout: Integer): Integer; cdecl; external libc name _PU + 'epoll_wait';
function epoll_pwait(epfd: Integer; events: pepoll_event; maxevents, timeout: Integer; sigmask: ptsigset): Integer; cdecl; external libc name _PU + 'epoll_pwait'; 
```
> As you can see above, compared to FPC Linux, the Delphi linker and compiler makes importing new headers quick and easy from Libc.
 
## SSL for Linux
In order to provide HTTP/S support and SSL to the base TCP class, we have updated [Grijjy.OpenSSL.API](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.OpenSSL.API.pas) to include constants for the Linux library.

```Delphi
  {$IFDEF LINUX}
  SSLEAY_DLL = 'libssl.so.1.0.0';
  LIBEAY_DLL = 'libcrypto.so.1.0.0';
  {$ELSE}
  SSLEAY_DLL = 'ssleay32.dll';
  LIBEAY_DLL = 'libeay32.dll';
  {$ENDIF}
```
The import references are exactly the same for Windows and Linux.

## Grijjy.SocketPool.Linux
To mirror the base classes we used under Windows such as `TgoSocketConnection` we created a version of the socketpool unit for Linux with the same structure.  This file is located under [https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.SocketPool.Linux.pas](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.SocketPool.Linux.pas) 

As we discussed in [Part 1](README.md), having a common syntax at the core socket connection allows us to build protocols on top of the foundation and seamlessly run our Windows related HTTP and TCP client projects with almost no modification on Linux.

```Delphi
  { Socket connection instance }
  TgoSocketConnection = class(TObject)
  public
    constructor Create(const AOwner: TgoClientSocketManager; const AHostname: String; const APort: Word);
    destructor Destroy; override;
  public
    { Connects the socket }
    function Connect(const AUseNagle: Boolean = True): Boolean;

    { Disconnects the socket }
    procedure Disconnect;

    { Sends the bytes to the socket }
    function Send(const ABuffer: Pointer; const ASize: Integer): Boolean; overload;
    function Send(const ABytes: TBytes): Boolean; overload;

    { Returns the pending operations as a string }
    function PendingToString: String;

    { Stops all future callback events }
    procedure StopCallbacks;
  public
    { Socket handle }
    property Socket: THandle read FSocket write FSocket;

    { Hostname }
    property Hostname: String read FHostname;

    { Port }
    property Port: Word read FPort;

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
This class has the same interface as the Windows version, but abstracts the complexities of performing the same operations under Linux as we do under Windows.

We have added a few new things such as PEM certificate and private key support. 

> We could almost merge these two units into a common unit and inherit a base class with the differences between IOCP and EPOLL except for an issue in the Delphi source code for sockets.  Delphi has a really nice set of cross-platform socket classes that you can really use reliably.  Most of the connect related routines rely on `TIPAddress` but unfortunately the `TIPAddress.LookupName()` routine doesn't null terminate the string it uses for requesting a hostname to IP conversion.  This creates some real issues on Linux and means that sometimes you get a hostname lookup answer and sometimes you get an error.  This requires us to recreate most of the core connection socket logic for Linux ourselves.

## HTTP/2
As with the Windows class, we included support for the [nghttp/2 library](https://nghttp2.org/) for HTTP/2 on Linux.  We have updated the nghttp2.pas unit to also support Linux,

```Delphi
  {$IF Defined(MSWINDOWS)}
  NGHTTP2_LIB = 'nghttp2.dll';
  {$ELSEIF Defined(LINUX)}
  NGHTTP2_LIB = 'libnghttp2.so';
  {$ENDIF}
```
   
To perform HTTP/2 requests we rely on the nghttp2 library. You will need to download and install nghttp2 in the typical manner,

```shell
./configure
sudo make
sudo make install
```
> This will create the libnghttp2.so and place it under your typical library location on Linux.  Under Ubuntu this is /usr/local/lib which you should also add to your Delphi SDK Manager library path for Linux.

In order to make HTTP/2 requests you simply need to construct the `TgoHttpClient` with the `HTTP2` parameter set to `True` as follows:

```delphi
FHTTP := TgoHttpClient.Create(True);
``` 

## Example application
The example [DelphiScalableHttp application](https://github.com/grijjy/DelphiScalableClientSockets) demonstrates how to make Http and Http/2 calls using the `TgoHttpClient` class. 

The full implementation of the base http class for Linux and Windows is contained in the repository [https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.Http.pas](https://github.com/grijjy/GrijjyFoundation/blob/master/Grijjy.Http.pas)

## Conclusion
We hope you enjoyed this continued discussion about scalable client sockets and find all this information useful and informative.

For more information about us, our support and services visit the [Grijjy homepage](http://www.grijjy.com) or the [Grijjy developers blog](http://blog.grijjy.com).

The base classes described herein are part of our [Grijjy Foundation library](https://github.com/grijjy/GrijjyFoundation).  The example program is hosted on GitHub at [https://github.com/grijjy/DelphiScalableClientSockets](https://github.com/grijjy/DelphiScalableClientSockets).