unit demoservice.Server;

// This is a generated unit! Do NOT edit!

interface

uses Ultraware.Grpc.Server, demoservice.Proto;

type
   IContinuousMoveStream_Recv = interface(IGrpcMemStream)
   ['{CDEB1D00-5297-4775-A0D8-40E2E10858A5}']
      function Recv(out aMove: TMove; const aWaitTimeout: Integer): TGrpcWaitResult;
   end;

   IWaitForMoveStream_Recv = interface(IGrpcMemStream)
   ['{6E2E82A3-85FB-4D2B-BBE0-1E0EEB3464DD}']
      function Recv(out aMove: TMove; const aWaitTimeout: Integer): TGrpcWaitResult;
   end;

   IWaitForMoveStream_Send = interface(ICallbackStream)
   ['{E545BCC2-4386-4448-A89F-1E00BDDC4182}']
      procedure Send(const aMove: TMove);
   end;

   IMoveService_Server = interface
      function ContinuousMove(const aContinuousMoveStream_Recv: IContinuousMoveStream_Recv): TMove;
      procedure WaitForMove(const aWaitForMoveStream_Recv: IWaitForMoveStream_Recv; const aWaitForMoveStream_Send: IWaitForMoveStream_Send);
   end;

   TMoveService_Server = class(TBaseGrpcImplementation, IMoveService_Server)
   protected
      procedure DoRegisterCalls; override;
   public
      class function BasePath: string; override;
   public
      function ContinuousMove(const aContinuousMoveStream_Recv: IContinuousMoveStream_Recv): TMove; virtual; abstract;
      procedure WaitForMove(const aWaitForMoveStream_Recv: IWaitForMoveStream_Recv; const aWaitForMoveStream_Send: IWaitForMoveStream_Send); virtual; abstract;
   end;

   TContinuousMoveStream_Recv = class(TBaseGrpcMemStream<TMove>, IContinuousMoveStream_Recv);

   TWaitForMoveStream_Recv = class(TBaseGrpcMemStream<TMove>, IWaitForMoveStream_Recv);

   TWaitForMoveStream_Send = class(TServerCallbackStream<TMove>, IWaitForMoveStream_Send);

implementation

 { TMoveService_Server }

class function TMoveService_Server.BasePath: string;
begin
   Result := '/demoservice.MoveService/';
end;

procedure TMoveService_Server.DoRegisterCalls;
begin
   RegisterInputStreamCall<TContinuousMoveStream_Recv,IContinuousMoveStream_Recv,TMove>('ContinuousMove',ContinuousMove);
   RegisterStreamCall<TWaitForMoveStream_Recv,IWaitForMoveStream_Recv,TWaitForMoveStream_Send,IWaitForMoveStream_Send>('WaitForMove',WaitForMove);
end;

end.
