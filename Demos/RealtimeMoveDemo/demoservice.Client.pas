unit demoservice.Client;

// This is a generated unit! Do NOT edit!

interface

uses Ultraware.Grpc.Client, demoservice.Proto;

type
   TWaitForMoveCallback = TProtoCallback<TMove>;

   IContinuousMoveStream_Send = interface
   ['{65D95CE7-8516-4502-8F21-DFB6F60862BF}']
      procedure Send(const aMove: TMove);
      function  CloseAndRecv(out aMove: TMove): Boolean;
   end;

   IWaitForMoveStream_Send = interface
   ['{2810C21F-BDEE-4245-9642-C5B13623C5F7}']
      procedure Send(const aMove: TMove);
      procedure CloseSend;
   end;

   IMoveService_Client = interface
      function ContinuousMove(): IContinuousMoveStream_Send;
      function WaitForMove(const aWaitForMoveCallback: TWaitForMoveCallback): IWaitForMoveStream_Send;
   end;

   TMoveService_Client = class(TBaseGrpcClient,IMoveService_Client)
      function ContinuousMove(): IContinuousMoveStream_Send;
      function WaitForMove(const aWaitForMoveCallback: TWaitForMoveCallback): IWaitForMoveStream_Send;
      function BasePath: string; override;
   end;

   TContinuousMoveStream_Send = class(TGrpcStream_Send<TMove,TMove>, IContinuousMoveStream_Send);

   TWaitForMoveStream_Send = class(TGrpcStream_Send<TMove,TMove>, IWaitForMoveStream_Send);

implementation

 { TMoveService_Client }

function TMoveService_Client.BasePath: string;
begin
   Result := '/demoservice.MoveService/';
end;

function TMoveService_Client.ContinuousMove(): IContinuousMoveStream_Send;
begin
   Result := DoInputStreamRequest<TContinuousMoveStream_Send>('ContinuousMove');
end;

function TMoveService_Client.WaitForMove(const aWaitForMoveCallback: TWaitForMoveCallback): IWaitForMoveStream_Send;
begin
   Result := DoInAndOutputStreamRequest<TWaitForMoveStream_Send,TMove>('WaitForMove', aWaitForMoveCallback);
end;

end.
