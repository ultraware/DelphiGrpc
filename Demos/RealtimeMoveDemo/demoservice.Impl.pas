unit demoservice.Impl;

interface

uses
  demoservice.Server, demoservice.Proto, System.Generics.Collections, System.Types, System.SyncObjs;

type
  TMoveService_Server_Impl = class(TMoveService_Server)
  protected
    class var FNamePos: TDictionary<string,TMove>;
    class var FNameWaiters: TDictionary<TEvent, string>;
    procedure UpdateMove(const aMove: TMove);
  public
    class constructor Create;
    class destructor  Destroy;

    function  ContinuousMove(const aContinuousMoveStream_Recv: IContinuousMoveStream_Recv): TMove; override;
    procedure WaitForMove(const aWaitForMoveStream_Recv: IWaitForMoveStream_Recv; const aWaitForMoveStream_Send: IWaitForMoveStream_Send); override;
  end;

implementation

uses
  Ultraware.Grpc.Server;

{ TMoveService_Server_Impl }

class constructor TMoveService_Server_Impl.Create;
begin
  FNamePos := TDictionary<string,TMove>.Create;
  FNameWaiters := TObjectDictionary<TEvent, string>.Create([doOwnsKeys]);
end;

class destructor TMoveService_Server_Impl.Destroy;
begin
  FNamePos.Free;
  fNameWaiters.Free;
end;

function TMoveService_Server_Impl.ContinuousMove(const aContinuousMoveStream_Recv: IContinuousMoveStream_Recv): TMove;
var t: TMove;
begin
  Result.X := 0;
  Result.Y := 0;
  Result.name := '';
  repeat
    case aContinuousMoveStream_Recv.Recv(t, 1 * 1000) of
      TGrpcWaitResult.wrTimeout, TGrpcWaitResult.wrNoData:
        Continue;
      TGrpcWaitResult.wrData:
        UpdateMove(t);
      TGrpcWaitResult.wrClosed:
        Break;
    end;
  until False;
end;

procedure TMoveService_Server_Impl.UpdateMove(const aMove: TMove);
var e: TEvent;
begin
//  Exit;
  FNamePos.AddOrSetValue(aMove.name, aMove);
  for e in FNameWaiters.Keys do
  begin
    if FNameWaiters.Items[e] = aMove.name then
      e.SetEvent;
  end;
end;

procedure TMoveService_Server_Impl.WaitForMove(const aWaitForMoveStream_Recv: IWaitForMoveStream_Recv; const aWaitForMoveStream_Send: IWaitForMoveStream_Send);
var
  t: TMove;
  e: TEvent;

  function _TryGetMove(aTimeoutMsec: Integer; aLoop: Boolean): Boolean;
  begin
    Result := False;
    repeat
      case aWaitForMoveStream_Recv.Recv(t, aTimeoutMsec) of
        TGrpcWaitResult.wrTimeout, TGrpcWaitResult.wrNoData:
          ;
        TGrpcWaitResult.wrData:
          Exit(True);
        TGrpcWaitResult.wrClosed:
          Break;
      end;
    until not aLoop or aWaitForMoveStream_Recv.IsClosed;
  end;

var
  sName: string;
begin
  e := nil;
  repeat
    if _TryGetMove(1 * 1000, True {loop}) then  //wait for first data
    if (e = nil) and (t.name <> '') then
    begin
      sName := t.name;
      e := TEvent.Create();
      FNameWaiters.Add(e, sName);    //not threadsafe! and ugly unique event list :)
      try
        repeat
          case e.WaitFor(1*500) of
            TWaitResult.wrSignaled:
            begin
              e.ResetEvent;
              if FNamePos.TryGetValue(sName, t) then
//                if not (aWaitForMoveStream_Send as ICallbackStream).IsClosed then
//                try
                  aWaitForMoveStream_Send.Send(t);
//                except
//                  Break;
//                end;
            end;
            TWaitResult.wrAbandoned,
            TWaitResult.wrError: Break;
          end;

          //get data to update closed state
          if _TryGetMove(1, False {peek}) then ;
        until aWaitForMoveStream_Recv.IsClosed; //(aWaitForMoveStream_Send as ICallbackStream).IsClosed;
      finally
        FNameWaiters.Remove(e);  //not threadsafe!
      end;
    end;
  until False or aWaitForMoveStream_Recv.IsClosed;

  aWaitForMoveStream_Send.CloseSend;
end;

end.
