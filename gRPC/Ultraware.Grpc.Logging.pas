unit Ultraware.Grpc.Logging;

interface

type
  TGrpcLogging = class
  public
    class procedure Log(const s: string);
    class procedure LogError(const s: string);
    class procedure LogInput<T>(aValue: T);
    class procedure LogOutput<T>(aValue: T);
  end;

implementation

uses
  System.Classes;

{ TGrpcLogging }

class procedure TGrpcLogging.Log(const s: string);
begin
  Exit;
  // Writeln(s);
end;

class procedure TGrpcLogging.LogError(const s: string);
begin
  TThread.Queue(nil,
    procedure
    begin
      Log(s);
    end);
end;

class procedure TGrpcLogging.LogInput<T>(aValue: T);
begin
  // Log('Input ' + TValue.From<T>(aValue).ToString);
  // Do something by RTTI?
end;

class procedure TGrpcLogging.LogOutput<T>(aValue: T);
begin
  // Log('Output ' + TValue.From<T>(aValue).ToString);
  // Do something by RTTI?
end;

end.
