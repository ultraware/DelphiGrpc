unit demoservice.Proto;

// This is a generated unit! Do NOT edit!

interface

uses Grijjy.ProtocolBuffers;

type
   TMove = record
      [Serialize(1)] X: Integer;
      [Serialize(2)] Y: Integer;
      [Serialize(3)] Name: string;
   end;
   TMoveArray =  Array of TMove;

implementation

end.
