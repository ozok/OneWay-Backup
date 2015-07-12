unit UnitFileCompare;

interface

uses Classes, SysUtils;

type
  TFileComperator = class
  private
    function StreamsAreIdentical(var aError: Integer;
      const aStream1, aStream2: TStream;
      const aBlockSize: Integer = 4096): Boolean;
  public
    function CompareFiles(var aError: Integer;
      const aFileName1, aFileName2: String;
      const aBlockSize: Integer = 4096): Boolean;

  end;

implementation

{ TFileComperator }

function TFileComperator.CompareFiles(var aError: Integer;
  const aFileName1, aFileName2: String;
  const aBlockSize: Integer = 4096): Boolean;
var
  lFileStream1, lFilestream2: TFileStream;

begin
  Result := False;
  try
    if not(FileExists(aFileName1) and FileExists(aFileName2)) then
    begin
      aError := 2; // file not found
      Exit;
    end;

    lFileStream1 := nil;
    lFilestream2 := nil;
    try
      lFileStream1 := TFileStream.Create(aFileName1, fmOpenRead or
        fmShareDenyNone);
      lFilestream2 := TFileStream.Create(aFileName2, fmOpenRead or
        fmShareDenyNone);
      Result := StreamsAreIdentical(aError, lFileStream1, lFilestream2,
        aBlockSize);
    finally
      if lFilestream2 <> nil then
        lFilestream2.Free;

      if lFileStream1 <> nil then
        lFileStream1.Free;
    end; // finally
  except
    aError := 3; // file open exception
  end; // except
end;

function TFileComperator.StreamsAreIdentical(var aError: Integer;
  const aStream1, aStream2: TStream; const aBlockSize: Integer): Boolean;
var
  LBuffer1: array of byte;
  LBuffer2: array of byte;
  lBuffer1Readed, lBuffer2Readed, lBlockSize: integer;
begin
  Result := False;
  aError := 0;
  try
    if aStream1.Size <> aStream2.Size then
      Exit;

    aStream1.Position := 0;
    aStream2.Position := 0;

    if aBlockSize > 0 then
      lBlockSize := aBlockSize
    else
      lBlockSize := 4096;

    SetLength(LBuffer1, lBlockSize);
    SetLength(LBuffer2, lBlockSize);

    lBuffer1Readed := 1; // just for entering while

    while (lBuffer1Readed > 0) and (aStream1.Position < aStream1.Size) do
    begin
      lBuffer1Readed := aStream1.Read(LBuffer1[0], lBlockSize);
      lBuffer2Readed := aStream2.Read(LBuffer2[0], lBlockSize);

      if (lBuffer1Readed <> lBuffer2Readed) or
        ((lBuffer1Readed <> lBlockSize) and (aStream1.Position < aStream1.Size))
      then
        Exit;

      if not CompareMem(@LBuffer1[0], @LBuffer2[0], lBuffer1Readed) then
        Exit;
    end; // while

    Result := True;
  except
    aError := 1; // stream read exception
  end;
end;

end.
