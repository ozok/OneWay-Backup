unit UnitFileCompare;

interface

uses Classes, SysUtils, Windows, IOUtils, DateUtils;

type
  TFileComperator = class
  private
    function StreamsAreIdentical(const FileStream1, FileStream2: TStream; const BlockSize: Integer = 4096): Boolean;
    function DoFilesHaveSameLastModifiedTime(const FilePath1: string; const FilePath2: string): Boolean;
  public
    Stop: Boolean;
    function CompareFiles(const FileName1, FileName2: String; const BlockSize: Integer = 4096): Boolean;
  end;

implementation

{ TFileComperator }

function TFileComperator.CompareFiles(const FileName1, FileName2: String; const BlockSize: Integer = 4096): Boolean;
var
  LFS1, LFS2: TFileStream;
begin
  Result := False;
  Stop := False;
  if not(FileExists(FileName1) and FileExists(FileName2)) then
  begin
    Exit;
  end
  else
  begin
    if DoFilesHaveSameLastModifiedTime(FileName1, FileName2) then
    begin
      Exit;
    end;
  end;

  try
    LFS1 := TFileStream.Create(FileName1, fmOpenRead or fmShareDenyNone);
    LFS2 := TFileStream.Create(FileName2, fmOpenRead or fmShareDenyNone);
    Result := StreamsAreIdentical(LFS1, LFS2, BlockSize);
  finally
    LFS2.Free;
    LFS1.Free;
  end;
end;

function TFileComperator.DoFilesHaveSameLastModifiedTime(const FilePath1, FilePath2: string): Boolean;
var
  LDate1, LDate2: TDateTime;
begin
  LDate1 := TFile.GetCreationTimeUtc(FilePath1);
  LDate2 := TFile.GetCreationTimeUtc(FilePath2);
  Result := CompareDateTime(LDate1, LDate2) = 0;
end;

function TFileComperator.StreamsAreIdentical(const FileStream1, FileStream2: TStream; const BlockSize: Integer): Boolean;
var
  LBuffer1: array of byte;
  LBuffer2: array of byte;
  LBuffer1Readed, LBuffer2Readed, lBlockSize: integer;
begin
  Result := False;
  if FileStream1.Size <> FileStream2.Size then
    Exit;

  FileStream1.Position := 0;
  FileStream2.Position := 0;

  if BlockSize > 0 then
    lBlockSize := BlockSize
  else
    lBlockSize := 4096;

  SetLength(LBuffer1, lBlockSize);
  SetLength(LBuffer2, lBlockSize);

  LBuffer1Readed := 1; // just for entering while

  while (LBuffer1Readed > 0) and (FileStream1.Position < FileStream1.Size) do
  begin
    if Stop then
    begin
      Break;
    end;
    LBuffer1Readed := FileStream1.Read(LBuffer1[0], lBlockSize);
    LBuffer2Readed := FileStream2.Read(LBuffer2[0], lBlockSize);

    if (LBuffer1Readed <> LBuffer2Readed) or ((LBuffer1Readed <> lBlockSize) and (FileStream1.Position < FileStream1.Size)) then
      Exit;

    if not CompareMem(@LBuffer1[0], @LBuffer2[0], LBuffer1Readed) then
      Exit;
  end; // while

  Result := True;
end;

end.
