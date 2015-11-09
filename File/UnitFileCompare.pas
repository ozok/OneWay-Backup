unit UnitFileCompare;

interface

uses Classes, SysUtils, Windows, IOUtils, DateUtils, IdHashMessageDigest,
  idHash;

type
  TFileComperator = class
  private
    function StreamsAreIdentical(const FileStream1, FileStream2: TStream; const BlockSize: Integer): Boolean;
    function DoFilesHaveSameLastModifiedTime(const FilePath1: string; const FilePath2: string): Boolean;
    function CompareMD5(const FileStream1, FileStream2: TStream): Boolean;
    function CalculateMD5(const FS: TStream): string;
    function CompareSizes(const FS1: TStream; const FS2: TStream): Boolean;
    function GetSizeOfFile(const FileName: String): Int64;
  public
    Stop: Boolean;
    function CompareFiles(const FileName1, FileName2: String; const BlockSize: Integer; const CompareMethod: integer; var Reason: string): Boolean;
  end;

implementation

{ TFileComperator }

function TFileComperator.CalculateMD5(const FS: TStream): string;
var
  idmd5: TIdHashMessageDigest5;
  hash: T4x4LongWordRecord;
begin
  idmd5 := TIdHashMessageDigest5.Create;
  try
    Result := idmd5.HashStreamAsHex(FS);
  finally
    FS.Free;
    idmd5.Free;
  end;
end;

function TFileComperator.CompareFiles(const FileName1, FileName2: String; const BlockSize: Integer; const CompareMethod: integer; var Reason: string): Boolean;
var
  LFS1, LFS2: TFileStream;
begin
  Result := False;
  Stop := False;
  if not(FileExists(FileName1) and FileExists(FileName2)) then
  begin
    Reason := '[DestDoesntExist] ';
    Exit;
  end
  else if DoFilesHaveSameLastModifiedTime(FileName1, FileName2) then
  begin
    Reason := '[DateDiff' + DateTimeToStr(TFile.GetCreationTimeUtc(FileName1)) + '/' + DateTimeToStr(TFile.GetCreationTimeUtc(FileName2)) + '] ';
    Result := not DoFilesHaveSameLastModifiedTime(FileName1, FileName2)
  end;

  try
    LFS1 := TFileStream.Create(FileName1, fmOpenRead or fmShareDenyWrite);
    LFS2 := TFileStream.Create(FileName2, fmOpenRead or fmShareDenyWrite);
    LFS1.Seek(0, soBeginning);
    LFS2.Seek(0, soBeginning);
    case CompareMethod of
      0:
        begin
          Reason := '[DiffStream] ';
          Result := StreamsAreIdentical(LFS1, LFS2, BlockSize);
        end;
      1:
        begin
          Reason := '[DiffMd5] ';
          Result := CompareMD5(LFS1, LFS2);
        end;
      2:
        begin
          Reason := '[DiffSize ' + FloatToStr(LFS1.Size) + '/' + FloatToStr(LFS2.Size) + '] ';
          Result := LFS1.Size = LFS2.Size;
        end;
    end;
    if Result then
    begin
      Reason := '';
    end;
  finally
    LFS2.Free;
    LFS1.Free;
  end;
end;

function TFileComperator.CompareMD5(const FileStream1, FileStream2: TStream): Boolean;
var
  LHashStr1, LHashStr2: string;
begin
  LHashStr1 := CalculateMD5(FileStream1);
  LHashStr2 := CalculateMD5(FileStream2);
  Result := LHashStr1 = LHashStr2;
end;

function TFileComperator.CompareSizes(const FS1, FS2: TStream): Boolean;
begin
  Result := FS1.Size = FS2.Size;
end;

function TFileComperator.DoFilesHaveSameLastModifiedTime(const FilePath1, FilePath2: string): Boolean;
var
  LDate1, LDate2: TDateTime;
begin
  LDate1 := TFile.GetCreationTimeUtc(FilePath1);
  LDate2 := TFile.GetCreationTimeUtc(FilePath2);
  Result := CompareDateTime(LDate1, LDate2) = 0;
end;

function TFileComperator.GetSizeOfFile(const FileName: String): Int64;
var
  Rec: TSearchRec;
begin
  Result := 0;
  if (FindFirst(FileName, faAnyFile, Rec) = 0) then
  begin
    Result := Rec.Size;
    SysUtils.FindClose(Rec);
  end;
end;

function TFileComperator.StreamsAreIdentical(const FileStream1, FileStream2: TStream; const BlockSize: Integer): Boolean;
var
  LBuffer1: array of byte;
  LBuffer2: array of byte;
  LBuffer1Readed, LBuffer2Readed, LBlockSize: integer;
begin
  Result := False;
  if FileStream1.Size <> FileStream2.Size then
    Exit;

  FileStream1.Position := 0;
  FileStream2.Position := 0;

  if BlockSize > 0 then
    LBlockSize := BlockSize
  else
    LBlockSize := 4096;

  SetLength(LBuffer1, LBlockSize);
  SetLength(LBuffer2, LBlockSize);

  LBuffer1Readed := 1; // just for entering while

  while (LBuffer1Readed > 0) and (FileStream1.Position < FileStream1.Size) do
  begin
    if Stop then
    begin
      Break;
    end;
    LBuffer1Readed := FileStream1.Read(LBuffer1[0], LBlockSize);
    LBuffer2Readed := FileStream2.Read(LBuffer2[0], LBlockSize);

    if (LBuffer1Readed <> LBuffer2Readed) or ((LBuffer1Readed <> LBlockSize) and (FileStream1.Position < FileStream1.Size)) then
      Exit;

    if not CompareMem(@LBuffer1[0], @LBuffer2[0], LBuffer1Readed) then
      Exit;
  end; // while

  Result := True;
end;

end.
