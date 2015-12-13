{Copyright (c) <2015> <ozok26@gmail.com>

Permission is hereby granted, free of charge, to any person obtaining a copy
of this software and associated documentation files (the "Software"), to deal
in the Software without restriction, including without limitation the rights
to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
copies of the Software, and to permit persons to whom the Software is
furnished to do so, subject to the following conditions:

The above copyright notice and this permission notice shall be included in
all copies or substantial portions of the Software.

THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT.  IN NO EVENT SHALL THE
AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
THE SOFTWARE.}

unit UnitLogItems;

interface

uses
  Classes, IniFiles, Generics.Collections, System.SysUtils, System.Zip;

type
  TLogItem = record
    LogType: string;
    AddDate: string;
    Source: string;
    Destination: string;
    Operation: string;
    Reason: string;
  end;

type
  TLogFile = class(TObject)
  private
    FLogItems: TList<TLogItem>;
    FSplitList: TStringList;
    function GetCount: integer;
    procedure SetLogItems(const Value: TList<TLogItem>);
    function GetAsString: string;
  public
    property Count: integer read GetCount;
    property LogItems: TList<TLogItem> read FLogItems write SetLogItems;
    property AsString: string read GetAsString;
    constructor Create;
    destructor Destroy;
    procedure LoadFromFile(const FilePath: string);
    procedure WriteToFile(const FilePath: string);
    function CompressLog(const LogFilePath: string): string;
    procedure Add(const Item: TLogItem);
    procedure Clear();
  end;

implementation

{ TLogFile }

procedure TLogFile.Add(const Item: TLogItem);
var
  LTmpItem: TLogItem;
begin
  LTmpItem := Item;
  LTmpItem.LogType := LTmpItem.LogType.Replace(';', '');
  LTmpItem.AddDate := LTmpItem.AddDate.Replace(';', '');
  LTmpItem.Source := LTmpItem.Source.Replace(';', '');
  LTmpItem.Destination := LTmpItem.Destination.Replace(';', '');
  LTmpItem.Operation := LTmpItem.Operation.Replace(';', '');
  LTmpItem.Reason := LTmpItem.Reason.Replace(';', '');
  LogItems.Add(LTmpItem);
end;

procedure TLogFile.Clear;
begin
  LogItems.Clear;
end;

function TLogFile.CompressLog(const LogFilePath: string): string;
var
  LZipFile: TZipFile;
  LOutputFileName: string;
begin
  Result := LogFilePath;
  if FileExists(LogFilePath) then
  begin
    LOutputFileName := ChangeFileExt(LogFilePath, '.zip');
    LZipFile := TZipFile.Create;
    try
      LZipFile.Open(LOutputFileName, zmWrite);
      LZipFile.Add(LogFilePath);
      LZipFile.Close;
      Result := LOutputFileName;
    finally
      LZipFile.Free;
    end;
  end;
end;

constructor TLogFile.Create;
begin
  LogItems := TList<TLogItem>.Create;
  FSplitList := TStringList.Create;
  FSplitList.StrictDelimiter := True;
  FSplitList.Delimiter := ';';
end;

destructor TLogFile.Destroy;
begin
  LogItems.Free;
  FSplitList.Free;
end;

function TLogFile.GetAsString: string;
var
  LResult: TStringList;
  I: Integer;
begin
  LResult := TStringList.Create;
  try
    for I := 0 to LogItems.Count - 1 do
    begin
      with LogItems[i] do
      begin
        LResult.Add(LogType + ';' + AddDate + ';' + Source + ';' + Destination + ';' + Operation + ';' + Reason);
      end;
    end;
  finally
    Result := LResult.Text;
    LResult.Free;
  end;
end;

function TLogFile.GetCount: integer;
begin
  Result := LogItems.Count;
end;

procedure TLogFile.LoadFromFile(const FilePath: string);
var
  LLine: string;
  LItem: TLogItem;
  LSR: TStreamReader;
begin
  Clear;
  try
    LSR := TStreamReader.Create(FilePath, TEncoding.UTF8);
    try
      while not LSR.EndOfStream do
      begin
        LLine := LSR.ReadLine.Trim;
        FSplitList.DelimitedText := LLine;
        if FSplitList.Count = 6 then
        begin
          with LItem do
          begin
            LogType := FSplitList[0];
            AddDate := FSplitList[1];
            Source := FSplitList[2];
            Destination := FSplitList[3];
            Operation := FSplitList[4];
            Reason := FSplitList[5];
          end;
          LogItems.Add(LItem);
        end;
      end;
    finally
      LSR.Close;
      LSR.Free;
    end;
  except
    on E: Exception do
    begin
      raise E;
    end;
  end;
end;

procedure TLogFile.SetLogItems(const Value: TList<TLogItem>);
begin
  FLogItems := Value;
end;

procedure TLogFile.WriteToFile(const FilePath: string);
var
  LSW: TStreamWriter;
  I: Integer;
  LLine: string;
  LItem: TLogItem;
begin
  LSW := TStreamWriter.Create(FilePath, True, TEncoding.UTF8);
  try
    for I := 0 to LogItems.Count - 1 do
    begin
      LItem := LogItems[i];
      with LItem do
      begin
        LLine := LogType + ';' + AddDate + ';' + Source + ';' + Destination + ';' + Operation + ';' + Reason;
      end;
      LSW.WriteLine(LLine);
    end;
  finally
    LSW.Close;
    LSW.Free;
  end;
end;

end.

