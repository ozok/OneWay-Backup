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
    function GetInfoCount: integer;
    function GetErrorCount: integer;
    function GetReadCount: integer;
    function GetSuccessCount: integer;
    function GetAsHtml: string;
  public
    property Count: integer read GetCount;
    property LogItems: TList<TLogItem> read FLogItems write SetLogItems;
    property AsString: string read GetAsString;
    property InfoCount: integer read GetInfoCount;
    property ErrorCount: integer read GetErrorCount;
    property SkipCount: integer read GetReadCount;
    property SuccessCount: integer read GetSuccessCount;
    property AsHtml: string read GetAsHtml;
    constructor Create;
    destructor Destroy;
    procedure LoadFromFile(const FilePath: string);
    procedure WriteToFile(const FilePath: string; const IsHtml: Boolean);
    procedure Add(const Item: TLogItem);
    procedure Clear();
    function CompressLog(const LogFilePath: string): string;
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
  FLogItems.Add(LTmpItem);
end;

procedure TLogFile.Clear;
begin
  FLogItems.Clear;
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
  FLogItems := TList<TLogItem>.Create;
  FSplitList := TStringList.Create;
  FSplitList.StrictDelimiter := True;
  FSplitList.Delimiter := ';';
end;

destructor TLogFile.Destroy;
begin
  FLogItems.Free;
  FSplitList.Free;
end;

function TLogFile.GetAsHtml: string;
var
  LResult: TStringBuilder;
  i: integer;
  LItem: TLogItem;
  LColor: string;
begin
  LResult := TStringBuilder.Create;
  try
    LResult.Append('<html><head><meta charset="utf-8" /><meta name="viewport" content="width=device-width, initial-scale=1.0">');
    LResult.Append('<title>OneWay Backup Report</title>');
    LResult.Append('<style>');
    LResult.Append('.col-ok-1{width:4,167% !important} ');
    LResult.Append('.col-ok-2{width:8,333% !important} ');
    LResult.Append('.col-ok-3{width:12,500% !important} ');
    LResult.Append('.col-ok-4{width:16,667% !important} ');
    LResult.Append('.col-ok-5{width:20,833% !important} ');
    LResult.Append('.col-ok-6{width:25,000% !important} ');
    LResult.Append('.col-ok-7{width:29,167% !important} ');
    LResult.Append('.col-ok-8{width:33,333% !important} ');
    LResult.Append('.col-ok-9{width:37,500% !important} ');
    LResult.Append('.col-ok-10{width:41,667% !important} ');
    LResult.Append('.col-ok-11{width:45,833% !important} ');
    LResult.Append('.col-ok-12{width:50,000% !important} ');
    LResult.Append('.col-ok-13{width:54,167% !important} ');
    LResult.Append('.col-ok-14{width:58,333% !important} ');
    LResult.Append('.col-ok-15{width:62,500% !important} ');
    LResult.Append('.col-ok-16{width:66,667% !important} ');
    LResult.Append('.col-ok-17{width:70,833% !important} ');
    LResult.Append('.col-ok-18{width:75,000% !important} ');
    LResult.Append('.col-ok-19{width:79,167% !important} ');
    LResult.Append('.col-ok-20{width:83,333% !important} ');
    LResult.Append('.col-ok-21{width:87,500% !important} ');
    LResult.Append('.col-ok-22{width:91,667% !important} ');
    LResult.Append('.col-ok-23{width:95,833% !important} ');
    LResult.Append('.col-ok-24{width:100,000% !important} ');
    LResult.Append('.table{max-width:100%;width:100%;margin-bottom:20px}th{text-align:left}.table>tbody>tr>td,.table>tbody>tr>th,.table>tfoot>tr>td,.table>tfoot>tr>th,.table>thead>tr>td,.table>thead>tr>th{padding:8px;line-height:1.428571429;vertical-align:top;border-top:1px ');
    LResult.Append('solid #ddd}.table>thead>tr>th{vertical-align:bottom;border-bottom:2px solid #ddd}.table>caption+thead>tr:first-child>td,.table>caption+thead>tr:first-child>th,.table>colgroup+thead>tr:first-child>td,.table>colgroup+thead>tr:first-child>th,.table>thead:fir');
    LResult.Append('st-child>tr:first-child>td,.table>thead:first-child>tr:first-child>th{border-top:0}.table>tbody+tbody{border-top:2px solid #ddd}.table .table{background-color:#fff}.table-condensed>tbody>tr>td,.table-condensed>tbody>tr>th,.table-condensed>tfoot>tr>td,.tab');
    LResult.Append('le-condensed>tfoot>tr>th,.table-condensed>thead>tr>td,.table-condensed>thead>tr>th{padding:5px}.table-bordered,.table-bordered>tbody>tr>td,.table-bordered>tbody>tr>th,.table-bordered>tfoot>tr>td,.table-bordered>tfoot>tr>th,.table-bordered>thead>tr>td,.tab');
    LResult.Append('le-bordered>thead>tr>th{border:1px solid #ddd}.table-bordered>thead>tr>td,.table-bordered>thead>tr>th{border-bottom-width:2px}.table-striped>tbody>tr:nth-child(odd)>td,.table-striped>tbody>tr:nth-child(odd)>th{background-color:#eaeaea}.table-hover>tbody>t');
    LResult.Append('r:hover>td,.table-hover>tbody>tr:hover>th{background-color:#f5f5f5}');
    LResult.Append('</style>');
    LResult.Append('</head><body>');

    if GetErrorCount > 0 then
    begin
      LResult.Append('<p style="color: red"><b>Errors: ' + FloatToStr(GetErrorCount) + '<b><br></p>');
    end
    else
    begin
      LResult.Append('<p>No errors<br></p>');
    end;

    LResult.Append('<table class="table table-responsive table-bordered table-hover table-striped" style="font-size: 14px;">');
    LResult.Append('<thead><tr><th class="col-ok-1">#</th><th class="col-ok-2">Date</th><th class="col-ok-2">' + 'Type</th class="col-ok-6"><th>Source / Messsage</th><th class="col-ok-3">Operation</th><th class="col-ok-6">Destination</th><th class="col-ok-4">Reason</th></tr></thead><tbody>');

    for I := 0 to FLogItems.Count - 1 do
    begin
      LResult.Append('<tr>');
      LResult.Append('<td class="col-ok-1">' + FloatToStr(i + 1) + '</td>');
      LResult.Append('<td class="col-ok-2">' + FLogItems[i].AddDate + '</td>');
      if FLogItems[i].LogType = 'Error' then
      begin
        LColor := 'red';
      end
      else if FLogItems[i].LogType = 'Info' then
      begin
        LColor := 'blue';
      end
      else if FLogItems[i].LogType = 'Success' then
      begin
        LColor := 'green';
      end
      else if FLogItems[i].LogType = 'Skip' then
      begin
        LColor := 'maroon';
      end
      else
      begin
        LColor := 'black';
      end;
      LResult.Append('<td class="col-ok-2" style="color: ' + LColor + '">' + FLogItems[i].LogType + '</td><td class="col-ok-6">' + FLogItems[i].Source + '</td><td class="col-ok-3">' + FLogItems[i].Operation + '</td><td class="col-ok-6">' + FLogItems[i].Destination + '</td><td class="col-ok-4">' + FLogItems[i].Reason + '</td></tr>');
    end;

    LResult.Append('</tbody></table></body></html></head>');
    Result := LResult.ToString;
  finally
    LResult.Free;
  end;
end;

function TLogFile.GetAsString: string;
var
  LResult: TStringList;
  I: Integer;
begin
  LResult := TStringList.Create;
  try
    for I := 0 to FLogItems.Count - 1 do
    begin
      with FLogItems[i] do
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
  Result := FLogItems.Count;
end;

function TLogFile.GetErrorCount: integer;
var
  LCounter: integer;
  I: Integer;
begin
  LCounter := 0;
  for I := 0 to FLogItems.Count - 1 do
  begin
    if FLogItems[i].LogType = 'Error' then
    begin
      Inc(LCounter);
    end;
  end;
  Result := LCounter;
end;

function TLogFile.GetInfoCount: integer;
var
  LCounter: integer;
  I: Integer;
begin
  LCounter := 0;
  for I := 0 to FLogItems.Count - 1 do
  begin
    if FLogItems[i].LogType = 'Info' then
    begin
      Inc(LCounter);
    end;
  end;
  Result := LCounter;
end;

function TLogFile.GetReadCount: integer;
var
  LCounter: integer;
  I: Integer;
begin
  LCounter := 0;
  for I := 0 to FLogItems.Count - 1 do
  begin
    if FLogItems[i].LogType = 'Skip' then
    begin
      Inc(LCounter);
    end;
  end;
  Result := LCounter;
end;

function TLogFile.GetSuccessCount: integer;
var
  LCounter: integer;
  I: Integer;
begin
  LCounter := 0;
  for I := 0 to FLogItems.Count - 1 do
  begin
    if FLogItems[i].LogType = 'Success' then
    begin
      Inc(LCounter);
    end;
  end;
  Result := LCounter;
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
          FLogItems.Add(LItem);
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

procedure TLogFile.WriteToFile(const FilePath: string; const IsHtml: Boolean);
var
  LSW: TStreamWriter;
  I: Integer;
  LLine: string;
  LItem: TLogItem;
begin
  LSW := TStreamWriter.Create(FilePath, True, TEncoding.UTF8);
  try
    if not IsHtml then
    begin
      // csv
      for I := 0 to FLogItems.Count - 1 do
      begin
        LItem := FLogItems[i];
        with LItem do
        begin
          LLine := LogType + ';' + AddDate + ';' + Source + ';' + Destination + ';' + Operation + ';' + Reason;
        end;
        LSW.WriteLine(LLine);
      end;
    end
    else
    begin
      // html
      LSW.Write(GetAsHtml);
    end;
  finally
    LSW.Close;
    LSW.Free;
  end;
end;

end.

