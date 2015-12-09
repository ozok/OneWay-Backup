unit UnitLogItems;

interface

uses Classes, IniFiles, Generics.Collections, System.SysUtils, Nvv.IO.CSV.Delphi.NvvCSVClasses;

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
    procedure Add(const Item: TLogItem);
  end;

implementation

{ TLogFile }

procedure TLogFile.Add(const Item: TLogItem);
begin
  LogItems.Add(Item);
end;

constructor TLogFile.Create;
begin
  LogItems := TList<TLogItem>.Create;
end;

destructor TLogFile.Destroy;
begin
  LogItems.Free;
end;

function TLogFile.GetAsString: string;
var
  LResult: TStringList;
  I: Integer;
begin
  LResult := TStringList.Create;
  try
    for I := 0 to LogItems.Count-1 do
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
  LCSVReader: TnvvCSVFileReader;
begin
  try
    LCSVReader := TnvvCSVFileReader.Create();
    try
      LCSVReader.SetFile(FilePath, TEncoding.UTF8);
      LCSVReader.HeaderPresent := False;
      LCSVReader.Open;
      while not LCSVReader.Eof do
      begin
//        if LCSVReader.FieldCount = 6 then
        begin
          with LItem do
          begin
            LogType := LCSVReader.Fields[0].Value;
            AddDate := LCSVReader.Fields[1].Value;
            Source := LCSVReader.Fields[2].Value;
            Destination := LCSVReader.Fields[3].Value;
            Operation := LCSVReader.Fields[4].Value;
            Reason := LCSVReader.Fields[5].Value;
          end;
          LogItems.Add(LItem);
        end;
        LCSVReader.Next;
      end;
      LCSVReader.Close;
    finally
      LCSVReader.Free;
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
  try
    LSW := TStreamWriter.Create(FilePath, True, TEncoding.UTF8);
    for I := 0 to LogItems.Count-1 do
    begin
      LItem := LogItems[i];
      with LItem do
      begin
        LLine := LogType + ';' + AddDate + ';' + Source + ';' + Destination + ';' + Operation + ';' + Reason;
      end;
      LSW.WriteLine(LLine);
    end;
  except on E: Exception do
  end;
end;

end.
