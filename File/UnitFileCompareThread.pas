unit UnitFileCompareThread;

interface

uses
  IdBaseComponent, IdThreadComponent, IdThread, Classes, UnitFileCompare,
  Windows, StrUtils, SysUtils, UnitFileCopyPair;

type
  TFileCompareThread = class(TObject)
  private
    FThread: TIdThreadComponent;
    FFilesToCompare: TStringList;
    FRunning: Boolean;
    FFileComparetor: TFileComperator;
    FFilesToCopy: TFileCopyPairs;
    FSourceDir: string;
    FDestDir: string;
    FBuffer: Integer;
    FStop: Boolean;
    FProgress: integer;
    FStateMsg: string;

    procedure ThreadRun(Sender: TIdThreadComponent);
    procedure ThreadStopped(Sender: TIdThreadComponent);
    procedure ThreadTerminate(Sender: TIdThreadComponent);
  public
    property FilesToCompare: TStringList read FFilesToCompare
      write FFilesToCompare;
    property Running: Boolean read FRunning;
    property FilesToCopy: TFileCopyPairs read FFilesToCopy;
    property SourceDir: string read FSourceDir write FSourceDir;
    property DestDir: string read FDestDir write FDestDir;
    property BufferLength: integer read FBuffer write FBuffer;
    property Progress: integer read FProgress;
    property StateMsg: string read FStateMsg;

    constructor Create;
    destructor Destroy; override;

    procedure Start;
    procedure Stop;
  end;

implementation

{ TFileCompareThread }

constructor TFileCompareThread.Create;
begin
  FThread := TIdThreadComponent.Create;
  FThread.StopMode := smTerminate;
  FThread.OnRun := ThreadRun;
  FThread.OnStopped := ThreadStopped;
  FThread.OnTerminate := ThreadTerminate;

  FFilesToCompare := TStringList.Create;
  FFilesToCopy := TFileCopyPairs.Create;
  FRunning := False;
  FFileComparetor := TFileComperator.Create;
end;

destructor TFileCompareThread.Destroy;
begin
  FFilesToCompare.Free;
  FFileComparetor.Free;
  FFilesToCopy.Free;
  inherited;
end;

procedure TFileCompareThread.Start;
begin
  FFilesToCopy.Clear;
  FStop := False;
  FProgress := 0;

  FThread.Start;
end;

procedure TFileCompareThread.Stop;
begin
  FStop := True;
  while not FThread.Terminated do
  begin
    FThread.Terminate;
  end;
end;

procedure TFileCompareThread.ThreadRun(Sender: TIdThreadComponent);
var
  I: Integer;
  LSourceFile, LDestFile: string;
  LCopyPair: TFileCopyPair;
begin
  FRunning := True;
  for I := 0 to FFilesToCompare.Count - 1 do
  begin
    if FStop then
      Break;
    FProgress := i + 1;
    LSourceFile := FFilesToCompare[i];
    LDestFile := LSourceFile.Replace(FSourceDir, FDestDir);
    FStateMsg := ' (' + LSourceFile + ')';
    if not FFileComparetor.CompareFiles(LSourceFile, LDestFile, FBuffer, 0) then
    begin
      LCopyPair.SourceFile := LSourceFile;
      LCopyPair.DestFile := LDestFile;
      FFilesToCopy.Add(LCopyPair);
    end;
  end;
  FRunning := False;
  FFilesToCompare.Clear;
  FProgress := 0;
  FThread.Terminate;
end;

procedure TFileCompareThread.ThreadStopped(Sender: TIdThreadComponent);
begin
  FRunning := False;
end;

procedure TFileCompareThread.ThreadTerminate(Sender: TIdThreadComponent);
begin
  FRunning := False;
end;

end.
