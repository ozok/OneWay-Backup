unit UnitProjectFile;

interface

uses Classes, IniFiles, Generics.Collections;

type
  TProjectFile = class
  private
    FSourceFolder: string;
    FDestFolder: string;
    FProjectName: string;
    FIniFilePath: string;
  public
    property SourceFolder: string read FSourceFolder write FSourceFolder;
    property DestFolder: string read FDestFolder write FDestFolder;
    property ProjectName: string read FProjectName write FProjectName;
    property IniFilePath: string read FIniFilePath;

    constructor Create(const IniFilePath: string);
    destructor Destroy; override;

    procedure Save();
  end;

  TProjectFiles = TList<TProjectFile>;

implementation

{ TProjectFile }

constructor TProjectFile.Create(const IniFilePath: string);
var
  LIniFile: TIniFile;
begin
  FIniFilePath := IniFilePath;

  LIniFile := TIniFile.Create(FIniFilePath);
  try
    with LIniFile do
    begin
      FSourceFolder := ReadString('Location', 'Source', '');
      FDestFolder := ReadString('Location', 'Dest', '');
      FProjectName := ReadString('General', 'Name', '');
    end;
  finally
    LIniFile.Free;
  end;
end;

destructor TProjectFile.Destroy;
begin

  inherited;
end;

procedure TProjectFile.Save();
var
  LIniFile: TIniFile;
begin
  LIniFile := TIniFile.Create(FIniFilePath);
  try
    with LIniFile do
    begin
      WriteString('Location', 'Source', FSourceFolder);
      WriteString('Location', 'Dest', FDestFolder);
      WriteString('General', 'Name', FProjectName);
    end;
  finally
    LIniFile.Free;
  end;
end;

end.
