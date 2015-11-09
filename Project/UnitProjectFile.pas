unit UnitProjectFile;

interface

uses Classes, IniFiles, Generics.Collections;

type
  TProjectFile = class
    SourceFolder: string;
    DestFolder: string;
    ProjectName: string;
    Active: Boolean;
    DeleteFromDest: Boolean;
    BufferSize: integer;
    IgnoredFileTypes: string;
  end;

  TProjectFiles = TList<TProjectFile>;

implementation

end.
