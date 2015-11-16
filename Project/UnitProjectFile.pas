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
    CompareMethod: integer;
  end;

  TProjectFiles = TList<TProjectFile>;

implementation

end.
