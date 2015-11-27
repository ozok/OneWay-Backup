unit UnitFolderCreatePair;

interface

uses Generics.Collections, IOUtils;

type
  TFolderCreatePair = record
    Directory: string;
    Attributes: TFileAttributes;
  end;

  TFolderCreatePairs = TList<TFolderCreatePair>;

implementation

end.
