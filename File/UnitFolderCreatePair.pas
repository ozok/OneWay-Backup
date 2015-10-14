unit UnitFolderCreatePair;

interface

uses Generics.Collections;

type
  TFolderCreatePair = record
    Directory: string;
    Attributes: integer;
  end;

  TFolderCreatePairs = TList<TFolderCreatePair>;

implementation

end.
